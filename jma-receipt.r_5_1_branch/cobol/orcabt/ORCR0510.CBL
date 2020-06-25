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
      *
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCR0510.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（自賠責）
      *  コンポーネント名  : 月次業務　自賠責明細書ファイル作成
      *                      ＯＣＲ様式
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/12/05    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-門脇    14/10/15  療養期間の記載修正
      *  04.07.02    NACL-門脇    15/03/09  症状詳記９０の対応
      *  04.07.03    NACL-門脇    15/09/08  自賠責（新様式）（ＯＣＲ様式）
      *                                     受領金額欄記載対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    15/08/27  自賠責レセプトの請求金額欄端数処理
      *                                     対応
      *  04.08.03    NACL-門脇    16/01/12  点検用レセプト院外処方一般名記載対応
      *  04.08.04    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.05    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE41-FILE     ASSIGN  RECE41PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE41-KEY
                                   FILE    STATUS  IS  STS-RECE41.
      *                             
           SELECT  RECE43-FILE     ASSIGN  RECE43PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE43-KEY
                                   FILE    STATUS  IS  STS-RECE43.
      *   
           SELECT  RECE47-FILE     ASSIGN  RECE47PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE47-KEY
                                   FILE    STATUS  IS  STS-RECE47.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
       FD  RECE43-FILE.
       01  RECE43-REC.
           COPY    "CPRCF043.INC".
      *
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE43//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
           COPY    "HCM76.INC".
      *
      *    続紙
           COPY    "HCM77.INC".
      *
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE43          PIC X(02).
           03  STS-RECE47          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE41          PIC 9(01).
           03  FLG-RECE43          PIC 9(01).
           03  FLG-RECE47          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-JIBAI-PRT       PIC 9(01).
      *
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-STS             PIC 9(01).
           03  FLG-DB-DELETE       PIC 9(01).
      *
           03  FLG-NEXT            PIC 9(01).
           03  FLG-MARK            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE47          PIC 9(06).
           03  CNT-RECE09          PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
           03  CNT-PDF             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDX4                PIC 9(02).
           03  IDX5                PIC 9(02).
           03  IDX6                PIC 9(02).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(03).
           03  IDXY                PIC 9(03).
      *
           03  IDXX                PIC 9(02).
           03  IDYY                PIC 9(02).
           03  IDZZ                PIC 9(05).
      *
           03  SYSKAN-IDX          PIC 9(03).
      *
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE.          
           03  KEY-NEW.
               05  KEY-N-HOSPNUM   PIC  9(02).
               05  KEY-N-SRYYM     PIC  9(06).
               05  KEY-N-NYUGAIKBN PIC  X(01).
               05  KEY-N-PTID      PIC  9(10).
               05  KEY-N-RECEKA    PIC  X(02).
           03  KEY-OLD.
               05  KEY-O-HOSPNUM   PIC  9(02).
               05  KEY-O-SRYYM     PIC  9(06).
               05  KEY-O-NYUGAIKBN PIC  X(01).
               05  KEY-O-PTID      PIC  9(10).
               05  KEY-O-RECEKA    PIC  X(02).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001     PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC     OCCURS  100.
               05  TBL-SYS1002     PIC  X(610).
      *    帳票別医療機関編集情報指定退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC         OCCURS  100.
               05  TBL-SYS1900         PIC  X(610).
      *
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC     OCCURS  100.
               05  TBL-SYS4001     PIC X(610).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-CREYMD  PIC  X(08).
               05  WRK-PARA-CREHMS  PIC  X(08).
               05  WRK-PARA-SRYYM   PIC  X(06).
               05  WRK-PARA-TERMID  PIC  X(16).
               05  WRK-PARA-SYSYMD  PIC  X(08).
               05  WRK-PARA-SYOKBN  PIC  X(01).
               05  WRK-PARA-RECEKBN PIC  X(01).
               05  WRK-PARA-RECEYM  PIC  X(06). 
               05  WRK-PARA-JOBID   PIC  9(07).
               05  WRK-PARA-SHELLID PIC  X(08).
           03  WRK-PARA-FRAME       PIC  X(01).
           03  WRK-PARA-PRTKBN      PIC  X(01).
           03  WRK-PARA-DLTKBN      PIC  X(01).
           03  WRK-PARA-HOSPNUM     PIC  9(02).
      *
           03  WRK-SYS-SRYYMD       PIC  X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RECENUM          PIC 9(06). 
      *
           03  WRK-NENREI          PIC 9(05).
           03  WRK-NENREI-R        REDEFINES   WRK-NENREI.
               05  WRK-NENREI-R1   PIC 9(03).
               05  WRK-NENREI-R2   PIC 9(02).
           03  WRK-BIRTHDAY.
               05  WRK-BIRTHYM     PIC 9(06).
               05  WRK-BIRTHDD     PIC 9(02). 
      *
           03  WRK-LINE-MAX        PIC 9(03).
           03  WRK-MOJISU-MAX      PIC 9(05).
      *
           03  WRK-GOKEI           PIC 9(07).            
      *
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-STYMDW          PIC X(07).
           03  WRK-EDYMDW          PIC X(07).
           03  WRK-CREYMDW         PIC X(07).
      *
           03  WRK-DEL-PTID        PIC 9(10).
           03  WRK-DEL-SRYYM       PIC X(06).
      *
           03  WRK-SINRYO-MM.
               05  WRK-SINRYO-MM2  PIC 9(02).
           03  WRK-KYUGYO-MM.
               05  WRK-KYUGYO-MM2  PIC 9(02).
      *
           03  WRK-OPECOM-MCP-TABLE
                                   PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME
                                   PIC X(64).
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-TEL             PIC X(15).
      *
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-CNT             PIC 9(10).
           03  WRK-CNT-X           REDEFINES   WRK-CNT
                                   PIC X(10).
      *
           03  WRK-EDIT-AREA.
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(04).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(04).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(04).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(04).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行（点数）               
               05  WRK-LINETEK                 PIC 9(03).
      *         
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1.
               05  WRK-BYOMEI1-NAME            PIC X(192).
               05  WRK-BYOMEI1-NAME-R  REDEFINES   WRK-BYOMEI1-NAME.
                   07  WRK-BYOMEI1-NAME-T      PIC X(48)   OCCURS  4.
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2.  
               05  WRK-BYOMEI2-NAME            PIC X(3390).
               05  WRK-BYOMEI2-NAME-R  REDEFINES   WRK-BYOMEI2-NAME.
                   07  WRK-BYOMEI2-NAME-T      PIC X(30)   OCCURS  113.
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT     PIC X(28)
                                       OCCURS  86.
           03  WRK-COMM            PIC X(2400).
           03  WRK-COMM1           PIC X(2400).
           03  WRK-COL             PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-RECECOM-NAME    PIC X(56).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
           03  WRK-RECECOM-SJKBN   PIC X(02).
      *
           03  WRK-TEKIYOU-G.
               05  WRK-TEKIYOU-T   PIC X(28)
                                       OCCURS  86.
           03  WRK-TEKIYOU-MAX     PIC 9(03).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE2       PIC Z9.
      *
       01  WRK-ERR-AREA.
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
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG.
               05  WRK-HENGENGO    PIC X(01).
               05  WRK-HENYMD.
                   07  WRK-HENYY   PIC X(02).
                   07  WRK-HENMM   PIC X(02).
                   07  WRK-HENDD   PIC X(02).
      *
      *    レセプト格納用
       01  TABLE-AREA                          VALUE   SPACE.
           03  TBL-RECE-MAX        PIC 9(02).
           03  TBL-RECE-OCC            OCCURS  99.
               05  TBL-RECE-PRTDATA
                                   PIC X(10000).
               05  TBL-RECE-FORMID PIC X(20). 
               05  TBL-RECE-RENNUM PIC 9(02).
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTENZ         PIC ZZ.
           03  WRK-KAISUZ          PIC ZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZ.
           03  WRK-TENSUZ2         PIC ZZZZZZ9.
           03  WRK-JNISSU          PIC ZZ9.
           03  WRK-TOTALTEN        PIC ZZZ,ZZZ.
           03  WRK-NENREI-X        PIC ZZ9.
           03  WRK-KINGAKUZ        PIC ZZZZZZ.
           03  WRK-ZZZ9            PIC ZZZ9.
           03  WRK-SEIKYUZ         PIC Z,ZZZ,ZZ9.
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-KGK4            PIC ZZZZ.
           03  WRK-KGK2            PIC ZZ.
      *
           03  WRK-TENSUX          PIC X(07).
           03  WRK-TENSUX-R        REDEFINES   WRK-TENSUX.
               05  WRK-TENSUX-T    PIC X(01)   OCCURS  7.
      *
       01  WRK-CONS-AREA.    
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   86.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
           03  WRK-CONS-SRYYM-201707-X.
               05  WRK-CONS-SRYYM-201707
                                   PIC 9(06)   VALUE   201707.
      *
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(03)   VALUE   192.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(04)   VALUE   930.
      *    傷病名最大文字数（続紙）
           03  WRK-BYOMEI2-HCM77-MAX
                                   PIC 9(04)   VALUE   3390.
      *
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   4.
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   31.
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HCM77-LINE
                                   PIC 9(03)   VALUE   113.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *   症状詳記区分の表題
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(56)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  FILLER              PIC X(56)   VALUE
               "【廃用症候群に係る評価表】".
           03  FILLER              PIC X(56)   VALUE
               "【上記以外の診療報酬明細書】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(56)   OCCURS   3.
      *
           COPY    "COMMON-SPA".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
           COPY    "CPSK1010.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
           COPY    "CPSK200501.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
      *    労災自賠医療機関情報
           COPY    "CPSK4001.INC".
      *
           COPY    "CPRCF008.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    手術歴レセプトコメントマスタ
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
       01  JIBAI-PRT-REC.
           COPY    "CPJIBAI-PRT.INC".
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      *****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(1000).
      *****************************************************************
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
           PERFORM 200-MAIN-SEC
                   UNTIL       FLG-STS     =   2
      *
           PERFORM 300-END-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               STOP    RUN
           ELSE
               EXIT    PROGRAM
           END-IF 
           .
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  TABLE-AREA
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
           INITIALIZE                  RECEERR
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-FRAME
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-DLTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
      *
           DISPLAY "WRK-PARA-HOSPNUM   =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN    =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM     =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN    =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-DLTKBN    =" WRK-PARA-DLTKBN
           DISPLAY "WRK-PARA-FRAME     =" WRK-PARA-FRAME
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ファイルＯＰＥＮ処理
           MOVE    SPACE               TO  RECE41PARA
           INITIALIZE                      RECE41PARA
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE43PARA
           INITIALIZE                      RECE43PARA
           MOVE    "RECE430"           TO  RECE43PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE43PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE43PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE47PARA
           INITIALIZE                      RECE47PARA
           MOVE    "RECE470"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE43PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE43PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE47PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0510"      TO  JOB-PGID
               MOVE    "自賠責保険・印刷データ編集（自賠責）"
                                   TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
      *
      *        処理中止設定処理
               IF      FLG-STS             =   ZERO
                   PERFORM 500-CANCEL-HENSYU-SEC
               END-IF
      *         
      *        ジョブ管理更新（中止フラグ非活性）処理
               IF      FLG-STS             =   ZERO
                   MOVE    "STP"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    3               TO  JOB-STOPFLG
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *
      *    システム管理マスタ（医療機関情報）
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
           AND   ( FLG-STS             =   ZERO      )
               PERFORM 5001-DB-DELETE-SEC
           END-IF
      *
      *    ファイルＯＰＥＮ処理
           OPEN    INPUT               RECE41-FILE
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE43-FILE
           IF      STS-RECE43          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE43PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE43          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE47-FILE
           IF      STS-RECE47          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE47PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE47          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    ZERO                TO  WRK-RECENUM
           ELSE
               MOVE    100                 TO  WRK-RECENUM
           END-IF
      *
           PERFORM 990-RECE41-READ-SEC
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *          
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1001"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1001-T
              INITIALIZE                      TBL-SYS1001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関情報（所在地ほか）
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1002"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1002-REC
              ELSE
             MOVE   "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1002-T
              INITIALIZE                      TBL-SYS1002-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1002"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1002-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1002-REC  TO  TBL-SYS1002(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-SYS-1900
                                           FLG-SYS-1901
                                           FLG-SYS-1902
      *
      *    帳票別指定情報
           IF   WRK-PARA-SRYYM     NOT =  ZERO
              INITIALIZE                   SYSKANRI-REC
              MOVE    "1900"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-1900-REC
              ELSE    
                  INITIALIZE                   SYS-1900-REC
              END-IF
              IF      SYS-1900-PRTKBN (3)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS1900-T
              INITIALIZE                      TBL-SYS1900-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "1900"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
              MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-PRTKBN (3)  =   SPACE
                        CONTINUE
                    ELSE
                        ADD     1             TO  SYSKAN-IDX
                        MOVE    SYS-1900-REC  TO
                                              TBL-SYS1900 (SYSKAN-IDX)
                        MOVE    1            TO  FLG-SYS-1900
                    END-IF 
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
                  END-PERFORM
              END-IF
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF 
      *
      *    自賠責区分取得
           IF      WRK-PARA-SRYYM  NOT =   ZERO
               INITIALIZE                   SYSKANRI-REC
               MOVE    "4001"           TO  SYS-KANRICD
               MOVE    "*"              TO  SYS-KBNCD
               MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
               MOVE    "01"             TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
               MOVE    "key10"          TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-4001-REC
                   IF     SYS-4001-JIBAIKBN    =   SPACE
                       MOVE    "1"                 TO  SYS-4001-JIBAIKBN
                   END-IF
               ELSE
                   MOVE   "労災自賠医療機関情報が取得できませんでした"
                                                   TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               END-IF
           ELSE
               MOVE     ZERO               TO  SYSKAN-IDX
               MOVE     SPACE              TO  TBL-SYS4001-T
               INITIALIZE                      TBL-SYS4001-T
      *
               INITIALIZE                      SYSKANRI-REC
               MOVE    "4001"              TO  SYS-KANRICD
               MOVE    "*"                 TO  SYS-KBNCD
               MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
               MOVE    SYSKANRI-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM  900-KANRIMST-NEXT-SEC
                   PERFORM UNTIL ( FLG-SYSKANRI    =   1   )
                           OR    ( SYSKAN-IDX      >=  100 )
                       MOVE    MCPDATA-REC         TO  SYS-4001-REC
                       ADD     1                   TO  SYSKAN-IDX
                       MOVE    SYS-4001-REC        TO
                                                TBL-SYS4001 (SYSKAN-IDX)
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key8"              TO  MCP-PATHNAME
                       PERFORM  900-KANRIMST-NEXT-SEC
                   END-PERFORM
               ELSE
                   MOVE    "労災自賠医療機関情報が取得できませんでした"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
              END-IF
      *
              MOVE    "tbl_syskanri"       TO  MCP-TABLE
              MOVE    "key8"               TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
           END-IF
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA          =   1
                   MOVE    "パラメタ情報が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *         
      *    レセプト編集区分情報
           MOVE    SPACE           TO  SYSKANRI-REC
           INITIALIZE                  SYSKANRI-REC
           MOVE    "2005"          TO  SYS-KANRICD
           MOVE    "01"            TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM  TO  SYS-STYUKYMD (1:6)
           MOVE    "01"            TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD    TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"  TO  WRK-MCP-TABLE
           MOVE    "key10"         TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-200501-REC
           ELSE    
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-INSPECT-CHARACTER-KBN
                                   =   SPACE
               MOVE    ZERO            TO
                                       SYS-200501-INSPECT-CHARACTER-KBN
           END-IF
           IF      SYS-200501-JIBAI-PDF
                                   =   SPACE
               MOVE    "0"             TO  SYS-200501-JIBAI-PDF
           END-IF
      *
      *    作成日和暦編集
           MOVE    WRK-PARA-CREYMD     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMDG         TO  WRK-CREYMDW
      *
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE41     =   1
                           OR      FLG-STS        =   2 
      *
      *        患者情報設定処理
               PERFORM 2001-PTINF-HEN-SEC 
      *            レセプト種別別件数カウント
               MOVE    ZERO            TO  CNT-KENSU
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE41     =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW    NOT =   KEY-OLD
      *
                   ADD     1                      TO  CNT-KENSU
      *                                      
      *            帳票編集処理
                   PERFORM 2002-SYORI-SEC
      *
                   PERFORM 990-RECE41-READ-SEC
               END-PERFORM
           END-PERFORM
      *
           MOVE    2               TO  FLG-STS
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者情報設定処理
      *****************************************************************
       2001-PTINF-HEN-SEC       SECTION.
      *     
      *    診療年月編集
           MOVE    RECE41-SRYYM        TO  WRK-STYMD (1:6)
           MOVE    "01"                TO  WRK-STYMD (7:2)
           MOVE    WRK-STYMD           TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMDG         TO  WRK-STYMDW
      *     
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE41-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-EDYMD 
           MOVE    WRK-EDYMD           TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMDG         TO  WRK-EDYMDW
           . 
       2001-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-SYORI-SEC              SECTION.
      *
           MOVE    1               TO  CNT-PAGE
           INITIALIZE                  WRK-EDIT-AREA
           INITIALIZE                  TABLE-AREA
      *
      *    帳票編集（ヘッダー部）処理
           PERFORM 20021-HCM76-HEAD-HEN-SEC
      *
      *    帳票編集（点数）処理
           PERFORM 20022-TENSU-HEN-SEC
      *
      *    帳票編集（傷病名）処理
           PERFORM 20023-BYOMEI-HEN-SEC
      *
      *    帳票編集（手術歴）処理
           PERFORM 20024-OPECOM-HEN-SEC
      *
      *    帳票編集（適用）処理
           PERFORM 20025-TEKIYOU-HEN-SEC
      *
      *    帳票編集（コメント）処理
           PERFORM 20026-COMMENT-HEN-SEC
      *
      *    摘要欄の終わり
           ADD     1               TO  WRK-LINETEK
           IF      CNT-PAGE    =   1 
               MOVE    "Ｅ"            TO  HCM76-MARK   (WRK-LINETEK)
               MOVE    "終わり"        TO  HCM76-TEKIYO (WRK-LINETEK)
           ELSE
               MOVE    "Ｅ"            TO  HCM77-MARK   (WRK-LINETEK)
               MOVE    "終わり"        TO  HCM77-TEKIYO (WRK-LINETEK)
           END-IF
      *
      *    帳票印刷処理（続紙）
           IF      CNT-PAGE        >   1
               PERFORM 20028-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      FLG-STS         =   ZERO
               MOVE    1               TO  CNT-PAGE
               PERFORM 20028-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票更新処理
           PERFORM 20029-DB-WRITE-SEC
      *
           .
       2002-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集（ＪＫ２１ヘッダー部）処理
      *****************************************************************
       20021-HCM76-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-LINE    TO  WRK-LINE-MAX
           MOVE    WRK-BYOMEI2-MAX     TO  WRK-MOJISU-MAX
      *     
           INITIALIZE                      HCM76
      *
      *    患者番号
           STRING   RECE41-PTNUM       DELIMITED   BY  SPACE
                    " "                DELIMITED   BY  SIZE
                    RECE41-RECEKA      DELIMITED   BY  SPACE
                                       INTO        HCM76-PTNUM
           END-STRING
      *    診療年月
           MOVE     WRK-STYMDW(1:1)    TO  HCM76-HAKKOUG
           MOVE     WRK-STYMDW(2:2)    TO  HCM76-HAKKOUYY
           MOVE     WRK-STYMDW(4:2)    TO  HCM76-HAKKOUMM 
      *    氏名
           STRING   RECE41-NAME        DELIMITED   BY  SPACE
                    "　　"             DELIMITED   BY  SIZE
                    RECE41-KANANAME    DELIMITED   BY  SPACE
                                       INTO        HCM76-NAME
           END-STRING
      *    性別
           EVALUATE    RECE41-SEX
               WHEN    "1"
                   MOVE    "レ"        TO  HCM76-SEX (1)
               WHEN    "2"
                   MOVE    "レ"        TO  HCM76-SEX (2)
           END-EVALUATE     
      *    生年月日
           MOVE    RECE41-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCM76-TANJYOG
           MOVE    WRK-HENYY           TO  HCM76-TANJYOYY
           MOVE    WRK-HENMM           TO  HCM76-TANJYOMM
           MOVE    WRK-HENDD           TO  HCM76-TANJYODD
      *    年齢
           MOVE    RECE41-BIRTHDAY     TO  WRK-BIRTHDAY    
           COMPUTE WRK-NENREI          =   RECE41-SRYYM
                                       -   WRK-BIRTHYM
           MOVE    WRK-NENREI-R1       TO  WRK-NENREI-X
           MOVE    WRK-NENREI-X        TO  HCM76-NENREI 
      *    実日数
           MOVE    RECE41-JNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HCM76-JITSUNISSU
      *    転帰
           EVALUATE    RECE41-TENKI
               WHEN    "1"
                   MOVE    "レ"        TO  HCM76-TENKI(1)
               WHEN    "3"
                   MOVE    "レ"        TO  HCM76-TENKI(2)
               WHEN    "5"
                   MOVE    "レ"        TO  HCM76-TENKI(3)
               WHEN    "7"
                   MOVE    "レ"        TO  HCM76-TENKI(4)
               WHEN    "9"
                   MOVE    "レ"        TO  HCM76-TENKI(5)
           END-EVALUATE
      *    受傷日
           MOVE    RECE41-SHOBYOYMD    TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCM76-JYUSYOUBIG
           MOVE    WRK-HENYY           TO  HCM76-JYUSYOUBIYY
           MOVE    WRK-HENMM           TO  HCM76-JYUSYOUBIMM
           MOVE    WRK-HENDD           TO  HCM76-JYUSYOUBIDD
      *    初診日
           MOVE    RECE41-SHOSHINYMD   TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCM76-SYOSHINBIG
           MOVE    WRK-HENYY           TO  HCM76-SYOSHINBIYY
           MOVE    WRK-HENMM           TO  HCM76-SYOSHINBIMM
           MOVE    WRK-HENDD           TO  HCM76-SYOSHINBIDD
      *
      *    診療期間
           IF      RECE41-RYOSTYMD     <   WRK-STYMD
               MOVE    WRK-STYMDW(1:1)     TO  HCM76-STRG
               MOVE    WRK-STYMDW(2:2)     TO  HCM76-STRYY
               MOVE    WRK-STYMDW(4:2)     TO  HCM76-STRMM
               MOVE    WRK-STYMDW(6:2)     TO  HCM76-STRDD
               MOVE    WRK-STYMD(7:2)      TO  IDX5
           ELSE      
               MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENGENGO        TO  HCM76-STRG
               MOVE    WRK-HENYY           TO  HCM76-STRYY
               MOVE    WRK-HENMM           TO  HCM76-STRMM
               MOVE    WRK-HENDD           TO  HCM76-STRDD
               MOVE    RECE41-RYOSTYMD(7:2)
                                           TO  IDX5
           END-IF    
      *
           IF      RECE41-RYOEDYMD     >   WRK-EDYMD
               MOVE    WRK-EDYMDW(1:1)     TO  HCM76-ENDG 
               MOVE    WRK-EDYMDW(2:2)     TO  HCM76-ENDYY 
               MOVE    WRK-EDYMDW(4:2)     TO  HCM76-ENDMM
               MOVE    WRK-EDYMDW(6:2)     TO  HCM76-ENDDD
               MOVE    WRK-EDYMD(7:2)      TO  IDX6
           ELSE      
               MOVE    RECE41-RYOEDYMD     TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENGENGO        TO  HCM76-ENDG
               MOVE    WRK-HENYY           TO  HCM76-ENDYY
               MOVE    WRK-HENMM           TO  HCM76-ENDMM
               MOVE    WRK-HENDD           TO  HCM76-ENDDD
               MOVE    RECE41-RYOEDYMD(7:2)
                                           TO  IDX6
           END-IF    
      *
           IF    RECE41-NYUINFLG  =  1
      *      療養開始年月日再セット
             PERFORM   VARYING   IDX4   FROM   IDX5   BY  1
                       UNTIL     IDX4    >     IDX6
                   IF  (RECE41-NYUINBITBL(IDX4)  =  SPACE)  OR
                       (RECE41-JNISSUTBL(IDX4)   =  "1"  )
                       MOVE    IDX4            TO  WRK-ZZ
                       MOVE    WRK-ZZ          TO  HCM76-STRDD
                       MOVE    IDX6            TO  IDX4
                   END-IF
             END-PERFORM
      *      療養終了年月日再セット
             PERFORM   VARYING   IDX4   FROM   IDX6   BY  -1
                       UNTIL     IDX4    <     IDX5
                   IF  (RECE41-NYUINBITBL(IDX4)   =  SPACE) OR
                       (RECE41-JNISSUTBL(IDX4)    =  "1"  )
                       MOVE    IDX4            TO  WRK-ZZ
                       MOVE    WRK-ZZ          TO  HCM76-ENDDD
                       MOVE    IDX5            TO  IDX4
                   END-IF
             END-PERFORM
           END-IF
      *
           IF      RECE41-JNISSU       =   999
               IF    ( RECE41-KYUGYOFLG        =   SPACE )
               OR    ( RECE41-KYUGYO-PRTKBN    =   "2"   )
                   MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
                   PERFORM 31012-SE-WA-YMDEDIT-SEC
                   MOVE    WRK-HENGENGO        TO  HCM76-STRG
                                                   HCM76-ENDG
                   MOVE    WRK-HENYY           TO  HCM76-STRYY
                                                   HCM76-ENDYY
                   MOVE    WRK-HENMM           TO  HCM76-STRMM
                                                   HCM76-ENDMM
                   MOVE    WRK-HENDD           TO  HCM76-STRDD
                                                   HCM76-ENDDD
               ELSE
                   MOVE    RECE41-RYOSTYMD     TO  WRK-SYMD
                   PERFORM 31012-SE-WA-YMDEDIT-SEC
                   MOVE    WRK-HENGENGO        TO  HCM76-STRG
                   MOVE    WRK-HENYY           TO  HCM76-STRYY
                   MOVE    WRK-HENMM           TO  HCM76-STRMM
                   MOVE    WRK-HENDD           TO  HCM76-STRDD
      *
                   IF    ( RECE41-KYUGYO-END   NOT   NUMERIC )
                   OR    ( RECE41-KYUGYO-END   =  "0000"     )
                       CONTINUE
                  ELSE
                       MOVE  RECE41-SRYYM(5:2) TO  WRK-SINRYO-MM
                       MOVE  RECE41-KYUGYO-END(1:2)
                                               TO  WRK-KYUGYO-MM
                       IF      WRK-KYUGYO-MM2  <=  WRK-SINRYO-MM2
                           MOVE   RECE41-SRYYM (1:4)
                                                   TO  WRK-SYMD(1:4)
                           MOVE   RECE41-KYUGYO-END
                                                   TO  WRK-SYMD(5:4)
                           PERFORM 31012-SE-WA-YMDEDIT-SEC
                           MOVE    WRK-HENGENGO    TO  HCM76-ENDG
                           MOVE    WRK-HENYY       TO  HCM76-ENDYY
                           MOVE    WRK-HENMM       TO  HCM76-ENDMM
                           MOVE    WRK-HENDD       TO  HCM76-ENDDD
                       ELSE
                           INITIALIZE                  STS-AREA-DAY
                           INITIALIZE                  LNK-DAY6-AREA
                           MOVE    "61"            TO  LNK-DAY6-IRAI
                           MOVE    RECE41-SRYYM    TO
                                                   LNK-DAY6-KIJUN(1:6)
                           MOVE    "01"            TO
                                                   LNK-DAY6-KIJUN(7:2)
                           MOVE    "2"             TO
                                                   LNK-DAY6-ZOGENPTN
                           MOVE    -12             TO
                                                   LNK-DAY6-ZOGEN
                           CALL    "ORCSDAY"       USING   STS-AREA-DAY
                                                           LNK-DAY6-AREA
                           MOVE    LNK-DAY6-KEISAN(1:4)
                                                   TO  WRK-SYMD(1:4)
                           MOVE    RECE41-KYUGYO-END
                                                   TO  WRK-SYMD(5:4)
                           PERFORM 31012-SE-WA-YMDEDIT-SEC
                           MOVE    WRK-HENGENGO    TO  HCM76-ENDG
                           MOVE    WRK-HENYY       TO  HCM76-ENDYY
                           MOVE    WRK-HENMM       TO  HCM76-ENDMM
                           MOVE    WRK-HENDD       TO  HCM76-ENDDD
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    請求額の計算
      *
      *    加算率（Ａ、Ｃ）
           IF      RECE41-KSNRATE(1:1)  =  "1"
               CONTINUE
           ELSE
             MOVE  RECE41-KSNRATE(1:1) TO  HCM76-SEIKYU-SEISU(1)
                                           HCM76-SEIKYU-SEISU(2)
           END-IF
           MOVE    RECE41-KSNRATE(2:2) TO  HCM76-SEIKYU(1)
                                           HCM76-SEIKYU(2)
      *    請求額計
           IF    RECE41-JIBAI-GOKEI-BE  >  ZERO
               MOVE    RECE41-JIBAI-GOKEI-AF
                                           TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCM76-SEIKYUGAKU (5)
      *
      *        請求額Ａ
               MOVE    RECE41-TOTALKIN     TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCM76-SEIKYUGAKU (1)
      *        請求額Ｂ
               MOVE    RECE41-TOTALKIN-YAK TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCM76-SEIKYUGAKU (2)
      *        請求額Ｃ
               MOVE    RECE41-TOTALKIN2    TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCM76-SEIKYUGAKU (3)
      *        請求額Ｄ
               MOVE    RECE41-TOTALKIN2-ETC
                                           TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCM76-SEIKYUGAKU (4)
           END-IF
      *
      *    通院日
      *
      *    月
           MOVE    WRK-STYMDW (4:2)    TO  HCM76-TSUINMM (1) 
      *    「１〜３１」
           PERFORM VARYING IDZ     FROM    1   BY   1
                   UNTIL   IDZ     >       31
               IF      RECE41-TUUIN-DAY(IDZ)
                                       NOT =   SPACE
                   MOVE    "レ"                TO  HCM76-TSUIN (1 IDZ)
               END-IF
           END-PERFORM
      *    計                                         
           MOVE    RECE41-TUUIN-KEI     TO  WRK-Z9
           MOVE    WRK-Z9               TO  HCM76-TSUINSUU(1)
      *
      *    請求額
      *    保険会社名
           IF    RECE41-JIBAI-GOKEI-BE  >  ZERO
             IF    RECE41-JIBAI-SKYKINFLG  =  1
               MOVE  RECE41-JIBAI-SKYKIN    TO  WRK-SEIKYUZ
               MOVE  WRK-SEIKYUZ            TO  HCM76-SEIKYUGAKUTOTAL(1)
               MOVE    RECE41-HOKENNAME     TO  HCM76-HOKENNAME(1)
               INSPECT HCM76-HOKENNAME(1) REPLACING  ALL "  "  BY  "　"
             END-IF
             IF    RECE41-JIBAI-JYUKINFLG  =  1
               MOVE  RECE41-JIBAI-JYUKIN    TO  WRK-SEIKYUZ
               MOVE  WRK-SEIKYUZ            TO  HCM76-SEIKYUGAKUTOTAL(2)
               IF    RECE41-JIBAI-JYUNAMEFLG  =  1
                 MOVE  RECE41-JIBAI-JYUNAME TO  HCM76-HOKENNAME(2)
               ELSE
                 MOVE  RECE41-NAME          TO  HCM76-HOKENNAME(2)
               END-IF
               INSPECT HCM76-HOKENNAME(2) REPLACING  ALL "  "  BY  "　"
             END-IF
           END-IF
      *    作成日
           MOVE    WRK-CREYMDW (1:1)   TO  HCM76-SEIKYUG
           MOVE    WRK-CREYMDW (2:2)   TO  HCM76-SEIKYUYY
           MOVE    WRK-CREYMDW (4:2)   TO  HCM76-SEIKYUMM
           MOVE    WRK-CREYMDW (6:2)   TO  HCM76-SEIKYUDD
      *    病床数
           MOVE    SYS-1001-BEDSU      TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  HCM76-BYOSYOSU
      *    郵便番号
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-POST       TO  HCM76-HOSPPOST-G
           ELSE
               MOVE    SYS-1002-POST       TO  HCM76-HOSPPOST-G
           END-IF
      *    所在地
           IF      FLG-SYS-1902        =   1
               STRING   SYS-1902-ADRS1     DELIMITED   BY  SPACE
                        SYS-1902-ADRS2     DELIMITED   BY  SPACE
                                           INTO    HCM76-HOSPJYUSYO
               END-STRING
           ELSE
               MOVE    SYS-1002-ADRS       TO  HCM76-HOSPJYUSYO
           END-IF
           INSPECT HCM76-HOSPJYUSYO REPLACING   ALL "  "    BY  "　"
      *    名称
           IF      FLG-SYS-1901        =   1
               STRING   SYS-1901-HOSPNAME1 DELIMITED   BY  SPACE
                        SYS-1901-HOSPNAME2 DELIMITED   BY  SPACE
                                           INTO    HCM76-HOSPNAME
               END-STRING
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HCM76-HOSPNAME
           END-IF
           INSPECT HCM76-HOSPNAME   REPLACING   ALL "  "    BY  "　"
      *    電話番号
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-TEL        TO  WRK-TEL
           ELSE
               MOVE    SYS-1002-TEL        TO  WRK-TEL
           END-IF
           MOVE    ZERO                TO  IDZ
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       15
                   OR      WRK-TEL (IDX:1) =   SPACE
               EVALUATE    WRK-TEL (IDX:1)
                   WHEN    "0"
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
                   WHEN    "4"
                   WHEN    "5"
                   WHEN    "6"
                   WHEN    "7"
                   WHEN    "8"
                   WHEN    "9"
                       ADD     1                   TO  IDZ
                       MOVE    WRK-TEL (IDX:1)     TO  HCM76-TEL (IDZ:1)
               END-EVALUATE
           END-PERFORM
      *    医師名
           IF      RECE41-DRCD         =   SPACE
               MOVE    SYS-1001-KANRINAME  TO  HCM76-ISYA
           ELSE
      *        職員情報
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1010"              TO  SYS-KANRICD
               MOVE    RECE41-DRCD         TO  SYS-KBNCD
               MOVE    RECE41-SRYYM        TO  SYS-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME       TO  HCM76-ISYA
               END-IF
           END-IF
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  HCM76-HOSPCD
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN NOT =   "1"
               IF      SYS-200501-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HCM76-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HCM76-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HCM76-LOPTION
           END-IF
      *
           .
       20021-HCM76-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（ＪＫ２２ヘッダー部）処理
      *****************************************************************
       20021-HCM77-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-HCM77-LINE
                                       TO  WRK-LINE-MAX
           MOVE    WRK-BYOMEI2-HCM77-MAX
                                       TO  WRK-MOJISU-MAX
      *
           INITIALIZE                  HCM77
           MOVE    "JK22"              TO  HCM77-CYOHYONAME
           MOVE    "（入院外）"        TO  HCM77-NYUGAINAME
      *    診療年月
           MOVE     WRK-STYMDW(1:1)    TO  HCM77-HAKKOUG
           MOVE     WRK-STYMDW(2:2)    TO  HCM77-HAKKOUYY
           MOVE     WRK-STYMDW(4:2)    TO  HCM77-HAKKOUMM 
      *    氏名
           STRING   RECE41-NAME    DELIMITED   BY  SPACE
                    "　　"         DELIMITED   BY  SIZE
                    RECE41-KANANAME
                                   DELIMITED   BY  SPACE
                                   INTO        HCM77-NAME
           END-STRING
      *    患者番号
           STRING   RECE41-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE41-RECEKA  DELIMITED   BY  SPACE
                                   INTO    HCM77-PTNUM         
           END-STRING         
      *
      *    点検用レセプト作成時
           MOVE    HCM76-LABEL          TO  HCM77-LABEL
           MOVE    HCM76-LOPTION        TO  HCM77-LOPTION
      *
           .
       20021-HCM77-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    改ページ処理
      *****************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *    最終行の記載
           IF      CNT-PAGE        =   1
               MOVE    "Ｃ"            TO  HCM76-MARK   (32)
               MOVE    "次ページへ続く"
                                       TO  HCM76-TEKIYO (32)
           ELSE
               MOVE    "Ｃ"            TO  HCM77-MARK   (114)
               MOVE    "次ページへ続く"
                                       TO  HCM77-TEKIYO (114)
           END-IF
      *
      *    帳票印刷処理
           IF      CNT-PAGE        >   1
               PERFORM 20028-PRINT-OUT-SEC
           END-IF    
      * 
      *    帳票編集<ヘッダー部>処理
           ADD     1               TO  CNT-PAGE
           PERFORM 20021-HCM77-HEAD-HEN-SEC
           MOVE    ZERO            TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       20022-TENSU-HEN-SEC      SECTION.
      *
      *    診療内容・点数（点）
      *
      *    初診                           
           EVALUATE    RECE41-SHOSHINTIME
               WHEN    "1"
                   MOVE    "レ"        TO  HCM76-SYOSHIN (1)
               WHEN    "2"
                   MOVE    "レ"        TO  HCM76-SYOSHIN (2)
               WHEN    "3"
                   MOVE    "レ"        TO  HCM76-SYOSHIN (3)
           END-EVALUATE
      *    乳幼児
           IF      RECE41-SHOSHIN-NYU  =   "1"
               MOVE    "レ"            TO  HCM76-SYOSHIN (4)
           END-IF
      *
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL       IDX     >       33
               COMPUTE IDZ     =   IDX     -   1
      * 
      *        単価
               EVALUATE    IDX
      *            投薬
                   WHEN    10
                   WHEN    13
                   WHEN    14
                       MOVE    RECE41-KHNTEN (IDX)
                                               TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  HCM76-KHNTEN   (IDZ)
               END-EVALUATE
      *        回数
               MOVE    RECE41-KAISU (IDX)      TO  WRK-KGK4
               MOVE    WRK-KGK4                TO  HCM76-KHNKAISU (IDZ)
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       33
      *        手技点数
               MOVE    RECE41-TENSU (IDX)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM76-KOUTEN   (IDX)
      *        薬剤等点数
               MOVE    RECE41-TENSU-YAK (IDX)  TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCM76-YKZTENSU (IDX)
           END-PERFORM
      *
      *    １０〜８０点数計
      ***  MOVE    RECE41-TOTALTEN     TO  WRK-TOTALTEN
      ***  MOVE    WRK-TOTALTEN        TO  HCM76-KOUTENG
           MOVE    RECE41-TOTALTEN     TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM76-KOUTENG
      *
      ***  MOVE    RECE41-TOTALTEN-YAK TO  WRK-TOTALTEN
      ***  MOVE    WRK-TOTALTEN        TO  HCM76-YKZTENSUG
           MOVE    RECE41-TOTALTEN-YAK TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  HCM76-YKZTENSUG
      *
      *    診療内容・金額
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       6
      *        金額
               MOVE    RECE41-KINGAKU2 (IDX)   TO  WRK-KINGAKUZ
               MOVE    WRK-KINGAKUZ            TO  HCM76-KINGAKU (IDX)
           END-PERFORM
      *     
      *    再診回数
           MOVE    RECE41-KAISU2 (2)   TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  HCM76-SAISINKAISU
      *     
      *    指導回数
           MOVE    RECE41-KAISU2 (3)   TO  WRK-ZZZ
           MOVE    WRK-ZZZ             TO  HCM76-SHIDOKAISU
      *
      *    診断書料
           MOVE    RECE41-SINDAN-TUU   TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  HCM76-SHINDANSYOSUU
      *    診断書金額
           MOVE    RECE41-SINDAN-KIN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  HCM76-SHINDANSYOMONEY
      *    明細書料
           MOVE    RECE41-MEISAI-TUU   TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  HCM76-MEISAISYOSUU
      *    明細書金額
           MOVE    RECE41-MEISAI-KIN   TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ        TO  HCM76-MEISAISYOMONEY
      *
           .
       20022-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（傷病名）処理
      *****************************************************************
       20023-BYOMEI-HEN-SEC        SECTION.
      *     
           MOVE    SPACE           TO  WRK-BYOMEI1
                                       WRK-BYOMEI2
      *
      *    傷病名の編集                   
           INITIALIZE                  RECE43-REC
           MOVE    RECE41-KEY      TO  RECE43-KEY1
      *
           START   RECE43-FILE
               KEY IS    >=   RECE43-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE43
               NOT INVALID
                   PERFORM 990-RECE43-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE43     =   1           )
               OR     ( RECE41-KEY NOT =   RECE43-KEY1 )
      *        病名編集処理
               PERFORM 200231-BYOMEI-SET-SEC
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE43
      *
           IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
               IF      WRK-BYOMEI1-NAME    NOT =   SPACE
                   PERFORM 200232-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF      WRK-BYOMEI2-NAME    NOT =   SPACE
                   PERFORM 200233-BYOMEI2-OUT-SEC
               END-IF
           END-IF
      *
           .
       20023-BYOMEI-HEN-EXT.
           EXIT.
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       200231-BYOMEI-SET-SEC         SECTION.
      *
      *    病名をつなげると何桁になるかを計算（　　＋病名）
           IF      WRK-CLM         =   ZERO
               MOVE    WRK-CLM         TO  WRK-WCLM
           ELSE
               COMPUTE WRK-WCLM    =   WRK-CLM +   4
           END-IF
           COMPUTE WRK-WCLM    = ( RECE43-BYOMEIMOJI   *   2 )
                               +   WRK-WCLM
      *
           EVALUATE    TRUE
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
      *            傷病名欄に編集
                   IF      WRK-WCLM        >   WRK-BYOMEI1-MAX
                       PERFORM 200232-BYOMEI1-OUT-SEC
                   ELSE
                       IF      WRK-CLM     >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM   =   WRK-CLM     +   1
                           MOVE    "　　"          TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                           COMPUTE WRK-CLM     =   WRK-CLM     +   4
                       END-IF
                   END-IF
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
      *            適用欄に編集
                   IF      WRK-WCLM        >   WRK-MOJISU-MAX
                       PERFORM 200233-BYOMEI2-OUT-SEC
                       MOVE    WRK-LINE-MAX    TO  WRK-LINETEK
                   ELSE
                       IF      WRK-CLM     >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM   =   WRK-CLM     +   1
                           MOVE    "　　"      TO  WRK-BYOMEI2-NAME
                                                           (WRK-STCLM:)
                           COMPUTE WRK-CLM     =   WRK-CLM     +   4
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    対象病名の開始桁
           COMPUTE     WRK-STCLM   =   WRK-CLM   +   1
      *
      *    病名文字数を計算
           COMPUTE WRK-MOJISU      =   RECE43-BYOMEIMOJI   *   2
      *     
           EVALUATE    TRUE
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
      *            傷病名欄に編集
                   MOVE    RECE43-BYOMEI   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
      *            適用欄に編集
                   MOVE    RECE43-BYOMEI   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *
           PERFORM 990-RECE43-NEXT-SEC
      *
           .
       200231-BYOMEI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *     傷病名欄出力処理
      ****************************************************************
       200232-BYOMEI1-OUT-SEC        SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       WRK-BYOMEI-LINE
                   OR      WRK-BYOMEI1-NAME-T (IDZZ)
                                   =       SPACE
               INSPECT WRK-BYOMEI1-NAME-T (IDZZ)
                           REPLACING    ALL   "  "   BY   "　"
      *
               MOVE    WRK-BYOMEI1-NAME-T (IDZZ)
                                       TO  HCM76-SYOBYO-S (IDZZ)
           END-PERFORM
      *
           MOVE    WRK-BYOMEI1-MAX TO  WRK-LINEBYO
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
      *
           .
       200232-BYOMEI1-OUT-EXT.
           EXIT.
      *
      ****************************************************************
      *     傷病名 適用欄出力処理
      ****************************************************************
       200233-BYOMEI2-OUT-SEC      SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF    
      *
      *    先頭行の記載
           IF      CNT-PAGE        =   1
               MOVE    "傷"            TO  HCM76-MARK (1)
           ELSE
               MOVE    "傷"            TO  HCM77-MARK (1)
           END-IF
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       WRK-LINE-MAX
                   OR      WRK-BYOMEI2-NAME-T (IDZZ)
                                   =       SPACE
               INSPECT WRK-BYOMEI2-NAME-T (IDZZ)
                           REPLACING    ALL   "  "   BY   "　"
      *
               IF      CNT-PAGE        =   1
                   MOVE    WRK-BYOMEI2-NAME-T (IDZZ)
                                       TO  HCM76-TEKIYO  (IDZZ)
               ELSE
                   MOVE    WRK-BYOMEI2-NAME-T (IDZZ)
                                       TO  HCM77-TEKIYO  (IDZZ)
               END-IF    
      *     
               MOVE    IDZZ        TO  WRK-LINETEK
           END-PERFORM
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
      *
           .
       200233-BYOMEI2-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<手術歴>処理
      *****************************************************************
       20024-OPECOM-HEN-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    "01"                TO  WRK-SRYKBN
      *
           INITIALIZE                      WRK-TEKIYOU-G
           MOVE    ZERO                TO  FLG-MARK
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE41-HKNID  )
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-TEKIYOU-G
                   MOVE    1                   TO  FLG-MARK
                   PERFORM 200241-OPECOM-HENSYU-SEC
                   MOVE    OPECOM-OPENAME      TO  WRK-TEKIYOU-G
                   PERFORM 200241-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-TEKIYOU-G
                       PERFORM 200241-OPECOM-HENSYU-SEC
                   END-IF
               END-IF
      *
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE41-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE41-PTID         TO  OPECOM-PTID
           MOVE    RECE41-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE41-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE41-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE41-HKNID  )
                   MOVE    OPECOM-OPENAME      TO  WRK-TEKIYOU-G
                   MOVE    1                   TO  FLG-MARK
                   PERFORM 200241-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-TEKIYOU-G
                       PERFORM 200241-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       20024-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメント編集処理
      *****************************************************************
       200241-OPECOM-HENSYU-SEC    SECTION.
      *
      *
      *    摘要欄編集処理
           MOVE    2               TO  WRK-TEKIYOU-MAX
           PERFORM 2002411-TEKIYOU-HENNSYU-SEC
           .
       200241-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要欄編集処理
      *****************************************************************
       2002411-TEKIYOU-HENNSYU-SEC      SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-TEKIYOU-MAX
                   OR      WRK-TEKIYOU-T (IDXX)
                                   =       SPACE
               INSPECT WRK-TEKIYOU-T (IDXX)    REPLACING   ALL "  "
                                                           BY  "　"
      *
               MOVE    0               TO  FLG-NEXT
               IF      IDXX            <   WRK-TEKIYOU-MAX
                   COMPUTE IDYY    =   IDXX    +   1 
                   IF      WRK-TEKIYOU-T (IDYY)
                                       NOT =   SPACE
                       MOVE    1               TO  FLG-NEXT
                   END-IF
               END-IF
      *
               IF      WRK-LINETEK     >=  WRK-LINE-MAX
                   PERFORM 34120-PAGE-SEC
               END-IF
      *
               ADD     1                   TO  WRK-LINETEK
      *
               IF      CNT-PAGE        =   1
                   IF      WRK-R061-SRYKBN-O
                                       NOT =   WRK-SRYKBN
                       MOVE    WRK-SRYKBN      TO
                                               HCM76-SRYKBN(WRK-LINETEK)
                   END-IF
      *
                   IF      FLG-MARK        >   ZERO
                       MOVE    "＊"            TO
                                               HCM76-MARK  (WRK-LINETEK)
                   END-IF
      *
                   MOVE    WRK-TEKIYOU-T (IDXX)
                                           TO  HCM76-TEKIYO(WRK-LINETEK)
                   IF      FLG-NEXT        =   1
                       MOVE    "＆"           TO
                                      HCM76-TEKIYO(WRK-LINETEK)(29:2)
                   END-IF
               ELSE                    
                   IF      WRK-R061-SRYKBN-O
                                       NOT =   WRK-SRYKBN
                       MOVE    WRK-SRYKBN      TO
                                               HCM77-SRYKBN(WRK-LINETEK)
                   END-IF
      *
                   IF      FLG-MARK        >   ZERO
                       MOVE    "＊"            TO
                                               HCM77-MARK  (WRK-LINETEK)
                   END-IF
      *
                   MOVE    WRK-TEKIYOU-T (IDXX)
                                           TO  HCM77-TEKIYO(WRK-LINETEK)
                   IF      FLG-NEXT        =   1
                       MOVE    "＆"            TO
                                      HCM77-TEKIYO(WRK-LINETEK)(29:2)
                   END-IF
               END-IF
      *
               MOVE    WRK-SRYKBN     TO  WRK-R061-SRYKBN-O
               MOVE    ZERO           TO  FLG-MARK
           END-PERFORM
      *
           MOVE    SPACE           TO  WRK-TEKIYOU-G
           .
       2002411-TEKIYOU-HENNSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（適用）処理
      *****************************************************************
       20025-TEKIYOU-HEN-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    ZERO                TO  CNT-RECE47
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE47-REC
           MOVE    RECE41-KEY          TO  RECE47-KEY1
      *
           START   RECE47-FILE
               KEY IS    >=   RECE47-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE47
               NOT INVALID
                   PERFORM 990-RECE47-NEXT-SEC
           END-START
           PERFORM UNTIL  ( FLG-RECE47     =   1           )
                   OR     ( RECE41-KEY NOT =   RECE47-KEY1 )
      *        レセプト０６ファイル編集処理
               PERFORM 200251-RECE47-HENSYU-SEC
      *
               PERFORM 990-RECE47-NEXT-SEC
           END-PERFORM
      *
           .
       20025-TEKIYOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       200251-RECE47-HENSYU-SEC        SECTION.
      *
           IF      WRK-LINETEK         >=  WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1               TO  WRK-LINETEK
      *
           IF      RECE47-ZAINUM     NOT =   WRK-R061-ZAINUM-O
      *        診療区分
               IF      RECE47-SRYKBN
                                   NOT =   WRK-R061-SRYKBN-O
                   IF      CNT-PAGE        =   1
                       MOVE    RECE47-SRYKBN   TO  HCM76-SRYKBN
                                                       (WRK-LINETEK)
                   ELSE
                       MOVE    RECE47-SRYKBN   TO  HCM77-SRYKBN
                                                       (WRK-LINETEK)
                   END-IF
               END-IF
           END-IF
      *
      *    明細
           INSPECT RECE47-TEKIYO-INF   REPLACING   ALL "  "    BY  "　"
      *
           IF      RECE47-TEKIYO2-KISAIFLG =   1
      ***      MOVE    RECE47-TEKIYO2-TEN  TO  WRK-TENSUZ
      ***      MOVE    WRK-TENSUZ          TO  WRK-TENSUX
               MOVE    RECE47-TEKIYO2-TEN  TO  WRK-TENSUZ2
               MOVE    WRK-TENSUZ2         TO  WRK-TENSUX
               PERFORM VARYING IDZZ    FROM    7   BY  -1
                       UNTIL   IDZZ    <       1
                   IF      WRK-TENSUX-T (IDZZ)     =   SPACE
                       IF      RECE47-TEKIYO2-TEN      <   0
                           MOVE    "-"         TO  WRK-TENSUX-T (IDZZ)
                       ELSE
                           IF      RECE47-TEKIYO2-YENFLG   =   1
                               MOVE    "\"     TO  WRK-TENSUX-T (IDZZ)
                           END-IF
                       END-IF
                       MOVE    1           TO  IDZZ
                   END-IF
               END-PERFORM
      *
               IF      CNT-PAGE        =   1
                   MOVE    WRK-TENSUX      TO  HCM76-TENSU (WRK-LINETEK)
                   MOVE    "×"            TO  HCM76-MARK2 (WRK-LINETEK)
                   MOVE    RECE47-TEKIYO2-KAISU
                                           TO  WRK-KAISUZ
                   MOVE    WRK-KAISUZ      TO  HCM76-KAISU (WRK-LINETEK)
               ELSE    
                   MOVE    WRK-TENSUX      TO  HCM77-TENSU (WRK-LINETEK)
                   MOVE    RECE47-TEKIYO2-KAISU
                                           TO  WRK-KAISUZ
                  MOVE    "×"             TO  HCM77-MARK2 (WRK-LINETEK)
                   MOVE    WRK-KAISUZ      TO  HCM77-KAISU (WRK-LINETEK)
                END-IF 
           END-IF    
      *    
           IF      CNT-PAGE        =   1
               MOVE    RECE47-ASTER    TO  HCM76-MARK   (WRK-LINETEK)
               MOVE    RECE47-TEKIYO   TO  HCM76-TEKIYO (WRK-LINETEK)
           ELSE    
               MOVE    RECE47-ASTER    TO  HCM77-MARK   (WRK-LINETEK)
               MOVE    RECE47-TEKIYO   TO  HCM77-TEKIYO (WRK-LINETEK)
           END-IF    
      *
           MOVE    RECE47-ZAINUM        TO  WRK-R061-ZAINUM-O
           MOVE    RECE47-SRYKBN        TO  WRK-R061-SRYKBN-O
      *
           .
       200251-RECE47-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（コメント）処理
      *****************************************************************
       20026-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 200261-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 200261-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 200261-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１、５２、９０）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key23"             TO  WRK-MCP-PATHNAME
           PERFORM 200261-RECECOM-HENSYU-SEC
           .
       20026-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       200261-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE41-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE41-PTID         TO  RECECOM-PTID
           MOVE    RECE41-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      *
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               IF      RECECOM-HKNCOMBI    =   RECE41-HKNCOMBI
                   PERFORM 2002611-COMMENT-HENSYU-SEC                 
               END-IF
      *                 
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       200261-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       2002611-COMMENT-HENSYU-SEC    SECTION.
      *                                          
           MOVE    "99"                TO  WRK-SRYKBN
           MOVE    1                   TO  FLG-MARK
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN
      *
      *    症状詳記区分の見出しを記載
           IF      RECECOM-SJKBN   =   "07"   OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
           END-IF
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
      *
               MOVE    SPACE           TO  WRK-RECECOM-NAME
               EVALUATE    WRK-RECECOM-SJKBN
                   WHEN    "51"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (1)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "52"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (2)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "90"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (3)
                                               TO  WRK-RECECOM-NAME
                   WHEN    OTHER
                       MOVE    9               TO  FLG-RECECOM-SJKBN
               END-EVALUATE
           ELSE
               IF      RECECOM-SJKBN       =   "99"
                   MOVE    1                   TO  FLG-MARK
               ELSE
                   MOVE    ZERO                TO  FLG-MARK
               END-IF
           END-IF
      *
           PERFORM     UNTIL       WRK-RECEHEN  =   1      
      *
               MOVE    SPACE           TO  WRK-COMMENT-G
                                           WRK-COMM1
               MOVE    ZERO            TO  WRK-COL
      *
               IF      WRK-COMM (1:1)  =   WRK-CONS-RETURN
                   MOVE    WRK-COMM (2:)   TO  WRK-COMM1
               ELSE             
                   INSPECT WRK-COMM    TALLYING    WRK-COL
                                               FOR ALL WRK-CONS-RETURN
                   IF      WRK-COL         >   ZERO
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >
                                           WRK-CONS-RECECOM-LENGTH-MAX
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1    =   IDX     -   1
                               COMPUTE IDY2    =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                               TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                               TO  WRK-COMM1
                               MOVE    WRK-CONS-RECECOM-LENGTH-MAX
                                               TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *
                   IF      WRK-COMMENT (1) NOT =   SPACE
      *                症状詳記区分の見出しを記載
                       IF      FLG-RECECOM-SJKBN   =   1
                           MOVE    2               TO  WRK-TEKIYOU-MAX
                           MOVE    WRK-RECECOM-NAME
                                                   TO  WRK-TEKIYOU-G
      *                    摘要欄編集処理
                           PERFORM 2002411-TEKIYOU-HENNSYU-SEC
      *
                           MOVE    ZERO        TO  FLG-RECECOM-SJKBN
                       END-IF
      *                                       
                       MOVE    SPACE           TO  WRK-TEKIYOU-G
                       MOVE    ZERO            TO  WRK-TEKIYOU-MAX
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       WRK-CONS-RECECOM-MAX
                               OR      WRK-COMMENT (IDX)   =   SPACE
                           MOVE    WRK-COMMENT (IDX)
                                               TO  WRK-TEKIYOU-T (IDX)
                           ADD     1           TO  WRK-TEKIYOU-MAX
                       END-PERFORM
      *
      *                摘要欄編集処理
                       PERFORM 2002411-TEKIYOU-HENNSYU-SEC
      *
                       MOVE    ZERO            TO  FLG-RECECOM-SJKBN
                   END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       2002611-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       20028-PRINT-OUT-SEC               SECTION.
      *
      *    レセプト種別不明のとき続紙は出力しない
           IF    ( RECE41-RECESYUBETU   =   ZERO )
           AND   ( CNT-PAGE             >   1    )
               GO  TO  20028-PRINT-OUT-EXT
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               MOVE    "HCM76.red" TO  TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCM76       TO  TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               MOVE    "HCM77.red" TO  TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCM77       TO  TBL-RECE-PRTDATA (TBL-RECE-MAX)
           END-IF        
      *
           .
       20028-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票更新処理
      *****************************************************************
       20029-DB-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               ADD     10                  TO  WRK-RECENUM
           END-IF
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL   IDXY    >       TBL-RECE-MAX
                   OR      FLG-ERR >       ZERO
      *
               IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
                   ADD     1                   TO  WRK-RECENUM
               END-IF
      * 
      *        レセプト明細管理ＤＢ
               PERFORM 3901-RECE08-INSERT-SEC
      *
               PERFORM 3905-JIBAI-PRT-HENSYU-SEC
      *
      *        レセプト帳票管理ファイル出力
               IF    ( FLG-ERR              =   ZERO     )
               AND   ( WRK-PARA-SHELLID NOT =  "RECEPTX" )
                   PERFORM 3903-RECE07-WRITE-SEC
               END-IF
           END-PERFORM
      *
      *    請求管理ＤＢ更新
           IF      FLG-ERR              =   ZERO
               PERFORM 3904-RECE10-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  FLG-STS
           END-IF
      *
           .
       20029-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細管理ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC23"              TO  RECE08-PRTID
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    ALL "9"         TO  RECE08-RECEYM
           ELSE
                   MOVE    WRK-PARA-SRYYM  TO  RECE08-RECEYM
           END-IF
           MOVE    WRK-PARA-CREYMD     TO  RECE08-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECE08-CREHMS
      ***  MOVE    RECEYMD-CREYMD      TO  RECE08-CREYMD
      ***  MOVE    RECEYMD-CREHMS      TO  RECE08-CREHMS
           MOVE    "0"                 TO  RECE08-PREFKBN
           MOVE    "00"                TO  RECE08-PREFNUM
           MOVE    RECE41-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    "4"                 TO  RECE08-TEISYUTUSAKI
      *****IF      RECE41-PTNUM        =   SPACE
      *         MOVE    ZERO                TO  RECE08-RECESYUBETU
      *     ELSE      
               IF      TBL-RECE-RENNUM (IDXY)  =   1                                 
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
               ELSE
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
                   MOVE    "2"                 TO  RECE08-RECESYUBETU
                                                                (4:1)
               END-IF                                                         
      *****END-IF    
           MOVE    RECE41-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE41-SRYYM        TO  RECE08-SRYYM
           MOVE    TBL-RECE-RENNUM (IDXY)
                                       TO  RECE08-RENNUM
      *****IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *****    MOVE    WRK-RECENUM         TO  RECE08-RECENUM
      *****ELSE
      *****    COMPUTE RECE08-RECENUM      =   WRK-RECENUM
      *****                                +   TBL-RECE-RENNUM (IDXY)
      *****END-IF
           MOVE    WRK-RECENUM         TO  RECE08-RECENUM
      *
           MOVE    "00"                TO  RECE08-SRYKA
           MOVE    "99999999"          TO  RECE08-HKNJANUM-S
           MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR0510 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE41-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 900-DB-ERR-ABORT-SEC
           ELSE    
               MOVE    RECE41-KANANAME     TO  RECE08-KANANAME
      *********MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    WRK-PARA-TERMID     TO  RECE08-TERMID
               MOVE    ZERO                TO  RECE08-PRTJYUN
                                               RECE08-PRTRENNUM
               MOVE    TBL-RECE-FORMID  (IDXY)
                                           TO  RECE08-FORMID
               MOVE    TBL-RECE-PRTDATA (IDXY)
                                           TO  RECE08-PRTDATA
      *        複数枚レセの場合連番を記載
      *********IF      TBL-RECE-MAX        >   1
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    TBL-RECE-RENNUM (IDXY) 
                                               TO  WRK-PAGE1
                   MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
                   EVALUATE    RECE08-FORMID
                       WHEN    "HCM76.red"
                           MOVE    RECE08-PRTDATA  TO HCM76
                           MOVE    WRK-PAGE1       TO HCM76-PAGE1
                           MOVE    WRK-PAGE2       TO HCM76-PAGE2
                           MOVE    HCM76           TO RECE08-PRTDATA
                       WHEN    "HCM77.red"                  
                           MOVE    RECE08-PRTDATA  TO HCM77
                           MOVE    WRK-PAGE1       TO HCM77-PAGE1
                           MOVE    WRK-PAGE2       TO HCM77-PAGE2
                           MOVE    HCM77            TO RECE08-PRTDATA
                   END-EVALUATE
      *********END-IF
      *
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE     RECE08-REC         TO  MCPDATA-REC
               PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT 
               ELSE 
                   DISPLAY "ORCR0510 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE41-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 900-DB-ERR-ABORT-SEC
               END-IF
           END-IF    
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力処理
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
           MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
           MOVE    RECE08-PRTID        TO  RECE07-PRTID
           IF      WRK-PARA-SRYYM      =   ZERO
               MOVE    "2"                 TO  RECE07-SYOKBN
           ELSE
               MOVE    "1"                 TO  RECE07-SYOKBN
           END-IF         
           MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
           MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
           MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
           MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
           MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
           MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN
           MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
           MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
           MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
           MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
           MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
           MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
           MOVE    RECE08-RECESYUBETU-SRT
                                       TO  RECE07-RECESYUBETU-SRT
           MOVE    RECE07-REC          TO  MCPDATA-REC 
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   1
               INITIALIZE                      RECE07-REC
               MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
               MOVE    RECE08-PRTID        TO  RECE07-PRTID
               IF      WRK-PARA-SRYYM      =   ZERO
                   MOVE    "2"                 TO  RECE07-SYOKBN
               ELSE
                   MOVE    "1"                 TO  RECE07-SYOKBN
               END-IF         
               MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
               MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
               MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
               MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
               MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN 
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    RECE08-HKNJANUM-S   TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
               MOVE    RECE08-RECESYUBETU-SRT
                                           TO  RECE07-RECESYUBETU-SRT
               MOVE    RECE08-TERMID       TO  RECE07-TERMID
               MOVE    RECE08-OPID         TO  RECE07-OPID
               IF      WRK-PARA-SYOKBN     =   "2"
                   EVALUATE    BTPARA-INFO-KBN
                       WHEN    "01"
                           MOVE    ZERO            TO  RECE07-SYORISBT
                       WHEN    "02"
                           MOVE    "1"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                   TO  RECE07-STYMD
                       WHEN    "03"
                           MOVE    "2"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  RECE07-STYMD
                           MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  RECE07-EDYMD
                       WHEN    "04"
                           MOVE    "3"             TO  RECE07-SYORISBT
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                   TO  RECE07-STYMD
                   END-EVALUATE
               END-IF 
               EVALUATE    WRK-PARA-PRTKBN
                   WHEN    "2"
                       MOVE    "1"                 TO  RECE07-PRTKBN
                   WHEN    "3"
                       MOVE    "2"                 TO  RECE07-PRTKBN
                   WHEN    "4"
                       MOVE    "3"                 TO  RECE07-PRTKBN
                   WHEN    "5"
                       MOVE    "4"                 TO  RECE07-PRTKBN
               END-EVALUATE
               MOVE    1                   TO  RECE07-PAGE
               MOVE    1                   TO  RECE07-KENSU
           ELSE
               ADD     1                   TO  RECE07-PAGE
               IF    ( CNT-KENSU           =   1 )
               AND   ( TBL-RECE-RENNUM (IDXY)
                                           <   3 )
                   ADD     1                   TO  RECE07-KENSU
               END-IF
           END-IF
      *
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE07          =   1
               MOVE    "DBINSERT"      TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"      TO  MCP-FUNC
           END-IF 
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0510 MCP-RC= "  MCP-RC   
               IF      FLG-RECE07         =   1    
               MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
           .
       3903-RECE07-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ更新
      *****************************************************************
       3904-RECE10-HENSYU-SEC          SECTION.
      * 
      *    テスト患者、レセプト種別不明のとき更新しない
           IF    ( RECE41-TSTPTNUMKBN  =   "1"  )
           OR    ( RECE41-RECESYUBETU  =   ZERO )
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      RECE10-REC
           MOVE    RECE41-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE10-PTID
           MOVE    ZERO                TO  RECE10-RECEKA
           MOVE    4                   TO  RECE10-TEISYUTUSAKI
           MOVE    RECE41-RECESYUBETU  TO  RECE10-RECESYUBETU
           MOVE    ZERO                TO  RECE10-HKNJANUM
           MOVE    ZERO                TO  RECE10-HOJOKBN-KEY
           MOVE    RECE41-HKNID        TO  RECE10-KOHID-KEY
           MOVE    RECE41-TEKSTYMD     TO  RECE10-TEKSTYMD
      *
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-RECE10          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    WRK-PARA-CREYMD     TO  RECE10-UPDYMD
               MOVE    WRK-PARA-CREHMS (1:6)
                                           TO  RECE10-UPDHMS
               IF      RECE10-SKYKBN       =   "2"
                   MOVE    "1"                 TO  RECE10-SKYKBN
               END-IF 
               MOVE    "2"                 TO  RECE10-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  RECE10-SKYKBN
               MOVE    RECE41-PTNUM        TO  RECE10-PTNUM
               MOVE    RECE41-HKNNUM       TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE41-HKNID        TO  RECE10-HKNID
               MOVE    ZERO                TO  RECE10-SKYYM
                                               RECE10-HENREIYM
                                               RECE10-TEISEIKBN
               MOVE    WRK-PARA-CREYMD     TO  RECE10-CREYMD
               MOVE    WRK-PARA-CREHMS (1:6)
                                           TO  RECE10-CREHMS
               MOVE    ZERO                TO  RECE10-UPDYMD
                                               RECE10-UPDHMS
           END-IF
      *
           MOVE    RECE41-AGE          TO  RECE10-AGE
      *
           MOVE    RECE41-COMMENT      TO  RECE10-COMMENT
      *
           MOVE    "4"                 TO  RECE10-RECEKBN
      *
      *    平成２９年７月以降で第三者行為以外のとき
           IF    ( RECE41-SRYYM        >=  WRK-CONS-SRYYM-201707 )
           AND   ( WRK-PARA-RECEKBN
                                   NOT =   "8"                   )
               MOVE    "5"                 TO  RECE10-HKNKBN
                                               RECE10-HKNKBN-RSI
           END-IF
      *
           IF      RECE41-JNISSU       =   999
               MOVE    ZERO                TO  RECE10-JNISSU       (1)
           ELSE
               MOVE    RECE41-JNISSU       TO  RECE10-JNISSU       (1)
           END-IF
           COMPUTE RECE10-TOTALTEN (1) =   RECE41-TOTALTEN     +
                                           RECE41-TOTALTEN-YAK
      *
           COMPUTE RECE10-FTNMONEY (1) =   RECE41-JIBAI-GOKEI-AF
      *
           COMPUTE RECE10-FTNMONEY-FTN (1)
                                       =   RECE41-TOTALKIN     +
                                           RECE41-TOTALKIN-YAK
           COMPUTE RECE10-FTNMONEY-KOJ (1) 
                                       =   RECE41-TOTALKIN2    +
                                           RECE41-TOTALKIN2-ETC
      *
           COMPUTE RECE10-TENSU (1 1)  =   RECE41-TENSU     (1)    +
                                           RECE41-TENSU-YAK (1)
           COMPUTE RECE10-TENSU (2 1)  =   RECE41-TENSU     (2)    +
                                           RECE41-TENSU     (3)    +
                                           RECE41-TENSU     (4)    +
                                           RECE41-TENSU     (5)    +
                                           RECE41-TENSU-YAK (2)    +
                                           RECE41-TENSU-YAK (3)    +
                                           RECE41-TENSU-YAK (4)    +
                                           RECE41-TENSU-YAK (5)
           COMPUTE RECE10-TENSU (3 1)  =   RECE41-TENSU     (6)    +
                                           RECE41-TENSU-YAK (6)
           COMPUTE RECE10-TENSU (4 1)  =   RECE41-TENSU     (7)    +
                                           RECE41-TENSU-YAK (7)
           COMPUTE RECE10-TENSU (5 1)  =   RECE41-TENSU     (9)    +
                                           RECE41-TENSU     (10)   +
                                           RECE41-TENSU     (11)   +
                                           RECE41-TENSU     (12)   +
                                           RECE41-TENSU     (13)   +
                                           RECE41-TENSU     (14)   +
                                           RECE41-TENSU     (15)   +
                                           RECE41-TENSU     (16)   +
                                           RECE41-TENSU-YAK (9)    +
                                           RECE41-TENSU-YAK (10)   +
                                           RECE41-TENSU-YAK (11)   +
                                           RECE41-TENSU-YAK (12)   +
                                           RECE41-TENSU-YAK (13)   +
                                           RECE41-TENSU-YAK (14)   +
                                           RECE41-TENSU-YAK (15)   +
                                           RECE41-TENSU-YAK (16)
           COMPUTE RECE10-TENSU (6 1)  =   RECE41-TENSU     (18)   +
                                           RECE41-TENSU     (19)   +
                                           RECE41-TENSU     (20)   +
                                           RECE41-TENSU     (21)   +
                                           RECE41-TENSU-YAK (18)   +
                                           RECE41-TENSU-YAK (19)   +
                                           RECE41-TENSU-YAK (20)   +
                                           RECE41-TENSU-YAK (21)
           COMPUTE RECE10-TENSU (7 1)  =   RECE41-TENSU     (23)   +
                                           RECE41-TENSU     (24)   +
                                           RECE41-TENSU-YAK (23)   +
                                           RECE41-TENSU-YAK (24)
           COMPUTE RECE10-TENSU (8 1)  =   RECE41-TENSU     (25)   +
                                           RECE41-TENSU     (26)   +
                                           RECE41-TENSU-YAK (25)   +
                                           RECE41-TENSU-YAK (26)
           COMPUTE RECE10-TENSU (9 1)  =   RECE41-TENSU     (27)   +
                                           RECE41-TENSU     (28)   +
                                           RECE41-TENSU-YAK (27)   +
                                           RECE41-TENSU-YAK (28)
           COMPUTE RECE10-TENSU (10 1) =   RECE41-TENSU     (29)   +
                                           RECE41-TENSU     (30)   +
                                           RECE41-TENSU-YAK (29)   +
                                           RECE41-TENSU-YAK (30)
           COMPUTE RECE10-TENSU (11 1) =   RECE41-TENSU     (31)   +
                                           RECE41-TENSU     (32)   +
                                           RECE41-TENSU     (33)   +
                                           RECE41-TENSU-YAK (31)   +
                                           RECE41-TENSU-YAK (32)   +
                                           RECE41-TENSU-YAK (33) 
           MOVE    RECE41-KINGAKU2 (1) TO  RECE10-TENSU (13 1) 
           MOVE    RECE41-KINGAKU2 (2) TO  RECE10-TENSU (14 1) 
           MOVE    RECE41-KINGAKU2 (3) TO  RECE10-TENSU (15 1) 
           MOVE    RECE41-KINGAKU2 (4) TO  RECE10-TENSU (16 1) 
           MOVE    RECE41-KINGAKU2 (6) TO  RECE10-TENSU (18 1) 
           MOVE    RECE41-SINDAN-KIN   TO  RECE10-TENSU (19 1)
           MOVE    RECE41-MEISAI-KIN   TO  RECE10-TENSU (20 1)
      *
           MOVE    RECE41-HKNCOMBI     TO  RECE10-HKNCOMBI (1)
      *
           MOVE    "00000000000000000000000000000000000000000000000000"
                                       TO  RECE10-RESERVE-AREA
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE10          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0510 SEIKYU MCP-RC= "  MCP-RC
               IF      FLG-RECE10          =   1
               MOVE    "請求管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "請求管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
           .
       3904-RECE10-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦ作成処理
      *****************************************************************
       3905-JIBAI-PRT-HENSYU-SEC          SECTION.
      *
      *    ＰＤＦ作成しない又は点検用レセプトのとき
           IF    ( WRK-PARA-PRTKBN NOT =   "1" )
           OR    ( SYS-200501-JIBAI-PDF
                                       =   "0" )
               GO  TO  3905-JIBAI-PRT-HENSYU-EXT
           END-IF
      *
      *    テスト患者、レセプト種別不明のとき更新しない
           IF    ( RECE41-TSTPTNUMKBN  =   "1"  )
           OR    ( RECE41-RECESYUBETU  =   ZERO )
               GO  TO  3905-JIBAI-PRT-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      JIBAI-PRT-REC
           MOVE    RECE41-HOSPNUM      TO  JIBAI-PRT-HOSPNUM
           MOVE    RECE41-SRYYM        TO  JIBAI-PRT-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  JIBAI-PRT-NYUGAIKBN
           MOVE    RECE41-PTID         TO  JIBAI-PRT-PTID
           MOVE    ZERO                TO  JIBAI-PRT-RECEKA
           MOVE    4                   TO  JIBAI-PRT-TEISYUTUSAKI
           MOVE    RECE41-RECESYUBETU  TO  JIBAI-PRT-RECESYUBETU
           MOVE    ZERO                TO  JIBAI-PRT-HKNJANUM
           MOVE    ZERO                TO  JIBAI-PRT-HOJOKBN-KEY
           MOVE    RECE41-HKNID        TO  JIBAI-PRT-KOHID-KEY
           MOVE    RECE41-TEKSTYMD     TO  JIBAI-PRT-TEKSTYMD
           MOVE    RECE08-RENNUM       TO  JIBAI-PRT-RENNUM
           MOVE    RECE08-PRTDATA      TO  JIBAI-PRT-PRTDATA
           MOVE    RECE08-FORMID       TO  JIBAI-PRT-FORMID
           MOVE    RECE41-PTNUM        TO  JIBAI-PRT-PTNUM
      *
           MOVE    WRK-PARA-CREYMD     TO  JIBAI-PRT-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  JIBAI-PRT-CREHMS
           MOVE    "973"               TO  JIBAI-PRT-HKNNUM
           MOVE    "4"                 TO  JIBAI-PRT-RECEKBN
           MOVE    "rk2"               TO  JIBAI-PRT-PRTKBN
      *
      *    平成２９年７月以降で第三者行為以外のとき
           IF    ( RECE41-SRYYM        >=  WRK-CONS-SRYYM-201707 )
           AND   ( WRK-PARA-RECEKBN
                                   NOT =   "8"                   )
               MOVE    "5"                 TO  JIBAI-PRT-HKNKBN
           END-IF
      *
           MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
           MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC              =   ZERO
               ADD     1                   TO  CNT-PDF
           ELSE
               DISPLAY "ORCR0489 JIBAI-PRT MCP-RC= "  MCP-RC
               IF      FLG-RECE10          =   1
               MOVE    "ＰＤＦ帳票ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "ＰＤＦ帳票ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF
               PERFORM 900-DB-ERR-ABORT-SEC
           END-IF
            .
       3905-JIBAI-PRT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC        SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦変換処理
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY2-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY2-EDTYMD1-G  TO  WRK-HENGENGO
               MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-HENYY
               MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-HENMM
               MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-HENDD
           ELSE
               MOVE    SPACE               TO  WRK-HENYMDG
           END-IF
           .
       31012-SE-WA-YMDEDIT-EXT.
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
      *            ジョブ管理開始処理
                   MOVE    "JBE"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "9999"          TO  JOB-ERRCD
                   PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
           MOVE    2                   TO  FLG-STS
      *
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
           STRING  "ORCR0510 "             DELIMITED   BY  SIZE
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
               PERFORM 900-CALL-ORCSJOB-SEC
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
                       PERFORM 900-CALL-ORCSJOB-SEC
                   END-IF
      *                             
                   MOVE    1                   TO  FLG-END
                   MOVE    2                   TO  FLG-STS
               END-IF
           END-IF
           .
       500-CANCEL-HENSYU-EXT.
           EXIT.
      * 
      *****************************************************************
      *    データ削除処理
      *****************************************************************
       5001-DB-DELETE-SEC               SECTION.
      *
           IF      WRK-PARA-SYOKBN     =   "1"
      *        請求管理削除処理（一括分）
               INITIALIZE                      RECE10-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "2"                 TO  RECE10-NYUGAIKBN
               MOVE    4                   TO  RECE10-TEISYUTUSAKI
               MOVE    "973"               TO  RECE10-HKNNUM
               MOVE    "5"                 TO  RECE10-HKNKBN-RSI
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               IF      WRK-PARA-SRYYM      >=  WRK-CONS-SRYYM-201707-X
                   IF      SYS-4001-JIBAIKBN   =   "2"
                       MOVE    "del15"             TO  MCP-PATHNAME
                   ELSE
                       MOVE    "del11"             TO  MCP-PATHNAME
                   END-IF
               ELSE
                   MOVE    "del8"              TO  MCP-PATHNAME
               END-IF
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE    "請求管理削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF      
      *        自賠責ＰＤＦ削除処理（一括分）
               INITIALIZE                      JIBAI-PRT-REC
               MOVE    WRK-PARA-HOSPNUM    TO  JIBAI-PRT-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  JIBAI-PRT-SRYYM
               MOVE    "2"                 TO  JIBAI-PRT-NYUGAIKBN
               MOVE    4                   TO  JIBAI-PRT-TEISYUTUSAKI
               MOVE    "973"               TO  JIBAI-PRT-HKNNUM
               MOVE    "5"                 TO  JIBAI-PRT-HKNKBN
               MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
               IF      WRK-PARA-SRYYM      >=  WRK-CONS-SRYYM-201707-X
                   IF      SYS-4001-JIBAIKBN   =   "2"
                       MOVE    "del6"              TO  MCP-PATHNAME
                   ELSE
                       MOVE    "del3"              TO  MCP-PATHNAME
                   END-IF
               ELSE
                   MOVE    "del1"              TO  MCP-PATHNAME
               END-IF
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE    "自賠責ＰＤＦ削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF      
      *        対象レセプトの健保準拠分を削除処理
               IF      WRK-PARA-SRYYM      >=  WRK-CONS-SRYYM-201707-X
                  PERFORM 50011-HKNKBN-DB-DELETE-SEC
               END-IF
      *
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC23"              TO  RECE07-PRTID
               MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               PERFORM 900-RECE07-READ-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-REC          TO  WRK-RECE07-REC
                   MOVE    RECE07-REC          TO  MCPDATA-REC 
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                   "レセプト管理削除処理（一括分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月レセプト削除処理（一括分）
                   INITIALIZE                  RECE08-REC
                   MOVE    WRK-RECE07-HOSPNUM  TO  RECE08-HOSPNUM
                   MOVE    WRK-RECE07-PRTID    TO  RECE08-PRTID
                   MOVE    WRK-RECE07-SRYYM    TO  RECE08-RECEYM
                   MOVE    WRK-RECE07-CREYMD   TO  RECE08-CREYMD
                   MOVE    WRK-RECE07-CREHMS   TO  RECE08-CREHMS
                   MOVE    WRK-RECE07-NYUGAIKBN
                                               TO  RECE08-NYUGAIKBN
                   MOVE    RECE08-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receprt"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセプト削除処理（一括分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF  
           ELSE
               MOVE    TBL-SYS1001 (1)     TO  SYS-1001-REC
      *
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC23"              TO  RECE07-PRTID
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                   "レセプト管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
      *        レセプト削除処理（個別分）
               INITIALIZE                  RECE08-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
               MOVE    "HC23"              TO  RECE08-PRTID
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    RECE08-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                       "レセプト削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
               EVALUATE    BTPARA-INFO-KBN
                   WHEN    "01"
                   WHEN    "04"
      *                レセプト指示（個別指示）コード読み込み
                       INITIALIZE                      SYS-2002-REC
                       MOVE    "2002"              TO  SYS-2002-KANRICD
                       MOVE    WRK-PARA-HOSPNUM    TO  SYS-2002-HOSPNUM
                       MOVE    ZERO                TO  SYS-2002-STYUKYMD
                       MOVE    ZERO                TO  SYS-2002-EDYUKYMD
                       MOVE    SYS-2002-REC        TO  MCPDATA-REC
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-KANRIMST-NEXT-SEC
                       ELSE
                           MOVE    1                   TO  FLG-SYSKANRI
                       END-IF
      *
                       PERFORM         UNTIL       FLG-SYSKANRI    =   1
                           MOVE    MCPDATA-REC     TO  SYS-2002-REC
      *
                           DISPLAY "DELETE 2002 PTNUM=" SYS-2002-PTNUM
                                              " SRYYM=" SYS-2002-SRYYM
      *
      *                    該当診療年月の労災自賠医療機関情報
                           MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                           MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                           PERFORM 50012-SYSKANRI-HENSYU-SEC
      *
      *                    診療年月の削除処理
                           MOVE    SYS-2002-PTID       TO  WRK-DEL-PTID
                           MOVE    SYS-2002-SRYYM      TO  WRK-DEL-SRYYM
                           PERFORM 50011-DB-DELETE-KOBETU-SEC
      *
                           MOVE    "tbl_syskanri"  TO  MCP-TABLE
                           MOVE    "key2"          TO  MCP-PATHNAME
                           PERFORM 900-KANRIMST-NEXT-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   WHEN    "02"
      *                該当診療年月の労災自賠医療機関情報
                       MOVE  BTPARA-INFO-PARA(17:6)
                                                 TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"                TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 50012-SYSKANRI-HENSYU-SEC
      *
                       INITIALIZE                      PTINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    "2"                 TO  PTINF-HKNKBN
                       MOVE    BTPARA-INFO-PARA(17:6) 
                                                   TO  PTINF-SRYYM
                       MOVE    BTPARA-INFO-PARA(17:6)
                                                   TO  PTINF-SRYYMD(1:6)
                       MOVE    "__"                TO  PTINF-SRYYMD(7:2)
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key13"             TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key13"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       ELSE
                           MOVE    1                  TO  FLG-PTINF
                       END-IF
      *
                       PERFORM     UNTIL       FLG-PTINF       =   1
      *                    診療年月の削除処理
                           MOVE    PTINF-PTID          TO  WRK-DEL-PTID
                           MOVE    BTPARA-INFO-PARA (17:6)
                                                       TO  WRK-DEL-SRYYM
                           PERFORM 50011-DB-DELETE-KOBETU-SEC
      *
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key13"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key13"             TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
                   WHEN    "03"
      *                該当診療年月の労災自賠医療機関情報
                       MOVE  BTPARA-INFO-PARA (1:6)
                                                 TO  WRK-SYS-SRYYMD(1:6)
                       MOVE  "01"                TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 50012-SYSKANRI-HENSYU-SEC
      *
                       INITIALIZE                      PTINF-REC
                       MOVE    WRK-PARA-HOSPNUM    TO  PTINF-HOSPNUM
                       MOVE    BTPARA-INFO-PARA (1:8)
                                                   TO  PTINF-ST-SRYYMD
                       MOVE    BTPARA-INFO-PARA (9:8)
                                                   TO  PTINF-ED-SRYYMD
                       MOVE    "2"                 TO  PTINF-NYUGAIKBN
                       MOVE    "2"                TO  PTINF-HKNKBN
                       MOVE    PTINF-REC           TO  MCPDATA-REC
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key11"             TO  MCP-PATHNAME
                       PERFORM 900-DBSELECT-SEC
                       IF      MCP-RC              =   ZERO
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key11"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       ELSE
                           MOVE    1                  TO  FLG-PTINF
                       END-IF
      *
                       PERFORM     UNTIL       FLG-PTINF       =   1
      *                    診療年月の削除処理
                           MOVE    PTINF-PTID          TO  WRK-DEL-PTID
                           MOVE    BTPARA-INFO-PARA (1:6)
                                                       TO  WRK-DEL-SRYYM
                           PERFORM 50011-DB-DELETE-KOBETU-SEC
      *
                           MOVE    "tbl_ptinf"         TO  MCP-TABLE
                           MOVE    "key11"             TO  MCP-PATHNAME
                           PERFORM 900-PTINF-READ-N-SEC
                       END-PERFORM
      *
                       MOVE    "tbl_ptinf"         TO  MCP-TABLE
                       MOVE    "key11"             TO  MCP-PATHNAME
                       PERFORM 900-CLOSE-SEC
               END-EVALUATE
      *
      *        対象レセプトの健保準拠分を削除処理
               PERFORM 50011-HKNKBN-DB-DELETE-SEC
           END-IF
           .
       5001-DB-DELETE-EXT.
           EXIT.    
      *
      *****************************************************************
      *    請求管理、自賠責ＰＤＦ削除処理（個別処理時）
      *****************************************************************
       50011-DB-DELETE-KOBETU-SEC      SECTION.
      *
           DISPLAY "DELETE PTID="  WRK-DEL-PTID
                        " SRYYM=" WRK-DEL-SRYYM
                        " JIBAIKBN=" SYS-4001-JIBAIKBN
      *
      *    診療年月の請求管理削除処理
           INITIALIZE                      RECE10-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
           MOVE    WRK-DEL-PTID        TO  RECE10-PTID
           MOVE    WRK-DEL-SRYYM       TO  RECE10-SRYYM
           MOVE    "2"                 TO  RECE10-NYUGAIKBN
           MOVE    4                   TO  RECE10-TEISYUTUSAKI
                                           RECE10-XXTEISYUTUSAKI 
           MOVE    "973"               TO  RECE10-HKNNUM
           MOVE    "5"                 TO  RECE10-HKNKBN-RSI
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           IF      WRK-DEL-SRYYM       >=  WRK-CONS-SRYYM-201707-X
               IF      SYS-4001-JIBAIKBN   =   "2"
                   IF      WRK-PARA-DLTKBN     =   "0"
                       MOVE    "del17"             TO  MCP-PATHNAME
                   ELSE
                       MOVE    "del16"             TO  MCP-PATHNAME
                   END-IF
               ELSE
                   IF      WRK-PARA-DLTKBN     =   "0"
                       MOVE    "del13"             TO  MCP-PATHNAME
                   ELSE
                       MOVE    "del14"             TO  MCP-PATHNAME
                   END-IF
               END-IF
           ELSE
               IF      WRK-PARA-DLTKBN     =   "0"
                   MOVE    "del9"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "del10"             TO  MCP-PATHNAME
               END-IF
           END-IF
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "DBPATH=" MCP-TABLE " "  MCP-PATHNAME
                       " MCP-RC=" MCP-RC
               MOVE    "請求管理削除処理（個別分）でエラーとなりました"
                                           TO  WRK-RECEERR
               MOVE    WRK-RECEERR         TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
      *    診療年月の自賠責ＰＤＦ削除処理
           INITIALIZE                      JIBAI-PRT-REC
           MOVE    WRK-PARA-HOSPNUM    TO  JIBAI-PRT-HOSPNUM
           MOVE    WRK-DEL-PTID        TO  JIBAI-PRT-PTID
           MOVE    WRK-DEL-SRYYM       TO  JIBAI-PRT-SRYYM
           MOVE    "2"                 TO  JIBAI-PRT-NYUGAIKBN
           MOVE    4                   TO  JIBAI-PRT-TEISYUTUSAKI
           MOVE    "973"               TO  JIBAI-PRT-HKNNUM
           MOVE    "5"                 TO  JIBAI-PRT-HKNKBN
           MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
           IF      WRK-DEL-SRYYM       >=  WRK-CONS-SRYYM-201707-X
               IF      SYS-4001-JIBAIKBN   =   "2"
                   MOVE    "del7"              TO  MCP-PATHNAME
               ELSE
                   MOVE    "del5"              TO  MCP-PATHNAME
               END-IF
           ELSE
               MOVE    "del2"              TO  MCP-PATHNAME
           END-IF
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "DBPATH=" MCP-TABLE " "  MCP-PATHNAME
                       " MCP-RC=" MCP-RC
               MOVE
                   "自賠責ＰＤＦ削除処理（個別分）でエラーとなりました"
                                           TO  WRK-RECEERR
               MOVE    WRK-RECEERR         TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           .
       50011-DB-DELETE-KOBETU-EXT.
           EXIT.    
      *
      *****************************************************************
      *    対象レセプトの健保準拠分を削除処理（平成２９年７月診療分以降）
      *****************************************************************
       50011-HKNKBN-DB-DELETE-SEC      SECTION.
      *
      *    対象レセプトの健保準拠分を削除する（請求区分は関係なし）
           MOVE    ZERO                TO  FLG-RECE41
           OPEN    INPUT               RECE41-FILE
           IF      STS-RECE41          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE41PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE41          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           PERFORM 900-RECE41-READ-NEXT-SEC
           PERFORM         UNTIL       FLG-RECE41  =   1
      *        診療年月の請求管理削除処理
      *        診療年月の請求管理削除処理
               MOVE    ZERO                TO  FLG-DB-DELETE
      *
               INITIALIZE                  RECE10-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
               MOVE    RECE41-PTID         TO  RECE10-PTID
               MOVE    RECE41-SRYYM        TO  RECE10-SRYYM
               MOVE    RECE41-NYUGAIKBN    TO  RECE10-NYUGAIKBN
               MOVE    4                   TO  RECE10-TEISYUTUSAKI
                                               RECE10-XXTEISYUTUSAKI 
               MOVE    "973"               TO  RECE10-HKNNUM
               MOVE    RECE41-HKNID        TO  RECE10-KOHID-KEY
      *
               MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
               MOVE    "key92"             TO  WRK-MCP-PATHNAME
               PERFORM 900-SEIKYU-INV-SEC
               IF      FLG-RECE10          =   ZERO
                   MOVE    MCPDATA-REC         TO  RECE10-REC
      *
      *            個別作成時に請求しないを削除する設定のときは無条件に削除
                   IF    ( WRK-PARA-SYOKBN     =   "2" )
                   AND   ( WRK-PARA-DLTKBN     =   "1" )
                       MOVE    1                   TO  FLG-DB-DELETE
                   ELSE
      *                健保準拠で請求しないのときは削除しない
      *****************IF    ( RECE10-HKNKBN-RSI   =   "5" )
      *****************AND   ( RECE10-SKYKBN       =   "0" )
                       IF    ( RECE10-SKYKBN       =   "0" )
                           CONTINUE
                       ELSE 
                           MOVE    1                   TO  FLG-DB-DELETE
                       END-IF
                   END-IF
      *
                   IF      FLG-DB-DELETE        =   1
                       DISPLAY "HKNKBN DELETE PTNUM=" RECE41-PTNUM
      *
                       MOVE    RECE10-REC          TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_seikyu"        TO  MCP-TABLE
                       MOVE    "del12"             TO  MCP-PATHNAME
                       PERFORM 900-ORCDBMAIN-SEC
                       IF      MCP-RC           NOT =   ZERO
                           DISPLAY "DBPATH=" MCP-TABLE " "  MCP-PATHNAME
                                   " MCP-RC=" MCP-RC
                           MOVE   "請求管理削除処理でエラーとなりました"
                                                   TO  WRK-RECEERR
                           PERFORM 500-ERR-HENSYU-SEC
                           PERFORM 900-DB-ERR-ABORT-SEC
                       END-IF
                   END-IF
      *
      *            診療年月の自賠責ＰＤＦ削除処理
                   INITIALIZE                  JIBAI-PRT-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  JIBAI-PRT-HOSPNUM
                   MOVE    RECE41-PTID         TO  JIBAI-PRT-PTID
                   MOVE    RECE41-SRYYM        TO  JIBAI-PRT-SRYYM
                   MOVE    RECE41-NYUGAIKBN    TO  JIBAI-PRT-NYUGAIKBN
                   MOVE    4                   TO
                                               JIBAI-PRT-TEISYUTUSAKI
                   MOVE    "973"               TO  JIBAI-PRT-HKNNUM
                   MOVE    RECE41-HKNID        TO  JIBAI-PRT-KOHID-KEY
                   MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
                   MOVE    "del4"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE   "自賠責ＰＤＦ削除処理でエラーとなりました"
                                              TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 900-DB-ERR-ABORT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-RECE41-READ-NEXT-SEC
           END-PERFORM
      *
           CLOSE    RECE41-FILE
           MOVE    ZERO            TO  FLG-RECE41
      *
           .
       50011-HKNKBN-DB-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ）
      *****************************************************************
       50012-SYSKANRI-HENSYU-SEC       SECTION.
      *
      *    該当診療年月の労災自賠医療機関情報
           MOVE    ZERO            TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                   UNTIL ( SYSKAN-IDX  >   100   )
                   OR    ( TBL-SYS4001 (SYSKAN-IDX)
                                       =   SPACE )
                   OR    ( FLG-SET     =   1     )
               MOVE    TBL-SYS4001 (SYSKAN-IDX)
                                           TO  SYS-4001-REC
               IF    ( SYS-4001-STYUKYMD   <=  WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD      <=  SYS-4001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
                   IF      SYS-4001-JIBAIKBN   =   SPACE
                       MOVE    "1"                 TO  SYS-4001-JIBAIKBN
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-SET         =   ZERO
               STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                          DELIMITED  BY  SIZE 
                   WRK-SYS-SRYYMD(1:6)    DELIMITED  BY  SIZE
                                          INTO    WRK-RECEERR
               END-STRING
               MOVE    WRK-RECEERR     TO  WRK-ERRMSG
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
      *
           DISPLAY "SYS-4001-JIBAIKBN=" SYS-4001-JIBAIKBN
           .
      *
       50012-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *          
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE41-FILE
           CLOSE                       RECE43-FILE
           CLOSE                       RECE47-FILE
      *
      *    ＰＵＳＨ通知処理
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
               PERFORM 3001-PUSH-SEC
           END-IF
      *
           DISPLAY "*** ORCR0510 IN     "  CNT-RECE41
           DISPLAY "*** ORCR0510 PAGE   "  CNT-OUT
           DISPLAY "*** ORCR0510 PDF    "  CNT-PDF
           DISPLAY "*** ORCR0510 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
               MOVE    CNT-RECE41      TO  JOB-UPDCNT1
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＳＨ通知処理
      *****************************************************************
       3001-PUSH-SEC        SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           PERFORM 900-CALL-ORCSJOB-SEC
           IF    ( SJOBKANRI-RETURN
                                   =   ZERO  )
           AND   ( JOB-KARTE-UID
                               NOT =   SPACE )
               INITIALIZE                  PUSHBA01-REC
               MOVE    JOB-KARTE-UID   TO  PUSHBA01API-KARTE-UID
               MOVE    JOB-ORCA-UID    TO  PUSHBA01API-ORCA-UID
               MOVE    "000"           TO  PUSHBA01API-RESULT
               MOVE    "receipt_make"  TO  PUSHBA01-EVENT
               IF      WRK-PARA-SRYYM  =   ZERO
                   MOVE    "9999-12"       TO  PUSHBA01-PERFORM-MONTH
               ELSE
                   MOVE    WRK-PARA-SRYYM  TO  WRK-SYMD
                   PERFORM 801-DAYHEN01-SEC
                   MOVE    WRK-HEN-DATE (1:7)
                                           TO  PUSHBA01-PERFORM-MONTH
               END-IF
               MOVE    CNT-RECE41      TO  WRK-CNT
               MOVE    WRK-CNT-X       TO  PUSHBA01-OUTPUT-COUNT
      *??
             display "push=" PUSHBA01-REC "#"
      *??
               MOVE    PUSHBA01-REC    TO  MCPDATA-REC
               MOVE    "PUSHEVENT"     TO  MCP-FUNC
               MOVE    "push_batch01"  TO  MCP-TABLE
               MOVE    "push_batch01"  TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           END-IF
           .
       3001-PUSH-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE               TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYMD (1:4)      TO  WRK-HEN-YY
               MOVE    WRK-SYMD (5:2)      TO  WRK-HEN-MM
               MOVE    WRK-SYMD (7:2)      TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE41-READ-SEC           SECTION.
      *
           READ    RECE41-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE41
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE41PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE41
                   IF    ( RECE41-PTNUM        =   SPACE  )
                   OR    ( RECE41-HKNKBN       =   "4"    )
                   OR    ( RECE41-RECESYUBETU  =   "0000" )
      *
                       IF    ( RECE41-TOTALKIN      =   ZERO )
                       AND   ( RECE41-TOTALKIN-YAK  =   ZERO )
                       AND   ( RECE41-TOTALKIN2     =   ZERO )
                       AND   ( RECE41-TOTALKIN2-ETC =   ZERO )
                               GO  TO  990-RECE41-READ-SEC
                       END-IF
      *
                       DISPLAY "RECE41  PTNUM=" RECE41-PTNUM
      *
                       ADD     1                   TO  CNT-RECE41
                       MOVE    RECE41-HOSPNUM      TO  KEY-N-HOSPNUM
                       MOVE    RECE41-SRYYM        TO  KEY-N-SRYYM
                       MOVE    RECE41-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                       MOVE    RECE41-PTID         TO  KEY-N-PTID
                       MOVE    RECE41-RECEKA       TO  KEY-N-RECEKA
                       IF      WRK-PARA-SRYYM      =  ZERO
      *                    医療機関ＩＤ編集
                           MOVE  RECE41-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
                           MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
                   ELSE
                       GO  TO  990-RECE41-READ-SEC 
                   END-IF        
           END-READ
      *
           .
       990-RECE41-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理（削除用）
      *****************************************************************
       900-RECE41-READ-NEXT-SEC           SECTION.
      *
           READ    RECE41-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE41
               NOT AT  END
                   IF      STS-RECE41          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE41PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE41      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE41
      *
      *            平成２９年７月診療分以降のみ対象
                   IF      RECE41-SRYYM    <   WRK-CONS-SRYYM-201707-X
                       GO  TO  900-RECE41-READ-NEXT-SEC
                   END-IF
      *
                   DISPLAY "DELETE RECE41-PTNUM=" RECE41-PTNUM
                           " SRYYM=" RECE41-SRYYM
                           " HKNID=" RECE41-HKNID
      *
           END-READ
      *
           .
       900-RECE41-READ-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-SYS-1901
                                           FLG-SYS-1902
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1001(SYSKAN-IDX) TO  SYS-1001-REC
               IF    ( SYS-1001-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1001-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING  "医療機関情報が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    医療機関情報（所在地ほか）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS1002(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
               MOVE    TBL-SYS1002(SYSKAN-IDX) TO  SYS-1002-REC
               IF    ( SYS-1002-STYUKYMD       <= WRK-SYS-SRYYMD    )
               AND   ( WRK-SYS-SRYYMD          <= SYS-1002-EDYUKYMD )
                   MOVE     1                  TO  FLG-SET
               END-IF
           END-PERFORM
           IF      FLG-SET             =   ZERO
               STRING
              "医療機関情報（所在地）が取得できませんでした。診療年月＝"
                                               DELIMITED  BY  SIZE 
                       WRK-SYS-SRYYMD (1:6)    DELIMITED  BY  SIZE
                                       INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               GO  TO  900-SYSKANRI-HENSYU-EXT
           END-IF
      *
      *    帳票別指定医療機関編集情報
           IF      FLG-SYS-1900        =   1
               MOVE    ZERO                TO  FLG-SET
      *
               PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                       UNTIL ( SYSKAN-IDX              >  100  )
                       OR    ( TBL-SYS1900(SYSKAN-IDX) = SPACE )
                       OR    ( FLG-SET                 =   1   )
                   MOVE    TBL-SYS1900(SYSKAN-IDX) TO  SYS-1900-REC
                   IF    ( SYS-1900-STYUKYMD   <= WRK-SYS-SRYYMD    )
                   AND   ( WRK-SYS-SRYYMD      <= SYS-1900-EDYUKYMD )
                           MOVE     1              TO  FLG-SET
                   END-IF
               END-PERFORM
               IF      FLG-SET             =   1
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (3)  TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-SYSKANRI     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1902-REC
                      MOVE    1                TO  FLG-SYS-1902
                  END-IF
              END-IF    
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE43-NEXT-SEC           SECTION.
      *
           READ    RECE43-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE43
               NOT AT  END
                   IF      STS-RECE43          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE43PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE43      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE43
                   IF      RECE43-TENKIKBN     =   "8"
                       GO  TO  990-RECE43-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE43-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE47-NEXT-SEC           SECTION.
      *
           READ    RECE47-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE47
               NOT AT  END
                   IF      STS-RECE47          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE47PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE47      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE47
                   ADD     1         TO  CNT-RECE47
                   IF      RECE47-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE47-SRYKBN
                   END-IF    
                   IF      RECE47-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE47-SRYKBN
                   END-IF
           END-READ
      *
           .
       990-RECE47-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE07
                   MOVE    MCPDATA-REC         TO  RECE07-REC
               ELSE
                   MOVE    1                   TO  FLG-RECE07
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE07
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト印刷読込処理
      *****************************************************************
       900-RECE08-READ-SEC           SECTION.
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECE08
               ELSE
                   MOVE    1                   TO  FLG-RECE08
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE08
           END-IF
      *
           MOVE    "tbl_receprt"       TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE08-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-NEXT-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           .
       900-RECECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-SEC           SECTION.
      *
           MOVE    OPECOM-REC          TO  MCPDATA-REC
           MOVE    WRK-OPECOM-MCP-TABLE
                                       TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME
                                       TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE
                                           TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME
                                           TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           ELSE
               MOVE    1                   TO  FLG-OPECOM
               INITIALIZE                      OPECOM-REC
           END-IF
      *
           .
       900-OPECOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴マスター読込
      *****************************************************************
       900-OPECOM-READ-N-SEC           SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  OPECOM-REC
               MOVE    ZERO                TO  FLG-OPECOM
           ELSE
               INITIALIZE                      OPECOM-REC
               MOVE    1                   TO  FLG-OPECOM
           END-IF
      *
           .
       900-OPECOM-READ-N-EXT.
           EXIT.
      *      
      *****************************************************************
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
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
           PERFORM 900-DBFETCH-SEC
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
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-RECE10
               ELSE
                   MOVE    1               TO  FLG-RECE10
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       900-PTINF-READ-N-SEC       SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTINF-REC
               MOVE    ZERO                TO  FLG-PTINF
           ELSE
               MOVE    1                   TO  FLG-PTINF
                INITIALIZE                  PTINF-REC
           END-IF
      *
           .
       900-PTINF-READ-N-EXT.
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
      *    ＤＢ更新エラー処理
      *****************************************************************
       900-DB-ERR-ABORT-SEC                SECTION.
      *
           PERFORM 500-ERR-HENSYU-SEC
      *
           MOVE    SPACE               TO  WRK-ERR-AREA
           INITIALIZE                      WRK-ERR-AREA
           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
           STRING  "ORCR0510 db err PTNUM="
                                       DELIMITED   BY  SIZE
                   RECE41-PTNUM        DELIMITED   BY  SPACE
                   " TABLE="           DELIMITED   BY  SIZE
                   MCP-TABLE           DELIMITED   BY  SPACE
                   " MCP-FUNC="        DELIMITED   BY  SIZE
                   MCP-FUNC            DELIMITED   BY  SPACE
                   " PATHNAME="        DELIMITED   BY  SIZE
                   MCP-PATHNAME        DELIMITED   BY  SPACE
                   " RC="              DELIMITED   BY  SIZE
                   WRK-MCP-RC          DELIMITED   BY  SIZE
                   LOW-VALUE           DELIMITED   BY  SIZE
                                       INTO    WRK-ERRMSG
           END-STRING
           PERFORM 500-COBABORT-SEC
           .
       900-DBINSERT-EXT.
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
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
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
               STRING  "ORCR0510 select err TABLE="
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
      *    ＤＢ　オープン処理
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
      *    ＤＢ　クローズ処理
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
