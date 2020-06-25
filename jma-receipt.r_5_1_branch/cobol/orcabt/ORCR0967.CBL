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
       PROGRAM-ID.                 ORCR0967.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（自賠責）
      *  コンポーネント名  : 月次業務　自賠責明細書ファイル作成
      *                      ＯＣＲ様式
      *  管理者            : 
      *  作成日付    作業者        記述
      *  13/12/05    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.01    NACL-門脇    15/09/08  自賠責（新様式）（ＯＣＲ様式）
      *                                     受領金額欄記載対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.02    NACL-門脇    15/08/27  自賠責レセプトの請求金額欄端数処理
      *                                     対応
      *  04.08.03    NACL-門脇    16/01/12  点検用レセプト院外処方一般名記載対応
      *  04.08.04    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.05    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *  04.08.06    NACL-門脇    18/04/24  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
           SELECT  RECE82-FILE     ASSIGN  RECE82PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE82-KEY
                                   FILE    STATUS  IS  STS-RECE82.
      *
           SELECT  RECE83-FILE     ASSIGN  RECE83PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE83-KEY
                                   FILE    STATUS  IS  STS-RECE83.
      *
           SELECT  RECE87-FILE     ASSIGN  RECE87PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE87-KEY
                                   FILE    STATUS  IS  STS-RECE87.
      *
           SELECT  RECE88-FILE     ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE88-KEY
                                   FILE    STATUS  IS  STS-RECE88.
      *
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
       FD  RECE83-FILE.
       01  RECE83-REC.
           COPY    "CPRCF0831.INC" REPLACING  //RECE831//
                                   BY         //RECE83//.
      *
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
      *
       FD  RECE88-FILE.
       01  RECE88-REC. 
           COPY    "CPRCF088.INC".
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
                                   BY         //RECE82//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE83//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE87//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE88//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
           COPY    "HCN70.INC".
      *
      *    続紙
           COPY    "HCM77.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE83          PIC X(02).
           03  STS-RECE87          PIC X(02).
           03  STS-RECE88          PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE82          PIC 9(01).
           03  FLG-RECE83          PIC 9(01).
           03  FLG-RECE87          PIC 9(01).
           03  FLG-RECE871          PIC 9(01).
           03  FLG-RECE88          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-JIBAI-PRT       PIC 9(01).
      *
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-SHOKUJI-ETC     PIC 9(01).
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
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE87          PIC 9(06).
           03  CNT-RECE871         PIC 9(06).
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
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(03).
           03  IDXY                PIC 9(03).
      *
           03  IDXX                PIC 9(04).
           03  IDYY                PIC 9(04).
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
           03  KEY-OLD.
               05  KEY-O-HOSPNUM   PIC  9(02).
               05  KEY-O-SRYYM     PIC  9(06).
               05  KEY-O-NYUGAIKBN PIC  X(01).
               05  KEY-O-PTID      PIC  9(10).
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
       01  SUM-AREA                          VALUE   ZERO.          
           03  SUM-NYUKHNNISSU         PIC 9(03).
           03  SUM-NYUKHNKEI           PIC 9(07).
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
           03  WRK-GOKEI           PIC 9(10).            
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
      *        適用欄編集行
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
      *    入退院履歴編集（適用欄）
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-RIREKI           PIC X(54).
               05  WRK-NYUTAI-RIREKI-W         REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  WRK-NYUTAI-MSG          PIC X(12).
                   07  WRK-NYUTAI-RIREKI-O.
                       09  WRK-NYUTAI-NYUINYMD PIC X(20).
                       09  WRK-NYUTAI-FL1      PIC X(02).
                       09  WRK-NYUTAI-TAIINYMD PIC X(20).
      *
           03  WRK-TEKIYOU-G.
               05  WRK-TEKIYOU-T   PIC X(28)
                                       OCCURS  86.
           03  WRK-TEKIYOU-MAX     PIC 9(03).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE2       PIC Z9.
      *                 
      *    食事基本料情報（その他）（摘要欄）
           03  WRK-SHOKUJIKHNINF-ETC-AREA.
               05  WRK-SHOKUJIKHNRYO-ETC-NM    PIC X(06).
               05  WRK-SHOKUJIKHNRYO-ETC       PIC X(08).
               05  WRK-SHOKUJIKHNRYO-ETC-FL    PIC X(04).
               05  WRK-SHOKUJIKHNNISSU-ETC     PIC X(04).
               05  WRK-SHOKUJIKHNNISSU-ETC-FL  PIC X(04).
      *
           03  WRK-HENSYU-KIN.
               05  WRK-HENSYU-KIN-9            PIC 9(04).
           03  WRK-HENSYU-DAY.
               05  WRK-HENSYU-DAY-9            PIC 9(02).
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
           03  WRK-KHNTENZ         PIC ZZZZZZZ.
           03  WRK-KAISUZ          PIC ZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZ.
           03  WRK-TENSUZ2         PIC ZZZZZZ9.
           03  WRK-JNISSU          PIC ZZ9.
           03  WRK-TOTALTEN        PIC ZZZ,ZZZ.
           03  WRK-NENREI-X        PIC ZZ9.
           03  WRK-KINGAKU6        PIC ZZZZZZ.
           03  WRK-ZZZ9            PIC ZZZ9.
           03  WRK-SEIKYUZ         PIC Z,ZZZ,ZZ9.
           03  WRK-Z9              PIC Z9.
           03  WRK-ZZ              PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-KGK9            PIC Z,ZZZ,ZZZ.
           03  WRK-KGK4            PIC ZZZZ.
           03  WRK-KGK2            PIC ZZ.
           03  WRK-KINGAKU8       PIC ZZZZZZZZ.
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
           03  WRK-BYOMEI2-MAX     PIC 9(04)   VALUE   750.
      *    傷病名最大文字数（続紙）
           03  WRK-BYOMEI2-HCM77-MAX
                                   PIC 9(04)   VALUE   3390.
      *
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   4.
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   25.
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HCM77-LINE
                                   PIC 9(03)   VALUE   113.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(56)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  FILLER              PIC X(56)   VALUE
               "【廃用症候群に係る評価表】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(56)   OCCURS   2.
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
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
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
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
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
           COPY    "COMMON-SPA".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
      *
      *    PUSH
           COPY    "CPPUSHBATCH01.INC".
      *
      ****************************************************************
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
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
           DISPLAY "WRK-PARA-FRAME     =" WRK-PARA-FRAME
           DISPLAY "WRK-PARA-DLTKBN    =" WRK-PARA-DLTKBN
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE82PARA
           INITIALIZE                      RECE82PARA
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE83PARA
           INITIALIZE                      RECE83PARA
           MOVE    "RECE831"           TO  RECE83PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE83PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE83PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE87PARA
           INITIALIZE                      RECE87PARA
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           MOVE    SPACE               TO  RECE88PARA
           INITIALIZE                      RECE88PARA
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE88PARA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE83PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE88PARA-BASENAME TO  SGETTEMP-BASENAMES  (5)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE82PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE83PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE87PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE88PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0967"      TO  JOB-PGID
               MOVE    "自賠責保険・印刷データ編集（自賠責）"
                                       TO  JOB-SHELLMSG
               PERFORM   900-CALL-ORCSJOB-SEC
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
                   PERFORM   900-CALL-ORCSJOB-SEC
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
           OPEN    INPUT               RECE82-FILE
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE83-FILE
           IF      STS-RECE83          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE83PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE83          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE87-FILE
           IF      STS-RECE87          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE87PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE87          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    INPUT               RECE88-FILE
           IF      STS-RECE88          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE88PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE88          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    ZERO                TO  WRK-RECENUM
           ELSE
               MOVE    100                 TO  WRK-RECENUM
           END-IF
      *
           IF      FLG-STS             =   ZERO
              PERFORM 990-RECE82-READ-SEC
           END-IF
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
                  MOVE    MCPDATA-REC   TO  SYS-1001-REC
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
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM  900-KANRIMST-NEXT-SEC
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
               MOVE    "医療機関情報（所在地）が取得できませんでした。"
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
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM  900-KANRIMST-NEXT-SEC
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
              IF      SYS-1900-PRTKBN (6)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (6)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (6)  TO  SYS-KBNCD
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
                    IF      SYS-1900-PRTKBN (6)  =   SPACE
                        CONTINUE
                    ELSE
                        ADD     1             TO  SYSKAN-IDX
                        MOVE    SYS-1900-REC  TO
                                              TBL-SYS1900 (SYSKAN-IDX)
                        MOVE    1            TO  FLG-SYS-1900
                    END-IF 
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM  900-KANRIMST-NEXT-SEC
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
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-25-REC
           ELSE    
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-INSPECT-CHARACTER-KBN
                                   =   SPACE
               MOVE    ZERO            TO
                                       SYS-25-INSPECT-CHARACTER-KBN
           END-IF
           IF      SYS-25-JIBAI-PDF
                                   =   SPACE
               MOVE    "0"             TO  SYS-25-JIBAI-PDF
           END-IF
      *
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               PERFORM 900-BTPARA-READ-SEC
               IF      FLG-BTPARA          =   1
                   MOVE
                   "パラメタ情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
               END-IF
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
           PERFORM         UNTIL   FLG-RECE82     =   1
                           OR      FLG-STS        =   2 
      *
      *        療養期間編集処理
               PERFORM 2001-SRYYM-HENSYU-SEC
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE82      =   1
                               OR      FLG-STS         =   2
                               OR      KEY-N-SRYYM NOT =   KEY-O-SRYYM
      *            レセプト編集処理
                   MOVE    ZERO            TO  CNT-KENSU
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-RECE82      =   1
                                   OR      FLG-STS         =   2
                                   OR      KEY-NEW     NOT =   KEY-OLD
                       ADD     1                       TO  CNT-KENSU
      *                帳票編集処理
                       PERFORM 2002-SYORI-SEC
      *
                       PERFORM 990-RECE82-READ-SEC
                   END-PERFORM
               END-PERFORM
           END-PERFORM    
      *
           MOVE    2               TO  FLG-STS
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    療養期間編集処理      
      *****************************************************************
       2001-SRYYM-HENSYU-SEC      SECTION.
      *     
      *    療養期間開始編集
           MOVE    RECE82-SRYYM    TO  WRK-STYMD (1:6)
           MOVE    "01"            TO  WRK-STYMD (7:2)
           MOVE    WRK-STYMD       TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMDG     TO  WRK-STYMDW
      *     
      *    療養期間終了編集
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY7-AREA
           MOVE    "71"            TO  LNK-DAY7-IRAI
           MOVE    RECE82-SRYYM    TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI TO  WRK-EDYMD 
           MOVE    WRK-EDYMD       TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMDG     TO  WRK-EDYMDW
           . 
       2001-SRYYM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集処理
      *****************************************************************
       2002-SYORI-SEC                SECTION.
      *
           MOVE    1         TO  CNT-PAGE
           INITIALIZE                  WRK-EDIT-AREA
           INITIALIZE                  TABLE-AREA
      *
      *    帳票編集（ヘッダー部）処理
           PERFORM 20021-HCN70-HEAD-HEN-SEC
      *
      *    帳票編集（点数）処理
           PERFORM 20022-TENSU-HEN-SEC
      *
      *    帳票編集（傷病名）処理
           PERFORM 20023-BYOMEI-HEN-SEC
      *
      *    帳票編集（入退院履歴・入院基本料種別の続き）処理
           PERFORM 20024-NYUTAI-RIREKI-HEN-SEC
      *
      *    帳票編集（手術歴）処理
           PERFORM 20025-OPECOM-HEN-SEC 
      *
      *    帳票編集（適用）処理
           PERFORM 20026-ZAI-HEN-SEC
      *
      *    帳票編集（コメント）処理
           PERFORM 20027-COMMENT-HEN-SEC 
      *
      *    摘要欄の終わり
           ADD     1               TO  WRK-LINETEK
           IF      CNT-PAGE    =   1 
               MOVE    "Ｅ"            TO  HCN70-MARK   (WRK-LINETEK)
               MOVE    "終わり"        TO  HCN70-TEKIYO (WRK-LINETEK)
           ELSE
               MOVE    "Ｅ"            TO  HCM77-MARK   (WRK-LINETEK)
               MOVE    "終わり"        TO  HCM77-TEKIYO (WRK-LINETEK)
           END-IF
      *
      *    帳票印刷処理（続紙）
           IF      CNT-PAGE           >   1
               PERFORM 20028-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      FLG-STS            =   ZERO
               MOVE    1                  TO  CNT-PAGE
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
      *    帳票編集（ＪＫ１１ヘッダー部）処理
      *****************************************************************
       20021-HCN70-HEAD-HEN-SEC        SECTION.
      *
           MOVE    WRK-TEKIYOU-LINE    TO  WRK-LINE-MAX
           MOVE    WRK-BYOMEI2-MAX     TO  WRK-MOJISU-MAX
      *     
           INITIALIZE                      HCN70
      *
      *    患者番号
           STRING   RECE82-PTNUM       DELIMITED   BY  SPACE
                    " "                DELIMITED   BY  SIZE
                    RECE82-RECEKA      DELIMITED   BY  SPACE
                                       INTO        HCN70-PTNUM
           END-STRING
      *    診療年月
           MOVE     WRK-STYMDW(1:1)    TO  HCN70-HAKKOUG
           MOVE     WRK-STYMDW(2:2)    TO  HCN70-HAKKOUYY
           MOVE     WRK-STYMDW(4:2)    TO  HCN70-HAKKOUMM 
      *    氏名
           STRING   RECE82-NAME        DELIMITED   BY  SPACE
                    "　　"             DELIMITED   BY  SIZE
                    RECE82-KANANAME    DELIMITED   BY  SPACE
                                       INTO        HCN70-NAME
           END-STRING
      *    性別
           EVALUATE    RECE82-SEX
               WHEN    "1"
                   MOVE    "レ"        TO  HCN70-SEX (1)
               WHEN    "2"
                   MOVE    "レ"        TO  HCN70-SEX (2)
           END-EVALUATE     
      *    生年月日
           MOVE    RECE82-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCN70-TANJYOG
           MOVE    WRK-HENYY           TO  HCN70-TANJYOYY
           MOVE    WRK-HENMM           TO  HCN70-TANJYOMM
           MOVE    WRK-HENDD           TO  HCN70-TANJYODD
      *    年齢
           MOVE    RECE82-BIRTHDAY     TO  WRK-BIRTHDAY    
           COMPUTE WRK-NENREI          =   RECE82-SRYYM
                                       -   WRK-BIRTHYM
           MOVE    WRK-NENREI-R1       TO  WRK-NENREI-X
           MOVE    WRK-NENREI-X        TO  HCN70-NENREI 
      *    実日数
           MOVE    RECE82-JNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HCN70-JITSUNISSU
      *    転帰
           EVALUATE    RECE82-TENKI
               WHEN    "1"
                   MOVE    "レ"        TO  HCN70-TENKI(1)
               WHEN    "3"
                   MOVE    "レ"        TO  HCN70-TENKI(2)
               WHEN    "5"
                   MOVE    "レ"        TO  HCN70-TENKI(3)
               WHEN    "7"
                   MOVE    "レ"        TO  HCN70-TENKI(4)
               WHEN    "9"
                   MOVE    "レ"        TO  HCN70-TENKI(5)
           END-EVALUATE
      *    受傷日
           MOVE    RECE82-SHOBYOYMD    TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCN70-JYUSYOUBIG
           MOVE    WRK-HENYY           TO  HCN70-JYUSYOUBIYY
           MOVE    WRK-HENMM           TO  HCN70-JYUSYOUBIMM
           MOVE    WRK-HENDD           TO  HCN70-JYUSYOUBIDD
      *    初診日
           MOVE    RECE82-SHOSHINYMD   TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCN70-SYOSHINBIG
           MOVE    WRK-HENYY           TO  HCN70-SYOSHINBIYY
           MOVE    WRK-HENMM           TO  HCN70-SYOSHINBIMM
           MOVE    WRK-HENDD           TO  HCN70-SYOSHINBIDD
      *
      *    診療期間
           IF      RECE82-RYOSTYMD   NOT =   SPACE
               IF      RECE82-RYOSTYMD     <   WRK-STYMD
                   MOVE    WRK-STYMDW(1:1)     TO  HCN70-STRG
                   MOVE    WRK-STYMDW(2:2)     TO  HCN70-STRYY
                   MOVE    WRK-STYMDW(4:2)     TO  HCN70-STRMM
                   MOVE    WRK-STYMDW(6:2)     TO  HCN70-STRDD
               ELSE
                   MOVE    RECE82-RYOSTYMD     TO  WRK-SYMD
                   PERFORM 31012-SE-WA-YMDEDIT-SEC
                   MOVE    WRK-HENGENGO        TO  HCN70-STRG
                   MOVE    WRK-HENYY           TO  HCN70-STRYY
                   MOVE    WRK-HENMM           TO  HCN70-STRMM
                   MOVE    WRK-HENDD           TO  HCN70-STRDD
               END-IF
           ELSE
               MOVE    WRK-STYMDW(1:1)     TO  HCN70-STRG
               MOVE    WRK-STYMDW(2:2)     TO  HCN70-STRYY
               MOVE    WRK-STYMDW(4:2)     TO  HCN70-STRMM
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       31
                     IF      RECE82-DAY (IDX)  =   RECE82-HKNCOMBI
                         MOVE    IDX               TO  WRK-KGK2
                         MOVE    WRK-KGK2          TO  HCN70-STRDD
                         MOVE    31                TO  IDX
                     END-IF
               END-PERFORM
           END-IF
      *
           IF    RECE82-RYOEDYMD   NOT =  SPACE
               IF      RECE82-RYOEDYMD     >   WRK-EDYMD
                   MOVE    WRK-EDYMDW(1:1)     TO  HCN70-ENDG 
                   MOVE    WRK-EDYMDW(2:2)     TO  HCN70-ENDYY 
                   MOVE    WRK-EDYMDW(4:2)     TO  HCN70-ENDMM
                   MOVE    WRK-EDYMDW(6:2)     TO  HCN70-ENDDD
               ELSE
                   MOVE    RECE82-RYOEDYMD     TO  WRK-SYMD
                   PERFORM 31012-SE-WA-YMDEDIT-SEC
                   MOVE    WRK-HENGENGO        TO  HCN70-ENDG
                   MOVE    WRK-HENYY           TO  HCN70-ENDYY
                   MOVE    WRK-HENMM           TO  HCN70-ENDMM
                   MOVE    WRK-HENDD           TO  HCN70-ENDDD
               END-IF
           ELSE
               MOVE    WRK-EDYMDW(1:1)     TO  HCN70-ENDG 
               MOVE    WRK-EDYMDW(2:2)     TO  HCN70-ENDYY 
               MOVE    WRK-EDYMDW(4:2)     TO  HCN70-ENDMM
               PERFORM VARYING IDX FROM    31  BY  -1
                       UNTIL   IDX <       1
                   IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                       MOVE    IDX                 TO  WRK-KGK2
                       MOVE    WRK-KGK2            TO  HCN70-ENDDD
                       MOVE    1                   TO  IDX
                   END-IF
               END-PERFORM
           END-IF
      *
      *    請求額の計算
      *
      *    加算率（Ａ、Ｃ）
           IF      RECE82-KSNRATE(1:1) =   "1"
               CONTINUE
           ELSE
               MOVE  RECE82-KSNRATE(1:1)   TO  HCN70-SEIKYU-SEISU(1)
                                               HCN70-SEIKYU-SEISU(2)
           END-IF
           MOVE    RECE82-KSNRATE(2:2) TO  HCN70-SEIKYU(1)
                                           HCN70-SEIKYU(2)
      *    請求額計
           IF    RECE82-JIBAI-GOKEI-BE  >  ZERO
               MOVE    RECE82-JIBAI-GOKEI-AF
                                           TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCN70-SEIKYUGAKU (5)
      *
      *        請求額Ａ
               MOVE    RECE82-TOTALKIN     TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCN70-SEIKYUGAKU (1)
      *        請求額Ｂ
               MOVE    RECE82-TOTALKIN-YAK TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCN70-SEIKYUGAKU (2)
      *        請求額Ｃ
               MOVE    RECE82-TOTALKIN2    TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCN70-SEIKYUGAKU (3)
      *        請求額Ｄ
               MOVE    RECE82-TOTALKIN-ETC
                                           TO  WRK-SEIKYUZ
               MOVE    WRK-SEIKYUZ         TO  HCN70-SEIKYUGAKU (4)
           END-IF
      *
      *    請求額
      *    保険会社名
           IF    RECE82-JIBAI-GOKEI-BE  >  ZERO
             IF    RECE82-JIBAI-SKYKINFLG  =  1
               MOVE  RECE82-JIBAI-SKYKIN    TO  WRK-SEIKYUZ
               MOVE  WRK-SEIKYUZ            TO  HCN70-SEIKYUGAKUTOTAL(1)
               MOVE    RECE82-HOKENNAME     TO  HCN70-HOKENNAME(1)
               INSPECT HCN70-HOKENNAME(1) REPLACING  ALL "  "  BY  "　"
             END-IF
             IF    RECE82-JIBAI-JYUKINFLG  =  1
               MOVE  RECE82-JIBAI-JYUKIN    TO  WRK-SEIKYUZ
               MOVE  WRK-SEIKYUZ            TO  HCN70-SEIKYUGAKUTOTAL(2)
               IF    RECE82-JIBAI-JYUNAMEFLG  =  1
                 MOVE  RECE82-JIBAI-JYUNAME TO  HCN70-HOKENNAME(2)
               ELSE
                 MOVE  RECE82-NAME          TO  HCN70-HOKENNAME(2)
               END-IF
               INSPECT HCN70-HOKENNAME(2) REPLACING  ALL "  "  BY  "　"
             END-IF
           END-IF
      *    作成日
           MOVE    WRK-CREYMDW (1:1)   TO  HCN70-SEIKYUG
           MOVE    WRK-CREYMDW (2:2)   TO  HCN70-SEIKYUYY
           MOVE    WRK-CREYMDW (4:2)   TO  HCN70-SEIKYUMM
           MOVE    WRK-CREYMDW (6:2)   TO  HCN70-SEIKYUDD
      *    病床数
           MOVE    SYS-1001-BEDSU      TO  WRK-ZZZ9
           MOVE    WRK-ZZZ9            TO  HCN70-BYOSYOSU
      *    郵便番号
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-POST       TO  HCN70-HOSPPOST-G
           ELSE
               MOVE    SYS-1002-POST       TO  HCN70-HOSPPOST-G
           END-IF
      *    所在地
           IF      FLG-SYS-1902        =   1
               STRING   SYS-1902-ADRS1     DELIMITED   BY  SPACE
                        SYS-1902-ADRS2     DELIMITED   BY  SPACE
                                           INTO    HCN70-HOSPJYUSYO
               END-STRING
           ELSE
               MOVE    SYS-1002-ADRS       TO  HCN70-HOSPJYUSYO
           END-IF
           INSPECT HCN70-HOSPJYUSYO    REPLACING   ALL "  "    BY  "　"
      *    名称
           IF      FLG-SYS-1901        =   1
               STRING   SYS-1901-HOSPNAME1 DELIMITED   BY  SPACE
                        SYS-1901-HOSPNAME2 DELIMITED   BY  SPACE
                                           INTO    HCN70-HOSPNAME
               END-STRING
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HCN70-HOSPNAME
           END-IF
           INSPECT HCN70-HOSPNAME      REPLACING   ALL "  "    BY  "　"
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
                       MOVE    WRK-TEL (IDX:1)     TO  HCN70-TEL (IDZ:1)
               END-EVALUATE
           END-PERFORM
      *    医師名
           IF      RECE82-DRCD         =   SPACE
               MOVE    SYS-1001-KANRINAME  TO  HCN70-ISYA
           ELSE
      *        職員情報
               INITIALIZE                      SYSKANRI-REC
               MOVE    "1010"              TO  SYS-KANRICD
               MOVE    RECE82-DRCD         TO  SYS-KBNCD
               MOVE    RECE82-SRYYM        TO  SYS-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-STYUKYMD (7:2)
               MOVE    SYS-STYUKYMD        TO  SYS-EDYUKYMD
               MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
               MOVE    "key10"             TO  WRK-MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1010-REC
                   MOVE    SYS-1010-NAME       TO  HCN70-ISYA
               END-IF
           END-IF
      *    医療機関コード
           MOVE    SYS-1001-HOSPCD     TO  HCN70-HOSPCD
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN NOT =   "1"
               IF      SYS-25-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HCN70-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HCN70-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HCN70-LOPTION
           END-IF
      *
           .
       20021-HCN70-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（ＪＫ１２ヘッダー部）処理
      *****************************************************************
       20021-HCM77-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-HCM77-LINE
                                       TO  WRK-LINE-MAX
           MOVE    WRK-BYOMEI2-HCM77-MAX
                                       TO  WRK-MOJISU-MAX
      *
           INITIALIZE                  HCM77
           MOVE    "JK12"              TO  HCM77-CYOHYONAME
           MOVE    "（入院）"          TO  HCM77-NYUGAINAME
      *    診療年月
           MOVE     WRK-STYMDW(1:1)    TO  HCM77-HAKKOUG
           MOVE     WRK-STYMDW(2:2)    TO  HCM77-HAKKOUYY
           MOVE     WRK-STYMDW(4:2)    TO  HCM77-HAKKOUMM 
      *    氏名
           STRING   RECE82-NAME    DELIMITED   BY  SPACE
                    "　　"         DELIMITED   BY  SIZE
                    RECE82-KANANAME
                                   DELIMITED   BY  SPACE
                                   INTO        HCM77-NAME
           END-STRING
      *    患者番号
           STRING   RECE82-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE82-RECEKA  DELIMITED   BY  SPACE
                                   INTO    HCM77-PTNUM         
           END-STRING         
      *
      *    点検用レセプト作成時
           MOVE    HCN70-LABEL          TO  HCM77-LABEL
           MOVE    HCN70-LOPTION        TO  HCM77-LOPTION
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
               MOVE    "Ｃ"            TO  HCN70-MARK   (26)
               MOVE    "次ページへ続く"
                                       TO  HCN70-TEKIYO (26)
           ELSE
               MOVE    "Ｃ"            TO  HCM77-MARK   (114)
               MOVE    "次ページへ続く"
                                       TO  HCM77-TEKIYO (114)
           END-IF
      *
      *    帳票印刷処理
           IF      CNT-PAGE      >   1
               PERFORM 20028-PRINT-OUT-SEC
           END-IF    
      * 
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 20021-HCM77-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
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
           EVALUATE    RECE82-SHOSHINTIME
               WHEN    "1"
                   MOVE    "レ"        TO  HCN70-SYOSHIN (1)
               WHEN    "2"
                   MOVE    "レ"        TO  HCN70-SYOSHIN (2)
               WHEN    "3"
                   MOVE    "レ"        TO  HCN70-SYOSHIN (3)
           END-EVALUATE
      *    乳幼児
           IF      RECE82-SHOSHIN-NYU  =   "1"
               MOVE    "レ"                TO  HCN70-SYOSHIN (4)
           END-IF
      *
           PERFORM VARYING     IDX     FROM    2   BY  1
                   UNTIL       IDX     >       22
               COMPUTE     IDZ =   IDX     -   1
      * 
      *        単価
               EVALUATE    IDX
      *            投薬
                   WHEN    7
                   WHEN    9
                       MOVE    RECE82-KHNTEN (IDX)
                                               TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  HCN70-KHNTEN   (IDZ)
               END-EVALUATE
      *        回数
               MOVE    RECE82-KAISU (IDX)      TO  WRK-KGK4
               MOVE    WRK-KGK4                TO  HCN70-KHNKAISU (IDZ)
           END-PERFORM
      *
      *    ９０入院            
      *
      *    入院年月日
           MOVE    RECE82-NYUINYMD     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HCN70-NYUING
           MOVE    WRK-HENYY           TO  HCN70-NYUINY
           MOVE    WRK-HENMM           TO  HCN70-NYUINM
           MOVE    WRK-HENDD           TO  HCN70-NYUIND
      *
      *    医療機関種別
           EVALUATE    RECE82-HOSPSBT
               WHEN    "1"
                   MOVE    "レ"        TO  HCN70-HOSPSBT (1)
               WHEN    "2"
                   MOVE    "レ"        TO  HCN70-HOSPSBT (2)
               WHEN    "3"
                   MOVE    "レ"        TO  HCN70-HOSPSBT (1)
                                           HCN70-HOSPSBT (3)
               WHEN    "4"
                   MOVE    "レ"        TO  HCN70-HOSPSBT (2)
                                           HCN70-HOSPSBT (3)
           END-EVALUATE        
      *    <入院基本料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
      *        <基本料>
               MOVE    RECE82-NYUKHNRYO (IDX)
                                             TO  WRK-KINGAKU6
               MOVE    WRK-KINGAKU6          TO  HCN70-NYUKHNTEN (IDX)
      *        <日数>
               MOVE    RECE82-NYUKHNNISSU(IDX)
                                             TO  WRK-KGK2
               MOVE    WRK-KGK2              TO  HCN70-NYUKAISU  (IDX)
      *        <金額>
               MOVE    RECE82-NYUKHNKEI(IDX)
                                             TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN70-NYUKOUTEN (IDX)
      *
           END-PERFORM
      *
           IF        RECE82-NYUKHNRYO (5)    >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    4   BY  1
                         UNTIL     IDX       >   20
                         OR        RECE82-NYUKHNRYO (IDX)    =   ZERO
      *            <日数>
                   ADD     RECE82-NYUKHNNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額>
                   ADD     RECE82-NYUKHNKEI(IDX)
                                                 TO  SUM-NYUKHNKEI
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HCN70-NYUKHNTEN (4)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KGK2
               MOVE    WRK-KGK2              TO  HCN70-NYUKAISU  (4)
      *        <金額>
               MOVE    SUM-NYUKHNKEI         TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN70-NYUKOUTEN (4)
           END-IF
      *    <入院基本料・加算>
           MOVE    RECE82-NYUKHNKEI2   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  WRK-TENSUX
           IF      RECE82-NYUKHNKEI2   <   ZERO
               PERFORM VARYING IDZ     FROM    6   BY  -1
                       UNTIL   IDZ     <       1
                   IF      WRK-TENSUX-T (IDZ)  =   SPACE
                       MOVE    "-"         TO  WRK-TENSUX-T (IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-TENSUX      TO  HCN70-NYUKOUTEN  (5)
      *
      *    <特定入院料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   3
               IF      RECE82-NYUTOKNISSU(IDX)   >   ZERO
                   COMPUTE   IDY  =   IDX    +   6
      *
                   MOVE    "×"              TO  HCN70-KAKERU (IDY)
                   MOVE    "日間"            TO  HCN70-NICHI  (IDY)
      *            <基本料>
                   IF      RECE82-NYUTOKRYO-RYAKU(IDX) =  SPACE
                       MOVE    RECE82-NYUTOKRYO(IDX)
                                             TO  WRK-KINGAKU6
                       MOVE    WRK-KINGAKU6  TO  HCN70-NYUKHNTEN(IDY)
                   ELSE
                       IF      RECE82-NYUTOKRYO-RYAKU (IDX) (11:)
                                             =   SPACE
                           MOVE    RECE82-NYUTOKRYO-RYAKU (IDX)
                                             TO  HCN70-NYUKHNTENN (IDY)
                       ELSE
                           MOVE    RECE82-NYUTOKRYO-RYAKU (IDX)
                                             TO  HCN70-NYUKHNTENN2(IDY)
                       END-IF
                   END-IF
      *            <日数>
                   MOVE    RECE82-NYUTOKNISSU(IDX)
                                             TO  WRK-KGK2
                   MOVE    WRK-KGK2          TO  HCN70-NYUKAISU(IDY)
      *            <金額>
                   MOVE    RECE82-NYUTOKKEI(IDX)
                                             TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ        TO  HCN70-NYUKOUTEN(IDY)
               END-IF
           END-PERFORM
      *
           IF        RECE82-NYUTOKNISSU (4)  >   ZERO
               MOVE      ZERO                TO  SUM-AREA
               PERFORM   VARYING   IDX   FROM    3   BY  1
                          UNTIL    IDX      >   20
                            OR     RECE82-NYUTOKNISSU(IDX)  =   ZERO
      *            <日数>
                   ADD     RECE82-NYUTOKNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額>
                   ADD     RECE82-NYUTOKKEI(IDX) TO  SUM-NYUKHNKEI
               END-PERFORM
      *        <基本料>
               MOVE    SPACE                 TO  HCN70-NYUKHNTEN   (9)
                                                 HCN70-NYUKHNTENN  (9)
                                                 HCN70-NYUKHNTENN2 (9)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KGK2
               MOVE    WRK-KGK2              TO  HCN70-NYUKAISU    (9)
      *        <金額>
               MOVE    SUM-NYUKHNKEI         TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN70-NYUKOUTEN   (9)
           END-IF
      *    <特定入院料・その他>
           MOVE    RECE82-NYUTOKKEI2   TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ          TO  WRK-TENSUX
           IF      RECE82-NYUTOKKEI2   <   ZERO
               PERFORM VARYING IDZ     FROM    6   BY  -1
                       UNTIL   IDZ     <       1
                   IF      WRK-TENSUX-T (IDZ)  =   SPACE
                       MOVE    "-"         TO  WRK-TENSUX-T (IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
           MOVE    WRK-TENSUX          TO  HCN70-NYUKOUTEN (10)
      *
      *       <入院基本料種別>
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       13
                   OR      RECE82-NYUSYUBETU (IDX) =   SPACE
               IF      RECE82-NYUSYUBETU (IDX) (11:)
                                       =   SPACE
                   MOVE    RECE82-NYUSYUBETU (IDX)
                                         TO  HCN70-NYUINSYUBETU-S (IDX)
               ELSE
                   MOVE    RECE82-NYUSYUBETU (IDX)
                                         TO  HCN70-NYUINSYUBETU2-S(IDX)
               END-IF
           END-PERFORM
      *
      *    ９０小計
           MOVE    RECE82-NYUTOTALTEN     TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ             TO  HCN70-NYUKOUTEN  (11)
      *
      *    小計イ
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       22
      *        <手技点数>
               MOVE    RECE82-TENSU(IDX)       TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCN70-KOUTEN   (IDX)
      *        <薬剤等点数>
               MOVE    RECE82-TENSU-YAK(IDX)   TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ              TO  HCN70-YKZTENSU (IDX)
           END-PERFORM
      *
      *    小計ロ
           MOVE    RECE82-TOTALTEN        TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ             TO  HCN70-KOUTENG
      *    薬剤等点数計
           MOVE    RECE82-TOTALTEN-YAK    TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ             TO  HCN70-YKZTENSUG
      *
      *    診療内容・金額
      *    （１０）診察
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       2
      *        <金額>
               MOVE    RECE82-KINGAKU2 (IDX)   TO  WRK-KINGAKU8
               MOVE    WRK-KINGAKU8           TO  HCN70-KINGAKU (IDX)
           END-PERFORM
      *
      *     （１０）小計（ハ）
           MOVE    RECE82-KINGAKU2-SYO TO  WRK-KINGAKU8
           MOVE    WRK-KINGAKU8        TO  HCN70-KINGAKU (3)
      *
      *    （８０）その他
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       3
               COMPUTE IDZ     =   IDX     +   3  
      *        <金額>
               MOVE    RECE82-KINGAKU3 (IDX)   TO  WRK-KINGAKU8
               MOVE    WRK-KINGAKU8            TO  HCN70-KINGAKU (IDZ)
           END-PERFORM
      *    <入院室料加算>
           MOVE    RECE82-HEYA-NINZU   TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  HCN70-SONOTANIN
           MOVE    RECE82-HEYA-NISSU   TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  HCN70-SONOTAKAISU
      *
      *     （８０）小計（二）
           MOVE    RECE82-KINGAKU3-SYO TO  WRK-KINGAKU8
           MOVE    WRK-KINGAKU8        TO  HCN70-KINGAKU (7)
      *
      *    ９７食事
           MOVE    RECE82-SHOKUJISYUBETU (1)
                                       TO  HCN70-KIJYUN
           PERFORM VARYING IDX     FROM    2   BY  1
                   UNTIL   IDX     >       4
               COMPUTE IDZ     =   IDX     -   1
               MOVE    RECE82-SHOKUJISYUBETU(IDX)
                                               TO  HCN70-SYOKBN (IDZ)
           END-PERFORM
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       4
               IF      RECE82-SHOKUJIKHNNISSU(IDX) >   ZERO      
                   MOVE    RECE82-SHOKUJIKHNRYO (IDX)
                                               TO  WRK-KGK4
                   MOVE    WRK-KGK4            TO  HCN70-SYOKHNTEN(IDX)          
                   MOVE    RECE82-SHOKUJIKHNNISSU (IDX)
                                               TO  WRK-ZZZ
                   MOVE    WRK-ZZZ             TO  HCN70-SYOKAISU (IDX)
                   EVALUATE    IDX
                       WHEN    4
                           MOVE    "円"            TO  HCN70-SYOYEN
                           MOVE    "×"            TO  HCN70-SYOKAKERU
                           MOVE    "日"            TO  HCN70-SYONICHI
                   END-EVALUATE
               END-IF                                        
           END-PERFORM          
      *
      *     （９７）小計
           MOVE    RECE82-SHOKUJITOTALKIN  TO  WRK-KINGAKU6
           MOVE    WRK-KINGAKU6            TO  HCN70-SYOKINGAKU
      *
      *    診断書料
           MOVE    RECE82-SINDAN-TUU       TO  WRK-ZZ
           MOVE    WRK-ZZ                  TO  HCN70-SHINDANSYOSUU
      *    診断書金額
           MOVE    RECE82-SINDAN-KIN       TO  WRK-KINGAKU6
           MOVE    WRK-KINGAKU6            TO  HCN70-SHINDANSYOMONEY
      *    明細書料
           MOVE    RECE82-MEISAI-TUU       TO  WRK-ZZ
           MOVE    WRK-ZZ                  TO  HCN70-MEISAISYOSUU
      *    明細書金額
           MOVE    RECE82-MEISAI-KIN       TO  WRK-KINGAKU6
           MOVE    WRK-KINGAKU6            TO  HCN70-MEISAISYOMONEY
      *
           .
       20022-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20023-BYOMEI-HEN-SEC        SECTION.
      *     
           MOVE    SPACE           TO  WRK-BYOMEI1
                                       WRK-BYOMEI2
      *
      *    傷病名の編集
           INITIALIZE                  RECE83-REC
           MOVE    RECE82-KEY      TO  RECE83-KEY1
      *
           START   RECE83-FILE
               KEY IS    >=   RECE83-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE83
               NOT INVALID
                   PERFORM 990-RECE83-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE83     =   1           )
               OR     ( RECE82-KEY NOT =   RECE83-KEY1 )
      *        病名編集処理
               PERFORM 200231-BYOMEI-SET-SEC
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE83     
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
       200231-BYOMEI-SET-SEC           SECTION.
      *
      *    病名をつなげると何桁になるかを計算（　　＋病名）
           IF      WRK-CLM         =   ZERO
               MOVE    WRK-CLM         TO  WRK-WCLM
           ELSE
               COMPUTE WRK-WCLM    =   WRK-CLM +   4
           END-IF
           COMPUTE WRK-WCLM    = ( RECE83-BYOMEIMOJI   *   2 )
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
           COMPUTE WRK-MOJISU      =   RECE83-BYOMEIMOJI   *   2
      *     
           EVALUATE    TRUE
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
      *            傷病名欄に編集
                   MOVE    RECE83-BYOMEI   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
      *            適用欄に編集
                   MOVE    RECE83-BYOMEI   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *
           PERFORM 990-RECE83-NEXT-SEC
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
                                       TO  HCN70-SYOBYO-S (IDZZ)
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
               MOVE    "傷"            TO  HCN70-MARK (1)
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
                                       TO  HCN70-TEKIYO  (IDZZ)
               ELSE
                   MOVE    WRK-BYOMEI2-NAME-T (IDZZ)
                                       TO  HCM77-TEKIYO  (IDZZ)
               END-IF    
      *     
               MOVE    IDZZ            TO  WRK-LINETEK
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
      *    帳票編集<入退院履歴情報・入院基本料種別の続き>処理
      *****************************************************************
       20024-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
      *
           MOVE    1                   TO  FLG-MARK
           MOVE    "01"                TO  WRK-SRYKBN
      * 
      *    最初の行のコメントのセット
           MOVE    SPACE               TO  WRK-NYUTAI-RIREKI-AREA
           MOVE    "入退院履歴"        TO  WRK-NYUTAI-MSG
      *          
           INITIALIZE                      RECE88-REC
           MOVE    RECE82-KEY          TO  RECE88-KEY1
      *
           START   RECE88-FILE
               KEY IS    >=   RECE88-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE88
               NOT INVALID
                   PERFORM 900-RECE88-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE88      =  1           )
                   OR    ( RECE82-KEY  NOT =  RECE88-KEY1 )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE88-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-NYUINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD (3:)
                   MOVE    "〜"                TO  WRK-NYUTAI-FL1
               END-IF
      *                          
               IF      RECE88-TAIINYMD (1:6)   
                                           >   RECE82-SRYYM
                   MOVE    SPACE               TO  WRK-NYUTAI-TAIINYMD
                   CONTINUE
               ELSE
                   MOVE    RECE88-TAIINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-TAIINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD (3:)
               END-IF
      *
               IF      RECE88-NYUINCHUKBN  =   ZERO
                   PERFORM 200241-NYUTAI-RIREKI-HENSYU-SEC
               END-IF
      *
               MOVE    RECE88-NYUINCHUKBN  TO  WRK-NYUINCHUKBN
      *
               PERFORM 900-RECE88-NEXT-SEC
           END-PERFORM
      *
      *    最後の履歴が入院中のときに編集する
           IF      WRK-NYUTAI-RIREKI-AREA
                                       NOT =   SPACE
               PERFORM 200241-NYUTAI-RIREKI-HENSYU-SEC
           END-IF
      *
      *    入院基本料種別
           IF      RECE82-NYUSYUBETU (14)  NOT =   SPACE
               MOVE    SPACE           TO  WRK-TEKIYOU-G
               MOVE    ZERO            TO  WRK-TEKIYOU-MAX
      *
               PERFORM VARYING IDZZ    FROM    14   BY  1
                       UNTIL   IDZZ    >       20
                       OR      RECE82-NYUSYUBETU (IDZZ)    =   SPACE
                   MOVE    1               TO  WRK-TEKIYOU-MAX
                   MOVE    RECE82-NYUSYUBETU (IDZZ)
                                           TO  
                                       WRK-TEKIYOU-T (WRK-TEKIYOU-MAX)
      *
      *            摘要欄編集処理
                   MOVE    1               TO  FLG-MARK
                   PERFORM 2002411-TEKIYOU-HENNSYU-SEC
               END-PERFORM
           END-IF    
      *
           .
       20024-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院履歴摘要欄編集処理
      *****************************************************************
       200241-NYUTAI-RIREKI-HENSYU-SEC SECTION.
      *
           IF      WRK-NYUTAI-MSG      =   SPACE
               MOVE    WRK-NYUTAI-RIREKI-O     TO  WRK-TEKIYOU-G
           ELSE
               MOVE    WRK-NYUTAI-RIREKI-AREA  TO  WRK-TEKIYOU-G
           END-IF
      *
      *    摘要欄編集処理
           MOVE    2               TO  WRK-TEKIYOU-MAX
           PERFORM 2002411-TEKIYOU-HENNSYU-SEC
      *
          MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
           .
       200241-NYUTAI-RIREKI-HENSYU-EXT.
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
                                               HCN70-SRYKBN(WRK-LINETEK)
                   END-IF
      *
                   IF      FLG-MARK        >   ZERO
                       MOVE    "＊"            TO
                                               HCN70-MARK  (WRK-LINETEK)
                   END-IF
      *
                   MOVE    WRK-TEKIYOU-T (IDXX)
                                           TO  HCN70-TEKIYO(WRK-LINETEK)
                   IF      FLG-NEXT        =   1
                       MOVE    "＆"           TO
                                      HCN70-TEKIYO(WRK-LINETEK)(29:2)
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
      *    帳票編集（手術歴）処理
      *****************************************************************
       20025-OPECOM-HEN-SEC      SECTION.
      *
           INITIALIZE                      WRK-TEKIYOU-G
           MOVE    ZERO                TO  FLG-MARK
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE82-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE82-PTID         TO  OPECOM-PTID
           MOVE    RECE82-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE82-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE82-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE82-HKNID  )
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-TEKIYOU-G
                   MOVE    1                   TO  FLG-MARK
                   PERFORM 200251-OPECOM-HENSYU-SEC
                   MOVE    OPECOM-OPENAME      TO  WRK-TEKIYOU-G
                   PERFORM 200251-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-TEKIYOU-G
                       PERFORM 200251-OPECOM-HENSYU-SEC
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
           MOVE    RECE82-HOSPNUM      TO  OPECOM-HOSPNUM
           MOVE    RECE82-PTID         TO  OPECOM-PTID
           MOVE    RECE82-SRYYM        TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE82-NYUGAIKBN    TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               IF    ( OPECOM-HKNNUM    =   RECE82-HKNNUM )
               AND   ( OPECOM-HKNID     =   RECE82-HKNID  )
                   MOVE    OPECOM-OPENAME      TO  WRK-TEKIYOU-G
                   MOVE    1                   TO  FLG-MARK
                   PERFORM 200251-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-TEKIYOU-G
                       PERFORM 200251-OPECOM-HENSYU-SEC
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
       20025-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    手術歴編集処理
      *****************************************************************
       200251-OPECOM-HENSYU-SEC    SECTION.
      *
      *    摘要欄編集処理
           MOVE    2               TO  WRK-TEKIYOU-MAX
           PERFORM 2002411-TEKIYOU-HENNSYU-SEC
           .
       200251-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集（適用）処理
      *****************************************************************
       20026-ZAI-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    ZERO                TO  CNT-RECE87
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE87-REC
           MOVE    RECE82-KEY          TO  RECE87-KEY1
      *
           START   RECE87-FILE
               KEY IS      >=  RECE87-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE87
               NOT INVALID
                   PERFORM 990-RECE87-NEXT-SEC
           END-START
           PERFORM UNTIL  ( FLG-RECE87         =   1           )
                   OR     ( RECE82-KEY     NOT =   RECE87-KEY1 )
      *        レセプト０６ファイル編集処理
               PERFORM 200261-RECE87-HENSYU-SEC
      *
               PERFORM 990-RECE87-NEXT-SEC
           END-PERFORM
      *
           .
       20026-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       200261-RECE87-HENSYU-SEC           SECTION.
      *
           IF      WRK-LINETEK         >=  WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1               TO  WRK-LINETEK
      *
           IF      RECE87-ZAINUM    NOT =   WRK-R061-ZAINUM-O
      *        診療区分
               IF      RECE87-SRYKBN
                                   NOT =   WRK-R061-SRYKBN-O
                   IF      CNT-PAGE        =   1
                       MOVE    RECE87-SRYKBN   TO  HCN70-SRYKBN
                                                       (WRK-LINETEK)
                   ELSE
                       MOVE    RECE87-SRYKBN   TO  HCM77-SRYKBN
                                                       (WRK-LINETEK)
                   END-IF
               END-IF
           END-IF
      *
      *    明細
           INSPECT RECE87-TEKIYO-INF   REPLACING   ALL "  "    BY  "　"
      *
           IF      RECE87-TEKIYO2-KISAIFLG =   1
      ***      MOVE    RECE87-TEKIYO2-TEN  TO  WRK-TENSUZ
      ***      MOVE    WRK-TENSUZ          TO  WRK-TENSUX
               MOVE    RECE87-TEKIYO2-TEN  TO  WRK-TENSUZ2
               MOVE    WRK-TENSUZ2         TO  WRK-TENSUX
               PERFORM VARYING IDZZ    FROM    7   BY  -1
                       UNTIL   IDZZ    <       1
                   IF      WRK-TENSUX-T (IDZZ)     =   SPACE
                       IF      RECE87-TEKIYO2-TEN      <   0
                           MOVE    "-"         TO  WRK-TENSUX-T (IDZZ)
                       ELSE
                           IF      RECE87-TEKIYO2-YENFLG   =   1
                               MOVE    "\"     TO  WRK-TENSUX-T (IDZZ)
                           END-IF
                       END-IF
                       MOVE    1           TO  IDZZ
                   END-IF
               END-PERFORM
      *
               IF      CNT-PAGE        =   1
                   MOVE    WRK-TENSUX      TO  HCN70-TENSU (WRK-LINETEK)
                   MOVE    "×"            TO  HCN70-MARK2 (WRK-LINETEK)
                   MOVE    RECE87-TEKIYO2-KAISU
                                           TO  WRK-KAISUZ
                   MOVE    WRK-KAISUZ      TO  HCN70-KAISU (WRK-LINETEK)
               ELSE    
                   MOVE    WRK-TENSUX      TO  HCM77-TENSU (WRK-LINETEK)
                   MOVE    RECE87-TEKIYO2-KAISU
                                           TO  WRK-KAISUZ
                  MOVE    "×"             TO  HCM77-MARK2 (WRK-LINETEK)
                   MOVE    WRK-KAISUZ      TO  HCM77-KAISU (WRK-LINETEK)
                END-IF 
           END-IF    
      *    
           IF      CNT-PAGE        =   1
               MOVE    RECE87-ASTER    TO  HCN70-MARK   (WRK-LINETEK)
               MOVE    RECE87-TEKIYO   TO  HCN70-TEKIYO (WRK-LINETEK)
           ELSE    
               MOVE    RECE87-ASTER    TO  HCM77-MARK   (WRK-LINETEK)
               MOVE    RECE87-TEKIYO   TO  HCM77-TEKIYO (WRK-LINETEK)
           END-IF    
      *
           MOVE    RECE87-ZAINUM       TO  WRK-R061-ZAINUM-O
           MOVE    RECE87-SRYKBN       TO  WRK-R061-SRYKBN-O
      *
           .
       200261-RECE87-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       20027-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 20024-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 20024-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 20024-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
           PERFORM 20024-RECECOM-HENSYU-SEC
           .
       20027-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       20024-RECECOM-HENSYU-SEC    SECTION.
      *     
           INITIALIZE                      RECECOM-REC
           MOVE    RECE82-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE82-PTID         TO  RECECOM-PTID
           MOVE    RECE82-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
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
               IF      RECECOM-HKNCOMBI    =   RECE82-HKNCOMBI
                   PERFORM 200241-COMMENT-HENSYU-SEC                 
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
       20024-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       200241-COMMENT-HENSYU-SEC    SECTION.
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
       200241-COMMENT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       20028-PRINT-OUT-SEC              SECTION.
      *
      *    レセプト種別不明のとき続紙は出力しない
           IF    ( RECE82-RECESYUBETU   =   ZERO )
           AND   ( CNT-PAGE             >   1    )
               GO  TO  20028-PRINT-OUT-EXT
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               MOVE    "HCN70.red" TO  TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCN70       TO  TBL-RECE-PRTDATA (TBL-RECE-MAX)
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
       20029-DB-WRITE-SEC               SECTION.
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
           MOVE    WRK-PARA-CREHMS     TO  RECE08-CREHMS
           MOVE    "0"                 TO  RECE08-PREFKBN
           MOVE    "00"                TO  RECE08-PREFNUM
           MOVE    RECE82-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    "4"                 TO  RECE08-TEISYUTUSAKI
           IF      TBL-RECE-RENNUM (IDXY)  =   1                                 
               MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
           ELSE
               MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
               MOVE    "2"                 TO  RECE08-RECESYUBETU
                                                            (4:1)
           END-IF                                                         
           MOVE    RECE82-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE82-SRYYM        TO  RECE08-SRYYM
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
               DISPLAY "ORCR0967 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE82-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 900-DB-ERR-ABORT-SEC
           ELSE    
               MOVE    RECE82-KANANAME     TO  RECE08-KANANAME
               MOVE    "1"                 TO  RECE08-NYUGAIKBN
               MOVE    WRK-PARA-TERMID     TO  RECE08-TERMID
               MOVE    ZERO                TO  RECE08-PRTJYUN
                                               RECE08-PRTRENNUM
               MOVE    TBL-RECE-FORMID  (IDXY)
                                           TO  RECE08-FORMID
               MOVE    TBL-RECE-PRTDATA (IDXY)
                                           TO  RECE08-PRTDATA
      *        連番を記載
               MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
               MOVE    TBL-RECE-RENNUM (IDXY) 
                                           TO  WRK-PAGE1
               MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
               EVALUATE    RECE08-FORMID
                   WHEN    "HCN70.red"
                       MOVE    RECE08-PRTDATA  TO HCN70
                       MOVE    WRK-PAGE1       TO HCN70-PAGE1
                       MOVE    WRK-PAGE2       TO HCN70-PAGE2
                       MOVE    HCN70           TO RECE08-PRTDATA
                   WHEN    "HCM77.red"                  
                       MOVE    RECE08-PRTDATA  TO HCM77
                       MOVE    WRK-PAGE1       TO HCM77-PAGE1
                       MOVE    WRK-PAGE2       TO HCM77-PAGE2
                       MOVE    HCM77           TO RECE08-PRTDATA
               END-EVALUATE
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
                   DISPLAY "ORCR0967 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE82-PTNUM           DELIMITED  BY  SIZE  
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
               DISPLAY "ORCR0967 MCP-RC= "  MCP-RC   
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
           IF    ( RECE82-TSTPTNUMKBN  =   "1"  )
           OR    ( RECE82-RECESYUBETU  =   ZERO )
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      RECE10-REC
           MOVE    RECE82-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE10-PTID
           MOVE    ZERO                TO  RECE10-RECEKA
           MOVE    4                   TO  RECE10-TEISYUTUSAKI
           MOVE    RECE82-RECESYUBETU  TO  RECE10-RECESYUBETU
           MOVE    ZERO                TO  RECE10-HKNJANUM
           MOVE    ZERO                TO  RECE10-HOJOKBN-KEY
           MOVE    RECE82-HKNID        TO  RECE10-KOHID-KEY
           MOVE    RECE82-TEKSTYMD     TO  RECE10-TEKSTYMD
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
               MOVE    RECE82-PTNUM        TO  RECE10-PTNUM
               MOVE    RECE82-HKNNUM       TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE82-HKNID        TO  RECE10-HKNID
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
           MOVE    RECE82-AGE          TO  RECE10-AGE
      *
           MOVE    RECE82-COMMENT      TO  RECE10-COMMENT
      *
           MOVE    "4"                 TO  RECE10-RECEKBN
      *
      *    平成２９年７月以降で第三者行為以外のとき
           IF    ( RECE82-SRYYM        >=  WRK-CONS-SRYYM-201707 )
           AND   ( WRK-PARA-RECEKBN
                                   NOT =   "8"                   )
               MOVE    "5"                 TO  RECE10-HKNKBN
                                               RECE10-HKNKBN-RSI
           END-IF
      *
           COMPUTE RECE10-TOTALTEN (1) =   RECE82-TOTALTEN     +
                                           RECE82-TOTALTEN-YAK
           COMPUTE RECE10-FTNMONEY-FTN (1)
                                       =   RECE82-TOTALKIN     +
                                           RECE82-TOTALKIN-YAK
           COMPUTE RECE10-FTNMONEY-KOJ (1) 
                                       =   RECE82-TOTALKIN2    +
                                           RECE82-KINGAKU3-SYO +
                                           RECE82-SINDAN-KIN   +
                                           RECE82-MEISAI-KIN
           MOVE    RECE82-SHOKUJITOTALKIN
                                       TO  RECE10-SHOKUJIRYOYOHI   (1)
      *
           COMPUTE RECE10-FTNMONEY (1) =   RECE82-JIBAI-GOKEI-AF
      *
           IF      RECE82-JNISSU       =   999
               MOVE    ZERO                TO  RECE10-JNISSU       (1)
           ELSE
               MOVE    RECE82-JNISSU       TO  RECE10-JNISSU       (1)
           END-IF
           MOVE    RECE82-SHOKUJITOTALDAY
                                       TO  RECE10-SHOKUJINISSU     (1) 
      *
           COMPUTE RECE10-TENSU (1 1)  =   RECE82-TENSU     (1)    +
                                           RECE82-TENSU-YAK (1)
           COMPUTE RECE10-TENSU (3 1)  =   RECE82-TENSU     (2)    +
                                           RECE82-TENSU-YAK (2)
           COMPUTE RECE10-TENSU (5 1)  =   RECE82-TENSU     (4)    +
                                           RECE82-TENSU     (5)    +
                                           RECE82-TENSU     (6)    +
                                           RECE82-TENSU     (7)    +
                                           RECE82-TENSU     (8)    +
                                           RECE82-TENSU     (9)    +
                                           RECE82-TENSU-YAK (4)    +
                                           RECE82-TENSU-YAK (5)    +
                                           RECE82-TENSU-YAK (6)    +
                                           RECE82-TENSU-YAK (7)    +
                                           RECE82-TENSU-YAK (8)    +
                                           RECE82-TENSU-YAK (9)
           COMPUTE RECE10-TENSU (6 1)  =   RECE82-TENSU     (11)   +
                                           RECE82-TENSU     (12)   +
                                           RECE82-TENSU-YAK (11)   +
                                           RECE82-TENSU-YAK (12)
           COMPUTE RECE10-TENSU (7 1)  =   RECE82-TENSU     (13)   +
                                           RECE82-TENSU     (14)   +
                                           RECE82-TENSU-YAK (13)   +
                                           RECE82-TENSU-YAK (14)
           COMPUTE RECE10-TENSU (8 1)  =   RECE82-TENSU     (15)   +
                                           RECE82-TENSU     (16)   +
                                           RECE82-TENSU-YAK (15)   +
                                           RECE82-TENSU-YAK (16)
           COMPUTE RECE10-TENSU (9 1)  =   RECE82-TENSU     (17)   +
                                           RECE82-TENSU     (18)   +
                                           RECE82-TENSU-YAK (17)   +
                                           RECE82-TENSU-YAK (18)
           COMPUTE RECE10-TENSU (10 1) =   RECE82-TENSU     (19)   +
                                           RECE82-TENSU     (20)   +
                                           RECE82-TENSU-YAK (19)   +
                                           RECE82-TENSU-YAK (20)
           COMPUTE RECE10-TENSU (11 1) =   RECE82-TENSU     (21)   +
                                           RECE82-TENSU     (22)   +
                                           RECE82-TENSU-YAK (21)   +
                                           RECE82-TENSU-YAK (22)
           MOVE    RECE82-NYUTOTALTEN  TO  RECE10-TENSU (12 1)
           MOVE    RECE82-KINGAKU2 (1) TO  RECE10-TENSU (13 1)
           MOVE    RECE82-KINGAKU2 (2) TO  RECE10-TENSU (16 1)
           MOVE    RECE82-KINGAKU3 (1) TO  RECE10-TENSU (17 1)
           COMPUTE RECE10-TENSU (18 1) =   RECE82-KINGAKU3 (2)     +
                                           RECE82-KINGAKU3 (3)
           MOVE    RECE82-SINDAN-KIN   TO  RECE10-TENSU (19 1)
           MOVE    RECE82-MEISAI-KIN   TO  RECE10-TENSU (20 1)
      *
           MOVE    RECE82-HKNCOMBI     TO  RECE10-HKNCOMBI (1)
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
               DISPLAY "ORCR0485 SEIKYU MCP-RC= "  MCP-RC
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
           OR    ( SYS-25-JIBAI-PDF    =   "0" )
               GO  TO  3905-JIBAI-PRT-HENSYU-EXT
           END-IF
      *
      *    テスト患者、レセプト種別不明のとき更新しない
           IF    ( RECE82-TSTPTNUMKBN  =   "1"  )
           OR    ( RECE82-RECESYUBETU  =   ZERO )
               GO  TO  3905-JIBAI-PRT-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      JIBAI-PRT-REC
           MOVE    RECE82-HOSPNUM      TO  JIBAI-PRT-HOSPNUM
           MOVE    RECE82-SRYYM        TO  JIBAI-PRT-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  JIBAI-PRT-NYUGAIKBN
           MOVE    RECE82-PTID         TO  JIBAI-PRT-PTID
           MOVE    ZERO                TO  JIBAI-PRT-RECEKA
           MOVE    4                   TO  JIBAI-PRT-TEISYUTUSAKI
           MOVE    RECE82-RECESYUBETU  TO  JIBAI-PRT-RECESYUBETU
           MOVE    ZERO                TO  JIBAI-PRT-HKNJANUM
           MOVE    ZERO                TO  JIBAI-PRT-HOJOKBN-KEY
           MOVE    RECE82-HKNID        TO  JIBAI-PRT-KOHID-KEY
           MOVE    RECE82-TEKSTYMD     TO  JIBAI-PRT-TEKSTYMD
           MOVE    RECE08-RENNUM       TO  JIBAI-PRT-RENNUM
           MOVE    RECE08-PRTDATA      TO  JIBAI-PRT-PRTDATA
           MOVE    RECE08-FORMID       TO  JIBAI-PRT-FORMID
           MOVE    RECE82-PTNUM        TO  JIBAI-PRT-PTNUM
      *
           MOVE    WRK-PARA-CREYMD     TO  JIBAI-PRT-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  JIBAI-PRT-CREHMS
           MOVE    "973"               TO  JIBAI-PRT-HKNNUM
           MOVE    "4"                 TO  JIBAI-PRT-RECEKBN
           MOVE    "rk2"               TO  JIBAI-PRT-PRTKBN
      *
      *    平成２９年７月以降で第三者行為以外のとき
           IF    ( RECE82-SRYYM        >=  WRK-CONS-SRYYM-201707 )
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
      *     MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦→和暦日付編集
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  STS-AREA-DAY
           INITIALIZE                      STS-AREA-DAY
           MOVE    SPACE               TO  LNK-DAY2-AREA
           MOVE    "21"                TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY2-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-HENYY
               MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-HENMM
               MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-HENDD
               MOVE    LNK-DAY2-EDTYMD1-G  TO  WRK-HENGENGO 
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
           STRING  "ORCR0967 "             DELIMITED   BY  SIZE
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
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
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
               MOVE    "1"                 TO  JIBAI-PRT-NYUGAIKBN
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
               MOVE    "1"                 TO  RECE07-NYUGAIKBN
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
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC23"              TO  RECE07-PRTID
               MOVE    "1"                 TO  RECE07-NYUGAIKBN
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
               MOVE    "1"                 TO  RECE08-NYUGAIKBN
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
      *        レセプト指示（個別指示）コード読み込み
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
                  PERFORM 900-KANRIMST-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SYSKANRI
               END-IF
      *
               PERFORM            UNTIL       FLG-SYSKANRI    =   1
                   MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
                   DISPLAY "DELETE 2002 PTNUM=" SYS-2002-PTNUM
                                      " SRYYM=" SYS-2002-SRYYM
      *
      *            該当診療年月の労災自賠医療機関情報
                   MOVE  SYS-2002-SRYYM TO  WRK-SYS-SRYYMD(1:6)
                   MOVE  "01"           TO  WRK-SYS-SRYYMD(7:2)
                   PERFORM 50012-SYSKANRI-HENSYU-SEC
      *
      *            診療年月の削除処理
                   MOVE    SYS-2002-PTID       TO  WRK-DEL-PTID
                   MOVE    SYS-2002-SRYYM      TO  WRK-DEL-SRYYM
                   PERFORM 50011-DB-DELETE-KOBETU-SEC
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-KANRIMST-NEXT-SEC
               END-PERFORM
      *   
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
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
           MOVE    "1"                 TO  RECE10-NYUGAIKBN
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
           MOVE    "1"                 TO  JIBAI-PRT-NYUGAIKBN
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
           MOVE    ZERO                TO  FLG-RECE82
           OPEN    INPUT               RECE82-FILE
           IF      STS-RECE82          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE82PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE82          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           PERFORM 900-RECE82-READ-NEXT-SEC
           PERFORM         UNTIL       FLG-RECE82  =   1
      *        診療年月の請求管理削除処理
               MOVE    ZERO                TO  FLG-DB-DELETE
      *
               INITIALIZE                      RECE10-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
               MOVE    RECE82-PTID         TO  RECE10-PTID
               MOVE    RECE82-SRYYM        TO  RECE10-SRYYM
               MOVE    RECE82-NYUGAIKBN    TO  RECE10-NYUGAIKBN
               MOVE    4                   TO  RECE10-TEISYUTUSAKI
                                               RECE10-XXTEISYUTUSAKI
               MOVE    "973"               TO  RECE10-HKNNUM
               MOVE    RECE82-HKNID        TO  RECE10-KOHID-KEY
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
                       DISPLAY "HKNKBN DELETE PTNUM=" RECE82-PTNUM
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
                   INITIALIZE                      JIBAI-PRT-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  JIBAI-PRT-HOSPNUM
                   MOVE    RECE82-PTID         TO  JIBAI-PRT-PTID
                   MOVE    RECE82-SRYYM        TO  JIBAI-PRT-SRYYM
                   MOVE    RECE82-NYUGAIKBN    TO  JIBAI-PRT-NYUGAIKBN
                   MOVE    4                   TO
                                               JIBAI-PRT-TEISYUTUSAKI
                   MOVE    "973"               TO  JIBAI-PRT-HKNNUM
                   MOVE    RECE82-HKNID        TO  JIBAI-PRT-KOHID-KEY
                   MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
                   MOVE    "del4"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " "  MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                       MOVE   "自賠責ＰＤＦ削除処理でエラーとなりました"
                                               TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       PERFORM 900-DB-ERR-ABORT-SEC
                   END-IF
               END-IF
      *
               PERFORM 900-RECE82-READ-NEXT-SEC
           END-PERFORM
      *
           CLOSE    RECE82-FILE
           MOVE    ZERO            TO  FLG-RECE82
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
           CLOSE                       RECE82-FILE
           CLOSE                       RECE83-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE88-FILE
      *
      *    ＰＵＳＨ通知処理
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
               PERFORM 3001-PUSH-SEC
           END-IF
      *
           DISPLAY "*** ORCR0967 IN   "  CNT-RECE82
           DISPLAY "*** ORCR0967 PAGE "  CNT-OUT
           DISPLAY "*** ORCR0967 PDF  "  CNT-PDF
           DISPLAY "*** ORCR0967 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
               MOVE    CNT-RECE82      TO  JOB-UPDCNT1
               PERFORM   900-CALL-ORCSJOB-SEC
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
               MOVE    CNT-RECE82      TO  WRK-CNT
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
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE82
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE82
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE82PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   IF      RECE82-PTNUM        =   SPACE
                   OR      RECE82-HKNKBN       =   "4"
                   OR      RECE82-RECESYUBETU  =   "0000"
      *
                       IF    ( RECE82-TOTALKIN      =   ZERO )
                       AND   ( RECE82-TOTALKIN-YAK  =   ZERO )
                       AND   ( RECE82-TOTALKIN2     =   ZERO )
                       AND   ( RECE82-TOTALKIN-ETC  =   ZERO )
                               GO  TO  990-RECE82-READ-SEC
                       END-IF
      *
                       DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                       ADD     1                   TO  CNT-RECE82
                       MOVE    RECE82-HOSPNUM      TO  KEY-N-HOSPNUM
                       MOVE    RECE82-SRYYM        TO  KEY-N-SRYYM
                       MOVE    RECE82-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                       MOVE    RECE82-PTID         TO  KEY-N-PTID
                       IF      WRK-PARA-SRYYM      =  ZERO
      *                    医療機関ＩＤ編集
                           MOVE  RECE82-SRYYM  TO  WRK-SYS-SRYYMD(1:6)
                           MOVE  "01"          TO  WRK-SYS-SRYYMD(7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
                   ELSE
                       GO  TO  990-RECE82-READ-SEC 
                   END-IF        
           END-READ
      *
           .
       990-RECE82-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトデータ順検索処理（削除用）
      *****************************************************************
       900-RECE82-READ-NEXT-SEC           SECTION.
      *
           READ    RECE82-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE82
               NOT AT  END
                   IF      STS-RECE82          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE82PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE82      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE82
      *
      *            平成２９年７月診療分以降のみ対象
                   IF      RECE82-SRYYM    <   WRK-CONS-SRYYM-201707-X
                       GO  TO  900-RECE82-READ-NEXT-SEC
                   END-IF
      *
                   DISPLAY "DELETE RECE82-PTNUM=" RECE82-PTNUM
                           " SRYYM=" RECE82-SRYYM
                           " HKNID=" RECE82-HKNID
      *
           END-READ
      *
           .
       900-RECE82-READ-NEXT-EXT.
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
                  MOVE    SYS-1900-PRTKBN (6)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (6)  TO  SYS-KBNCD
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
       990-RECE83-NEXT-SEC           SECTION.
      *
           READ    RECE83-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE83
               NOT AT  END
                   IF      STS-RECE83          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE83PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE83      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE83
                   IF      RECE83-TENKIKBN     =   "8"
                       GO  TO  990-RECE83-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE83-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE87-NEXT-SEC           SECTION.
      *
           READ    RECE87-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE87
               NOT AT  END
                   IF      STS-RECE87          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE87PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE87      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE87
                   ADD     1         TO  CNT-RECE87
                   IF      RECE87-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE87-SRYKBN
                   END-IF    
                   IF      RECE87-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE87-SRYKBN
                   END-IF
           END-READ
      *
           .
       990-RECE87-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み
      ****************************************************************
       900-RECE88-NEXT-SEC           SECTION.
      *
           READ    RECE88-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE88
               NOT AT  END
                   IF      STS-RECE88          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE88PARA      TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE88      TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE88
           END-READ
      *
           .
       900-RECE88-NEXT-EXT.
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
      *    パラメタ読込
      *****************************************************************
       900-BTPARA-READ-SEC         SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  BTPARA-HOSPNUM
           MOVE    BTPARA-REC          TO  MCPDATA-REC
           MOVE    "tbl_btpara"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
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
           PERFORM   900-DBFETCH-SEC
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
           STRING  "ORCR0967 db err PTNUM="
                                       DELIMITED   BY  SIZE
                   RECE82-PTNUM        DELIMITED   BY  SPACE
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
               STRING  "ORCR0967 select err TABLE="
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
