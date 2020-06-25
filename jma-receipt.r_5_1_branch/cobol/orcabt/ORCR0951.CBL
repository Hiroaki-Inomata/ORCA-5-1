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
       PROGRAM-ID.                 ORCR0951.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      傷病年金・印刷データ編集（労災）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-門脇    04/01/26  カナ氏名の長さ等修正
      *
      *  ２００４年４月改正
      *  01.01.01    NACL-藤原    04/03/29  請求点の桁数の変更
      *  01.01.02    NACL-藤原    04/03/30  手術歴の記載
      *  01.01.03    NACL-藤原    04/05/13  傷病の経過の編集の修正
      *  01.01.04    NACL-藤原    04/09/13  帳票別指定編集情報からの編集
      *  01.01.05    NACL-藤原    05/01/21  受診日指定対応
      *
      *  02.06.01    NACL-藤原    05/07/20  レセコメントのコメントの長さ
      *                                     変更による対応
      *  02.06.03    NACL-藤原    05/09/05  複数枚レセの連番記載
      *  02.06.04    NACL-門脇    05/12/01  労災レセプト回数対応
      *
      *  02.07.01    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *  02.07.02    NACL-門脇    06/01/24  DBCLOSECURSOR追加
      *  02.07.03    NACL-門脇    06/02/08  食事基本料情報（その他）編集
      *  02.09.01    NACL-門脇    06/04/13  ２００６年４月改正対応
      *
      *  03.00.01    NACL-藤原    06/06/20  レセコメントを保険組合せ・
      *                                     症状詳記区分順に記載する
      *  03.01.01    NACL-門脇    06/07/13  項目クリア追加
      *
      *  03.03.01    NACL-藤原    06/10/13  ２００６年１０月改正対応
      *
      *  03.05.01    NACL-門脇    07/06/01  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-門脇    13/01/21  労災レセ電対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
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
           SELECT  RECE871-FILE    ASSIGN  RECE871PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE871-KEY
                                   FILE    STATUS  IS  STS-RECE871.
      *    入退院履歴情報
           SELECT  RECE88-FILE     ASSIGN  RECE88PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE88-KEY
                                   FILE    STATUS  IS  STS-RECE88.
      *    日付ファイル
           SELECT  RECEYMD-FILE    ASSIGN  RECEYMD
                                   FILE    STATUS  IS  STS-RECEYMD.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    保険別患者情報ファイル
       FD  RECE82-FILE.
       01  RECE82-REC.
           COPY    "CPRCF082.INC".
      *
      *    病名ファイル    
       FD  RECE83-FILE.
       01  RECE83-REC.
           COPY    "CPRCF0831.INC" REPLACING  //RECE831//
                                   BY         //RECE83//.
      *
      *    点数摘要欄ファイル
       FD  RECE87-FILE.
       01  RECE87-REC. 
           COPY    "CPRCF087.INC".
      *
      *    金額摘要欄ファイル
       FD  RECE871-FILE.
       01  RECE871-REC. 
           COPY    "CPRCF0871.INC".
      *
       FD  RECE88-FILE.
       01  RECE88-REC. 
           COPY    "CPRCF088.INC".
      *
      *    日付ファイル
       FD  RECEYMD-FILE.
       01  RECEYMD-REC.
           03  RECEYMD-CREYMD      PIC X(08).
           03  RECEYMD-CREHMS      PIC X(06). 
           03  RECEYMD-RECENUM     PIC 9(06). 
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
                                   BY         //RECE871//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE88//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //RECEYMD//.
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    傷病年金（３４７０４）     
      *****COPY    "HCN21.INC"    REPLACING   //HCN21//
      *****COPY    "HCN21V01.INC" REPLACING   //HCN21V01//
      *****                        BY         //HC21//.
      *****COPY    "HCN21V02.INC" REPLACING   //HCN21V02//
      *****COPY    "HCN21V03.INC" REPLACING   //HCN21V03//
           COPY    "HCN21V04.INC" REPLACING   //HCN21V04//
                                   BY         //HC21//.
      *
      *    （続紙）     
      *****COPY    "HCM24V01.INC"  REPLACING  //HCM24V01//
      *****COPY    "HCM24V02.INC"  REPLACING  //HCM24V02//
      *****COPY    "HCM24V03.INC"  REPLACING  //HCM24V03//
           COPY    "HCM24V04.INC"  REPLACING  //HCM24V04//
                                   BY         //HC24//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE82          PIC X(02).
           03  STS-RECE83          PIC X(02).
           03  STS-RECE87          PIC X(02).
           03  STS-RECE871         PIC X(02).
           03  STS-RECE88          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEYMD         PIC X(02).
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
      *
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-SHOKUJI-ETC     PIC 9(01).
           03  FLG-STS             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE82          PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE87          PIC 9(06).
           03  CNT-RECE871         PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME.
               05  SYS-THH         PIC 9(02).
               05  SYS-TMM         PIC 9(02).
               05  SYS-TSS         PIC 9(02).
               05  SYS-TDD         PIC 9(02).
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDW                 PIC 9(04).
           03  IDW1                PIC 9(04).
           03  IDW2                PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDA                 PIC 9(04).
           03  IDB                 PIC 9(04).
           03  IDX1                PIC 9(02).
           03  IDX2                PIC 9(02).
           03  IDX3                PIC 9(02).
           03  IDXP                PIC 9(02).
           03  IDY1                PIC 9(03).
           03  IDY2                PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDXX                PIC 9(03).
           03  IDXY                PIC 9(03).
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
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC     OCCURS  100.
               05  TBL-SYS4001     PIC  X(610).
      *    帳票別医療機関編集情報指定退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC         OCCURS  100.
               05  TBL-SYS1900         PIC  X(610).
      *    
       01  SUM-AREA                          VALUE   ZERO.          
           03  SUM-NYUKHNNISSU         PIC 9(03).
           03  SUM-NYUKHNKEI           PIC 9(07)     OCCURS  2.
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-CREYMD  PIC  X(08).
               05  WRK-PARA-CREHMS  PIC  X(08).
               05  WRK-PARA-SRYYM   PIC  X(06).
               05  WRK-PARA-TERMID  PIC  X(16).
               05  WRK-PARA-SYSYMD  PIC  X(08).
               05  WRK-PARA-RECEKBN PIC  X(01).
               05  WRK-PARA-RECEYM  PIC  X(06).
               05  WRK-PARA-JOBID   PIC  9(07).
               05  WRK-PARA-SHELLID PIC  X(08).
           03  WRK-PARA-HOSPNUM     PIC  9(02).
      *
           03  WRK-SYS-SRYYMD       PIC  X(08).
      *
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-RECEERR         PIC X(200).
      *
           03  WRK-RECENUM          PIC 9(06). 
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-NENREI          PIC 9(05).
           03  WRK-NENREI-R        REDEFINES   WRK-NENREI.
               05  WRK-NENREI-R1   PIC 9(03).
               05  WRK-NENREI-R2   PIC 9(02).
           03  WRK-BIRTHDAY.
               05  WRK-BIRTHYM     PIC 9(06).
               05  WRK-BIRTHDD     PIC 9(02). 
      *
           03  WRK-LINE-MAX        PIC 9(03).          
           03  WRK-KEIKA-LINE      PIC 9(03).          
      *
           03  WRK-GOKEI           PIC 9(07).            
      *
           03  WRK-STYMD           PIC X(08).
           03  WRK-EDYMD           PIC X(08).
           03  WRK-STYMDW          PIC X(06).
           03  WRK-EDYMDW          PIC X(06).
      *
           03  WRK-OPECOM-MCP-TABLE
                                   PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME
                                   PIC X(64).
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG.
               05  WRK-HENGENGO    PIC X(01).
               05  WRK-HENYMD.
                   07  WRK-HENYY   PIC X(02).
                   07  WRK-HENMM   PIC X(02).
                   07  WRK-HENDD   PIC X(02).
      *
           03  WRK-EDIT-AREA.
      *        編集傷病名文字数 
               05  WRK-CLM                     PIC 9(03).
      *        ワーク編集傷病名文字数 
               05  WRK-WCLM                    PIC 9(03).
      *        病名文字数         
               05  WRK-MOJISU                  PIC 9(03).
      *        編集開始位置         
               05  WRK-STCLM                   PIC 9(03).
      *        傷病名欄編集行         
               05  WRK-LINEBYO                 PIC 9(02).
      *        適用欄編集行（点数）               
               05  WRK-LINETEK                 PIC 9(03).
      *        適用欄編集行（金額）               
               05  WRK-LINETEK1                PIC 9(03).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *         
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1.
               05  WRK-BYOMEI1-NAME            PIC X(44).
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2.  
               05  WRK-BYOMEI2-NAME            PIC X(52).
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  16.
           03  WRK-COMM                        PIC X(800).
           03  WRK-COMM1                       PIC X(800).
           03  WRK-COL                         PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
      *
      *    手術歴編集
           03  WRK-OPECOM          PIC 9(01).
           03  WRK-OPECOM-HEN      PIC X(50).
      *                 
      *    入退院履歴編集（適用欄）
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-RIREKI           PIC X(52).
               05  WRK-NYUTAI-RIREKI-W         REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  WRK-NYUTAI-MSG          PIC X(12).
                   07  WRK-NYUTAI-NYUINYMD     PIC X(18).
                   07  WRK-NYUTAI-FL1          PIC X(02).
                   07  WRK-NYUTAI-TAIINYMD     PIC X(18).
                   07  WRK-NYUTAI-FL2          PIC X(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE-HEN    PIC X(01).
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
      *    レセプト格納用
       01  TABLE-AREA                          VALUE   SPACE.
           03  TBL-RECE-MAX        PIC 9(02).
           03  TBL-RECE-OCC            OCCURS  99.
               05  TBL-RECE-PRTDATA
                                   PIC X(10000).
               05  TBL-RECE-FORMID PIC X(20). 
               05  TBL-RECE-RENNUM PIC 9(02).
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "―".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(16)   VALUE
                                   "＝＝＝＝＝＝＝＝".
               05  FILLER          PIC X(20)   VALUE
                                   "　金額行為（続き）　".
               05  FILLER          PIC X(16)   VALUE
                                   "＝＝＝＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(44)   VALUE
               ALL "―".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTENZ         PIC ZZZZZZZ.
           03  WRK-KAISUZ          PIC ZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZ.
           03  WRK-JNISSU          PIC ZZ9.
           03  WRK-TOTALTEN        PIC ZZ,ZZZ,ZZZ.
           03  WRK-TOTALTEN2       PIC ZZZ,ZZZ.
           03  WRK-NENREI-X        PIC ZZ9.
           03  WRK-KINGAKUZ        PIC ZZZ,ZZZ.
           03  WRK-KGK9            PIC ZZ,ZZZ,ZZ9.
           03  WRK-KGK4            PIC ZZ,ZZZ,ZZZ.
           03  WRK-KGK2            PIC ZZ.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-RECEKAISU       PIC ZZZ.
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
       01  WRK-CONS-SRYYM-200610   PIC 9(06)   VALUE   200610.
      *
       01  WRK-BYOMEI-AREA.
      * 
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   44.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   52.
      *     
      *    適用欄最大行数（点数）
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   22.
      *    適用欄最大行数（金額）
           03  WRK-TEKIYOU1-LINE   PIC 9(02)   VALUE   7.
      *     
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *     
      *    傷病の経過欄最大行数
           03  WRK-KEIKA-HC21-LINE PIC 9(02)   VALUE   8.
      *     
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HC24-LINE
                                   PIC 9(03)   VALUE   118.
      *
      *    ワーク日付ファイル
       01  WRK-RECEYMD-REC.
           03  WRK-RECEYMD-CREYMD      PIC X(08).
           03  WRK-RECEYMD-CREHMS      PIC X(06). 
           03  WRK-RECEYMD-RECENUM     PIC 9(06). 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK4001.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
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
      *    手術歴レセプトコメントマスター
       01  OPECOM-REC.
           COPY    "CPOPECOM.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
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
      *    主　　処理
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
      *    初期　処理
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
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    "RECE831"           TO  RECE83PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE83PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE83PARA-HOSPNUM
      *
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           MOVE    "RECE871"           TO  RECE871PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE871PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE871PARA-HOSPNUM
      *
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE88PARA-HOSPNUM
      *
           MOVE    "RECEYMD"           TO  RECEYMD-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEYMD-TERMID
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECEYMD-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE82PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE83PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE87PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECE871PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    RECE88PARA-BASENAME TO  SGETTEMP-BASENAMES  (6)
           MOVE    RECEYMD             TO  SGETTEMP-BASENAMES  (7)
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
                                       TO  RECE871PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE88PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECEYMD
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0951"      TO  JOB-PGID
               MOVE    "傷病年金・印刷データ編集（労災）"
                                       TO  JOB-SHELLMSG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           OPEN    INPUT               RECE82-FILE
           OPEN    INPUT               RECE83-FILE
           OPEN    INPUT               RECE87-FILE
           OPEN    INPUT               RECE871-FILE
           OPEN    INPUT               RECE88-FILE
      *
           MOVE    ZERO                TO  WRK-RECENUM
      *
           OPEN    INPUT               RECEYMD-FILE
           READ    RECEYMD-FILE
               AT  END
                   MOVE    "パラメタ日付ファイルがありません。"
                                         TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   CLOSE   RECEYMD-FILE
                   GO  TO  100-INIT-EXT 
               NOT AT  END
                   MOVE    RECEYMD-REC     TO  WRK-RECEYMD-REC
                   MOVE    WRK-RECEYMD-RECENUM
                                           TO  WRK-RECENUM
           END-READ
           CLOSE    RECEYMD-FILE
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
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
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
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
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
      *    労災医療機関情報
           IF   WRK-PARA-SRYYM    NOT =  ZERO
              INITIALIZE                   SYSKANRI-REC
              MOVE    "4001"           TO  SYS-KANRICD
              MOVE    "*"              TO  SYS-KBNCD
              MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
              MOVE    "01"             TO  SYS-STYUKYMD (7:2)
              MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
              MOVE    "tbl_syskanri"   TO  WRK-MCP-TABLE
              MOVE    "key10"          TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-SYSKANRI     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-4001-REC
                  IF      SYS-4001-RSIIRYOCDN  =   LOW-VALUE
                      MOVE    SPACE        TO  SYS-4001-RSIIRYOCDN
                  END-IF     
              ELSE    
              MOVE    "労災自賠責医療機関情報が取得できませんでした。"
                                           TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           ELSE
              MOVE     ZERO               TO  SYSKAN-IDX
              MOVE     SPACE              TO  TBL-SYS4001-T
              INITIALIZE                      TBL-SYS4001-T
      *
              INITIALIZE                      SYSKANRI-REC
              MOVE    "4001"              TO  SYS-KANRICD
              MOVE    "*"                 TO  SYS-KBNCD
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-4001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-4001-REC  TO  TBL-SYS4001(SYSKAN-IDX)
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
              MOVE    "労災自賠責医療機関情報が取得できませんでした。"
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
              IF      SYS-1900-PRTKBN (5)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (5)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (5)  TO  SYS-KBNCD
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
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-PRTKBN (5)  =   SPACE
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
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
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
                       PERFORM 2002-RECEIPT-HENSYU-SEC
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
           MOVE    WRK-HENYMD      TO  WRK-STYMDW
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
           MOVE    WRK-HENYMD      TO  WRK-EDYMDW
           . 
       2001-SRYYM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト編集処理
      *****************************************************************
       2002-RECEIPT-HENSYU-SEC                SECTION.
      *
           MOVE    1         TO  CNT-PAGE
           INITIALIZE                  WRK-EDIT-AREA
           INITIALIZE                  TABLE-AREA
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 20021-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 20022-TENSU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 20023-BYOMEI-HEN-SEC
      *
      *    帳票編集<傷病の経過>処理
           PERFORM 20024-COMMENT-HEN-SEC 
      *
      *    帳票編集<入退院履歴・入院基本料種別の続き>処理
           PERFORM 380-HC101-NYUTAI-RIREKI-HEN-SEC
      *
      *    帳票編集<手術歴>処理
           PERFORM 380-HC101-OPECOM-HEN-SEC 
      *
      *    帳票編集<適用（点数）>処理
           PERFORM 20025-ZAI-HEN-SEC
      *
      *    帳票編集<適用（金額）>処理
           PERFORM 20026-ZAI-HEN1-SEC
      *
      *    帳票印刷処理（続紙）
           IF      CNT-PAGE           >   1
               PERFORM 20027-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      FLG-STS            =   ZERO
               MOVE    1                  TO  CNT-PAGE
               PERFORM 20027-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票更新処理
           PERFORM 20028-DB-WRITE-SEC
      *
           .
       2002-RECEIPT-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       20021-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-LINE    TO  WRK-LINE-MAX
      *     
           INITIALIZE                      HC21
      *
      *    災害区分
           EVALUATE    RECE82-SAIGAIKBN
               WHEN    "1"
                   MOVE    "ギョウム"      TO  HC21-SAIGAIKBN
               WHEN    "2"
                   MOVE    "ツウサイ"      TO  HC21-SAIGAIKBN
           END-EVALUATE        
      *    患者番号
           MOVE    RECE82-PTNUM        TO  HC21-PTNUM
      *    指定病院等の番号
           MOVE    SYS-4001-RSIIRYOCDN TO  HC21-RSIIRYOCD
      *    病院等の名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC21-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC21-HOSPNAME (31:)
               MOVE    SYS-1901-HOSPNAME3  TO  HC21-HOSPNAME (61:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC21-HOSPNAME
           END-IF
           INSPECT HC21-HOSPNAME  REPLACING   ALL "  "    BY  "　"
      *    カナ氏名
           MOVE    RECE82-KANANAME     TO  HC21-KANANAME
      *    氏名
           MOVE    ZERO    TO   IDW2
           PERFORM   VARYING   IDW1   FROM   50   BY  -2
                           UNTIL   IDW1   <   1
               IF   RECE82-NAME(IDW1 - 1:2)  NOT =   SPACE
                           COMPUTE IDW2 = IDW1 / 2
                           MOVE   2   TO  IDW1
               END-IF
           END-PERFORM
           IF     IDW2   > 13
                MOVE    RECE82-NAME     TO  HC21-NAME2
           ELSE
                MOVE    RECE82-NAME     TO  HC21-NAME
           END-IF
      *    年齢
           MOVE    RECE82-BIRTHDAY     TO  WRK-BIRTHDAY    
           COMPUTE WRK-NENREI          =   RECE82-SRYYM
                                       -   WRK-BIRTHYM
           MOVE    WRK-NENREI-R1       TO  WRK-NENREI-X                            
           MOVE    WRK-NENREI-X        TO  HC21-NENREI
      *    年金証書番号
           MOVE    RECE82-KOFUNUM(1:2) TO  HC21-KOFUNUM (1:2)
           MOVE    RECE82-KOFUNUM(4:)  TO  HC21-KOFUNUM (4:)
      *    新継区分
           MOVE    RECE82-SINKEI       TO  HC21-SINKEI
      *    転帰事由
           MOVE    RECE82-TENKI        TO  HC21-TENKI 
      *    合計額
           COMPUTE WRK-GOKEI   =   RECE82-TOTALKIN
                               +   RECE82-KINGAKU2-SYO
                               +   RECE82-SHOKUJITOTALKIN
           MOVE    WRK-GOKEI           TO  WRK-KHNTENZ
           MOVE    WRK-KHNTENZ         TO  HC21-GOKEI
      *    生年月日
           MOVE    RECE82-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           EVALUATE    WRK-HENGENGO
               WHEN    "M"
                   MOVE    "○"            TO  HC21-TANJYOG (1)    
               WHEN    "T"
                   MOVE    "○"            TO  HC21-TANJYOG (2)    
               WHEN    "S"
                   MOVE    "○"            TO  HC21-TANJYOG (3)   
           END-EVALUATE     
           MOVE    WRK-HENYY           TO  HC21-TANJYOY
           MOVE    WRK-HENMM           TO  HC21-TANJYOM
           MOVE    WRK-HENDD           TO  HC21-TANJYOD
      *    療養開始年月日
           IF    RECE82-RYOSTYMD   NOT =  SPACE
             IF      RECE82-RYOSTYMD     <   WRK-STYMD
                 MOVE    WRK-STYMDW          TO  HC21-RYOSTYMD
             ELSE      
                 MOVE    RECE82-RYOSTYMD     TO  WRK-SYMD
                 PERFORM 31012-SE-WA-YMDEDIT-SEC
                 MOVE    WRK-HENYMD          TO  HC21-RYOSTYMD
             END-IF    
           ELSE
             MOVE   WRK-STYMDW(1:4)   TO   HC21-RYOSTYMD (1:4)
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       31
                 IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                     MOVE    IDX                 TO  WRK-KGK2
                     MOVE    WRK-KGK2            TO  HC21-RYOSTYMD (5:2)
                     MOVE    31                  TO  IDX
                 END-IF
             END-PERFORM
           END-IF
      *    療養終了年月日
           IF    RECE82-RYOEDYMD   NOT =  SPACE
             IF      RECE82-RYOEDYMD     >   WRK-EDYMD
                 MOVE    WRK-EDYMDW          TO  HC21-RYOEDYMD
             ELSE      
                 MOVE    RECE82-RYOEDYMD     TO  WRK-SYMD
                 PERFORM 31012-SE-WA-YMDEDIT-SEC
                 MOVE    WRK-HENYMD          TO  HC21-RYOEDYMD
             END-IF    
           ELSE
             MOVE   WRK-EDYMDW(1:4)   TO   HC21-RYOEDYMD (1:4)
             PERFORM VARYING IDX FROM    31  BY  -1
                     UNTIL   IDX <       1
                 IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                     MOVE    IDX                 TO  WRK-KGK2
                     MOVE    WRK-KGK2            TO  HC21-RYOEDYMD (5:2)
                     MOVE    1                   TO  IDX
                 END-IF
             END-PERFORM
           END-IF
      *    実日数
           MOVE    RECE82-JNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HC21-NISSU
      *
      *    労災レセプト回数
           IF   RECE82-ROUKAISU   =   "2"
             MOVE    RECE82-RECEKAISU  TO  WRK-RECEKAISU
             MOVE    WRK-RECEKAISU     TO  HC21-KAI
           END-IF
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HC24-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-HC24-LINE    TO  WRK-LINE-MAX
      *
           INITIALIZE                 HC24
      *      
      *    災害区分
           EVALUATE    RECE82-SAIGAIKBN
               WHEN    "1"
                   MOVE    "ギョウム"      TO  HC24-SAIGAIKBN
               WHEN    "2"
                   MOVE    "ツウサイ"      TO  HC24-SAIGAIKBN
           END-EVALUATE        
      *    患者番号
           STRING   RECE82-PTNUM   DELIMITED   BY  SPACE
                    "-"            DELIMITED   BY  SIZE
                    CNT-PAGE (3:1) DELIMITED   BY  SIZE
                                   INTO    HC24-PTNUM         
           END-STRING
      *    指定病院等の番号
           MOVE    SYS-4001-RSIIRYOCDN TO  HC24-HOSPNUM
      *    病院等の名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC24-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC24-HOSPNAME (31:)
               MOVE    SYS-1901-HOSPNAME3  TO  HC24-HOSPNAME (61:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC24-HOSPNAME
           END-IF
           INSPECT HC24-HOSPNAME  REPLACING   ALL "  "    BY  "　"
      *
           EVALUATE    RECE82-HKNKBN
               WHEN    "1"
      *            労働保険者番号
                   MOVE    RECE82-KOFUNUM      TO  HC24-HKNNUM 
               WHEN    "2"
      *            年金証書番号
                   MOVE    RECE82-KOFUNUM      TO  HC24-NENNUM
           END-EVALUATE        
      *    氏名
           MOVE    RECE82-NAME         TO  HC24-PTNAME
      *    カナ氏名
           MOVE    RECE82-KANANAME     TO  HC24-PTKANANAME
      *    年齢
           MOVE    WRK-NENREI-X        TO  HC24-PTAGE
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20023-BYOMEI-HEN-SEC      SECTION.
      *     
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2                  
      *
      *    傷病の部位編集
           IF      RECE82-COMMENT      =   SPACE
               CONTINUE
           ELSE
               ADD     1               TO  WRK-LINEBYO
               MOVE    RECE82-COMMENT  TO  HC21-SYOBYOMEI (WRK-LINEBYO)
               IF      RECE82-COMMENT (45:)    NOT =   SPACE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    RECE82-COMMENT (45:44)
                                       TO  HC21-SYOBYOMEI (WRK-LINEBYO)
               END-IF                        
               IF      RECE82-COMMENT (89:)    NOT =   SPACE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    RECE82-COMMENT (89:)
                                       TO  HC21-SYOBYOMEI (WRK-LINEBYO)
               END-IF                        
           END-IF
      *
      *    傷病名の編集                    
           INITIALIZE                      RECE83-REC
           MOVE    RECE82-HOSPNUM      TO  RECE83-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE83-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE83-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE83-PTID
           MOVE    RECE82-RECEKA       TO  RECE83-RECEKA
           MOVE    RECE82-HKNID        TO  RECE83-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE83-TEKSTYMD
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
               UNTIL  ( FLG-RECE83          =   1                 )
               OR     ( RECE82-HOSPNUM   NOT =   RECE83-HOSPNUM   )
               OR     ( RECE82-SRYYM     NOT =   RECE83-SRYYM     )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE83-NYUGAIKBN )
               OR     ( RECE82-PTID      NOT =   RECE83-PTID      )
               OR     ( RECE82-RECEKA    NOT =   RECE83-RECEKA    )
               OR     ( RECE82-HKNID     NOT =   RECE83-HKNID     )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE83-TEKSTYMD  )
      *        病名編集処理
               PERFORM 200231-BYOMEI-SET-SEC
      *
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE83     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 200232-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
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
      *    病名をつなげると何桁になるかを計算（，＋病名）
           IF      WRK-CLM           =   ZERO
               MOVE    WRK-CLM   TO  WRK-WCLM
           ELSE
               COMPUTE WRK-WCLM  =   WRK-CLM   +   2
           END-IF
           COMPUTE     WRK-WCLM  =
                              ( RECE83-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                       IF  WRK-BYOMEI1  NOT =  SPACE
                           PERFORM 200232-BYOMEI1-OUT-SEC
                       END-IF
                   ELSE
                       IF  WRK-CLM       >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM =   WRK-CLM   +   1
                           MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                       END-IF
                   END-IF
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                       PERFORM 200233-BYOMEI2-OUT-SEC
                   ELSE
                       IF  WRK-CLM       >   ZERO
      *                前の病名の直後に編集
                           COMPUTE WRK-STCLM =   WRK-CLM   +
                                                           1
                           MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +
                                                           2
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM       =   WRK-CLM   +   1
      *     
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE83-BYOMEIMOJI   *   2
      *     
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    RECE83-BYOMEI   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       PERFORM 200232-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    RECE83-BYOMEI(45:)
                                                   TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                       ELSE
                           MOVE    RECE83-BYOMEI(45:)
                                                   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                       END-IF                                   
                       MOVE    1               TO  WRK-CONTINUE    
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    RECE83-BYOMEI   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       PERFORM  200233-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    RECE83-BYOMEI(53:)
                                               TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *     
      *
           PERFORM 990-RECE83-NEXT-SEC
      *
           .
       200231-BYOMEI-SET-EXT.
           EXIT.
      *
      ****************************************************************
      *     傷病名欄出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       200232-BYOMEI1-OUT-SEC        SECTION.
      *
           ADD     1               TO  WRK-LINEBYO
      *
           MOVE    WRK-BYOMEI1     TO  HC21-SYOBYOMEI (WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
      *
           .
       200232-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名 適用欄出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       200233-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF    
      *     
           ADD     1               TO  WRK-LINETEK
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
      *
           IF      CNT-PAGE        =   1
               MOVE    WRK-BYOMEI2     TO  HC21-TEKIYO2 (WRK-LINETEK)
           ELSE
               MOVE    WRK-BYOMEI2     TO  HC24-TEKIYO  (WRK-LINETEK)
           END-IF    
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           .
       200233-BYOMEI2-OUT-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<適用（金額）>処理
      *****************************************************************
       20026-ZAI-HEN1-SEC      SECTION.
      *
           MOVE    ZERO                TO  CNT-RECE871
                                           WRK-LINETEK1
      *
      * 
           INITIALIZE                      RECE871-REC
           MOVE    RECE82-HOSPNUM      TO  RECE871-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE871-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE871-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE871-PTID
           MOVE    RECE82-RECEKA       TO  RECE871-RECEKA
           MOVE    RECE82-HKNID        TO  RECE871-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE871-TEKSTYMD
      *
           START   RECE871-FILE
               KEY IS    >=   RECE871-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE871
               NOT INVALID
                   PERFORM 990-RECE871-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE871          =   1                )
                   OR    ( RECE82-HOSPNUM  NOT =   RECE871-HOSPNUM   )
                   OR    ( RECE82-SRYYM    NOT =   RECE871-SRYYM     )
                   OR    ( RECE82-NYUGAIKBN
                                           NOT =   RECE871-NYUGAIKBN )
                   OR    ( RECE82-PTID     NOT =   RECE871-PTID      )
                   OR    ( RECE82-RECEKA   NOT =   RECE871-RECEKA    )
                   OR    ( RECE82-HKNID    NOT =   RECE871-HKNID     )
                   OR    ( RECE82-TEKSTYMD NOT =   RECE871-TEKSTYMD  )
      *
      *        明細
               INSPECT RECE871-TEKIYO-INF
                               REPLACING    ALL   "  "   BY   "　"
      *                         
               EVALUATE    TRUE        
                   WHEN    WRK-LINETEK1        <   WRK-TEKIYOU1-LINE
                       ADD     1                   TO  WRK-LINETEK1
                       MOVE    RECE871-TEKIYO-INF  TO  HC21-TEKIYO1
                                                       (WRK-LINETEK1)
                   WHEN    WRK-LINETEK1        =   WRK-TEKIYOU1-LINE
                       IF      WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK         >=   WRK-LINE-MAX
                               PERFORM 34120-PAGE-SEC
                           END-IF    
                           ADD     1           TO  WRK-LINETEK
                                                   WRK-LINETEK1
                           IF      CNT-PAGE        =   1 
                               MOVE    WRK-KUGIRI2     TO 
                                               HC21-TEKIYO2(WRK-LINETEK)
                           ELSE
                               MOVE    WRK-KUGIRI2     TO 
                                               HC24-TEKIYO (WRK-LINETEK)
                           END-IF
                       END-IF
      ***              ADD     1                   TO  WRK-LINETEK
                       IF      WRK-LINETEK          >=   WRK-LINE-MAX
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       ADD     1                   TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1 
                           MOVE    RECE871-TEKIYO-INF  TO
                                               HC21-TEKIYO2(WRK-LINETEK)
                       ELSE
                           MOVE    RECE871-TEKIYO-INF  TO
                                               HC24-TEKIYO (WRK-LINETEK)
                       END-IF
                   WHEN    WRK-LINETEK1        >   WRK-TEKIYOU1-LINE
                       IF      WRK-LINETEK     >=  WRK-LINE-MAX
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1                   TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1 
                           MOVE    RECE871-TEKIYO-INF  TO
                                               HC21-TEKIYO2(WRK-LINETEK)
                       ELSE
                           MOVE    RECE871-TEKIYO-INF  TO
                                               HC24-TEKIYO (WRK-LINETEK)
                       END-IF
               END-EVALUATE        
      *
               PERFORM 990-RECE871-NEXT-SEC
           END-PERFORM
      *
           .
       20026-ZAI-HEN1-EXT.
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
                   MOVE  "○"       TO  HC21-SYOSHIN1
               WHEN    "2"
                   MOVE  "○"       TO  HC21-SYOSHIN2
               WHEN    "3"
                   MOVE  "○"       TO  HC21-SYOSHIN3
           END-EVALUATE
      *
      *    １１初診〜８０他         
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   22
      *        <回数>
               MOVE    RECE82-KAISU(IDX)         TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ                TO  HC21-KAISU   (IDX)
      *        <点数>
               MOVE    RECE82-TENSU(IDX)         TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ                TO  HC21-TENSU   (IDX)
           END-PERFORM
      *
      *    ９０入院            
      *    <入院基本料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
      *        <基本料>
               MOVE    RECE82-NYUKHNRYO (IDX)
                                             TO  WRK-KHNTENZ
               MOVE    WRK-KHNTENZ           TO  HC21-NYUKHNTEN (IDX)
      *        <日数>
               MOVE    RECE82-NYUKHNNISSU(IDX)
                                             TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HC21-NYUKAISU  (IDX)
      *        <金額>
               MOVE    RECE82-NYUKHNKEI(IDX)
                                             TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HC21-NYUTENSU  (IDX)
           END-PERFORM
      *
           IF        RECE82-NYUKHNRYO (5)    >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    4   BY  1
                         UNTIL     IDX       >   10
                         OR        RECE82-NYUKHNRYO (IDX)    =   ZERO
      *            <日数>
                   ADD     RECE82-NYUKHNNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額>
                   ADD     RECE82-NYUKHNKEI(IDX)
                                                 TO  SUM-NYUKHNKEI (1)
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HC21-NYUKHNTEN (4)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HC21-NYUKAISU  (4)
      *        <金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HC21-NYUTENSU  (4)
           END-IF
      *    <入院基本料・加算>
           MOVE    RECE82-NYUKHNKEI2         TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ                 TO  HC21-NYUTENSU  (5)
           IF      RECE82-NYUKHNKEI2         <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
                       MOVE    "▲"        TO  HC21-NYUTENSUN (5 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
      *     
      *    <特定入院料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   3
               IF      RECE82-NYUTOKNISSU(IDX) >   ZERO      
                   COMPUTE IDY     =   IDX     +   5
      *
                   MOVE    "×"              TO  HC21-KAKERU    (IDX)                    
                   MOVE    "日間"           TO  HC21-NICHI     (IDX)                    
      *            <基本料>
                   IF      RECE82-NYUTOKRYO-RYAKU (IDX)
                                             =   SPACE
                       MOVE    RECE82-NYUTOKRYO (IDX)
                                             TO  WRK-KHNTENZ
                       MOVE    WRK-KHNTENZ   TO  HC21-NYUKHNTEN (IDY)
                   ELSE
                       MOVE    RECE82-NYUTOKRYO-RYAKU (IDX)
                                             TO  HC21-NYUKHNTENN(IDY)
                   END-IF    
      *            <日数>
                   MOVE    RECE82-NYUTOKNISSU(IDX)
                                             TO  WRK-KAISUZ
                   MOVE    WRK-KAISUZ        TO  HC21-NYUKAISU  (IDY)
      *            <金額>
                   MOVE    RECE82-NYUTOKKEI(IDX)
                                             TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ        TO  HC21-NYUTENSU  (IDY)
               END-IF    
           END-PERFORM
      *
           IF        RECE82-NYUTOKNISSU (4)  >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    3   BY  1
                         UNTIL     IDX       >   10
                         OR        RECE82-NYUTOKNISSU (IDX)  =   ZERO
      *            <日数>
                   ADD     RECE82-NYUTOKNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額>
                   ADD     RECE82-NYUTOKKEI(IDX)
                                                 TO  SUM-NYUKHNKEI (1)
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HC21-NYUKHNTEN (8)
                                                 HC21-NYUKHNTENN(8)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HC21-NYUKAISU  (8)
      *        <金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HC21-NYUTENSU  (8)
           END-IF
      *    <特定入院料・その他>
           MOVE    RECE82-NYUTOKKEI2       TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HC21-NYUTENSU  (9)
           IF      RECE82-NYUTOKKEI2       <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
                       MOVE    "▲"        TO
                                           HC21-NYUTENSUN (9 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
      *
      *        <入院年月日>
           MOVE    RECE82-NYUINYMD     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HC21-NYUINYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HC21-NYUINMM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  HC21-NYUINDD
      *
      *        <医療機関種別>
           EVALUATE    RECE82-HOSPSBT
               WHEN    "1"
                   MOVE    "○"        TO  HC21-HOSPSBT (1)
               WHEN    "2"
                   MOVE    "○"        TO  HC21-HOSPSBT (2)
               WHEN    "3"
                   MOVE    "○"        TO  HC21-HOSPSBT (1)
                                           HC21-HOSPSBT (3)
               WHEN    "4"
                   MOVE    "○"        TO  HC21-HOSPSBT (2)
                                           HC21-HOSPSBT (3)
           END-EVALUATE        
      *
      *       <入院基本料種別>
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       11
                   OR      RECE82-NYUSYUBETU (IDX) =   SPACE
               MOVE    RECE82-NYUSYUBETU (IDX)
                                     TO  HC21-NYUSYUBETSU (IDX)
           END-PERFORM
      *
      *    小計イ
           MOVE    RECE82-TOTALTEN        TO  WRK-TOTALTEN
           MOVE    WRK-TOTALTEN           TO  HC21-TOTALTEN
           IF      RECE82-TOTALKIN            >   ZERO
               MOVE    RECE82-TOTALKIN        TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC21-TOTALMONEY
           END-IF
      *
      *    診療内容・金額                     
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   5
      *        <金額>
               MOVE    RECE82-KINGAKU2(IDX)      TO  WRK-KINGAKUZ
               MOVE    WRK-KINGAKUZ              TO  HC21-KINGAKU (IDX)
           END-PERFORM
      *
      *    <小計ロ>
           IF      RECE82-KINGAKU2-SYO        >   ZERO
               MOVE    RECE82-KINGAKU2-SYO    TO  WRK-TOTALTEN2
               MOVE    WRK-TOTALTEN2          TO  HC21-SYOKEI
           END-IF
      *
      *    ９７食事
           STRING    RECE82-SHOKUJISYUBETU (1) DELIMITED   BY  SPACE
                     "　"                      DELIMITED   BY  SIZE
                     RECE82-SHOKUJISYUBETU (2) DELIMITED   BY  SPACE
                     "　"                      DELIMITED   BY  SIZE
                     RECE82-SHOKUJISYUBETU (3) DELIMITED   BY  SPACE
                                               INTO  HC21-SYOKJN
           END-STRING 
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
               IF      RECE82-SHOKUJIKHNRYO (IDX)    >   ZERO      
                   MOVE    RECE82-SHOKUJIKHNRYO (IDX)
                                             TO  WRK-KINGAKUZ
                   MOVE    WRK-KINGAKUZ      TO  HC21-SYOKHNTEN(IDX)          
                   MOVE    RECE82-SHOKUJIKHNNISSU (IDX)
                                             TO  WRK-ZZZ
                   MOVE    WRK-ZZZ           TO  HC21-SYOKAISU (IDX)
                   IF   RECE82-SRYYM  >=  "200604"
                     EVALUATE    IDX
                       WHEN    4
                           MOVE    "円×"    TO  HC21-SYOKAKERU
                           MOVE    "日間"    TO  HC21-SYONICHI
                     END-EVALUATE
                   ELSE
                     EVALUATE    IDX
                       WHEN    3
                           MOVE    "特"      TO  HC21-SYOKBN (IDX)
                       WHEN    4
                           MOVE    "選"      TO  HC21-SYOKBN (IDX)
                           MOVE    "円×"    TO  HC21-SYOKAKERU
                           MOVE    "日間"    TO  HC21-SYONICHI
                     END-EVALUATE
                   END-IF
               END-IF                                        
           END-PERFORM          
      *
      *    食事療養（日）          
           MOVE    RECE82-SHOKUJITOTALDAY
                                     TO  WRK-KGK2
           MOVE    WRK-KGK2          TO  HC21-SYONISSU
      *
      *    食事療養（ハ）          
           MOVE    RECE82-SHOKUJITOTALKIN
                                     TO  WRK-KGK4
           MOVE    WRK-KGK4          TO  HC21-SYOMONEY
      *
           .
       20022-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<剤>処理
      *****************************************************************
       20025-ZAI-HEN-SEC      SECTION.
      *
      *    摘要欄に既に印刷されている場合、１行あける
           IF  WRK-LINETEK             >   ZERO
      *
               IF  WRK-LINETEK          >=   WRK-LINE-MAX
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-KUGIRI1 TO  HC21-TEKIYO2 (WRK-LINETEK)
               ELSE   
                   MOVE    WRK-KUGIRI1 TO  HC24-TEKIYO  (WRK-LINETEK)
               END-IF    
           END-IF
      *
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  CNT-RECE87
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE87-REC
           MOVE    RECE82-HOSPNUM      TO  RECE87-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE87-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE87-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE87-PTID
           MOVE    RECE82-RECEKA       TO  RECE87-RECEKA
           MOVE    RECE82-HKNID        TO  RECE87-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE87-TEKSTYMD
      *
           START   RECE87-FILE
               KEY IS    >=   RECE87-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE87
               NOT INVALID
                   PERFORM 990-RECE87-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE87           =   1                  )
               OR     ( RECE82-HOSPNUM   NOT =   RECE87-HOSPNUM     )
               OR     ( RECE82-SRYYM     NOT =   RECE87-SRYYM       )
               OR     ( RECE82-NYUGAIKBN NOT =   RECE87-NYUGAIKBN   )
               OR     ( RECE82-PTID      NOT =   RECE87-PTID        )
               OR     ( RECE82-RECEKA    NOT =   RECE87-RECEKA      )
               OR     ( RECE82-HKNID     NOT =   RECE87-HKNID       )
               OR     ( RECE82-TEKSTYMD  NOT =   RECE87-TEKSTYMD    )
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE87-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE87-HENSYU-SEC
      *
               MOVE    RECE87-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE87-SRYKBN        TO  WRK-R061-SRYKBN-O
      *
               PERFORM 990-RECE87-NEXT-SEC
           END-PERFORM
      *
           .
       20025-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE87-HENSYU-SEC           SECTION.
      *
           IF  CNT-ZAIMEISAISU           =   1
      *        次の行為明細数を追加すると何行になるか
               COMPUTE   WRK-WLINETEK    =   WRK-LINETEK
                                         +   RECE87-ZAIMEISAISU
               IF  CNT-RECE87           >   1
      *            診療区分が変更している場合、診療区切り線分を追加
                   IF  RECE87-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       ADD     1             TO  WRK-WLINETEK
                       MOVE    1             TO  FLG-KUGIRI
                   END-IF
               END-IF
      *
               IF  WRK-WLINETEK          >         WRK-LINE-MAX

                   PERFORM 34120-PAGE-SEC
      *
      *            区切り線
                   IF  FLG-KUGIRI        =   2
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1               TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1
                       MOVE    RECE87-SRYKBN  TO  HC21-SRYKBN
                                                       (WRK-LINETEK)
                   ELSE
                       MOVE    RECE87-SRYKBN  TO  HC24-SRYKBN
                                                       (WRK-LINETEK)
                   END-IF                                    
               ELSE
      *            区切り線
                   IF  FLG-KUGIRI    NOT =   ZERO
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1             TO  WRK-LINETEK
                   IF  RECE87-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       IF      CNT-PAGE        =   1
                           MOVE    RECE87-SRYKBN  TO  HC21-SRYKBN
                                                       (WRK-LINETEK)
                       ELSE
                           MOVE    RECE87-SRYKBN  TO  HC24-SRYKBN
                                                       (WRK-LINETEK)
                       END-IF                                    
                   END-IF
               END-IF
           ELSE
               ADD     1                 TO  WRK-LINETEK
           END-IF
      *
      *    明細
           INSPECT RECE87-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    RECE87-TEKIYO-INF  TO  HC21-TEKIYO2(WRK-LINETEK)
           ELSE    
               MOVE    RECE87-TEKIYO-INF  TO  HC24-TEKIYO (WRK-LINETEK)
           END-IF    
           .
       3410-RECE87-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           IF      CNT-PAGE        =   1
               MOVE    WRK-KUGIRI1 TO  HC21-TEKIYO2 (WRK-LINETEK)
           ELSE    
               MOVE    WRK-KUGIRI1 TO  HC24-TEKIYO  (WRK-LINETEK)
           END-IF    
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    改ページ処理
      *****************************************************************
       34120-PAGE-SEC       SECTION.
      *
      *    帳票印刷処理
           IF      CNT-PAGE      >   1
               PERFORM 20027-PRINT-OUT-SEC
           END-IF    
      * 
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HC24-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       20024-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
                                           WRK-KEIKA-LINE
      *     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
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
grpsys     PERFORM 900-DBSELECT-SEC
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
       20024-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       200241-COMMENT-HENSYU-SEC    SECTION.
      *                                          
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
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
                               UNTIL   IDX >       800
                           IF      WRK-COMM(IDX:1) =   WRK-CONS-RETURN
                               COMPUTE IDY1     =   IDX     -   1
                               COMPUTE IDY2     =   IDX     +   1   
                               MOVE    WRK-COMM (1:IDY1)
                                                TO  WRK-COMMENT-G
                               MOVE    WRK-COMM (IDY2:)
                                                TO  WRK-COMM1
                               MOVE    800      TO  IDX
                           END-IF
                       END-PERFORM                                                                                  
                   ELSE
                       MOVE    WRK-COMM        TO  WRK-COMMENT-G
                       MOVE    1               TO  WRK-RECEHEN
                   END-IF
      *                                       
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       16
                           OR      WRK-COMMENT (IDX)   =   SPACE
                       IF      WRK-KEIKA-LINE  <   WRK-KEIKA-HC21-LINE
      *                    傷病の経過欄へ編集
                           ADD     1           TO  WRK-KEIKA-LINE       
                           MOVE    WRK-COMMENT (IDX)
                                               TO
                                           HC21-KEIKA (WRK-KEIKA-LINE)
                       ELSE
      *                    摘要欄へ編集
                           IF      WRK-RECECOM         =   ZERO
      *                        摘要欄に１行あける
                               MOVE    1               TO  WRK-RECECOM
                               IF      WRK-LINETEK         >   ZERO
                                   IF      WRK-LINETEK >=  WRK-LINE-MAX
                                       PERFORM 34120-PAGE-SEC
                                   ELSE
                                       PERFORM 34110-KUGIRI-LINE-SEC
                                   END-IF
                               END-IF    
                           END-IF
      *                 
                           IF      WRK-LINETEK      >=  WRK-LINE-MAX
                               PERFORM 34120-PAGE-SEC
                           END-IF
                           ADD     1               TO  WRK-LINETEK
                           IF      CNT-PAGE    =   1 
                               MOVE    WRK-COMMENT (IDX)
                                           TO  HC21-TEKIYO2(WRK-LINETEK)
                           ELSE
                               MOVE    WRK-COMMENT (IDX)
                                           TO  HC24-TEKIYO (WRK-LINETEK)
                           END-IF
                       END-IF                    
                   END-PERFORM
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       200241-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<入退院履歴情報・入院基本料種別の続き>処理
      *****************************************************************
       380-HC101-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
      *    最初の行のとき２行あける
           COMPUTE WRK-WLINETEK    =   WRK-LINETEK +   2                                       
           IF  WRK-WLINETEK        >   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     2               TO  WRK-LINETEK     
      *
           MOVE    ZERO                TO  WRK-NYUINCHUKBN
      * 
      *    最初の行のコメントのセット
           MOVE    SPACE               TO  WRK-NYUTAI-RIREKI-AREA
           MOVE    "入退院履歴"        TO  WRK-NYUTAI-MSG
      *          
           INITIALIZE                      RECE88-REC
           MOVE    RECE82-HOSPNUM      TO  RECE88-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE88-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE88-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE88-PTID
           MOVE    RECE82-RECEKA       TO  RECE88-RECEKA
           MOVE    RECE82-HKNID        TO  RECE88-HKNID
           MOVE    RECE82-TEKSTYMD     TO  RECE88-TEKSTYMD
      *
           START   RECE88-FILE
               KEY IS    >=   RECE88-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE88
               NOT INVALID
                   PERFORM 900-RECE88-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE88           =  1                )
                   OR    ( RECE82-HOSPNUM   NOT =  RECE88-HOSPNUM   )
                   OR    ( RECE82-SRYYM     NOT =  RECE88-SRYYM     )
                   OR    ( RECE82-NYUGAIKBN NOT =  RECE88-NYUGAIKBN )
                   OR    ( RECE82-PTID      NOT =  RECE88-PTID      )
                   OR    ( RECE82-RECEKA    NOT =  RECE88-RECEKA    )
                   OR    ( RECE82-HKNID     NOT =  RECE88-HKNID     )
                   OR    ( RECE82-TEKSTYMD   NOT =  RECE88-TEKSTYMD )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE88-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD
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
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD
               END-IF
      *
               IF      RECE88-NYUINCHUKBN  =   ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                               BY  "　"  
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC21-TEKIYO2(WRK-LINETEK)
                   ELSE                    
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HC24-TEKIYO(WRK-LINETEK)
                   END-IF                    
                   MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
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
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1                   TO  WRK-LINETEK
      *
               INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                           BY  "　"  
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC21-TEKIYO2(WRK-LINETEK)
               ELSE                    
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HC24-TEKIYO(WRK-LINETEK)
               END-IF                    
               MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
           END-IF
      *
      *    最後の行の後に１行あける
           IF      WRK-NYUTAI-RIREKI-AREA
                                       =   SPACE                   
               IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK     
           END-IF
      *
      *       <入院基本料種別>
           IF      RECE82-NYUSYUBETU (12)  NOT =   SPACE
               PERFORM VARYING IDXX    FROM    12   BY  1
                       UNTIL   IDXX    >       20
                       OR      RECE82-NYUSYUBETU (IDXX)    =   SPACE
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1   
                       MOVE    RECE82-NYUSYUBETU (IDXX)
                                       TO  HC21-TEKIYO2(WRK-LINETEK)
                   ELSE                    
                           MOVE    RECE82-NYUSYUBETU (IDXX)
                                       TO  HC24-TEKIYO(WRK-LINETEK)
                   END-IF                    
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK     
           END-IF    
      *
      *       <食事基本料情報（その他）>
           MOVE    ZERO                TO  FLG-SHOKUJI-ETC
           IF      RECE82-SHOKUJIKHNRYO-ETC(1)  NOT =   ZERO
               PERFORM VARYING IDXX    FROM     1   BY  1
                       UNTIL   IDXX    >        3
                       OR      RECE82-SHOKUJIKHNRYO-ETC(IDXX)  =  ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF
                   ADD     1               TO  WRK-LINETEK
      *
                   MOVE    SPACE    TO     WRK-SHOKUJIKHNINF-ETC-AREA
      *
                   MOVE  RECE82-SHOKUJIKHNRYO-ETC(IDXX)
                                           TO  WRK-HENSYU-KIN-9
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-HENSYU-KIN  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:8)
                                           TO  WRK-SHOKUJIKHNRYO-ETC
                   END-IF
      *
                   MOVE  RECE82-SHOKUJIKHNNISSU-ETC(IDXX)
                                           TO  WRK-HENSYU-DAY-9
                   IF      WRK-HENSYU-DAY(1:1) =  "0"
                      MOVE      SPACE      TO  WRK-HENSYU-DAY(1:1)
                   END-IF
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    WRK-HENSYU-DAY  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                        MOVE    KANACHK-OUT-INPUT(1:4)
                                           TO  WRK-SHOKUJIKHNNISSU-ETC
                   END-IF
      *
                   IF   FLG-SHOKUJI-ETC  =  ZERO
                     MOVE  "食事　" TO   WRK-SHOKUJIKHNRYO-ETC-NM
                     MOVE   1       TO   FLG-SHOKUJI-ETC
                   ELSE
                     MOVE  "　　　" TO   WRK-SHOKUJIKHNRYO-ETC-NM
                   END-IF
                   MOVE    "円×"   TO   WRK-SHOKUJIKHNRYO-ETC-FL
                   MOVE    "日間"   TO   WRK-SHOKUJIKHNNISSU-ETC-FL
      *
                   IF      CNT-PAGE        =   1
                       MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC21-TEKIYO2(WRK-LINETEK)
                   ELSE
                       MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC24-TEKIYO(WRK-LINETEK)
                   END-IF
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
           END-IF
      *
           .
       380-HC101-NYUTAI-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<手術歴>処理
      *****************************************************************
       380-HC101-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-OPECOM
                                           WRK-OPECOM-HEN
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
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
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
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3801-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3801-OPECOM-HENSYU-SEC
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
       380-HC101-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3801-OPECOM-HENSYU-SEC    SECTION.
      *
           IF      WRK-OPECOM          =   ZERO
      *        傷病名との間に区切り線を出力
               IF      WRK-LINETEK         >   ZERO
                   IF      WRK-LINETEK >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
               END-IF
           ELSE
      *        手術歴とコメントの間に改行
               IF    ( WRK-OPECOM-MCP-PATHNAME
                                           =   "key5" )
               AND   ( WRK-OPECOM          =   1      )
                   IF      WRK-LINETEK     >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1                   TO  WRK-LINETEK
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-OPECOM-MCP-PATHNAME =   "key4"
               MOVE    1                   TO  WRK-OPECOM
           ELSE
               MOVE    2                   TO  WRK-OPECOM
           END-IF                 
      *
           IF      WRK-LINETEK         >=  WRK-LINE-MAX
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
           IF      CNT-PAGE            =   1   
               MOVE    WRK-OPECOM-HEN      TO  HC21-TEKIYO2(WRK-LINETEK)
           ELSE                
               MOVE    WRK-OPECOM-HEN      TO  HC24-TEKIYO (WRK-LINETEK)
           END-IF                
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       3801-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       20027-PRINT-OUT-SEC              SECTION.
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               MOVE    "HCN21V04.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HC21                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               MOVE    "HCM24V04.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HC24                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           END-IF        
      *
           .
       20027-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票更新処理
      *****************************************************************
       20028-DB-WRITE-SEC               SECTION.
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
      *        レセプト帳票管理ファイル出力
               IF    ( FLG-ERR              =   ZERO     )
               AND   ( WRK-PARA-SHELLID NOT =  "RECEPTX" )
                   PERFORM 3903-RECE07-WRITE-SEC
               END-IF
           END-PERFORM
      *
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  FLG-STS
           END-IF
      *
           .
       20028-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC21"              TO  RECE08-PRTID
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
                   MOVE    ALL "9"         TO  RECE08-RECEYM
           ELSE
                   MOVE    WRK-PARA-SRYYM  TO  RECE08-RECEYM
           END-IF
           MOVE    WRK-PARA-CREYMD     TO  RECE08-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECE08-CREHMS
           MOVE    "0"                 TO  RECE08-PREFKBN
           MOVE    "00"                TO  RECE08-PREFNUM
           MOVE    RECE82-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    "3"                 TO  RECE08-TEISYUTUSAKI
           IF      TBL-RECE-RENNUM (IDXY)  =   1                                 
               MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
            ELSE
               MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
               MOVE    "3"                 TO  RECE08-RECESYUBETU
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
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-RECENUM         TO  RECE08-RECENUM
           ELSE
               COMPUTE RECE08-RECENUM      =   WRK-RECENUM
                                           +   TBL-RECE-RENNUM (IDXY)
           END-IF
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
               DISPLAY "ORCR0951 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE82-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
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
      *        複数枚レセの場合連番を記載
               IF      TBL-RECE-MAX        >   1
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    TBL-RECE-RENNUM (IDXY) 
                                               TO  WRK-PAGE1
                   MOVE    "/"                 TO  WRK-PAGE-HEN
                   MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
                   EVALUATE    RECE08-FORMID
                       WHEN    "HCN21V04.red"                  
                           MOVE    RECE08-PRTDATA  TO HC21
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC21-PAGE
                           MOVE    HC21            TO RECE08-PRTDATA
                       WHEN    "HCM24V04.red"                  
                           MOVE    RECE08-PRTDATA  TO HC24
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC24-PAGE
                           MOVE    HC24            TO RECE08-PRTDATA
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE     RECE08-REC         TO  MCPDATA-REC
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT 
               ELSE 
                   DISPLAY "ORCR0951 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE82-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                  TO  FLG-ERR
               END-IF
           END-IF    
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細管理ＤＢ
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
grpsys     MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
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
grpsys         MOVE    RECE08-HOSPNUM      TO  RECE07-HOSPNUM
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
               IF      WRK-PARA-SRYYM      =   ZERO
                   MOVE    ZERO            TO  RECE07-SYORISBT
               END-IF
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0951 MCP-RC= "  MCP-RC   
               IF      FLG-RECE07         =   1    
                   MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
                   MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
       3903-RECE07-WRITE-EXT.
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           OPEN    OUTPUT              RECEYMD-FILE 
           MOVE    WRK-RECENUM         TO  WRK-RECEYMD-RECENUM
           MOVE    WRK-RECEYMD-REC     TO  RECEYMD-REC
           WRITE   RECEYMD-REC
           CLOSE   RECEYMD-FILE
      *
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE82-FILE
           CLOSE                       RECE83-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE871-FILE
           CLOSE                       RECE88-FILE
      *
      *****IF      CNT-OUT            =   ZERO 
      *****    MOVE    "対象データがありませんでした" TO  WRK-RECEERR
      *****    PERFORM 500-ERR-HENSYU-SEC
      *****END-IF
           DISPLAY "*** ORCR0951 IN   "  CNT-RECE82
           DISPLAY "*** ORCR0951 OUT  "  CNT-OUT
           DISPLAY "*** ORCR0951 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
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
      *    レセプト基本データ読み込み処理
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE82
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE82
                   IF      RECE82-SRYYM        >=  WRK-CONS-SRYYM-200610
                       GO  TO  990-RECE82-READ-SEC
                   END-IF
                   IF      RECE82-HKNKBN           =   "2"
                   AND     RECE82-RECESYUBETU  NOT =    "0000"
      *
                       DISPLAY "RECE82  PTNUM=" RECE82-PTNUM
      *
                       ADD     1                   TO  CNT-RECE82
                       MOVE    RECE82-HOSPNUM      TO  KEY-N-HOSPNUM
                       MOVE    RECE82-SRYYM        TO  KEY-N-SRYYM
                       MOVE    RECE82-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                       MOVE    RECE82-PTID         TO  KEY-N-PTID
      *
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
      *    労災自賠医療機関情報
           MOVE    ZERO           TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM  1  BY  1
                   UNTIL ( SYSKAN-IDX              >  100  )
                   OR    ( TBL-SYS4001(SYSKAN-IDX) = SPACE )
                   OR    ( FLG-SET                 =   1   )
      *
               MOVE   TBL-SYS4001(SYSKAN-IDX)  TO  SYS-4001-REC
               IF   ( SYS-4001-STYUKYMD    <=  WRK-SYS-SRYYMD    )
               AND  ( WRK-SYS-SRYYMD       <=  SYS-4001-EDYUKYMD )
                   MOVE    1               TO    FLG-SET
                   IF      SYS-4001-RSIIRYOCDN =   LOW-VALUE
                       MOVE  SPACE             TO  SYS-4001-RSIIRYOCDN
                   END-IF
               END-IF
           END-PERFORM
           IF     FLG-SET  =  ZERO
               STRING
               "労災自賠医療機関情報が取得できませんでした。診療年月＝"
                                                DELIMITED  BY  SIZE 
                             RECE82-SRYYM       DELIMITED  BY  SIZE
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
                  MOVE    SYS-1900-PRTKBN (5)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (5)  TO  SYS-KBNCD
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
                   MOVE    ZERO      TO  FLG-RECE87
                   ADD     1         TO  CNT-RECE87
                   IF      RECE87-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE87-SRYKBN
                   END-IF    
           END-READ
      *
           .
       990-RECE87-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE871-NEXT-SEC           SECTION.
      *
           READ    RECE871-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE871
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE871
      *             ADD     1         TO  CNT-RECE871
                   IF      RECE871-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE871-SRYKBN
                   END-IF    
           END-READ
      *
           .
       990-RECE871-NEXT-EXT.
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
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
grpsys     PERFORM 900-DBSELECT-SEC
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
grpsys     PERFORM 900-DBFETCH-SEC
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
      *    ＤＢＳＥＬＥＣＴ処理
      *****************************************************************
       900-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
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
       900-DBDISCONNECT-EXT.
           EXIT.
