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
       PROGRAM-ID.                 ORCR0963.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト入院（労災・自賠責）
      *  コンポーネント名  : 月次業務  労災自賠責明細書ファイル作成
      *                      労災（短期給付）印刷データ編集
      *                      新様式
      *  管理者            : 
      *  作成日付    作業者        記述
      *  11/08/10    NACL-藤原     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.01    NACL-門脇    13/02/07  金額項目７桁対応
      *  04.06.01    NACL-藤原    13/04/30  「傷病の経過」記載対応
      *  04.06.02    NACL-門脇    13/06/07  合計額ワーク領域のクリア
      *
      *  04.07.01    NACL-門脇    13/01/21  労災レセ電対応
      *  04.07.02    NACL-藤原    13/05/27  症状詳記の表題を平成２５年７月
      *                                     診療分以降は全て記載
      *  04.07.03    NACL-藤原    13/08/20  ０９５９３で始まるコードの有
      *                                     無区分を追加
      *  04.07.04    NACL-門脇    14/10/02  摘要欄の最終行まで記載してから
      *                                     改ページするよう修正
      *  04.07.05    NACL-藤原    14/12/03  平成２６年１１月改正対応
      *  04.07.06    NACL-門脇    15/06/23  食事回数記載修正
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-藤原    17/03/02  未コード化特定器材コード廃止対応
      *  04.08.04    NACL-門脇    18/04/24  平成３０年４月改正対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-伊藤    17/12/22  診療情報システム対応
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
      *
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
           COPY    "HCN66V01.INC"  REPLACING  //HCN66V01//
                                   BY         //HCN66//.
      *
      *    続紙
           COPY    "HCM24V06.INC"  REPLACING  //HCM24V06//
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
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-SHOKUJI-ETC     PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-RECECOM-HENSYU  PIC 9(01).
           03  FLG-STS             PIC 9(01).
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
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
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
           03  TBL-SYS1900-OCC     OCCURS  100.
               05  TBL-SYS1900     PIC  X(610).
      *
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
               05  WRK-PARA-SYOKBN  PIC  X(01).
               05  WRK-PARA-RECEKBN PIC  X(01).
               05  WRK-PARA-RECEYM  PIC  X(06).
               05  WRK-PARA-JOBID   PIC  9(07).
               05  WRK-PARA-SHELLID PIC  X(08).
           03  WRK-PARA-RECECOM     PIC  X(01).
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
           03  WRK-HOSPNAME        PIC X(100).
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
      *         
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1.
               05  WRK-BYOMEI1-NAME            PIC X(44).
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2.  
               05  WRK-BYOMEI2-NAME            PIC X(52).
      *
      *    傷病名が３行になるときの編集
           03  WRK-BYOMEI3                     PIC X(52).
           03  WRK-MOJISU3                     PIC 9(03).
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT     PIC X(50)
                                       OCCURS  48.
           03  WRK-COMM            PIC X(2400).
           03  WRK-COMM1           PIC X(2400).
           03  WRK-COL             PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-RECECOM-NAME    PIC X(52).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
           03  WRK-RECECOM-SJKBN   PIC X(02).
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
       01  WRK-ERR-AREA.
           03  WRK-RECEERR                         PIC X(200).
           03  WRK-ERRMSG                          PIC X(300).
           03  WRK-MCP-RC                          PIC 9(9).
           03  WRK-ERR-FILE-STS                    PIC X(02).
           03  WRK-ERR-FILE-MSG                    PIC X(100).
           03  WRK-ERR-FILE-MSG1                   PIC X(17).
           03  WRK-ERR-FLG                         PIC 9(01).
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
           03  WRK-KGK22           PIC Z9.
           03  WRK-ZZZ             PIC ZZZ.
           03  WRK-RECEKAISU       PIC ZZZ.
           03  WRK-KINGAKUZ9       PIC Z,ZZZ,ZZZ.
      *
       01  WRK-ERR-MSG-AREA.
           03  WRK-ERR-MSG-1.
               05  FILLER          PIC X(50)   VALUE
               "★★０５９で始まる「その他材料コード」が使用されて". 
               05  FILLER          PIC X(10)   VALUE
               "います★★". 
      *
       01  WRK-CONS-AREA.    
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
           03  WRK-CONS-SRYYM-200610
                                   PIC 9(06)   VALUE   200610.
      *
       01  WRK-BYOMEI-AREA.     
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
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   3.
      *     
      *    傷病の経過欄最大行数
           03  WRK-KEIKA-HCN66-LINE PIC 9(02)   VALUE   4.
      *     
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HC24-LINE
                                   PIC 9(03)   VALUE   118.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *   症状詳記区分の表題
       01  TBL-RECECOM-AREA.
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状】".
           03  FILLER              PIC X(52)   VALUE
               "【主たる疾患の診断根拠となる臨床症状の診察検査所見】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の必要性】".
           03  FILLER              PIC X(52)   VALUE
               "【主な治療行為（手術、処置、薬物治療等）の経過】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の薬剤に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【合計点数が１００万点以上の処置に係る症状等】".
           03  FILLER              PIC X(52)   VALUE
               "【その他】".
           03  FILLER              PIC X(52)   VALUE
               "【薬事法に規定する治験に係る治験概要】".
           03  FILLER              PIC X(52)   VALUE
               "【疾患別リハビリテーションに係る治療継続の理由等】".
           03  FILLER              PIC X(52)   VALUE
               "【廃用症候群に係る評価表】".
           03  FILLER              PIC X(52)   VALUE
               "【医薬品医療機器等法に規定する治験に係る治験概要】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(52)   OCCURS   11.
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
           COPY    "CPSK200501.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
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
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
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
                                               WRK-PARA-RECECOM
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
           DISPLAY "WRK-PARA-RECECOM   =" WRK-PARA-RECECOM
           DISPLAY "WRK-PARA-DLTKBN    =" WRK-PARA-DLTKBN
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           MOVE    "RECE820"           TO  RECE82PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE82PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE82PARA-HOSPNUM
      *
           MOVE    "RECE831"           TO  RECE83PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE83PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE83PARA-HOSPNUM
      *
           MOVE    "RECE870"           TO  RECE87PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE87PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE87PARA-HOSPNUM
      *
           MOVE    "RECE871"           TO  RECE871PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE871PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE871PARA-HOSPNUM
      *
           MOVE    "RECE880"           TO  RECE88PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE88PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE88PARA-HOSPNUM
      *
           MOVE    "RECEYMD"           TO  RECEYMD-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEYMD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEYMD-HOSPNUM
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
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           PERFORM 100-DBOPEN-SEC
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR0963"      TO  JOB-PGID
               MOVE    "短期給付・印刷データ編集（労災）"
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
           OPEN    INPUT               RECE871-FILE
           IF      STS-RECE871         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE871PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE871         TO  WRK-ERR-FILE-STS
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
           OPEN    OUTPUT              RECEYMD-FILE     
      *
           MOVE    ZERO                TO  WRK-RECENUM
      *
      *    診療情報システム用のとき処理終了
           IF      WRK-PARA-PRTKBN     =   "B"
               MOVE    2                   TO  FLG-STS
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
              MOVE  "医療機関情報（所在地）が取得できませんでした。"
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
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       労災医療機関情報
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
           IF      RECE82-SRYYM        <   201307
               IF      WRK-PARA-RECECOM    =   "0"
                   PERFORM 20024-COMMENT-HEN-SEC
               END-IF
           ELSE
               PERFORM 20024-KEIKA-HEN-SEC
           END-IF 
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
      *    帳票編集<傷病の経過>処理
           IF      RECE82-SRYYM        <   201307
               IF      WRK-PARA-RECECOM    =   "1"
                   PERFORM 20024-COMMENT-HEN-SEC
               END-IF
           ELSE
               PERFORM 20024-COMMENT-HEN-SEC
           END-IF
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
           INITIALIZE                      HCN66
      *
      *    災害区分
           IF      RECE82-RECESYUBETU  =   ZERO
               CONTINUE
           ELSE    
               EVALUATE    RECE82-SAIGAIKBN
                   WHEN    "1"
                       MOVE    "ギョウム"      TO  HCN66-SAIGAIKBN
                   WHEN    "2"
                       MOVE    "ツウサイ"      TO  HCN66-SAIGAIKBN
               END-EVALUATE
           END-IF        
      *    患者番号
           MOVE    RECE82-PTNUM    TO  HCN66-PTNUM
      *    指定病院等の番号
           MOVE    SYS-4001-RSIIRYOCDN TO  HCN66-RSIIRYOCD
      *    病院等の名称
           IF      FLG-SYS-1901        =   1
               MOVE    SPACE               TO  WRK-HOSPNAME
               STRING   SYS-1901-HOSPNAME1     DELIMITED  BY  SPACE
                        "　"                   DELIMITED  BY  SIZE
                        SYS-1901-HOSPNAME2     DELIMITED  BY  SPACE
                        "　"                   DELIMITED  BY  SIZE
                        SYS-1901-HOSPNAME3     DELIMITED  BY  SPACE
                                               INTO    WRK-HOSPNAME
               END-STRING
               INITIALIZE                      ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-HOSPNAME        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF    ( KANACHK-RC          =   ZERO )
               AND   ( KANACHK-SYUBETU     =   "2"  )
                   IF      KANACHK-MAX         >   64
                       MOVE    WRK-HOSPNAME        TO  HCN66-HOSPNAME2
                   ELSE
                       MOVE    WRK-HOSPNAME        TO  HCN66-HOSPNAME
                   END-IF
               END-IF      
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HCN66-HOSPNAME
           END-IF  
           INSPECT HCN66-HOSPNAME  REPLACING   ALL "  "    BY  "　"
           INSPECT HCN66-HOSPNAME2 REPLACING   ALL "  "    BY  "　"
      *    労働保険者番号
           MOVE    RECE82-KOFUNUM      TO  HCN66-KOFUNUM
      *    新継区分
           MOVE    RECE82-SINKEI       TO  HCN66-SINKEI
      *    転帰事由
           MOVE    RECE82-TENKI        TO  HCN66-TENKI
      *    生年月日
           MOVE    RECE82-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           EVALUATE    WRK-HENGENGO
               WHEN    "M"
                   MOVE    "1"             TO  HCN66-TANJYOYMD (1:1)      
               WHEN    "T"
                   MOVE    "3"             TO  HCN66-TANJYOYMD (1:1)      
               WHEN    "S"
                   MOVE    "5"             TO  HCN66-TANJYOYMD (1:1)      
               WHEN    "H"
                   MOVE    "7"             TO  HCN66-TANJYOYMD (1:1)      
           END-EVALUATE     
           MOVE    WRK-HENYMD          TO  HCN66-TANJYOYMD (2:)
      *    傷病年月日
           MOVE    RECE82-SHOBYOYMD    TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENYMD          TO  HCN66-SYOBYOYMD 
      *    療養開始年月日
           IF    RECE82-RYOSTYMD   NOT =  SPACE
             IF      RECE82-RYOSTYMD     <   WRK-STYMD
                 MOVE    WRK-STYMDW          TO  HCN66-STRYMD
             ELSE      
                 MOVE    RECE82-RYOSTYMD     TO  WRK-SYMD
                 PERFORM 31012-SE-WA-YMDEDIT-SEC
                 MOVE    WRK-HENYMD          TO  HCN66-STRYMD
             END-IF    
           ELSE
             MOVE   WRK-STYMDW(1:4)   TO   HCN66-STRYMD (1:4)
             PERFORM VARYING IDX FROM    1   BY  1
                     UNTIL   IDX >       31
                 IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                     MOVE    IDX                 TO  WRK-KGK2
                     MOVE    WRK-KGK2            TO  HCN66-STRYMD (5:2)
                     MOVE    31                  TO  IDX
                 END-IF
             END-PERFORM
           END-IF
      *    療養終了年月日
           IF    RECE82-RYOEDYMD   NOT =  SPACE
             IF      RECE82-RYOEDYMD     >   WRK-EDYMD
                 MOVE    WRK-EDYMDW          TO  HCN66-ENDYMD
             ELSE      
                 MOVE    RECE82-RYOEDYMD     TO  WRK-SYMD
                 PERFORM 31012-SE-WA-YMDEDIT-SEC
                 MOVE    WRK-HENYMD          TO  HCN66-ENDYMD
             END-IF    
           ELSE
             MOVE   WRK-EDYMDW(1:4)   TO   HCN66-ENDYMD (1:4)
             PERFORM VARYING IDX FROM    31  BY  -1
                     UNTIL   IDX <       1
                 IF      RECE82-DAY (IDX)    =   RECE82-HKNCOMBI
                     MOVE    IDX                 TO  WRK-KGK2
                     MOVE    WRK-KGK2            TO  HCN66-ENDYMD (5:2)
                     MOVE    1                   TO  IDX
                 END-IF
             END-PERFORM
           END-IF
      *    実日数
           MOVE    RECE82-JNISSU       TO  WRK-JNISSU
           MOVE    WRK-JNISSU          TO  HCN66-NISSU
      *    合計額
           MOVE    ZERO                TO  WRK-GOKEI
           COMPUTE WRK-GOKEI   =   RECE82-TOTALKIN
                               +   RECE82-KINGAKU2-SYO
                               +   RECE82-SHOKUJITOTALKIN
           MOVE    WRK-GOKEI           TO  WRK-KHNTENZ
           MOVE    WRK-KHNTENZ         TO  HCN66-GOKEI
      *    カナ氏名
           MOVE    RECE82-KANANAME     TO  HCN66-KANANAME
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
                MOVE    RECE82-NAME     TO  HCN66-NAME2
           ELSE
                MOVE    RECE82-NAME     TO  HCN66-NAME
           END-IF
      *    年齢
           MOVE    RECE82-BIRTHDAY     TO  WRK-BIRTHDAY    
           COMPUTE WRK-NENREI          =   RECE82-SRYYM
                                       -   WRK-BIRTHYM
           MOVE    WRK-NENREI-R1       TO  WRK-NENREI-X                            
           MOVE    WRK-NENREI-X        TO  HCN66-NENREI 
      *    事業の名称
           MOVE    RECE82-JIGYOUNAME   TO  HCN66-JIGYOUNAME
      *    事業場の所在地
           MOVE    RECE82-PREFNAME     TO  HCN66-TODOFUKEN
           MOVE    RECE82-CITYNAME     TO  HCN66-GUNSIKU
           EVALUATE    RECE82-PREFKBN
               WHEN    "1"  
                   MOVE    "○"        TO  HCN66-TODOFUKENKBN1
               WHEN    "2"  
                   MOVE    "○"        TO  HCN66-TODOFUKENKBN2
               WHEN    "3"  
                   MOVE    "○"        TO  HCN66-TODOFUKENKBN3
               WHEN    "4"  
                   MOVE    "○"        TO  HCN66-TODOFUKENKBN4
           END-EVALUATE
           EVALUATE    RECE82-CITYKBN
               WHEN    "1"
                   MOVE    "○"        TO  HCN66-GUNSIKUKBN1
               WHEN    "2"
                   MOVE    "○"        TO  HCN66-GUNSIKUKBN2
               WHEN    "3"
                   MOVE    "○"        TO  HCN66-GUNSIKUKBN3
           END-EVALUATE                             
      *
      *    労災レセプト回数
           IF   RECE82-ROUKAISU   =   "2"
             MOVE    RECE82-RECEKAISU  TO  WRK-RECEKAISU
             MOVE    WRK-RECEKAISU     TO  HCN66-KAI
           END-IF
      *
      *    労災基準監督署コード
           IF      RECE82-SYOKAIFLG    =   "1"
               MOVE   RECE82-SYOCD         TO  HCN66-SYOCD
           END-IF     
      *
      *    レセプト種別不明のときは原因を編集する
           IF      CNT-PAGE            =   1
               IF    ( RECE82-RECESYUBETU  =    ZERO )
               OR    ( RECE82-ERR-ERRKBN   >    ZERO )
                   PERFORM 3101-FUMEI-HENSYU-SEC
               END-IF
           END-IF
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN NOT =   "1"
               IF      SYS-200501-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HCN66-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HCN66-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HCN66-LOPTION
           END-IF
      *
           .
       20021-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別不明のとき不明の原因を編集処理
      *****************************************************************
       3101-FUMEI-HENSYU-SEC           SECTION.
      *
           EVALUATE    RECE82-ERR-ERRKBN
               WHEN    15
                   MOVE    WRK-ERR-MSG-1   TO   HCN66-HEAD (2)
           END-EVALUATE
           INSPECT HCN66-HEAD-G REPLACING  ALL "  "    BY  "　"
      *
           .
       3101-FUMEI-HENSYU-EXT.
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
           INSPECT HC24-HOSPNAME   REPLACING   ALL "  "    BY  "　"
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
      *    点検用レセプト作成時
           MOVE    HCN66-LABEL         TO  HC24-LABEL
           MOVE    HCN66-LOPTION       TO  HC24-LOPTION
      *
           .
       310-HC24-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20023-BYOMEI-HEN-SEC      SECTION.
      *     
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
                                           WRK-BYOMEI3
           MOVE    ZERO                TO  WRK-MOJISU3
      *
      *    傷病の部位編集
           IF      RECE82-COMMENT      =   SPACE
               CONTINUE
           ELSE
               ADD     1               TO  WRK-LINEBYO
               MOVE    RECE82-COMMENT  TO  HCN66-SYOBYOMEI (WRK-LINEBYO)
               IF      RECE82-COMMENT (45:)    NOT =   SPACE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    RECE82-COMMENT (45:44)
                                       TO  HCN66-SYOBYOMEI (WRK-LINEBYO)
               END-IF                        
               IF      RECE82-COMMENT (89:)    NOT =   SPACE
                   ADD     1           TO  WRK-LINEBYO
                   MOVE    RECE82-COMMENT (89:)
                                       TO  HCN66-SYOBYOMEI (WRK-LINEBYO)
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
           PERFORM UNTIL ( FLG-RECE83           =   1                )
                   OR    ( RECE82-HOSPNUM   NOT =   RECE83-HOSPNUM   )
                   OR    ( RECE82-SRYYM     NOT =   RECE83-SRYYM     )
                   OR    ( RECE82-NYUGAIKBN NOT =   RECE83-NYUGAIKBN )
                   OR    ( RECE82-PTID      NOT =   RECE83-PTID      )
                   OR    ( RECE82-RECEKA    NOT =   RECE83-RECEKA    )
                   OR    ( RECE82-HKNID     NOT =   RECE83-HKNID     )
                   OR    ( RECE82-TEKSTYMD  NOT =   RECE83-TEKSTYMD  )
      *        病名編集処理
               PERFORM 200231-BYOMEI-SET-SEC
           END-PERFORM
      *
           IF      WRK-LINEBYO         <  WRK-BYOMEI-LINE
               IF      WRK-BYOMEI1-NAME   NOT =   SPACE
                   PERFORM 200232-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF      WRK-BYOMEI2-NAME   NOT =   SPACE
                   PERFORM 200233-BYOMEI2-OUT-SEC
               END-IF
           END-IF
           .
       20023-BYOMEI-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *    病名編集処理
      ****************************************************************
       200231-BYOMEI-SET-SEC           SECTION.
      *
      *    病名をつなげると何桁になるかを計算（，＋病名）
           IF  WRK-CLM           =   ZERO
               MOVE    WRK-CLM   TO  WRK-WCLM
           ELSE
               COMPUTE WRK-WCLM  =   WRK-CLM   +   2
           END-IF
           COMPUTE     WRK-WCLM  =
                      ( RECE83-BYOMEIMOJI  *   2 )  +   WRK-WCLM
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
                           COMPUTE WRK-STCLM =   WRK-CLM   +    1
                           MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +   2
                       END-IF
                   END-IF
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                       PERFORM 200233-BYOMEI2-OUT-SEC
                   ELSE
                       IF  WRK-CLM       >   ZERO
      *                    前の病名の直後に編集
                           COMPUTE WRK-STCLM =   WRK-CLM   +   1
                           MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                           COMPUTE WRK-CLM   =   WRK-CLM   +   2
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM       =   WRK-CLM   +   1
      *     
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE83-BYOMEIMOJI   *   2
           MOVE    SPACE               TO  WRK-BYOMEI3
           MOVE    ZERO                TO  WRK-MOJISU3
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
      *
                           MOVE    RECE83-BYOMEI(89:)  TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       88
                       ELSE
                           MOVE    RECE83-BYOMEI(45:)
                                                   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
      *
                           MOVE    RECE83-BYOMEI(97:)  TO  WRK-BYOMEI3
                           COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                       96
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    RECE83-BYOMEI   TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       PERFORM  200233-BYOMEI2-OUT-SEC
                       MOVE    RECE83-BYOMEI(53:)
                                               TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
      *
                       MOVE    RECE83-BYOMEI(105:) TO  WRK-BYOMEI3
                       COMPUTE WRK-MOJISU3     =   WRK-MOJISU   -
                                                   104
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *
           IF      WRK-BYOMEI3       NOT =   SPACE
               EVALUATE    TRUE
      *            傷病名欄に編集
                   WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                       PERFORM 200232-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                       ELSE
                           MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                       END-IF                                   
      *            適用欄に編集
                   WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                       PERFORM 200233-BYOMEI2-OUT-SEC
                       MOVE    WRK-BYOMEI3     TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
               END-EVALUATE
      *
               COMPUTE     WRK-CLM           =   WRK-MOJISU3
                                             +   WRK-CLM
           END-IF
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
           ADD     1               TO  WRK-LINEBYO
      *
           INSPECT WRK-BYOMEI1
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI1     TO  HCN66-SYOBYOMEI (WRK-LINEBYO)
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
               MOVE    WRK-BYOMEI2     TO  HCN66-TEKIYO2 (WRK-LINETEK)
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
                       MOVE    RECE871-TEKIYO-INF  TO  HCN66-TEKIYO1
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
                                           HCN66-TEKIYO2(WRK-LINETEK)
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
                                           HCN66-TEKIYO2(WRK-LINETEK)
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
                                           HCN66-TEKIYO2(WRK-LINETEK)
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
                   MOVE  "○"       TO  HCN66-SYOSHIN1
               WHEN    "2"
                   MOVE  "○"       TO  HCN66-SYOSHIN2
               WHEN    "3"
                   MOVE  "○"       TO  HCN66-SYOSHIN3
           END-EVALUATE
      *
      *    １１初診〜８０他         
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   22
      *        <回数>
               MOVE    RECE82-KAISU(IDX)         TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ                TO  HCN66-KAISU   (IDX)
      *        <点数>
               MOVE    RECE82-TENSU(IDX)         TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ                TO  HCN66-TENSU   (IDX)
           END-PERFORM
      *
      *    ９０入院            
      *    <入院基本料>
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
      *        <基本料>
               MOVE    RECE82-NYUKHNRYO (IDX)
                                             TO  WRK-KHNTENZ
               MOVE    WRK-KHNTENZ           TO  HCN66-NYUKHNTEN (IDX)
      *        <日数>
               MOVE    RECE82-NYUKHNNISSU(IDX)
                                             TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HCN66-NYUKAISU  (IDX)
      *        <金額>
               MOVE    RECE82-NYUKHNKEI(IDX)
                                             TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN66-NYUTENSU  (IDX)
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
                                                 TO  SUM-NYUKHNKEI (1)
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HCN66-NYUKHNTEN (4)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HCN66-NYUKAISU  (4)
      *        <金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN66-NYUTENSU  (4)
           END-IF
      *    <入院基本料・加算>
           MOVE    RECE82-NYUKHNKEI2         TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ                 TO  HCN66-NYUTENSU  (5)
           IF      RECE82-NYUKHNKEI2         <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
                       MOVE    "▲"        TO  HCN66-NYUTENSUN (5 IDZ)
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
                   MOVE    "×"              TO  HCN66-KAKERU    (IDX)                    
                   MOVE    "日間"           TO  HCN66-NICHI     (IDX)                    
      *            <基本料>
                   IF      RECE82-NYUTOKRYO-RYAKU (IDX)
                                             =   SPACE
                       MOVE    RECE82-NYUTOKRYO (IDX)
                                             TO  WRK-KHNTENZ
                       MOVE    WRK-KHNTENZ   TO  HCN66-NYUKHNTEN (IDY)
                   ELSE
                       IF      RECE82-NYUTOKRYO-RYAKU (IDX) (11:)
                                             =   SPACE
                           MOVE    RECE82-NYUTOKRYO-RYAKU (IDX)
                                             TO  HCN66-NYUKHNTENN (IDY)
                       ELSE
                           MOVE    RECE82-NYUTOKRYO-RYAKU (IDX)
                                             TO  HCN66-NYUKHNTENN2(IDY)
                       END-IF
                   END-IF    
      *            <日数>
                   MOVE    RECE82-NYUTOKNISSU(IDX)
                                             TO  WRK-KAISUZ
                   MOVE    WRK-KAISUZ        TO  HCN66-NYUKAISU  (IDY)
      *            <金額>
                   MOVE    RECE82-NYUTOKKEI(IDX)
                                             TO  WRK-TENSUZ
                   MOVE    WRK-TENSUZ        TO  HCN66-NYUTENSU  (IDY)
               END-IF    
           END-PERFORM
      *
           IF        RECE82-NYUTOKNISSU (4)  >   ZERO
               MOVE    ZERO                  TO  SUM-AREA  
               PERFORM   VARYING   IDX   FROM    3   BY  1
                         UNTIL     IDX       >   20
                         OR        RECE82-NYUTOKNISSU (IDX)  =   ZERO
      *            <日数>
                   ADD     RECE82-NYUTOKNISSU(IDX)
                                                 TO  SUM-NYUKHNNISSU
      *            <金額>
                   ADD     RECE82-NYUTOKKEI(IDX)
                                                 TO  SUM-NYUKHNKEI (1)
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE                 TO  HCN66-NYUKHNTEN  (8)
                                                 HCN66-NYUKHNTENN (8)
                                                 HCN66-NYUKHNTENN2(8)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU       TO  WRK-KAISUZ
               MOVE    WRK-KAISUZ            TO  HCN66-NYUKAISU  (8)
      *        <金額>
               MOVE    SUM-NYUKHNKEI(1)      TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ            TO  HCN66-NYUTENSU  (8)
           END-IF
      *    <特定入院料・その他>
           MOVE    RECE82-NYUTOKKEI2       TO  WRK-TENSUZ
           MOVE    WRK-TENSUZ              TO  HCN66-NYUTENSU  (9)
           IF      RECE82-NYUTOKKEI2       <   ZERO
               PERFORM VARYING IDZ FROM    6   BY  -1
                       UNTIL   IDZ <       1
                   IF      WRK-TENSUZ (IDZ:1)  =   SPACE
                       MOVE    "▲"        TO
                                           HCN66-NYUTENSUN (9 IDZ)
                       MOVE    1           TO  IDZ
                   END-IF
               END-PERFORM
           END-IF
      *
      *        <入院年月日>
           MOVE    RECE82-NYUINYMD     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HCN66-NYUINYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HCN66-NYUINMM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  HCN66-NYUINDD
      *
      *        <医療機関種別>
           EVALUATE    RECE82-HOSPSBT
               WHEN    "1"
                   MOVE    "○"        TO  HCN66-HOSPSBT (1)
               WHEN    "2"
                   MOVE    "○"        TO  HCN66-HOSPSBT (2)
               WHEN    "3"
                   MOVE    "○"        TO  HCN66-HOSPSBT (1)
                                           HCN66-HOSPSBT (3)
               WHEN    "4"
                   MOVE    "○"        TO  HCN66-HOSPSBT (2)
                                           HCN66-HOSPSBT (3)
           END-EVALUATE        
      *
      *       <入院基本料種別>
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       11
                   OR      RECE82-NYUSYUBETU (IDX) =   SPACE
               IF      RECE82-NYUSYUBETU (IDX) (11:)
                                       =   SPACE
                   MOVE    RECE82-NYUSYUBETU (IDX)
                                         TO  HCN66-NYUSYUBETSU (IDX)
               ELSE
                   MOVE    RECE82-NYUSYUBETU (IDX)
                                         TO  HCN66-NYUSYUBETSU2(IDX)
               END-IF
           END-PERFORM
      *
      *    小計イ
           MOVE    RECE82-TOTALTEN        TO  WRK-TOTALTEN
           MOVE    WRK-TOTALTEN           TO  HCN66-TOTALTEN
           IF      RECE82-TOTALKIN            >   ZERO
               MOVE    RECE82-TOTALKIN        TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HCN66-TOTALMONEY
           END-IF
      *
      *    診療内容・金額                     
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   5
      *        <金額>
               MOVE    RECE82-KINGAKU2(IDX)      TO  WRK-KINGAKUZ9
               MOVE    WRK-KINGAKUZ9             TO  HCN66-KINGAKU (IDX)
           END-PERFORM
      *
      *    <小計ロ>
           IF      RECE82-KINGAKU2-SYO        >   ZERO
               MOVE    RECE82-KINGAKU2-SYO    TO  WRK-KINGAKUZ9
               MOVE    WRK-KINGAKUZ9          TO  HCN66-SYOKEI
           END-IF
      *
      *    ９７食事
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
               MOVE    RECE82-SHOKUJISYUBETU (IDX)
                                             TO  HCN66-SYOKJN (IDX)
           END-PERFORM
      * 
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   4
      *        IF      RECE82-SHOKUJIKHNRYO (IDX)    >   ZERO      
               IF      RECE82-SHOKUJIKHNNISSU(IDX)   >   ZERO      
                   MOVE    RECE82-SHOKUJIKHNRYO (IDX)
                                             TO  WRK-KINGAKUZ
                   MOVE    WRK-KINGAKUZ      TO  HCN66-SYOKHNTEN(IDX)          
                   MOVE    RECE82-SHOKUJIKHNNISSU (IDX)
                                             TO  WRK-ZZZ
                   MOVE    WRK-ZZZ           TO  HCN66-SYOKAISU (IDX)
                   EVALUATE    IDX
                       WHEN    4
                           MOVE    "円×"    TO  HCN66-SYOKAKERU
                           MOVE    "　日"    TO  HCN66-SYONICHI
                   END-EVALUATE
               END-IF                                        
           END-PERFORM
      *
      *    食事療養（日）          
           IF   RECE82-SHOKUJITOTALKIN  >  0
             MOVE  RECE82-SHOKUJITOTALDAY
                                     TO  WRK-KGK22
             MOVE  WRK-KGK22         TO  HCN66-SYONISSU
           END-IF
      *
      *    食事療養（ハ）          
           MOVE    RECE82-SHOKUJITOTALKIN
                                     TO  WRK-KGK4
           MOVE    WRK-KGK4          TO  HCN66-SYOMONEY
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
               PERFORM 34110-KUGIRI-LINE-SEC
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
      ***  IF  CNT-ZAIMEISAISU           =   1
      *        次の行為明細数を追加すると何行になるか
      ***      COMPUTE   WRK-WLINETEK    =   WRK-LINETEK
      ***                                +   RECE87-ZAIMEISAISU
      ***      IF  CNT-RECE87           >   1
      *            診療区分が変更している場合、診療区切り線分を追加
      ***          IF  RECE87-SRYKBN    NOT =   WRK-R061-SRYKBN-O
      ***              ADD     1             TO  WRK-WLINETEK
      ***              MOVE    1             TO  FLG-KUGIRI
      ***          END-IF
      ***      END-IF
      *
      ***      IF  WRK-WLINETEK          >         WRK-LINE-MAX
      ***          PERFORM 34120-PAGE-SEC
      *
      *            区切り線
      ***          IF  FLG-KUGIRI        =   2
      ***              PERFORM 34110-KUGIRI-LINE-SEC
      ***          END-IF
      *            診療区分
      ***          ADD     1               TO  WRK-LINETEK
      ***          IF      CNT-PAGE        =   1
      ***              MOVE    RECE87-SRYKBN  TO  HCN66-SRYKBN
      ***                                              (WRK-LINETEK)
      ***          ELSE
      ***              MOVE    RECE87-SRYKBN  TO  HC24-SRYKBN
      ***                                              (WRK-LINETEK)
      ***          END-IF                                    
      ***      ELSE
      *            区切り線
      ***          IF  FLG-KUGIRI    NOT =   ZERO
      ***              PERFORM 34110-KUGIRI-LINE-SEC
      ***          END-IF
      *            診療区分
      ***          ADD     1             TO  WRK-LINETEK
      ***          IF  RECE87-SRYKBN    NOT =   WRK-R061-SRYKBN-O
      ***              IF      CNT-PAGE        =   1
      ***                  MOVE    RECE87-SRYKBN  TO  HCN66-SRYKBN
      ***                                              (WRK-LINETEK)
      ***              ELSE
      ***                  MOVE    RECE87-SRYKBN  TO  HC24-SRYKBN
      ***                                              (WRK-LINETEK)
      ***              END-IF                                    
      ***          END-IF
      ***      END-IF
      ***  ELSE
      ***      ADD     1                 TO  WRK-LINETEK
      ***  END-IF
      *
           IF  CNT-ZAIMEISAISU           =   1
             IF  CNT-RECE87            >   1
      *          診療区分が変更している場合、診療区切り線分を追加
                 IF  RECE87-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                     MOVE    1             TO  FLG-KUGIRI
                 END-IF
             END-IF
           END-IF
      *
           IF      WRK-LINETEK          >=   WRK-LINE-MAX
             PERFORM 34120-PAGE-SEC
           ELSE
      *      区切り線
             IF      FLG-KUGIRI    NOT =   ZERO
                 PERFORM 34110-KUGIRI-LINE-SEC
             END-IF
           END-IF
      *
      *    診療区分
           IF      WRK-LINETEK          >=   WRK-LINE-MAX
             PERFORM 34120-PAGE-SEC
           END-IF
      *
           ADD     1                TO  WRK-LINETEK
      *
           IF   ( WRK-R061-SRYKBN-O  =   SPACE  )
           OR   ( FLG-KUGIRI     NOT =   ZERO   )
           OR   ((CNT-PAGE           >   1     ) AND
                 (WRK-LINETEK        =   1     ))
             IF      CNT-PAGE        =   1
               MOVE  RECE87-SRYKBN  TO  HCN66-SRYKBN(WRK-LINETEK)
             ELSE
               MOVE  RECE87-SRYKBN  TO  HC24-SRYKBN(WRK-LINETEK)
             END-IF
           END-IF
      *
      *    明細
           INSPECT RECE87-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    RECE87-TEKIYO-INF  TO  HCN66-TEKIYO2(WRK-LINETEK)
           ELSE    
               MOVE    RECE87-TEKIYO-INF  TO  HC24-TEKIYO (WRK-LINETEK)
           END-IF    
      *
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
               MOVE    WRK-KUGIRI1 TO  HCN66-TEKIYO2 (WRK-LINETEK)
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
      *    帳票編集<傷病の経過>処理
      *****************************************************************
       20024-KEIKA-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
                                           WRK-KEIKA-LINE
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key16"             TO  WRK-MCP-PATHNAME
           PERFORM 20024-RECECOM-HENSYU-SEC
      *     
           .
       20024-KEIKA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       20024-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
                                           WRK-KEIKA-LINE
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *
      *    適用欄に記載するときの設定
           IF    ( WRK-PARA-RECECOM    =   "1"    )
           OR    ( RECE82-SRYYM        >=  201307 )
               MOVE    WRK-KEIKA-HCN66-LINE    TO  WRK-KEIKA-LINE
           END-IF
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
       20024-COMMENT-HEN-EXT.
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
           IF      WRK-MCP-PATHNAME    =   "key16"
               MOVE    "AA"                TO  RECECOM-SJKBN
           END-IF
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
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
           MOVE    ZERO            TO  FLG-RECECOM-SJKBN
      *
      *    症状詳記区分の見出しを記載
      *   （平成２５年７月以降は全て記載）
           IF      RECE82-SRYYM    <   201307
               IF      RECECOM-SJKBN   =   "51"   OR  "52"
                   MOVE    RECECOM-SJKBN   TO  RECECOM-SJKBN
               ELSE
                   MOVE    SPACE           TO  RECECOM-SJKBN
               END-IF
           ELSE
               IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
                   MOVE    "07"            TO  RECECOM-SJKBN
               END-IF
           END-IF 
           IF      RECECOM-SJKBN   NOT =   WRK-RECECOM-SJKBN
               MOVE   RECECOM-SJKBN        TO  WRK-RECECOM-SJKBN
      *
               MOVE    SPACE           TO  WRK-RECECOM-NAME
               EVALUATE    WRK-RECECOM-SJKBN
                   WHEN    "01"    THRU    "07"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    RECECOM-SJKBN   TO  WRK-SJKBN-X
                       MOVE    TBL-RECECOM-NAME (WRK-SJKBN)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "50"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       IF      RECE82-SRYYM    <   201412
                           MOVE    TBL-RECECOM-NAME (8)
                                                   TO  WRK-RECECOM-NAME
                       ELSE
                           MOVE    TBL-RECECOM-NAME (11)
                                                   TO  WRK-RECECOM-NAME
                       END-IF
                   WHEN    "51"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (9)
                                               TO  WRK-RECECOM-NAME
                   WHEN    "52"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (10)
                                               TO  WRK-RECECOM-NAME
               END-EVALUATE
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
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-CONS-RECECOM-MAX
                           OR      WRK-COMMENT (IDX)   =   SPACE
      * 
                       MOVE    ZERO            TO  FLG-RECECOM-HENSYU 
      *                                          
                       IF      WRK-KEIKA-LINE  <   WRK-KEIKA-HCN66-LINE
      *                  傷病の経過欄へ症状詳記区分の見出しを記載
                         IF      FLG-RECECOM-SJKBN   =   1
                             ADD     1           TO  WRK-KEIKA-LINE       
                             MOVE    WRK-RECECOM-NAME
                                       TO  HCN66-KEIKA (WRK-KEIKA-LINE)
                             MOVE    ZERO        TO  FLG-RECECOM-SJKBN
                         END-IF            
                         IF      WRK-KEIKA-LINE
                                             <   WRK-KEIKA-HCN66-LINE
      *                    傷病の経過欄へ編集
                           ADD     1           TO  WRK-KEIKA-LINE       
                           MOVE    WRK-COMMENT (IDX)
                                               TO
                                           HCN66-KEIKA (WRK-KEIKA-LINE)
                           MOVE    1           TO  FLG-RECECOM-HENSYU                                           
                         END-IF
                       END-IF
      * 
                       IF      FLG-RECECOM-HENSYU  =   ZERO
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
      *                    症状詳記区分の見出しを記載
                           IF      FLG-RECECOM-SJKBN   =   1
                               IF      WRK-LINETEK      >=
                                                        WRK-LINE-MAX
                                   PERFORM 34120-PAGE-SEC
                               END-IF
                               ADD     1               TO  WRK-LINETEK
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-RECECOM-NAME
                                           TO HCN66-TEKIYO2(WRK-LINETEK)
                               ELSE                
                                   MOVE    WRK-RECECOM-NAME
                                           TO  HC24-TEKIYO (WRK-LINETEK)
                               END-IF
                               MOVE    ZERO        TO  FLG-RECECOM-SJKBN
                           END-IF            
      *                 
                           IF      WRK-LINETEK      >=  WRK-LINE-MAX
                               PERFORM 34120-PAGE-SEC
                           END-IF
                           ADD     1               TO  WRK-LINETEK
                           IF      CNT-PAGE    =   1 
                               MOVE    WRK-COMMENT (IDX)
                                       TO  HCN66-TEKIYO2(WRK-LINETEK)
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
           IF  WRK-WLINETEK        >   WRK-LINE-MAX
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
                   IF  WRK-LINETEK         >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                               BY  "　"  
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HCN66-TEKIYO2(WRK-LINETEK)
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
               IF  WRK-LINETEK         >=  WRK-LINE-MAX
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1                   TO  WRK-LINETEK
      *
               INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                           BY  "　"  
               IF      CNT-PAGE        =   1   
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HCN66-TEKIYO2(WRK-LINETEK)
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
               IF  WRK-LINETEK         >=  WRK-LINE-MAX
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
                   IF  WRK-LINETEK         >=  WRK-LINE-MAX
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1   
                       MOVE    RECE82-NYUSYUBETU (IDXX)
                                       TO  HCN66-TEKIYO2(WRK-LINETEK)
                   ELSE                    
                           MOVE    RECE82-NYUSYUBETU (IDXX)
                                       TO  HC24-TEKIYO(WRK-LINETEK)
                   END-IF                    
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-LINE-MAX
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
                   IF  WRK-LINETEK         >=  WRK-LINE-MAX
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
                                       TO  HCN66-TEKIYO2(WRK-LINETEK)
                   ELSE
                       MOVE    WRK-SHOKUJIKHNINF-ETC-AREA
                                       TO  HC24-TEKIYO(WRK-LINETEK)
                   END-IF
               END-PERFORM
               IF      WRK-LINETEK         >=  WRK-LINE-MAX
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
               MOVE    WRK-OPECOM-HEN  TO  HCN66-TEKIYO2(WRK-LINETEK)
           ELSE                
               MOVE    WRK-OPECOM-HEN  TO  HC24-TEKIYO (WRK-LINETEK)
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
      *    レセプト種別不明のとき続紙は出力しない
           IF    ( RECE82-RECESYUBETU   =   ZERO )
           AND   ( CNT-PAGE             >   1    )
               GO  TO  20027-PRINT-OUT-EXT
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               MOVE    "HCN66V01.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCN66               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               MOVE    "HCM24V06P.red"     TO
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
       20028-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
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
           IF      RECE82-ERR-ERRKBN   >    ZERO
               MOVE    ZERO                TO  RECE08-RECESYUBETU
           ELSE
               IF      TBL-RECE-RENNUM (IDXY)  =   1
                   MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
                ELSE
                   MOVE    RECE82-RECESYUBETU  TO  RECE08-RECESYUBETU
                   MOVE    "3"                 TO  RECE08-RECESYUBETU
                                                                (4:1)
               END-IF
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
               DISPLAY "ORCR0963 MCP-RC=" MCP-RC " " RECE08-KEY 
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
      *        複数枚レセの場合連番を記載
               IF      TBL-RECE-MAX        >   1
                   MOVE    SPACE               TO  WRK-HEN-PAGE-AREA
                   MOVE    TBL-RECE-RENNUM (IDXY) 
                                               TO  WRK-PAGE1
                   MOVE    "/"                 TO  WRK-PAGE-HEN
                   MOVE    TBL-RECE-MAX        TO  WRK-PAGE2
                   EVALUATE    RECE08-FORMID
                       WHEN    "HCN66V01.red"
                           MOVE    RECE08-PRTDATA  TO HCN66
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HCN66-PAGE
                           MOVE    HCN66           TO RECE08-PRTDATA
                       WHEN    "HCM24V06P.red"                  
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
               PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT 
               ELSE 
                   DISPLAY "ORCR0963 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE82-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
                   END-STRING
      *************PERFORM 500-ERR-HENSYU-SEC
                   PERFORM 900-DB-ERR-ABORT-SEC
               END-IF
           END-IF    
           .
       3901-RECE08-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理ＤＢ出力処理
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
                   MOVE    ZERO            TO  RECE07-SYORISBT
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
               DISPLAY "ORCR0963 MCP-RC= "  MCP-RC   
               IF      FLG-RECE07         =   1    
               MOVE    "レセプト管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "レセプト管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
      *********PERFORM 500-ERR-HENSYU-SEC
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
           OR    ( RECE82-ERR-ERRKBN   >   ZERO )
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      RECE10-REC
           MOVE    RECE82-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    RECE82-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE82-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE82-PTID         TO  RECE10-PTID
           MOVE    ZERO                TO  RECE10-RECEKA
           MOVE    3                   TO  RECE10-TEISYUTUSAKI
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
           MOVE    RECE82-SYOCD        TO  RECE10-SYOCD
      *****MOVE    ZERO                TO  RECE10-HKNJANUM-S
           IF      RECE82-SYOCD        =   SPACE
               CONTINUE   
           ELSE
               IF      RECE82-SYOKAIFLG    =   "1"
                   MOVE    RECE82-SYOCD        TO  RECE10-HKNJANUM-S
               END-IF
           END-IF
           MOVE    "1"                 TO  RECE10-HKNKBN
           MOVE    RECE82-SYOKAIFLG    TO  RECE10-SYOKAIFLG
           MOVE    RECE82-KOFUNUM      TO  RECE10-KOFUNUM
           MOVE    RECE82-COMMENT      TO  RECE10-COMMENT
           MOVE    RECE82-SAIGAIKBN    TO  RECE10-SAIGAIKBN
      * 
           IF      RECE82-JNISSU       =   999
               MOVE    ZERO                TO  RECE10-JNISSU       (1)
           ELSE
               MOVE    RECE82-JNISSU       TO  RECE10-JNISSU       (1)
           END-IF
           MOVE    RECE82-TOTALTEN     TO  RECE10-TOTALTEN     (1)
           COMPUTE RECE10-FTNMONEY (1) =   RECE82-TOTALKIN     +
                                           RECE82-KINGAKU2-SYO +
                                           RECE82-SHOKUJITOTALKIN
           MOVE    RECE82-TOTALKIN     TO  RECE10-FTNMONEY-FTN (1)
           MOVE    RECE82-KINGAKU2-SYO TO  RECE10-FTNMONEY-KOJ (1)
           MOVE    RECE82-SHOKUJITOTALKIN
                                       TO  RECE10-SHOKUJIRYOYOHI   (1)
           MOVE    RECE82-SHOKUJITOTALDAY
                                       TO  RECE10-SHOKUJINISSU (1) 
      *
           MOVE    RECE82-TENSU (1)    TO  RECE10-TENSU (1 1)
           MOVE    RECE82-TENSU (2)    TO  RECE10-TENSU (3 1)
           MOVE    RECE82-TENSU (3)    TO  RECE10-TENSU (4 1)
           COMPUTE RECE10-TENSU (5 1)  =   RECE82-TENSU (4)    +
                                           RECE82-TENSU (5)    +
                                           RECE82-TENSU (6)    +
                                           RECE82-TENSU (7)    +
                                           RECE82-TENSU (8)    +
                                           RECE82-TENSU (9)
           COMPUTE RECE10-TENSU (6 1)  =   RECE82-TENSU (10)   +
                                           RECE82-TENSU (11)   +
                                           RECE82-TENSU (12)
           COMPUTE RECE10-TENSU (7 1)  =   RECE82-TENSU (13)   +
                                           RECE82-TENSU (14)
           COMPUTE RECE10-TENSU (8 1)  =   RECE82-TENSU (15)   +
                                           RECE82-TENSU (16)
           COMPUTE RECE10-TENSU (9 1)  =   RECE82-TENSU (17)   +
                                           RECE82-TENSU (18)
           COMPUTE RECE10-TENSU (10 1) =   RECE82-TENSU (19)   +
                                           RECE82-TENSU (20)
           COMPUTE RECE10-TENSU (11 1) =   RECE82-TENSU (21)   +
                                           RECE82-TENSU (22)
           MOVE    RECE82-NYUTOTALTEN  TO  RECE10-TENSU (12 1)
           MOVE    RECE82-KINGAKU2 (1) TO  RECE10-TENSU (13 1)
           MOVE    RECE82-KINGAKU2 (2) TO  RECE10-TENSU (18 1)
      *
           MOVE    RECE82-HKNCOMBI     TO  RECE10-HKNCOMBI (1)
      *
           MOVE    "00000000000000000000000000000000000000000000000000"
                                       TO  RECE10-RESERVE-AREA
      *
           MOVE    RECE82-ORIGINALCDKBN
                                       TO  RECE10-FLG (1)
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
               DISPLAY "ORCR0960 SEIKYU MCP-RC= "  MCP-RC
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
           STRING  "ORCR0963 "             DELIMITED   BY  SIZE
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
      *    中止処理時データ削除処理
      *****************************************************************
       5001-DB-DELETE-SEC               SECTION.
      *
      *    診療情報システム用のとき
           IF      WRK-PARA-PRTKBN     =   "B"
               IF      WRK-PARA-SYOKBN     =   "1"
      *            レセ電
                   INITIALIZE                  RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
      *********    MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
      *********    MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "1"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden_tmp"   TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                   "診療情報システム用削除処理でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                   END-IF      
               END-IF
               GO  TO  5001-DB-DELETE-EXT
           END-IF
      *
           IF      WRK-PARA-SYOKBN     =   "1"
      *        請求管理削除処理（一括分）
               INITIALIZE                      RECE10-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
               MOVE    3                   TO  RECE10-TEISYUTUSAKI
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "del6"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE    "請求管理削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF      
      *        レセ電
               INITIALIZE                  RECEDEN-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
               MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
               MOVE    "1"                 TO  RECEDEN-NYUGAIKBN
               MOVE    RECEDEN-REC         TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE
                   "レセ電削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF      
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC21"              TO  RECE07-PRTID
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
               MOVE    "HC21"              TO  RECE07-PRTID
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
               MOVE    "HC21"              TO  RECE08-PRTID
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
               MOVE    WRK-PARA-HOSPNUM    TO  SYS-2002-HOSPNUM
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
                   DISPLAY "DELETE PTNUM=" SYS-2002-PTNUM
                           " SRYYM=" SYS-2002-SRYYM
      *
      *            診療年月の請求管理削除処理
                   INITIALIZE                  RECE10-REC
                   MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  RECE10-PTID
                   MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  RECE10-NYUGAIKBN
                   MOVE    3                   TO  RECE10-TEISYUTUSAKI
                                                   RECE10-XXTEISYUTUSAKI 
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   IF      WRK-PARA-DLTKBN     =   "0"
                       MOVE    "del4"          TO  MCP-PATHNAME
                   ELSE
                       MOVE    "del7"          TO  MCP-PATHNAME
                   END-IF
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   PERFORM   900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                     DISPLAY "TABLE=" MCP-TABLE "PATHNAME=" MCP-PATHNAME
                     DISPLAY " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                        PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月のレセ電削除処理
                   INITIALIZE                  RECEDEN-REC
                   MOVE    WRK-PARA-HOSPNUM    TO
                                                   RECEDEN-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                   MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
                   MOVE    3                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del4"              TO  MCP-PATHNAME
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE
                                       " " MCP-PATHNAME
                                       " MCP-RC=" MCP-RC
                       MOVE
                          "レセ電削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-KANRIMST-NEXT-SEC
               END-PERFORM
      *   
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       5001-DB-DELETE-EXT.
           EXIT.    
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           MOVE    SPACE               TO  RECEYMD-REC
           MOVE    WRK-RECENUM         TO  RECEYMD-RECENUM
           WRITE   RECEYMD-REC
           IF      STS-RECEYMD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE           TO  WRK-ERR-AREA
               INITIALIZE                  WRK-ERR-AREA
               MOVE    3               TO  WRK-ERR-FLG
               MOVE    RECEYMD         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECEYMD     TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *          
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE82-FILE
           CLOSE                       RECE83-FILE
           CLOSE                       RECE87-FILE
           CLOSE                       RECE871-FILE
           CLOSE                       RECE88-FILE
           CLOSE                       RECEYMD-FILE 
      *
           DISPLAY "*** ORCR0963 IN       "  CNT-RECE82
      *    DISPLAY "*** ORCR0963 RECE871= "  CNT-RECE871
           DISPLAY "*** ORCR0963 PAGE     "  CNT-OUT
           DISPLAY "*** ORCR0963 END ***"
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
      *    レセプト基本データ読み込み処理
      *****************************************************************
       990-RECE82-READ-SEC           SECTION.
      *
           READ    RECE82-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE82
                   MOVE    HIGH-VALUE          TO  KEY-NEW
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
                   IF      RECE82-SRYYM        <   WRK-CONS-SRYYM-200610
                       GO  TO  990-RECE82-READ-SEC
                   END-IF
      * 
                   IF      RECE82-PTNUM        =   SPACE
                   OR      RECE82-HKNKBN       =   "1"
                   OR      RECE82-RECESYUBETU  =    "0000"
      *
                       IF    ( RECE82-TOTALKIN         =   ZERO )
                       AND   ( RECE82-KINGAKU2-SYO     =   ZERO )
                       AND   ( RECE82-SHOKUJITOTALKIN  =   ZERO )
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
                   IF      RECE87-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE87-SRYKBN
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
                   IF      STS-RECE871         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE871PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE871     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE871
      *             ADD     1         TO  CNT-RECE871
                   IF      RECE871-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE871-SRYKBN
                   END-IF    
                   IF      RECE871-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE871-SRYKBN
                   END-IF    
                   IF      RECE871-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE871-SRYKBN
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
           STRING  "ORCR0963 db err PTNUM="
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
               STRING  "ORCR0963 select err TABLE="
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
