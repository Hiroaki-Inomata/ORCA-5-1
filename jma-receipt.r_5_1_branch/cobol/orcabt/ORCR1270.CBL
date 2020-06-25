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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCR1270.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : 公害レセプト（入院）
      *                      レセプト印刷
      *  管理者            : 
      *  作成日付    作業者        記述 
      *  07/11/16    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.02.01    NACL-藤原    08/02/19  誕生年の記載の修正
      *  04.02.02    NACL-藤原    08/03/13  平成２０年４月改正対応
      *                                     フラグ、データエリア追加
      *  04.02.03    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     診療区分６４：病理診断対応
      *  04.02.04    NACL-藤原    08/04/18  「レセ電通信医２０００６号」
      *                                     症状詳記区分コード５２の追加
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.02    NACL-藤原    08/08/22  リアルタイムプレビュー時の表示
      *  04.03.03    NACL-藤原    08/08/29  記号番号の記載方法の変更
      *  04.03.04    NACL-藤原    09/01/16  症状詳記区分の表題の変更
      *  04.03.05    NACL-門脇    09/02/04  請求点が０の場合の対応
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-門脇    10/01/27  治験対応
      *  04.05.03    NACL-藤原    10/03/03  平成２２年４月改正対応
      *                                     平成２２年４月分以降の処理を
      *                                     行わない
      *  04.05.04    NACL-藤原    10/03/29  「改正未対応」の記載をやめる
      *                                     平成２２年４月分以降の処理を
      *                                     行うようにする
      *  04.05.05    NACL-藤原    11/08/02  点検レセ印字位置変更対応
      *  04.05.06    NACL-藤原    11/09/05  「請求しない」設定の請求管理
      *                                     の保険の記号番号を更新する
      *  04.05.07    NACL-藤原    11/10/24  「請求しない」設定の請求管理
      *                                     の削除処理の有無対応
      *  04.05.08    NACL-藤原    12/04/11  平成２４年４月改正対応
      *                                     入院略称の変更
      *
      *  04.06.01    NACL-藤原    10/10/04  レセプトコメント文字数増対応
      *  04.06.02    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *                                     キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.03    NACL-藤原    11/10/07  傷病名を３行まで記載可能とする
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.02    NACL-門脇    12/10/12  保険別請求チェック表（標準帳票）
      *                                     の機能追加対応
      *  04.07.03    NACL-門脇    14/06/17  栄養管理体制減算規定該当対応
      *  04.07.04    NACL-藤原    14/12/03  平成２６年１１月改正対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *  04.08.03    NACL-門脇    16/04/15  平成２８年４月改正対応
      *  04.08.04    NACL-門脇    18/04/24  平成３０年４月改正対応
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
           SELECT  RECE200-FILE    ASSIGN  RECE200PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE200-KEY
                                   FILE    STATUS  IS  STS-RECE200.
           SELECT  RECE210-FILE    ASSIGN  RECE210PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE210-KEY
                                   FILE    STATUS  IS  STS-RECE210.
           SELECT  RECE240-FILE    ASSIGN  RECE240PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE240-KEY
                                   FILE    STATUS  IS  STS-RECE240.
           SELECT  RECE250-FILE    ASSIGN  RECE250PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE250-KEY
                                   FILE    STATUS  IS  STS-RECE250.
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
       FD  RECE200-FILE.
       01  RECE200-REC.
           COPY    "CPRCF200.INC".
      *
       FD  RECE210-FILE.
       01  RECE210-REC.
           COPY    "CPRCF110.INC"  REPLACING  //RECE110//
                                   BY         //RECE210//.
      *
       FD  RECE240-FILE.
       01  RECE240-REC. 
           COPY    "CPRCF240.INC".
      *
       FD  RECE250-FILE.
       01  RECE250-REC. 
           COPY    "CPRCF250.INC".
      *
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "COMMON-SPA".
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE200//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE210//.
      *
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE240//.
      *
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
      *****COPY    "HCN58V01.INC"  REPLACING  //HCN58V01//
      *****COPY    "HCN58V02.INC"  REPLACING  //HCN58V02//
           COPY    "HCN58V03.INC"  REPLACING  //HCN58V03//
                                   BY         //HCN58//.
      *****COPY    "HCN59V01.INC"  REPLACING  //HCN59V01//
           COPY    "HCM59V02.INC"  REPLACING  //HCM59V02//
                                   BY         //HCM59//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE200         PIC X(02).
           03  STS-RECE210         PIC X(02).
           03  STS-RECE240         PIC X(02).
           03  STS-RECE250         PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE200         PIC 9(01).
           03  FLG-RECE210         PIC 9(01).
           03  FLG-RECE240         PIC 9(01).
           03  FLG-RECE250         PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-OPECOM          PIC 9(01).
           03  FLG-BTPARA          PIC 9(01).
      *
           03  FLG-TENKI           PIC 9(01).
           03  FLG-SRYKBNPRT       PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-OPECOM-CHK      PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-STS             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE200         PIC 9(06).
           03  CNT-RECE210         PIC 9(06).
           03  CNT-RECE240         PIC 9(06).
           03  CNT-RECE250         PIC 9(06).
           03  CNT-OUT             PIC 9(06).
      *
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
           03  CNT-COMMIT          PIC 9(03). 
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
           03  IDX4                PIC 9(02).
           03  IDY1                PIC 9(04).
           03  IDY2                PIC 9(04).
           03  IDY3                PIC 9(03).
           03  IDY4                PIC 9(03).                           
           03  IDY5                PIC 9(03).
           03  IDXX                PIC 9(03).                           
           03  IDXY                PIC 9(03).
      *     
           03  SYSKAN-IDX          PIC 9(03).
      *
      *    
       01  SUM-AREA                    VALUE   ZERO.          
           03  SUM-NYUKHNNISSU     PIC 9(03).
           03  SUM-NYUKHNKEI       PIC 9(07).
           03  SUM-NYUKHNKEI2      PIC S9(07).
      *
      *    
       01  KEY-AREA                    VALUE   LOW-VALUE.          
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-HOSPNUM   PIC  9(02).
                   07  KEY-N-SRYYM     PIC  9(06).
                   07  KEY-N-NYUGAIKBN PIC  X(01).
                   07  KEY-N-PTID      PIC  9(10).
                   07  KEY-N-RECEKA    PIC  X(02).
               05  KEY-N-HKNID         PIC  9(10).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-HOSPNUM   PIC  9(02).
                   07  KEY-O-SRYYM     PIC  9(06).
                   07  KEY-O-NYUGAIKBN PIC  X(01).
                   07  KEY-O-PTID      PIC  9(10).
                   07  KEY-O-RECEKA    PIC  X(02).
               05  KEY-O-HKNID         PIC  X(10).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC     OCCURS  100.
               05  TBL-SYS1001     PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC     OCCURS  100.
               05  TBL-SYS1002     PIC  X(610).
      *    帳票別指定医療機関編集情報退避用領域
       01  TBL-SYS1900-T.
           03  TBL-SYS1900-OCC     OCCURS  100.
               05  TBL-SYS1900     PIC  X(610).
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
           03  WRK-NUM             PIC X(50).
           03  WRK-KIGO            PIC X(50).
           03  WRK-KIGO-LEN        PIC 9(03).
      *
           03  WRK-HCN58-HEAD1-G   PIC X(128).
      *
           03  WRK-OPECOM-MCP-TABLE       PIC X(64).
           03  WRK-OPECOM-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
           03  WRK-EDIT-AREA.
      *        傷病名連番     
               05  WRK-LINE                    PIC 9(02).
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
      *        適用欄編集行         
               05  WRK-LINETEK                 PIC 9(03).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE                PIC 9(03).
      *    適用欄行数
           03  WRK-TEKIYOU-LINE-GYO            PIC 9(03).
      *    傷病名編集
           03  WRK-BYOMEI                      PIC X(150).
           03  WRK-BYOMEIW                     PIC X(150).
      *    傷病名編集フラグ（疑い）
           03  WRK-HENFLG1                     PIC X(01).
      *    傷病名編集フラグ（急性）
           03  WRK-HENFLG2                     PIC X(01). 
      *    傷病名編集（傷病名欄）
           03  WRK-BYOMEI1                     PIC X(54).
           03  WRK-BYOMEI1-W   REDEFINES       WRK-BYOMEI1.
               05  WRK-BYOMEI1-NO              PIC X(06).
               05  WRK-BYOMEI1-NAME            PIC X(48).
               
      *    傷病名編集（適用欄）
           03  WRK-BYOMEI2                     PIC X(52).
           03  WRK-BYOMEI2-W   REDEFINES       WRK-BYOMEI2.
               05  WRK-BYOMEI2-NO              PIC X(08).
               05  WRK-BYOMEI2-NAME            PIC X(44).
               05  WRK-BYOMEI2-NAMEW   REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(20).
                   07  WRK-BYOMEI2-ELSE.
                       09  WRK-BYOMEI2-YMD     PIC X(20).
                       09  WRK-BYOMEI2-TENKI   PIC X(04).
      *                 
      *    入退院履歴編集（適用欄）
           03  WRK-NYUTAI-RIREKI-AREA.
               05  WRK-NYUTAI-RIREKI           PIC X(52).
               05  WRK-NYUTAI-RIREKI-W         REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  WRK-NYUTAI-MSG          PIC X(12).
                   07  FILLER                  PIC X(40).
               05  WRK-NYUTAI-RIREKI-W1        REDEFINES
                                               WRK-NYUTAI-RIREKI.
                   07  FILLER                  PIC X(08).
                   07  WRK-NYUTAI-NYUINYMD     PIC X(20).
                   07  WRK-NYUTAI-FL1          PIC X(02).
                   07  WRK-NYUTAI-TAIINYMD     PIC X(20).
                   07  WRK-NYUTAI-FL2          PIC X(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-TNSIDX                      PIC 9(02).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
      *
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-KGKZ                        PIC ZZZ,ZZZ,ZZZ.
           03  WRK-KGK9                        PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KGK7                        PIC ZZZZ,ZZ9.
           03  WRK-KGK6                        PIC ZZZ,ZZ9.
           03  WRK-KGK4                        PIC ZZZZ.
           03  WRK-KGK2                        PIC Z9.
           03  WRK-KGK10                       PIC ZZ,ZZZ,ZZ9.
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
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE-HEN    PIC X(01).
               05  WRK-PAGE2       PIC Z9.
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-SRYKACD         PIC X(04).
           03  WRK-SRYKANAME       PIC X(40).
      *
           03  WRK-SYOKUJIKEI      PIC 9(07).
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
           03  WRK-HENYMDG         PIC X(22).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *
           03  WRK-HENTIME.
               05  WRK-THH         PIC 99.
               05  FILLER          PIC X(01)   VALUE  ":".
               05  WRK-TMM         PIC 99.
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
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   54.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *    適用欄最大行数
           03  WRK-TEKIYOU1-LINE   PIC 9(02)   VALUE   50.
      *    適用欄（続紙）最大行数
           03  WRK-TEKIYOU2-LINE   PIC 9(03)   VALUE   110.
      *    適用欄（続紙）の行別行数
           03  WRK-TEKIYOU2-LINE-GYO
                                   PIC 9(03)   VALUE   55.
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
      *    疑い編集
       01  WRK-UTAGAI-AREA.
      *    疑い
           03  WRK-CONS-UTAGAI     PIC X(06)   VALUE   "の疑い".
           03  WRK-CONS-UTAGAI-MOJISU
                                   PIC 9(02)   VALUE   6.       
      *    急性
           03  WRK-CONS-KYUSEI     PIC X(04)   VALUE   "急性".      
           03  WRK-CONS-KYUSEI-MOJISU
                                   PIC 9(02)   VALUE   4.       
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "―".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTEN.
               05  WRK-KHNTENZ     PIC  ZZZZZZZ.
           03  WRK-KAISU.
               05  WRK-KAISUZ      PIC  ZZZ.
           03  WRK-TENSU.
               05  WRK-TENSUZ      PIC  ZZZZZZZ.
           03  WRK-JINISSU2.
               05  WRK-JINISSU2Z   PIC  Z9.
           03  WRK-TOTALTENZ       PIC  ZZ,ZZZ,ZZ9.
           03  WRK-TENSU2.
               05  WRK-TENSU2Z     PIC  ------Z.
      *
       01  WRK-ERR-MSG-AREA.
           03  WRK-ERR-MSG-1.
               05  WRK-ERR-HKNCOMBINUM-1
                                   PIC X(08).
               05  FILLER          PIC X(28)   VALUE
                   "の保険組合せが存在しません。". 
      *
           03  WRK-ERR-MSG-2.
               05  WRK-ERR-DAY-2   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-21.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-2
                                   PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用開始日が". 
                   07  WRK-ERR-TEKSTYMD-2
                                   PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-3.
               05  WRK-ERR-DAY-3   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-31.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-3
                                   PIC X(08).
                   07  FILLER          PIC X(14)   VALUE
                   "は適用終了日が". 
                   07  WRK-ERR-TEKEDYMD-3
                                   PIC X(20).
                   07  FILLER          PIC X(14)   VALUE
                   "となっています". 
      *
           03  WRK-ERR-MSG-4.
               05  WRK-ERR-DAY-4   PIC X(04).
               05  FILLER          PIC X(04)   VALUE
                   "日の". 
               05  WRK-ERR-MSG-41.
                   07  FILLER          PIC X(10)   VALUE
                   "保険組合せ". 
                   07  WRK-ERR-HKNCOMBINUM-4
                                   PIC X(08).
                   07  FILLER          PIC X(28)   VALUE
                   "は適用年月日外となっています". 
      *
           03  WRK-ERR-MSG-6           PIC X(30)  VALUE
               "【未請求設定の患者です】".
      *
           03  WRK-ERR-MSG-7.
               05  FILLER          PIC X(48)   VALUE
                   "【リアルタイムプレビューより作成のため提出不可】". 
      *
       01  WRK-CONS-AREA.    
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *    レセ電診療科コードテーブル
           COPY    "CMRECEDENSRYKACD.INC".
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
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
           COPY    "CPSK1901.INC".
           COPY    "CPSK1902.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
      *
           COPY    "CPRCF008.INC".
      *
           COPY    "CPRCF010.INC".
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
      *    パラメタ
       01  BTPARA-REC.
           COPY    "CPBTPARA.INC".
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
      *    数値変換サブ
           COPY    "CPORCSNUM.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
      *
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
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
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  SPA-AREA
           INITIALIZE                  CNT-AREA
                                       TABLE-AREA
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
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
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ファイルＯＰＥＮ処理
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAME   TO  RECEERR
      *
           MOVE    "RECE700"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE200PARA
           MOVE    "RECE710"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE210PARA
           MOVE    "RECE740"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE240PARA
           MOVE    "RECE750"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    TEMPFLPARA          TO  RECE250PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    INPUT               RECE200-FILE
           IF      STS-RECE200         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE200PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE200         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE210-FILE
           IF      STS-RECE210         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE210PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE210         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE240-FILE
           IF      STS-RECE240         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE240PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE240         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE250-FILE
           IF      STS-RECE250         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE250PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE250         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理開始処理
               MOVE    "STS"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    "ORCR1270"      TO  JOB-PGID
               MOVE    "印刷データ編集"
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
           IF      FLG-STS             =   ZERO
              PERFORM 900-RECE200-READ-SEC
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC   TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                        TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2               TO  FLG-STS
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
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-KANRIMST  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  FLG-STS
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1002-REC
              ELSE    
             MOVE    "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  FLG-STS
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
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-KANRIMST  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1002-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1002-REC  TO  TBL-SYS1002(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"      TO  MCP-TABLE
                    MOVE    "key8"              TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
      *
              IF      SYSKAN-IDX          =   ZERO
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  FLG-STS
                  GO  TO  120-HOSPINF-GET-EXT
              END-IF
           END-IF
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
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC      TO  SYS-1900-REC
              ELSE    
                  INITIALIZE                   SYS-1900-REC
              END-IF
              IF      SYS-1900-PRTKBN (15) =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (15) TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (15) TO  SYS-KBNCD
                  MOVE    WRK-PARA-SRYYM       TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
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
              PERFORM   900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-KANRIMST  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1900-REC
                    IF      SYS-1900-PRTKBN (15) =   SPACE
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
      *
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
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
           IF      FLG-KANRIMST    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-25-REC
           ELSE    
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-INSPECT-CHARACTER-KBN
                                   =   SPACE
               MOVE    ZERO            TO
                                       SYS-25-INSPECT-CHARACTER-KBN
           END-IF
           DISPLAY "SYS-200501-INSPECT-CHARACTER-KBN="
                                    SYS-25-INSPECT-CHARACTER-KBN
                    " " WRK-PARA-PRTKBN
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE200    =   1
                           OR      FLG-STS        =   2 
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE200    =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW1   NOT =   KEY-OLD1
      *            レセプト種別別件数カウント
                   MOVE    ZERO            TO  CNT-KENSU
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-RECE200    =   1
                                   OR      FLG-STS        =   2
                                   OR      KEY-NEW    NOT =   KEY-OLD
      *
                       ADD     1                      TO  CNT-KENSU
      *                                      
                       PERFORM 2001-SYORI-SEC
      *
                       ADD     1                 TO  CNT-COMMIT
                       IF  CNT-COMMIT      =    300
                           MOVE  ZERO      TO      CNT-COMMIT
                           PERFORM 900-DBCOMMIT-SEC
                           PERFORM 900-DBSTART-SEC
                       END-IF
      *
                       PERFORM 900-RECE200-READ-SEC
                   END-PERFORM
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
      *    主処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1                  TO  CNT-PAGE
           MOVE    WRK-TEKIYOU1-LINE  TO  WRK-TEKIYOU-LINE
           INITIALIZE                     TABLE-AREA
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HCN06-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *    帳票編集<入退院履歴・入院基本料種別の続き>処理
           PERFORM 380-HC101-NYUTAI-RIREKI-HEN-SEC
      *
      *    帳票編集<手術歴>処理
           PERFORM 381-HC101-OPECOM-HEN-SEC 
      *
      *    帳票編集<剤>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<コメント>処理
           PERFORM 360-HC101-COMMENT-HEN-SEC 
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
      *    帳票更新処理
           PERFORM 391-DB-WRITE-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HCN06-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HCN58
      *
      *    患者番号
           MOVE    RECE200-PTNUM       TO  HCN58-PTNUM
      *
      *    診療年月（診療年月日として退避）
           MOVE    RECE200-SRYYM       TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           MOVE    WRK-SYMD            TO  WRK-SRYYMD
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  LNK-DAY2-EDTYMD3 (1:16) DELIMITED  BY  SIZE
                   "分"                    DELIMITED  BY  SIZE 
                                           INTO    HCN58-SRYYM
           END-STRING
      *
      *    記号番号
           MOVE    SPACE               TO  WRK-KIGO
                                           WRK-NUM
           MOVE    ZERO                TO  WRK-KIGO-LEN
           IF      RECE200-KIGO    NOT =   SPACE 
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    RECE200-KIGO        TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-KIGO
                   ADD     KANACHK-MAX         TO  WRK-KIGO-LEN
               END-IF
           END-IF
           IF      RECE200-NUM     NOT =   SPACE 
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI 
               MOVE    RECE200-NUM         TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
               IF      KANACHK-RC          =   ZERO
                   MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                               TO  WRK-NUM
                   ADD     KANACHK-MAX         TO  WRK-KIGO-LEN
               END-IF
           END-IF
           IF      WRK-NUM             =   SPACE
               MOVE    WRK-KIGO            TO  HCN58-KIGONUM
           ELSE
               IF      WRK-KIGO-LEN        <=  22
                   STRING  WRK-KIGO            DELIMITED   BY  SPACE
                           "―"                DELIMITED   BY  SIZE
                           WRK-NUM             DELIMITED   BY  SPACE
                                               INTO    HCN58-KIGONUM
                   END-STRING
               ELSE
                   STRING  WRK-KIGO            DELIMITED   BY  SPACE
                           "―"                DELIMITED   BY  SIZE
                                               INTO    HCN58-KIGONUM2(1)
                   END-STRING
                   MOVE    WRK-NUM             TO  HCN58-KIGONUM2(2)
               END-IF
           END-IF     
      *
      *    カナ氏名
           MOVE    RECE200-KANANAME    TO  HCN58-KANANAME
      *
      *    氏名
           MOVE    RECE200-NAME        TO  HCN58-NAME
      *
      *    性別
           EVALUATE    RECE200-SEX
               WHEN    "1"
                   MOVE    "○"            TO  HCN58-SEX (1)
               WHEN    "2"
                   MOVE    "○"            TO  HCN58-SEX (2)
           END-EVALUATE
      *
      *    生年月日
           MOVE    RECE200-BIRTHDAY    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           EVALUATE    LNK-DAY2-EDTYMD1-G
               WHEN    "M"
                   MOVE    "○"            TO  HCN58-BIRTHG (1)
               WHEN    "T"
                   MOVE    "○"            TO  HCN58-BIRTHG (2)
               WHEN    "S"
                   MOVE    "○"            TO  HCN58-BIRTHG (3)
               WHEN    "H"
                   MOVE    "○"            TO  HCN58-BIRTHG (4)
           END-EVALUATE
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HCN58-BIRTHYY
      *
      *    医療機関所在地
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-ADRS1      TO  HCN58-HOSPADRS (1:)
               MOVE    SYS-1902-ADRS2      TO  HCN58-HOSPADRS (51:)
           ELSE
               MOVE    SYS-1002-ADRS       TO  HCN58-HOSPADRS
           END-IF
           INSPECT HCN58-HOSPADRS  REPLACING   ALL "  "    BY  "　"
      *
      *    医療機関名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HCN58-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HCN58-HOSPNAME (41:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HCN58-HOSPNAME
           END-IF
           INSPECT HCN58-HOSPNAME  REPLACING   ALL "  "    BY  "　"
      *
      *    医療機関ＴＥＬ
           IF      FLG-SYS-1902        =   1
               MOVE    SYS-1902-TEL        TO  HCN58-HOSPTEL
           ELSE
               MOVE    SYS-1002-TEL        TO  HCN58-HOSPTEL
           END-IF
      *
      *    診療実日数
           MOVE    RECE200-JNISSU      TO  WRK-JINISSU2Z
           MOVE    WRK-JINISSU2        TO  HCN58-JNISSU
      *
      *    病棟区分
      *****PERFORM 3110-BYOTOKBN-SET-SEC
      *
           IF      RECE200-RECESYUBETU =    ZERO
               PERFORM 3101-FUMEI-HENSYU-SEC
           END-IF
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN     =   "2" OR  "3" OR  "4" OR  "5"
               IF      SYS-25-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HCN58-LABEL
                    MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HCN58-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HCN58-LOPTION
           END-IF
      *
      *    リアルタイムプレビュー時の表示
           IF      WRK-PARA-SHELLID    =   "RECEPTX"
               MOVE    SPACE                TO  WRK-HCN58-HEAD1-G
               MOVE    HCN58-HEAD1-G (5:)   TO  WRK-HCN58-HEAD1-G
               MOVE    WRK-ERR-MSG-7        TO  HCN58-HEAD1-G (5:)
               MOVE    WRK-HCN58-HEAD1-G    TO  HCN58-HEAD1-G (53:)
           END-IF
      *
           .
       310-HCN06-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別不明のとき不明の原因を編集処理
      *****************************************************************
       3101-FUMEI-HENSYU-SEC              SECTION.
      *
           EVALUATE    RECE200-ERR-ERRKBN
               WHEN    1
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE200-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-1
                   END-IF
                   MOVE    WRK-ERR-MSG-1   TO   HCN58-HEAD1-G (5:)
               WHEN    2
               WHEN    3
                   IF      RECE200-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE200-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-2
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE200-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-2
                   END-IF
                   MOVE    RECE200-ERR-TEKSTYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKSTYMD-2
                   IF      RECE200-ERR-ERRKBN    =   3
                       MOVE    WRK-ERR-MSG-21  TO   HCN58-HEAD1-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-2   TO   HCN58-HEAD1-G (5:)
                   END-IF
               WHEN    4
               WHEN    5
                   IF      RECE200-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE200-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-3
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE200-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-3
                   END-IF
                   MOVE    RECE200-ERR-TEKEDYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKEDYMD-3
                   IF      RECE200-ERR-ERRKBN    =   5
                       MOVE    WRK-ERR-MSG-31  TO   HCN58-HEAD1-G (5:)
                   ELSE
                       MOVE    WRK-ERR-MSG-3   TO   HCN58-HEAD1-G (5:)
                   END-IF
               WHEN    6
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE200-ERR-DAY  TO  WRK-KGK2
                   MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-DAY-4
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE200-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-4
                   END-IF
                   MOVE    WRK-ERR-MSG-4   TO   HCN58-HEAD1-G (5:)
           END-EVALUATE
           INSPECT HCN58-HEAD1-G  REPLACING ALL "  "    BY  "　"
      *
           .
       3101-FUMEI-HENSYU-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HCN061-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HCM59
      *
      *    患者番号
           MOVE    HCN58-PTNUM         TO  HCM59-PTNUM
      *
      *    氏名
           MOVE    HCN58-NAME          TO  HCM59-NAME
      *
      *    カナ氏名
           MOVE    HCN58-KANANAME      TO  HCM59-KANANAME
      *
      *    点検用レセプト作成時
           MOVE    HCN58-LABEL         TO  HCM59-LABEL
           MOVE    HCN58-LOPTION       TO  HCM59-LOPTION
          .
       310-HCN061-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <病棟区分>編集処理                                  
      *****************************************************************
       3110-BYOTOKBN-SET-SEC           SECTION.                         
      *                                                                 
           MOVE    1               TO  IDY5                                  
      *                                                                 
           PERFORM VARYING IDY4    FROM    1   BY  1              
                   UNTIL   IDY4    >       4                   
                   OR      RECE200-BYOTOKBN (IDY4) =  SPACE                
               EVALUATE    RECE200-BYOTOKBN (IDY4)                         
                   WHEN    "01"                                            
      *****            MOVE    "精神　"    TO  HCN58-NYUINKBN(IDY5:6)        
                       ADD     6           TO  IDY5                         
               END-EVALUATE                                            
           END-PERFORM                                                  
      *                                                                 
           .                                                            
       3110-BYOTOKBN-SET-EXT.                                           
           EXIT.                                                        
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
           MOVE    ZERO                TO  FLG-TENKI
      *     
           INITIALIZE                      RECE210-REC
           MOVE    RECE200-KEY         TO  RECE210-KEY1
      *
           START   RECE210-FILE
               KEY IS    >=   RECE210-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE210
               NOT INVALID
                   PERFORM 900-RECE210-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE210        =   1            )
               OR     ( RECE200-KEY    NOT =   RECE210-KEY1 )
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE210     
      *
           IF  WRK-LINEBYO      <  WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2-NAME  NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
               END-IF
           END-IF
           .
       320-HC101-BYOMEI-HEN-EXT.
           EXIT.
      *
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       3210-BYOMEI-SET-SEC           SECTION.
      *     
           MOVE    RECE210-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE210-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE210-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE210-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE210-BYOMEIMOJI   *   2
           MOVE    RECE210-BYOMEI      TO  WRK-BYOMEI
           MOVE    ZERO                TO  WRK-CONTINUE
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                   IF      WRK-MOJISU      >   WRK-BYOMEI1-MAX
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                           MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                   IF      WRK-MOJISU      >   WRK-BYOMEI2-MAX 
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                   END-IF                                       
           END-EVALUATE
      *
           IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
                   IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                       MOVE    WRK-BYOMEI(97:) TO  WRK-BYOMEI1-NAME
                       MOVE    1               TO  WRK-CONTINUE
                   ELSE
                       MOVE    WRK-BYOMEI(97:) TO  WRK-BYOMEI2-NAME
                       MOVE    1               TO  WRK-CONTINUE    
                   END-IF                                   
               END-IF
           ELSE
               IF  WRK-BYOMEI2   NOT =   SPACE
                   IF      WRK-BYOMEI(89:)   =   SPACE
                       MOVE    1             TO  FLG-TENKI
                   ELSE
                       MOVE    ZERO          TO  FLG-TENKI
                   END-IF
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    1               TO  WRK-CONTINUE
                   MOVE    WRK-BYOMEI(89:) TO  WRK-BYOMEI2-NAME
                END-IF
           END-IF
      *
           IF  WRK-LINEBYO       <   WRK-BYOMEI-LINE
               IF  WRK-BYOMEI1-NAME  NOT =   SPACE
                   PERFORM 3221-BYOMEI1-OUT-SEC
               END-IF
           ELSE
               IF  WRK-BYOMEI2   NOT =   SPACE
                   MOVE    1             TO  FLG-TENKI
                   PERFORM 3222-BYOMEI2-OUT-SEC
                   MOVE    ZERO          TO  FLG-TENKI
                END-IF
           END-IF
      *
           PERFORM 900-RECE210-NEXT-SEC
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *     傷病名１出力処理
      *      WRK-CONTINUE = 1 の場合、２行編集の２行目
      ****************************************************************
       3221-BYOMEI1-OUT-SEC        SECTION.
      *
      *    傷病名欄の行カウント
           ADD     1               TO  WRK-LINEBYO
      *
           IF  WRK-CONTINUE        =   ZERO
               MOVE    "　　　"        TO  WRK-BYOMEI1-NO
      *
      *        主病が複数のとき先頭以外の項番の記載はしない
               IF    ( WRK-LINE        =   1   )
               AND   ( WRK-LINEBYO     >   1   )
               AND   ( RECE210-SYUBYOFLG
                                       =   "1" )
                   CONTINUE
               ELSE
      *            病名の項番カウント
                   ADD     1                   TO  WRK-LINE
      *            項番
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    "("             TO  KANACHK-MAE-INPUT (1:1)
                   MOVE    WRK-LINE (2:1)  TO  KANACHK-MAE-INPUT (2:1)
                   MOVE    ")"             TO  KANACHK-MAE-INPUT (3:1)
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-BYOMEI1-NO 
                   END-IF
               END-IF    
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  HCN58-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  HCN58-SRYYMD
                                                    (WRK-LINEBYO)(7:)
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
                   WHEN    "1"
                       MOVE    "○"      TO  HCN58-TENKI (WRK-LINEBYO 1)
                   WHEN    "2"
                       MOVE    "○"      TO  HCN58-TENKI (WRK-LINEBYO 2)
                   WHEN    "3"
                       MOVE    "○"      TO  HCN58-TENKI (WRK-LINEBYO 3)
               END-EVALUATE
           END-IF
      *    病名
           INSPECT WRK-BYOMEI1
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    WRK-BYOMEI1     TO  HCN58-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3221-BYOMEI1-OUT-EXT.
           EXIT.
      ****************************************************************
      *     傷病名２出力処理
      *      WRK-CONTINUE = 0 の場合、２行編集の１行目
      ****************************************************************
       3222-BYOMEI2-OUT-SEC        SECTION.
      *
      *    適用欄の行カウント
           IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF    
           ADD     1               TO  WRK-LINETEK
      
      *
           IF  WRK-CONTINUE        =   ZERO
               MOVE    "　　　"        TO  WRK-BYOMEI2-NO
      *
      *        主病が複数のとき項番の記載はしない
               IF    ( WRK-LINE        =   1   )
               AND   ( RECE210-SYUBYOFLG
                                       =   "1" )
                   CONTINUE
               ELSE
      *            病名の項番カウント
                   ADD     1                   TO  WRK-LINE
      *            項番
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   IF  WRK-LINE            <   10
                       MOVE    "("         TO  KANACHK-MAE-INPUT (1:1)
                       MOVE    WRK-LINE (2:1)
                                           TO  KANACHK-MAE-INPUT (2:1)
                       MOVE    ")"         TO  KANACHK-MAE-INPUT (3:1)
                   ELSE    
                       MOVE    "("         TO  KANACHK-MAE-INPUT (1:1)
                       MOVE    WRK-LINE (1:2)
                                           TO  KANACHK-MAE-INPUT (2:2)
                       MOVE    ")"         TO  KANACHK-MAE-INPUT (4:1)
                   END-IF
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-BYOMEI2-NO 
                   END-IF
               END-IF
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      WRK-BYOMEI2-ELSE    =   SPACE
      *        診療年月日・転帰編集処理
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
               MOVE    ZERO            TO  FLG-TENKI
           END-IF
      *
           INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    WRK-BYOMEI2 TO  HCN58-TEKIYO (WRK-LINETEK)
           ELSE
               MOVE    WRK-BYOMEI2 TO  HCM59-TEKIYO (WRK-LINETEK)
           END-IF    
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *        診療年月日・転帰編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               INSPECT WRK-BYOMEI2
                           REPLACING    ALL   "  "   BY   "　"
               IF      CNT-PAGE        =   1
                   MOVE    WRK-BYOMEI2 TO  HCN58-TEKIYO (WRK-LINETEK)
               ELSE
                   MOVE    WRK-BYOMEI2 TO  HCM59-TEKIYO (WRK-LINETEK)
               END-IF    
           END-IF
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    診療開始日
           MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *    転帰
           EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2-TENKI
           END-EVALUATE
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
           EVALUATE    RECE200-SHOSHINTIME
               WHEN    "1"
                   MOVE    "○"            TO  HCN58-SHOSHIN (1)
               WHEN    "2"
                   MOVE    "○"            TO  HCN58-SHOSHIN (3)
               WHEN    "3"
                   MOVE    "○"            TO  HCN58-SHOSHIN (2)
               WHEN    "4"
                   MOVE    "○"            TO  HCN58-SHOSHIN (1)
                   MOVE    "○"            TO  HCN58-SHOSHIN (3)
               WHEN    "5"
                   MOVE    "○"            TO  HCN58-SHOSHIN (1)
                   MOVE    "○"            TO  HCN58-SHOSHIN (2)
               WHEN    "6"
                   MOVE    "○"            TO  HCN58-SHOSHIN (3)
                   MOVE    "○"            TO  HCN58-SHOSHIN (2)
               WHEN    "7"
                   MOVE    "○"            TO  HCN58-SHOSHIN (1)
                   MOVE    "○"            TO  HCN58-SHOSHIN (3)
                   MOVE    "○"            TO  HCN58-SHOSHIN (2)
           END-EVALUATE
      *            
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       21
      *        回数
               MOVE    RECE200-KAISU(IDX)      TO  WRK-KAISUZ
               MOVE    WRK-KAISU               TO  HCN58-KAISU  (IDX)
      *        点数
               IF   IDX  =  3
                 MOVE    RECE200-TENSU(IDX 1)
                                           TO  WRK-TENSUZ
                 MOVE    WRK-TENSU         TO  HCN58-TENSU
                                                   (1 IDX)
                 MOVE    RECE200-TENSU(IDX 2)
                                           TO  WRK-TENSUZ
                 MOVE    WRK-TENSU         TO  HCN58-TENSU
                                                   (2 IDX)
               ELSE
                 EVALUATE    IDX
                   WHEN    1   THRU    2
                   WHEN    7   THRU    10
                   WHEN    12
                   WHEN    14
                   WHEN    16
                   WHEN    18
                   WHEN    20
                       MOVE    1           TO  WRK-TNSIDX
                   WHEN    OTHER
                       MOVE    2           TO  WRK-TNSIDX
                 END-EVALUATE
                 MOVE    RECE200-TENSU(IDX WRK-TNSIDX)
                                           TO  WRK-TENSUZ
                 MOVE    WRK-TENSU         TO  HCN58-TENSU
                                                   (WRK-TNSIDX IDX)
               END-IF
           END-PERFORM
      *
      *    ９０入院            
      *    入院年月日
           MOVE    RECE200-NYUINYMD    TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2-G  TO  HCN58-NYUING
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HCN58-NYUINYY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HCN58-NYUINMM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  HCN58-NYUINDD
      *
      *    医療機関種別
           EVALUATE    RECE200-HOSPSBT
               WHEN    "1"
                   MOVE    "○"        TO  HCN58-HOSPSBT (1)
               WHEN    "2"
                   MOVE    "○"        TO  HCN58-HOSPSBT (2)
           END-EVALUATE        
      *
      *    入院基本料種別
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
                   OR      RECE200-NYUSYUBETU (IDX) =   SPACE
               IF      RECE200-NYUSYUBETU (IDX) (11:)
                                       =   SPACE
                   MOVE    RECE200-NYUSYUBETU (IDX)
                                         TO  HCN58-NYUSYUBETU (IDX)
               ELSE
                   MOVE    RECE200-NYUSYUBETU (IDX)
                                         TO  HCN58-NYUSYUBETU2(IDX)
               END-IF
           END-PERFORM
      *
      *    入院基本料
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       4
      *        <基本料>
               MOVE    RECE200-NYUKHNRYO (IDX)
                                           TO  WRK-KHNTENZ
               MOVE    WRK-KHNTEN          TO  HCN58-NYUKHNTEN (IDX)
      *        <日数>
               MOVE    RECE200-NYUKHNNISSU(IDX)
                                           TO  WRK-KAISUZ
               MOVE    WRK-KAISU           TO  HCN58-NYUKAISU  (IDX)
      *        <金額>
               MOVE    RECE200-NYUKHNKEI(IDX)
                                           TO  WRK-TENSUZ
               MOVE    WRK-TENSU           TO  HCN58-NYUTENSU  (1 IDX)
           END-PERFORM
      *
           IF        RECE200-NYUKHNRYO (5) >   ZERO
               MOVE    ZERO                    TO  SUM-AREA  
               PERFORM VARYING     IDX     FROM    4   BY  1
                       UNTIL       IDX     >      20
                       OR          RECE200-NYUKHNRYO (IDX) =   ZERO
                   ADD     RECE200-NYUKHNNISSU (IDX)
                                                   TO  SUM-NYUKHNNISSU
                   ADD     RECE200-NYUKHNKEI   (IDX)
                                                   TO  SUM-NYUKHNKEI
               END-PERFORM    
      *        <基本料>
               MOVE    SPACE               TO  HCN58-NYUKHNTEN (4)
      *        <日数>
               MOVE    SUM-NYUKHNNISSU     TO  WRK-KAISUZ
               MOVE    WRK-KAISU           TO  HCN58-NYUKAISU  (4)
      *        <金額>
               MOVE    SUM-NYUKHNKEI       TO  WRK-TENSUZ
               MOVE    WRK-TENSU           TO  HCN58-NYUTENSU  (1 4)
           END-IF
      *
      *    <入院基本料・加算>
           IF      RECE200-NYUKHNKEI2  <   ZERO
               MOVE    RECE200-NYUKHNKEI2  TO  WRK-TENSU2Z
               MOVE    WRK-TENSU2          TO  HCN58-NYUTENSU (1 5)
           ELSE
               MOVE    RECE200-NYUKHNKEI2  TO  WRK-TENSUZ
               MOVE    WRK-TENSU           TO  HCN58-NYUTENSU (1 5)
           END-IF
      *
      *    公害入院療養指導
           MOVE    5           TO      IDX4
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       3
               ADD     1       TO      IDX4
      *        <基本料>
               MOVE    RECE200-KOGAISIDORYO(IDX)
                                           TO  WRK-KHNTENZ
               MOVE    WRK-KHNTEN          TO  HCN58-NYUKHNTEN (IDX4)
      *        <日数>
               MOVE    RECE200-KOGAISIDONISSU(IDX)
                                           TO  WRK-KAISUZ
               MOVE    WRK-KAISU           TO  HCN58-NYUKAISU  (IDX4)
      *        <金額>
               MOVE    RECE200-KOGAISIDOKEI(IDX)
                                           TO  WRK-TENSUZ
               MOVE    WRK-TENSU           TO  HCN58-NYUTENSU  (2 IDX4)
           END-PERFORM
      *
      *    清浄空気室管理料
      *    <日数>
           MOVE    RECE200-SEIKANNISSU     TO  WRK-KAISUZ
           MOVE    WRK-KAISU               TO  HCN58-NYUKAISU  (9)
      *    <金額>
           MOVE    RECE200-SEIKANKEI       TO  WRK-TENSUZ
           MOVE    WRK-TENSU               TO  HCN58-NYUTENSU  (2 9)
      *
      *    <特定入院料・その他>
           MOVE    ZERO                    TO  SUM-AREA
           PERFORM VARYING   IDX   FROM    1   BY  1
                   UNTIL     IDX       >   20
                   OR        RECE200-NYUTOKNISSU (IDX)  =   ZERO
      *        <日数>
               ADD     RECE200-NYUTOKNISSU(IDX)
                                               TO  SUM-NYUKHNNISSU
      *        <金額>
               ADD     RECE200-NYUTOKKEI(IDX)  TO  SUM-NYUKHNKEI2
           END-PERFORM
      *    <金額>
           ADD         RECE200-NYUTOKKEI2      TO  SUM-NYUKHNKEI2
      *    <日数>
           MOVE    SUM-NYUKHNNISSU     TO  WRK-KAISUZ
           MOVE    WRK-KAISU           TO  HCN58-NYUKAISU (10)
      *    <金額>
      ***  MOVE    SUM-NYUKHNKEI       TO  WRK-TENSUZ
      ***  MOVE    WRK-TENSU           TO  HCN58-NYUTENSU (1 10)
           IF      SUM-NYUKHNKEI2   <   ZERO
               MOVE    SUM-NYUKHNKEI2  TO  WRK-TENSU2Z
               MOVE    WRK-TENSU2      TO  HCN58-NYUTENSU (1 10)
           ELSE
               MOVE    SUM-NYUKHNKEI2  TO  WRK-TENSUZ
               MOVE    WRK-TENSU       TO  HCN58-NYUTENSU (1 10)
           END-IF
      *
      *    ９７食事
           MOVE    RECE200-SHOKUJISYUBETU-KHN
                                       TO  HCN58-SYOKJN-KHN
           MOVE    RECE200-SHOKUJISYUBETU(3)
                                       TO  HCN58-SYOKJN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       4
               IF      RECE200-SHOKUJIKHNNISSU (IDX) >   ZERO
                   MOVE    RECE200-SHOKUJIKHNRYO (IDX)
                                               TO  WRK-KGK4
                   MOVE    WRK-KGK4            TO  HCN58-SYOKHNTEN (IDX)          
                   MOVE    RECE200-SHOKUJIKHNNISSU (IDX)
                                               TO  WRK-KGK2
                   MOVE    WRK-KGK2            TO  HCN58-SYOKAISU  (IDX)
      *
                   IF   RECE200-SRYYM   >=  "201604"
                   AND  IDX              =   1
                     MOVE    RECE200-SHOKUJIKHNRYO-KEI
                                               TO  WRK-TENSUZ
                     MOVE    WRK-TENSU         TO  HCN58-SYOKINGAKU(IDX)
                   ELSE
                     COMPUTE WRK-SYOKUJIKEI =
                             RECE200-SHOKUJIKHNRYO(IDX) *
                             RECE200-SHOKUJIKHNNISSU(IDX)
                     MOVE    WRK-SYOKUJIKEI    TO  WRK-TENSUZ
                     MOVE    WRK-TENSU         TO  HCN58-SYOKINGAKU(IDX)
                   END-IF
                   EVALUATE    IDX
                     WHEN    4
                           MOVE    "円×"      TO  HCN58-SYOKAKERU
                           MOVE    "日間"      TO  HCN58-SYONICHI
                           MOVE    "円"        TO  HCN58-SYOEN
                   END-EVALUATE
               END-IF                                        
           END-PERFORM          
      *
      *    小計
           MOVE    RECE200-SYOUKEI (1) TO  WRK-TENSUZ
           MOVE    WRK-TENSU           TO  HCN58-SYOUKEI (1)
           MOVE    RECE200-SYOUKEI (2) TO  WRK-TENSUZ
           MOVE    WRK-TENSU           TO  HCN58-SYOUKEI (2)
           MOVE    RECE200-SYOUKEI (3) TO  WRK-TENSUZ
           MOVE    WRK-TENSU           TO  HCN58-SYOUKEI (3)
      *    
      *    合計
           MOVE    RECE200-TOTALTEN(1) TO  WRK-TOTALTENZ
           MOVE    WRK-TOTALTENZ       TO  HCN58-GOKEI   (1)
           MOVE    RECE200-TOTALTEN(2) TO  WRK-TOTALTENZ
           MOVE    WRK-TOTALTENZ       TO  HCN58-GOKEI   (2)
           MOVE    RECE200-TOTALTEN(3) TO  WRK-TOTALTENZ
           MOVE    WRK-TOTALTENZ       TO  HCN58-GOKEI   (3)
           MOVE    RECE200-TOTALTEN(4) TO  WRK-TOTALTENZ
           MOVE    WRK-TOTALTENZ       TO  HCN58-GOKEI   (4)
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    適用欄処理
      *****************************************************************
       340-HC101-ZAI-HEN-SEC      SECTION.
      *
      *    摘要欄に病名が表示されている場合、１行あける
           IF  WRK-LINETEK             >   ZERO
      *
               IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               MOVE    1           TO  FLG-KUGIRI
               PERFORM 34110-KUGIRI-LINE-SEC
               MOVE    ZERO        TO  FLG-KUGIRI   
           END-IF
      *         
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  CNT-RECE240
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE240-REC
           MOVE    RECE200-KEY         TO  RECE240-KEY1
      *
           START   RECE240-FILE
               KEY IS    >=   RECE240-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE240
               NOT INVALID
                   PERFORM 900-RECE240-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE240           =   1            )
               OR     ( RECE200-KEY       NOT =   RECE240-KEY1 )
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE240-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE240-HENSYU-SEC
      *
               MOVE    RECE240-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE240-SRYKBN        TO  WRK-R061-SRYKBN-O
      *
               PERFORM 900-RECE240-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE240-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                  TO   FLG-KUGIRI
      *
           IF      CNT-ZAIMEISAISU       =   1
               IF      CNT-RECE240           >   1
      *                診療区分が変更している場合診療区切り線分を追加
                   IF      RECE240-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       MOVE    1                 TO  FLG-KUGIRI
                   END-IF
               END-IF
           END-IF    
      *
           IF      WRK-LINETEK          >=   WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
      *
      *        区切り線（改頁後には診療区切り線は表示しない）
      *****    IF      FLG-KUGIRI        =   2
      *****        PERFORM 34110-KUGIRI-LINE-SEC
      *****     END-IF
           ELSE
      *        区切り線
               IF      FLG-KUGIRI    NOT =   ZERO
                   PERFORM 34110-KUGIRI-LINE-SEC
               END-IF
           END-IF    
      *
      *    診療区分
           IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
           END-IF
      *              
           ADD     1                 TO  WRK-LINETEK
      *
           MOVE    ZERO                  TO  FLG-SRYKBNPRT
           IF      CNT-PAGE          >   1 
               IF      WRK-LINETEK     =   56
                   MOVE    1               TO  FLG-SRYKBNPRT
               END-IF
           END-IF
      *
           IF    ( FLG-KUGIRI        =   1 )
           OR    ( WRK-LINETEK       =   1 )
           OR    ( CNT-RECE240       =   1 )
           OR    ( FLG-SRYKBNPRT     =   1 ) 
               IF      CNT-PAGE        =   1   
                   MOVE    RECE240-SRYKBN  TO  HCN58-SRYKBN
                                                       (WRK-LINETEK)
               ELSE                                    
                   MOVE    RECE240-SRYKBN  TO  HCM59-SRYKBN
                                                       (WRK-LINETEK)
               END-IF
           END-IF                                    
      *
      *    明細
           INSPECT RECE240-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1   
               MOVE    RECE240-TEKIYO-INF  TO  HCN58-TEKIYO
                                                       (WRK-LINETEK)
           ELSE    
               MOVE    RECE240-TEKIYO-INF  TO  HCM59-TEKIYO
                                                       (WRK-LINETEK)
           END-IF  
      *
           .
       3410-RECE240-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34110-KUGIRI-LINE-SEC       SECTION.
      *
           ADD     1               TO  WRK-LINETEK
      *
           EVALUATE    FLG-KUGIRI
               WHEN    1
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-KUGIRI1 TO  HCN58-TEKIYO
                                                       (WRK-LINETEK)
                   ELSE    
                       MOVE    WRK-KUGIRI1 TO  HCM59-TEKIYO
                                                       (WRK-LINETEK)
                   END-IF    
           END-EVALUATE
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
           PERFORM 390-PRINT-OUT-SEC
      *     
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HCN061-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
      *    続紙の最大行をセット     
           MOVE    WRK-TEKIYOU2-LINE TO  WRK-TEKIYOU-LINE
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       360-HC101-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *     
      *    適用欄コメント     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key7"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（０１〜０６）
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key8"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（その他）    
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key9"              TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *    症状詳記コメント（５０、５１、５２）     
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key12"             TO  WRK-MCP-PATHNAME
           PERFORM 3601-RECECOM-HENSYU-SEC
           .
       360-HC101-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセコメント読み込み処理
      *****************************************************************
       3601-RECECOM-HENSYU-SEC    SECTION.
      *
           INITIALIZE                      RECECOM-REC
           MOVE    RECE200-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE200-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE200-PTID         TO  RECECOM-PTID
           MOVE    RECE200-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
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
               IF      RECECOM-HKNCOMBI    =   RECE200-HKNCOMBI
                   PERFORM 3601-COMMENT-HENSYU-SEC                 
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
           .
       3601-RECECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3601-COMMENT-HENSYU-SEC    SECTION.
      *
           MOVE    RECECOM-COMMENT TO  WRK-COMM
           MOVE    ZERO            TO  WRK-RECEHEN                      
           MOVE    ZERO            TO  FLG-RECECOM-SJKBN
      *
      *    症状詳記区分の見出しを記載
           IF      RECECOM-SJKBN   =   "07"    OR  "08"    OR  "09"
               MOVE    "07"            TO  RECECOM-SJKBN
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
                       IF      RECE200-SRYYM  <   201412
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
                   IF      WRK-COMMENT-G   >   SPACE
      *                摘要欄に１行あける
                     IF      WRK-RECECOM         =   ZERO
                       MOVE    1               TO  WRK-RECECOM
                       IF  WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           ELSE
                               MOVE    1           TO  FLG-KUGIRI
                               PERFORM 34110-KUGIRI-LINE-SEC
                               MOVE    ZERO        TO  FLG-KUGIRI   
                           END-IF
                       END-IF
                     END-IF
      *
      *              症状詳記区分の見出しを記載
                     IF      FLG-RECECOM-SJKBN   =   1
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-RECECOM-NAME
                                       TO  HCN58-TEKIYO (WRK-LINETEK)
                       ELSE                
                            MOVE    WRK-RECECOM-NAME
                                       TO  HCM59-TEKIYO(WRK-LINETEK)
                       END-IF
                       MOVE    ZERO            TO  FLG-RECECOM-SJKBN
                     END-IF            
      *                 
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       WRK-CONS-RECECOM-MAX
                           OR      WRK-COMMENT (IDW)   =   SPACE
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       IF      CNT-PAGE        =   1   
                           MOVE    WRK-COMMENT (IDW)
                                       TO  HCN58-TEKIYO (WRK-LINETEK)
                       ELSE                
                           MOVE    WRK-COMMENT (IDW)
                                       TO  HCM59-TEKIYO (WRK-LINETEK)
                       END-IF                
                     END-PERFORM
                 END-IF
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       3601-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<入退院履歴情報・入院基本料種別の続き>処理
      *****************************************************************
       380-HC101-NYUTAI-RIREKI-HEN-SEC      SECTION.
      *
      *    入退院履歴情報
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
           IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
      *
           INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                       BY  "　"
           IF      CNT-PAGE        =   1
               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HCN58-TEKIYO(WRK-LINETEK)
           ELSE
               MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HCM59-TEKIYO(WRK-LINETEK)
           END-IF
           MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
      *
           INITIALIZE                      RECE250-REC
           MOVE    RECE200-KEY         TO  RECE250-KEY1
      *
           START   RECE250-FILE
               KEY IS    >=   RECE250-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE250
               NOT INVALID
                   PERFORM 900-RECE250-NEXT-SEC
           END-START
      *
           PERFORM UNTIL ( FLG-RECE250           =  1            )
                   OR    ( RECE200-KEY       NOT =  RECE250-KEY1 )
      *
               IF      WRK-NYUINCHUKBN     =   ZERO
                   MOVE    RECE250-NYUINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-NYUINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-NYUINYMD (3:)
                   MOVE    "〜"                TO  WRK-NYUTAI-FL1
               END-IF
      *                          
               IF      RECE250-TAIINYMD (1:6)   
                                           >   RECE200-SRYYM
                   MOVE    SPACE               TO  WRK-NYUTAI-TAIINYMD
                   CONTINUE
               ELSE
                   MOVE    RECE250-TAIINYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)   TO
                                               WRK-NYUTAI-TAIINYMD (1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)    TO  
                                               WRK-NYUTAI-TAIINYMD (3:)
               END-IF
      *
               IF      RECE250-NYUINCHUKBN  =   ZERO
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
      *
                   INSPECT WRK-NYUTAI-RIREKI-AREA  REPLACING   ALL "  "
                                                               BY  "　"  
                   IF      CNT-PAGE        =   1   
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HCN58-TEKIYO(WRK-LINETEK)
                   ELSE                    
                       MOVE    WRK-NYUTAI-RIREKI-AREA
                                           TO  HCM59-TEKIYO(WRK-LINETEK)
                   END-IF                    
                   MOVE    SPACE           TO  WRK-NYUTAI-RIREKI-AREA
               END-IF
      *
               MOVE    RECE250-NYUINCHUKBN  TO  WRK-NYUINCHUKBN    
      *
               PERFORM 900-RECE250-NEXT-SEC
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
                                       TO  HCN58-TEKIYO (WRK-LINETEK)
               ELSE                    
                   MOVE    WRK-NYUTAI-RIREKI-AREA
                                       TO  HCM59-TEKIYO (WRK-LINETEK)
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
      *    入院基本料種別の続き
           IF      RECE200-NYUSYUBETU (6)  NOT =   SPACE
               PERFORM VARYING IDXX    FROM    6   BY  1
                       UNTIL   IDXX    >       20
                       OR      RECE200-NYUSYUBETU (IDXX)    =   SPACE
                   IF  WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   END-IF    
                   ADD     1                   TO  WRK-LINETEK
                   IF      CNT-PAGE        =   1   
                       MOVE    RECE200-NYUSYUBETU (IDXX)
                                       TO  HCN58-TEKIYO (WRK-LINETEK)
                   ELSE                    
                       MOVE    RECE200-NYUSYUBETU (IDXX)
                                       TO  HCM59-TEKIYO(WRK-LINETEK)
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
       381-HC101-OPECOM-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-OPECOM
           MOVE    SPACE               TO  WRK-OPECOM-HEN
      *
      *    手術歴編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE200-HOSPNUM     TO  OPECOM-HOSPNUM
           MOVE    RECE200-PTID        TO  OPECOM-PTID
           MOVE    RECE200-SRYYM       TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE200-NYUGAIKBN   TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key4"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE200-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE200-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-SRYYMD       TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3    TO  WRK-OPECOM-HEN
                   PERFORM 3811-OPECOM-HENSYU-SEC                 
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE      TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME   TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE      TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME   TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
      *    コメント編集
           INITIALIZE                      OPECOM-REC
           MOVE    RECE200-HOSPNUM     TO  OPECOM-HOSPNUM
           MOVE    RECE200-PTID        TO  OPECOM-PTID
           MOVE    RECE200-SRYYM       TO  OPECOM-TEKSTYM
                                           OPECOM-TEKEDYM
           MOVE    RECE200-NYUGAIKBN   TO  OPECOM-NYUGAIKBN
           MOVE    "tbl_opecom"        TO  WRK-OPECOM-MCP-TABLE
           MOVE    "key5"              TO  WRK-OPECOM-MCP-PATHNAME
           PERFORM 900-OPECOM-READ-SEC
      *
           PERFORM             UNTIL   FLG-OPECOM   =   1
               MOVE    ZERO                TO  FLG-OPECOM-CHK
               IF      OPECOM-HKNNUM   NOT =   SPACE
                   IF    ( OPECOM-HKNNUM       =   RECE200-HKNNUM )
                   AND   ( OPECOM-HKNID        =   RECE200-HKNID  )
                       MOVE    1                   TO  FLG-OPECOM-CHK
                   END-IF    
               ELSE
                   MOVE    1                   TO  FLG-OPECOM-CHK
               END-IF
               IF      FLG-OPECOM-CHK          =   1
                   MOVE    OPECOM-OPENAME      TO  WRK-OPECOM-HEN
                   PERFORM 3811-OPECOM-HENSYU-SEC
                   IF      OPECOM-OPECOMMENT   NOT =   SPACE                
                       MOVE    OPECOM-OPECOMMENT   TO  WRK-OPECOM-HEN
                       PERFORM 3811-OPECOM-HENSYU-SEC
                   END-IF                 
               END-IF               
      *                 
               MOVE    WRK-OPECOM-MCP-TABLE      TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME   TO  MCP-PATHNAME
               PERFORM 900-OPECOM-READ-N-SEC
           END-PERFORM
      *
           MOVE    WRK-OPECOM-MCP-TABLE     TO   MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME  TO   MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
      *
           .
       381-HC101-OPECOM-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3811-OPECOM-HENSYU-SEC    SECTION.
      *
           IF      WRK-OPECOM          =   ZERO
      *        傷病名との間に区切り線を出力
               IF      WRK-LINETEK         >   ZERO
                   IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       MOVE    1               TO  FLG-KUGIRI
                       PERFORM 34110-KUGIRI-LINE-SEC
                       MOVE    ZERO            TO  FLG-KUGIRI
                   END-IF
               END-IF
           ELSE
      *        手術歴とコメントの間に改行
               IF    ( WRK-OPECOM-MCP-PATHNAME =   "key5" )
               AND   ( WRK-OPECOM              =   1      )
                   IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                       PERFORM 34120-PAGE-SEC
                   ELSE
                       ADD     1                   TO  WRK-LINETEK
                   END-IF
               END-IF
           END-IF
      *
           IF      WRK-OPECOM-MCP-PATHNAME     =   "key4"
               MOVE    1                   TO  WRK-OPECOM
           ELSE
               MOVE    2                   TO  WRK-OPECOM
           END-IF                 
      *
           IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
               PERFORM 34120-PAGE-SEC
           END-IF
           ADD     1                   TO  WRK-LINETEK
           IF      CNT-PAGE            =   1   
               MOVE    WRK-OPECOM-HEN      TO  HCN58-TEKIYO(WRK-LINETEK)
           ELSE                
               MOVE    WRK-OPECOM-HEN      TO  HCM59-TEKIYO(WRK-LINETEK)
           END-IF                
      *
           MOVE    SPACE               TO  WRK-OPECOM-HEN
           .
       3811-OPECOM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       390-PRINT-OUT-SEC               SECTION.
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
      *********MOVE    "HCN58V01.red"      TO
      *********MOVE    "HCN58V02.red"      TO
               MOVE    "HCN58V03.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCN58               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
      *********MOVE    "HCN59V01.red"      TO
               MOVE    "HCM59V02.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               MOVE    HCM59               TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           END-IF        
      *
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票更新処理
      *****************************************************************
       391-DB-WRITE-SEC                SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           PERFORM VARYING IDXY    FROM    1   BY  1
                   UNTIL   IDXY   >       TBL-RECE-MAX
                   OR      FLG-ERR >       ZERO
      *
               ADD     1                    TO  WRK-RECENUM
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
       391-DB-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
           MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC25"              TO  RECE08-PRTID
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
           MOVE    RECE200-TEKSTYMD    TO  RECE08-TEKSTYMD
           MOVE    "5"                 TO  RECE08-TEISYUTUSAKI
           IF      RECE200-PTNUM       =   SPACE
               MOVE    ZERO                TO  RECE08-RECESYUBETU
           ELSE      
               MOVE    RECE200-RECESYUBETU TO  RECE08-RECESYUBETU
           END-IF    
           MOVE    RECE200-PTNUM       TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE200-SRYYM       TO  RECE08-SRYYM
           MOVE    TBL-RECE-RENNUM (IDXY)
                                       TO  RECE08-RENNUM
           IF      WRK-PARA-SYOKBN     =   "1"
               MOVE    WRK-RECENUM         TO  RECE08-RECENUM
           ELSE
               IF      BTPARA-INFO-KBN     =   "01"    OR  "04"
                                                       OR  "05"
                   COMPUTE RECE08-RECENUM  =
                           RECE200-PRTSEQ   *   100 +   
                                               TBL-RECE-RENNUM (IDXY)
               ELSE
                   MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               END-IF
           END-IF
      *
           MOVE    "00"                TO  RECE08-SRYKA
           MOVE    "99999999"          TO  RECE08-HKNJANUM-S
           MOVE    "999"               TO  RECE08-RECESYUBETU-SRT
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
                                           RECE08-KOUGAKU
      *
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR1270 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE200-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           ELSE    
               MOVE    RECE200-KANANAME     TO  RECE08-KANANAME
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
      *****************WHEN    "HCN58V01.red"
      *****************WHEN    "HCN58V02.red"
                       WHEN    "HCN58V03.red"
                           MOVE    RECE08-PRTDATA  TO HCN58
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HCN58-PAGE
                           MOVE    HCN58            TO RECE08-PRTDATA
      *****************WHEN    "HCN59V01.red"
                       WHEN    "HCM59V02.red"                  
                           MOVE    RECE08-PRTDATA  TO HCM59
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HCM59-PAGE
                           MOVE    HCM59           TO RECE08-PRTDATA
                   END-EVALUATE
               END-IF
      *
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    SPA-HOSPNUM         TO  RECE08-HOSPNUM
               MOVE    RECE08-REC          TO  MCPDATA-REC
               PERFORM   900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT
               ELSE 
                   DISPLAY "ORCR1270 RECE08=" MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE200-PTNUM           DELIMITED  BY  SIZE  
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
      *    出力処理
      *****************************************************************
       3903-RECE07-WRITE-SEC        SECTION.
      *
           INITIALIZE                      RECE07-REC
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
           MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
           MOVE    RECE07-REC          TO  MCPDATA-REC 
           MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
           MOVE    "key4"              TO  WRK-MCP-PATHNAME
           PERFORM 900-RECE07-READ-SEC
           IF      FLG-RECE07          =   1
               INITIALIZE                      RECE07-REC
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
                                           =   1 )
                   ADD     1                   TO  RECE07-KENSU
               END-IF
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE07          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC 
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF
           PERFORM   900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR1270 RECEKANRI MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
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
       3904-RECE10-HENSYU-SEC        SECTION.
      * 
      *    テスト患者、レセプト種別不明のとき更新しない
           IF    ( RECE200-TSTPTNUMKBN =   "1"  )
           OR    ( RECE200-RECESYUBETU =   ZERO )
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                      RECE10-REC
           MOVE    RECE200-HOSPNUM     TO  RECE10-HOSPNUM
           MOVE    RECE200-SRYYM       TO  RECE10-SRYYM
           MOVE    RECE200-NYUGAIKBN   TO  RECE10-NYUGAIKBN
           MOVE    RECE200-PTID        TO  RECE10-PTID
           IF      RECE200-RECEKA      =   SPACE
               MOVE    ZERO            TO  RECE10-RECEKA
           ELSE
               MOVE    RECE200-RECEKA  TO  RECE10-RECEKA
           END-IF
           MOVE    5                   TO  RECE10-TEISYUTUSAKI
           MOVE    RECE200-RECESYUBETU TO  RECE10-RECESYUBETU
           MOVE    ZERO                TO  RECE10-HKNJANUM
           MOVE    ZERO                TO  RECE10-HOJOKBN-KEY
           MOVE    ZERO                TO  RECE10-KOHID-KEY
           IF      RECE200-TEKSTYMD    =   SPACE
               MOVE    ZERO                TO  RECE10-TEKSTYMD
           ELSE
               MOVE    RECE200-TEKSTYMD    TO  RECE10-TEKSTYMD
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
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
               MOVE    RECE200-PTNUM       TO  RECE10-PTNUM
               MOVE    RECE200-HKNNUM      TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE200-HKNID       TO  RECE10-HKNID
               MOVE    ZERO                TO  RECE10-SKYYM
                                               RECE10-HENREIYM
                                               RECE10-TEISEIKBN
               MOVE    ZERO                TO  RECE10-HKNJANUM-S
               MOVE    WRK-PARA-CREYMD     TO  RECE10-CREYMD
               MOVE    WRK-PARA-CREHMS (1:6)
                                           TO  RECE10-CREHMS
               MOVE    ZERO                TO  RECE10-UPDYMD
                                               RECE10-UPDHMS
           END-IF
      *
           MOVE    RECE200-KIGO        TO  RECE10-KIGO
           MOVE    RECE200-NUM         TO  RECE10-NUM
      *
           MOVE    RECE200-JNISSU      TO  RECE10-JNISSU   (1)
      *    小計（入院）
           COMPUTE RECE10-TOTALTEN (1) =   
                                       RECE200-SYOUKEI (1) +
                                       RECE200-SYOUKEI (2)
      *    小計（食事）
           MOVE    RECE200-SYOUKEI (3) TO
                                       RECE10-YKZFTN (1)
      *    合計
           MOVE    RECE200-TOTALTEN (4)
                                       TO  RECE10-FTNMONEY (1)
      *    １１初診〜８０他
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       21
               MOVE    RECE200-KHNTEN (IDX)  TO
                                       RECE10-KHNTEN (IDX)
               MOVE    RECE200-KAISU  (IDX)  TO
                                       RECE10-KAISU  (IDX)
               MOVE    RECE200-TENSU  (IDX 1)  TO
                                       RECE10-TENSU (IDX 1)
               ADD     RECE200-TENSU  (IDX 2)  TO
                                       RECE10-TENSU (IDX 1)
           END-PERFORM
      *
      *    ９０入院基本料・加算
           MOVE    ZERO              TO  IDY
           PERFORM   VARYING   IDX   FROM    22   BY  1
                     UNTIL     IDX       >   25
               ADD     1                 TO  IDY
               MOVE    RECE200-NYUKHNRYO  (IDY)
                                         TO  RECE10-KHNTEN (IDX)
               MOVE    RECE200-NYUKHNNISSU(IDY)
                                         TO  RECE10-KAISU  (IDX)
               MOVE    RECE200-NYUKHNKEI  (IDY)
                                         TO  RECE10-TENSU  (IDX 1)
           END-PERFORM
           IF        RECE200-NYUKHNRYO (5)    >   ZERO
               MOVE    ZERO              TO  RECE10-KHNTEN (25)
               PERFORM   VARYING   IDY   FROM    5   BY  1
                         UNTIL     IDY       >   20
                         OR        RECE200-NYUKHNRYO (IDY)    =   ZERO
                   ADD     RECE200-NYUKHNNISSU(IDY)
                                         TO  RECE10-KAISU  (25)
                   ADD     RECE200-NYUKHNKEI  (IDY)
                                         TO  RECE10-TENSU  (25 1)
               END-PERFORM    
           END-IF
           MOVE    RECE200-NYUKHNKEI2    TO  RECE10-TENSU  (26 1)
      *
      *    公害入院療養指導
           MOVE    ZERO              TO  IDY
           PERFORM   VARYING   IDX   FROM    27   BY  1
                     UNTIL     IDX       >   29
               ADD     1                 TO  IDY
               MOVE    RECE200-KOGAISIDORYO(IDY)
                                         TO  RECE10-KHNTEN (IDX)
               MOVE    RECE200-KOGAISIDONISSU(IDY)
                                         TO  RECE10-KAISU  (IDX)
               MOVE    RECE200-KOGAISIDOKEI(IDY)
                                         TO  RECE10-TENSU  (IDX 1)
           END-PERFORM
      *
      *    清浄空気室管理料
           MOVE    RECE200-SEIKANNISSU   TO  RECE10-KAISU  (30)
           MOVE    RECE200-SEIKANKEI     TO  RECE10-TENSU  (30 1)
      *
      *    ９２特定入院料・加算
           MOVE    ZERO    TO    RECE10-KAISU(31)
           MOVE    ZERO    TO    RECE10-TENSU(31 1)
           PERFORM   VARYING   IDY   FROM    1   BY  1
                     UNTIL     IDY       >   20
                     OR        RECE200-NYUTOKNISSU(IDY)  =   ZERO
               ADD     RECE200-NYUTOKNISSU(IDY)
                                         TO  RECE10-KAISU  (31)
               ADD     RECE200-NYUTOKKEI  (IDY)
                                         TO  RECE10-TENSU  (31 1)
           END-PERFORM
           ADD     RECE200-NYUTOKKEI2    TO  RECE10-TENSU  (31 1)
      *
           MOVE    RECE200-AGE         TO  RECE10-AGE  
      *    一部負担金・食事療養費標準負担額区分
           MOVE    SPACE               TO  RECE10-SHOKUJIFTNKBN
      *
           MOVE    RECE200-HKNCOMBI    TO  RECE10-HKNCOMBI(1)
      *
           MOVE    RECE200-TOTALTEN(3) TO  RECE10-SHOKUJIRYOYOHI(1)
           MOVE    RECE200-SHOKUJITOTALDAY
                                       TO  RECE10-SHOKUJINISSU(1)
      *
           MOVE    "00000000000000000000000000000000000000000000000000"
                                       TO  RECE10-RESERVE-AREA
      *
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SEIKYU          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF
           PERFORM   900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR1270 SEIKYU MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
               MOVE    "請求管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "請求管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
      *********PERFORM 500-ERR-HENSYU-SEC
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
           STRING  "ORCR1270 "             DELIMITED   BY  SIZE
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
           IF      WRK-PARA-SYOKBN     =   "1"
      *        請求管理削除処理（一括分）
               INITIALIZE                      RECE10-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
               MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
               MOVE    "1"                 TO  RECE10-NYUGAIKBN
               MOVE    5                   TO  RECE10-TEISYUTUSAKI
               MOVE    RECE10-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "del6"              TO  MCP-PATHNAME
               PERFORM 900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                           " MCP-RC=" MCP-RC
                   MOVE    "請求管理削除処理でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF      
      *
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                      RECE07-REC
               MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC25"              TO  RECE07-PRTID
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
                           "レセプト管理削除処理でエラーとなりました"
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
                           "レセプト削除処理でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF
           ELSE   
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
               MOVE    "HC25"              TO  RECE07-PRTID
               MOVE    "1"                 TO  RECE07-NYUGAIKBN
               MOVE    SPA-HOSPNUM         TO  RECE07-HOSPNUM
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               PERFORM   900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "TABLE=" MCP-TABLE "PATHNAME=" MCP-PATHNAME
                   DISPLAY " MCP-RC=" MCP-RC
                   MOVE
                   "レセプト管理削除処理（個別分）でエラーとなりました"
                                           TO    WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   GO  TO  5001-DB-DELETE-EXT
               END-IF
      *
      *        レセプト削除処理（個別分）
               INITIALIZE                  RECE08-REC
               MOVE    "HC25"              TO  RECE08-PRTID
               MOVE    "1"                 TO  RECE08-NYUGAIKBN
               MOVE    SPA-HOSPNUM         TO  RECE08-HOSPNUM
               MOVE    RECE08-REC          TO  MCPDATA-REC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
               MOVE    "DBDELETE"          TO  MCP-FUNC
               PERFORM   900-ORCDBMAIN-SEC
               IF      MCP-RC           NOT =   ZERO
                   DISPLAY "TABLE=" MCP-TABLE "PATHNAME=" MCP-PATHNAME
                   DISPLAY " MCP-RC=" MCP-RC
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
                   MOVE    1                   TO  FLG-KANRIMST
               END-IF
      *
               PERFORM            UNTIL       FLG-KANRIMST    =   1
                   MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *
      *            診療年月の請求管理削除処理
                   INITIALIZE                  RECE10-REC
                   MOVE    SYS-1001-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  RECE10-PTID
                   MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  RECE10-NYUGAIKBN
                   MOVE    5                   TO  RECE10-TEISYUTUSAKI
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
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE200-FILE
           CLOSE                       RECE210-FILE
           CLOSE                       RECE240-FILE
           CLOSE                       RECE250-FILE
      *
      *    ＰＵＳＨ通知処理
           IF    ( WRK-PARA-SHELLID
                                   NOT =   "RECEPTX" )
               PERFORM 3001-PUSH-SEC
           END-IF
      *
           DISPLAY "RECE200  IN   " CNT-RECE200   
           DISPLAY "         PAGE " CNT-OUT
           DISPLAY "*** ORCR1270 END ***"
      *
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
      *        ステップ管理終了処理
               MOVE    "STE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    CNT-OUT         TO  JOB-UPDCNT
               MOVE    CNT-RECE200     TO  JOB-UPDCNT1
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
               MOVE    CNT-RECE200     TO  WRK-CNT
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
      *    基本情報データ読み込み処理
      *****************************************************************
       900-RECE200-READ-SEC           SECTION.
      *
           READ    RECE200-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE200
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   IF      STS-RECE200         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE200PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE200     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO                TO  FLG-RECE200
      *
                   IF    RECE200-TOTALTEN(4)  =   ZERO
                       GO  TO  900-RECE200-READ-SEC
                   END-IF
      *
                   DISPLAY "RECE200  PTNUM=" RECE200-PTNUM     
      *
                   ADD     1                   TO  CNT-RECE200
                   MOVE    RECE200-HOSPNUM     TO  KEY-N-HOSPNUM
                   MOVE    RECE200-SRYYM       TO  KEY-N-SRYYM
                   MOVE    RECE200-NYUGAIKBN   TO  KEY-N-NYUGAIKBN
                   MOVE    RECE200-PTID        TO  KEY-N-PTID
                   MOVE    RECE200-RECEKA      TO  KEY-N-RECEKA
      *
                   IF      WRK-PARA-SRYYM  =   ZERO
      *                医療機関ＩＤ編集
                       MOVE    RECE200-SRYYM   TO  WRK-SYS-SRYYMD(1:6)
                       MOVE    "01"            TO  WRK-SYS-SRYYMD(7:2)
                       PERFORM 900-SYSKANRI-HENSYU-SEC
                     END-IF
           END-READ
      *
           .
       900-RECE200-READ-EXT.
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
               MOVE    2           TO  FLG-STS
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
               MOVE    2                   TO  FLG-STS
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
                  MOVE    SYS-1900-PRTKBN (15) TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
                      MOVE    MCPDATA-REC      TO  SYS-1901-REC
                      MOVE    1                TO  FLG-SYS-1901
                  END-IF
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1902"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (15) TO  SYS-KBNCD
                  MOVE    WRK-SYS-SRYYMD (1:6) TO  SYS-STYUKYMD (1:6)
                  MOVE    "01"                 TO  SYS-STYUKYMD (7:2)
                  MOVE    SYS-STYUKYMD         TO  SYS-EDYUKYMD
                  MOVE    "tbl_syskanri"       TO  WRK-MCP-TABLE
                  MOVE    "key10"              TO  WRK-MCP-PATHNAME
                  PERFORM 960-KANRIMST-READ-SEC
                  IF      FLG-KANRIMST     =   ZERO
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
      *    傷病名情報データ読み込み処理
      ****************************************************************
       900-RECE210-NEXT-SEC           SECTION.
      *
           READ    RECE210-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE210
               NOT AT  END
                   IF      STS-RECE210         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE210PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE210     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE210
                   IF      RECE210-TENKIKBN     =   "8"
                       GO  TO  900-RECE210-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       900-RECE210-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    適用欄明細情報データ読み込み処理
      ****************************************************************
       900-RECE240-NEXT-SEC           SECTION.
      *
           READ    RECE240-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE240
               NOT AT  END
                   IF      STS-RECE240         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE240PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE240     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE240
                   ADD     1         TO  CNT-RECE240
                   IF      RECE240-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE240-SRYKBN
                   END-IF    
                   IF      RECE240-SRYKBN    =   "64"
                       MOVE    "60"          TO  RECE240-SRYKBN
                   END-IF    
                   IF      RECE240-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE240-SRYKBN
                   END-IF
           END-READ
      *
           .
       900-RECE240-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    入退院履歴情報データ読み込み処理
      ****************************************************************
       900-RECE250-NEXT-SEC           SECTION.
      *
           READ    RECE250-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE250
               NOT AT  END
                   IF      STS-RECE250         =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE           TO  WRK-ERR-AREA
                       INITIALIZE                  WRK-ERR-AREA
                       MOVE    2               TO  WRK-ERR-FLG
                       MOVE    RECE250PARA     TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE250     TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE250
           END-READ
      *
           .
       900-RECE250-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
      *
           MOVE   WRK-MCP-TABLE        TO  MCP-TABLE
           MOVE   WRK-MCP-PATHNAME     TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE   WRK-MCP-TABLE        TO  MCP-TABLE
               MOVE   WRK-MCP-PATHNAME     TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           MOVE   WRK-MCP-TABLE        TO  MCP-TABLE
           MOVE   WRK-MCP-PATHNAME     TO  MCP-PATHNAME
           PERFORM   900-CLOSE-SEC
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           PERFORM   900-CLOSE-SEC
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
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-KANRIMST-NEXT-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KANRIMST
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込（主キー）
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    WRK-MCP-PATHNAME    TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプトコメントマスタ　ＲＥＡＤ
      *****************************************************************
       900-RECECOM-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-RECECOM
               MOVE    MCPDATA-REC     TO  RECECOM-REC
           ELSE
               MOVE    1               TO  FLG-RECECOM
           END-IF
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
           MOVE    WRK-OPECOM-MCP-TABLE      TO  MCP-TABLE
           MOVE    WRK-OPECOM-MCP-PATHNAME   TO  MCP-PATHNAME
      *
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-OPECOM-MCP-TABLE      TO  MCP-TABLE
               MOVE    WRK-OPECOM-MCP-PATHNAME   TO  MCP-PATHNAME
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
           PERFORM   900-DBFETCH-SEC
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
           STRING  "ORCR1270 db err PTNUM="
                                       DELIMITED   BY  SIZE
                   RECE200-PTNUM       DELIMITED   BY  SPACE
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
       900-DB-ERR-ABORT-EXT.
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
               STRING  "ORCR1270 select err TABLE="
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
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBOPEN"            TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
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
           PERFORM   900-ORCDBMAIN-SEC
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC
           PERFORM   900-ORCDBMAIN-SEC
      *
           .
       900-DBDISCONNECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
      *
           .
       900-DBCOMMIT-EXT.
           EXIT.
      *      
      *****************************************************************
      *    ＤＢ　ＳＴＡＲＴ処理
      *****************************************************************
       900-DBSTART-SEC                SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBSTART"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
