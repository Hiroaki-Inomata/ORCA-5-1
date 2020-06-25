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
       PROGRAM-ID.                 ORCR0482.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（労災・自賠責）
      *  コンポーネント名  : 月次業務　労災自賠責明細書ファイル作成
      *                      アフターケア・印刷データ編集（労災）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-藤原      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-藤原    02/10/03  入外区分・処理区分の追加
      *  01.00.02    NACL-藤原    02/11/26  日付ファイルのクローズ追加
      *  01.00.03    NACL-藤原    03/01/09  通し番号の追加のため
      *                                     ＭＯＮＰＥに変更
      *  01.00.04    NACL-門脇    03/02/12  医療機関名の長さ変更のため
      *                                     帳票ＩＤ変更
      *  01.00.05    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.06    NACL-藤原    03.03.05  レセプトコメントＤＢに入外区分を追加
      *  01.00.07    NACL-藤原    03/04/17  出力件数をジョブ管理ＤＢへ更新
      *  01.00.08    NACL-門脇    03.06.03  外来診療料対応
      *                                     （平成１５年６月以降）
      *  01.00.09    NACL-門脇    04/01/26  カナ氏名の長さ等修正
      *  01.00.10    NACL-藤原    04/05/13  傷病の経過の編集の修正
      *  01.00.11    NACL-藤原    04/09/13  帳票別指定編集情報からの編集
      *
      *  01.01.01    NACL-藤原    05/01/14  受診日指定対応
      *  01.01.02    NACL-藤原    05/02/10  訂正日指定対応
      *
      *  02.06.01    NACL-藤原    05/07/20  レセコメントのコメントの長さ
      *                                     変更による対応
      *  02.06.02    NACL-藤原    05/09/05  複数枚レセの連番記載
      *
      *  02.07.01    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *  02.07.02    NACL-門脇    06/01/24  DBCLOSECURSOR追加
      *
      *  03.00.01    NACL-藤原    06/06/20  レセコメントを保険組合せ・
      *                                     症状詳記区分順に記載する
      *  03.01.01    NACL-門脇    06/07/13  項目クリア追加
      *  03.01.02    NACL-門脇    06/07/18  不要ロジック削除
      *
      *  03.04.01    NACL-藤原    06/11/01  照会機能連携対応
      *
      *  03.05.01    NACL-門脇    07/05/25  グループ診療対応
      *  03.05.02    NACL-藤原    07/06/01  realtime preview 処理追加
      *  03.05.03    NACL-門脇    07/07/17  個別時のシステム管理編集修正
      *  04.02.01    NACL-門脇    08/03/24  平成２０年４月改正対応
      *                                     （診療区分６４：病理診断対応）
      *
      *  04.03.01    NACL-藤原    08/07/09  仮レセ作成（院外処方含む）対応
      *  04.03.02    NACL-藤原    08/08/07  労災の枠印刷選択機能対応
      *  04.03.03    NACL-藤原    08/08/11  コメント記載箇所選択機能対応
      *  04.03.04    NACL-門脇    09/02/04  請求点が０の場合の対応
      *  04.03.05    NACL-藤原    09/03/10  請求点イが１０万以上のときの
      *                                     編集の変更
      *
      *  04.05.01    NACL-門脇    09/06/10  摘要欄下部へのコメント記載対応
      *  04.05.02    NACL-藤原    10/03/03  平成２２年４月改正対応
      *  04.05.03    NACL-藤原    10/03/29  「改正未対応」の記載をやめる
      *  04.05.04    NACL-藤原    11/08/02  点検レセ印字位置変更対応
      *
      *  04.06.01    NACL-藤原    10/10/04  レセプトコメント文字数増対応
      *  04.06.02    NACL-藤原    13/04/30  「傷病の経過」記載対応
      *  04.06.03    NACL-門脇    13/06/07  合計額ワーク領域のクリア
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/01/24  monpe-0.6.20対応
      *  04.07.03    NACL-藤原    13/05/27  症状詳記の表題を平成２５年７月
      *                                     診療分以降は全て記載
      *  04.07.04    NACL-門脇    14/10/02  摘要欄の最終行まで記載してから
      *                                     改ページするよう修正
      *  04.07.05    NACL-藤原    14/12/03  平成２６年１１月改正対応
      *  04.07.06    NACL-門脇    15/03/09  症状詳記９０の対応
      *
      *  04.08.01    NACL-藤原    12/01/08  包括診療分の点検用レセプト記
      *                                     載対応
      *  04.08.02    NACL-藤原    14/07/07  一時ディレクトリ対応
      *
      *  05.00.01    NACL-藤原    17/07/04  ＨＡＯＲＩ対応
      *  05.00.02    NACL-藤原    19/03/25  元号改正対応
      *                                     Warning 対応
      *  05.00.03    NACL-門脇    20/03/06  摘要欄の診療識別名称修正
      *                                      「手術」→「麻酔」
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
       FD  RECE41-FILE.
       01  RECE41-REC.
           COPY    "CPRCF041.INC".
      *
      *    病名ファイル    
       FD  RECE43-FILE.
       01  RECE43-REC.
           COPY    "CPRCF043.INC".
      *
      *    点数摘要欄ファイル
       FD  RECE47-FILE.
       01  RECE47-REC. 
           COPY    "CPRCF047.INC".
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
                                   BY         //RECE41//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE43//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1//
                                   BY         //RECE47//.
           COPY    "CPRECEDAT1.INC"
                                   REPLACING  //RECEDAT1PARA//
                                   BY         //RECEYMD//.
      *****03  FILLER              PIC X(04)   VALUE   ".dat".
      *    エラーファイル 名称領域 
           COPY    "CPERRFL.INC"   REPLACING  //ERRFLPARA//
                                   BY         //RECEERR//.
      *
      *    アフターケア（３７７０２）     
      *****COPY    "HC18.INC".
      *****COPY    "HCM18.INC"     REPLACING  //HCM18//
      *****                        BY         //HC18//.
      *****COPY    "HCM18V01.INC"  REPLACING  //HCM18V01//
      *****COPY    "HCM18V02.INC"  REPLACING  //HCM18V02//
      *****COPY    "HCM18V03.INC"  REPLACING  //HCM18V03//
      *****COPY    "HCM18V04.INC"  REPLACING  //HCM18V04//
      *****COPY    "HCM18V05.INC"  REPLACING  //HCM18V05//
           COPY    "HCM18V06.INC"  REPLACING  //HCM18V06//
                                   BY         //HC18//.
      *
      *    アフターケア（続紙） 
      *****COPY    "HC20.INC".
      *****COPY    "HCM20.INC"     REPLACING  //HCM20//
      *****                        BY         //HC20//.
      *****COPY    "HCM20V01.INC"  REPLACING  //HCM20V01//
      *****COPY    "HCM20V02.INC"  REPLACING  //HCM20V02//
      *****COPY    "HCM20V03.INC"  REPLACING  //HCM20V03//
      *****COPY    "HCM20V04.INC"  REPLACING  //HCM20V04//
      *****COPY    "HCM20V05.INC"  REPLACING  //HCM20V05//
           COPY    "HCM20V06.INC"  REPLACING  //HCM20V06//
                                   BY         //HC20//.
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE41          PIC X(02).
           03  STS-RECE43          PIC X(02).
           03  STS-RECE47          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECEYMD         PIC X(02).
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
           03  FLG-BTPARA          PIC 9(01).
           03  FLG-RECE10          PIC 9(01).
      *
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SYS-1900        PIC 9(01).
           03  FLG-SYS-1901        PIC 9(01).
           03  FLG-SYS-1902        PIC 9(01).
           03  FLG-RECECOM-SJKBN   PIC 9(01).
           03  FLG-RECECOM-HENSYU  PIC 9(01).
           03  FLG-STS             PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE41          PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE47          PIC 9(06).
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
      *    労災自賠医療機関退避用領域
       01  TBL-SYS4001-T.
           03  TBL-SYS4001-OCC     OCCURS  100.
               05  TBL-SYS4001     PIC  X(610).
      *    帳票別医療機関編集情報指定退避用領域
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
           03  WRK-PARA-RECECOM     PIC  X(01).
           03  WRK-PARA-FRAME       PIC  X(01).
           03  WRK-PARA-PRTKBN      PIC  X(01).
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
           03  WRK-LINE-MAX        PIC 9(03).          
           03  WRK-KEIKA-LINE      PIC 9(03).          
      *
           03  WRK-GOKEI           PIC 9(07).            
      *
           03  WRK-EDIT-AREA.
      *        適用欄編集行（点数）               
               05  WRK-LINETEK                 PIC 9(03).
      *
               05  WRK-SRYKBN                  PIC X(04).         
      *
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
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
           03  WRK-HENGENGO2       PIC X(01).
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
           03  WRK-RECECOM-SJKBN-X PIC X(02).
      *
      *    複数枚レセ用連番
           03  WRK-HEN-PAGE-AREA.
               05  WRK-PAGE1       PIC Z9.
               05  WRK-PAGE-HEN    PIC X(01).
               05  WRK-PAGE2       PIC Z9.
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
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTENZ         PIC ZZZZZZZ.
           03  WRK-KAISUZ          PIC ZZZ.
           03  WRK-TENSUZ          PIC ZZZZZZZ.
           03  WRK-JNISSU          PIC ZZ9.
           03  WRK-TOTALTEN        PIC ZZ,ZZZ.
           03  WRK-TOTALTEN2       PIC ZZZ,ZZZ.
           03  WRK-TOTALTEN3       PIC ZZZZZZ.
           03  WRK-NENREI-X        PIC ZZ9.
           03  WRK-KINGAKUZ        PIC ZZZ,ZZZ.
           03  WRK-KGK9            PIC ZZ,ZZZ,ZZ9.
           03  WRK-BYOSYOSU        PIC ZZZ9.
      *
      *    ワーク日付ファイル
       01  WRK-RECEYMD-REC.
           03  WRK-RECEYMD-CREYMD      PIC X(08).
           03  WRK-RECEYMD-CREHMS      PIC X(06). 
           03  WRK-RECEYMD-RECENUM     PIC 9(06). 
      *
       01  WRK-CONS-AREA.    
      *    改行コード
           03  WRK-CONS-RETURN     PIC X(01)   VALUE   X"0A".
           03  WRK-CONS-RECECOM-MAX
                                   PIC 9(02)   VALUE   48.
           03  WRK-CONS-RECECOM-LENGTH-MAX
                                   PIC 9(04)   VALUE   2400.
      *
       01  WRK-BYOMEI-AREA.     
      *     
      *    適用欄最大行数（点数）
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   30.
      *     
      *    傷病の経過欄最大行数
           03  WRK-KEIKA-HC18-LINE PIC 9(02)   VALUE   3.
      *     
      *    適用欄最大行数（続紙）
           03  WRK-TEKIYOU-HC20-LINE
                                   PIC 9(03)   VALUE   100.
      *     
           COPY    "ORCA-DBMETA".
      *
           COPY    "COMMON-SPA".
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
               "【上記以外の診療報酬明細書】".
           03  FILLER              PIC X(52)   VALUE
               "【医薬品医療機器等法に規定する治験に係る治験概要】".
       01  TBL-RECECOM-AREA-R      REDEFINES   TBL-RECECOM-AREA.
           03  TBL-RECECOM-NAME    PIC X(52)   OCCURS   12.
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
           COPY    "CPSK200501.INC"
                                   REPLACING  //SYS-200501//
                                   BY         //SYS-25//.
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
                                               WRK-PARA-RECECOM
                                               WRK-PARA-FRAME
                                               WRK-PARA-PRTKBN
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           DISPLAY "WRK-PARA-HOSPNUM   =" WRK-PARA-HOSPNUM
           DISPLAY "WRK-PARA-SYOKBN    =" WRK-PARA-SYOKBN
           DISPLAY "WRK-PARA-SRYYM     =" WRK-PARA-SRYYM
           DISPLAY "WRK-PARA-PRTKBN    =" WRK-PARA-PRTKBN
           DISPLAY "WRK-PARA-RECECOM   =" WRK-PARA-RECECOM
           DISPLAY "WRK-PARA-FRAME     =" WRK-PARA-FRAME
      *
      *    ファイルＯＰＥＮ処理
           MOVE    "RECE410"           TO  RECE41PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE41PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE41PARA-HOSPNUM
      *
           MOVE    "RECE430"           TO  RECE43PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE43PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE43PARA-HOSPNUM
      *
           MOVE    "RECE470"           TO  RECE47PARA-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECE47PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECE47PARA-HOSPNUM
      *
           MOVE    "RECEYMD"           TO  RECEYMD-FILE-ID
           MOVE    WRK-PARA-TERMID     TO  RECEYMD-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEYMD-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    RECE41PARA-BASENAME TO  SGETTEMP-BASENAMES  (2)
           MOVE    RECE43PARA-BASENAME TO  SGETTEMP-BASENAMES  (3)
           MOVE    RECE47PARA-BASENAME TO  SGETTEMP-BASENAMES  (4)
           MOVE    RECEYMD-BASENAME    TO  SGETTEMP-BASENAMES  (5)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE41PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE43PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE47PARA
           MOVE    SGETTEMP-FULLNAMES (5)
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
               MOVE    "ORCR0482"      TO  JOB-PGID
               MOVE    "アフターケア・印刷データ編集（労災）"
                                       TO  JOB-SHELLMSG
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF    
      *
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
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
           MOVE    ZERO                TO  WRK-RECENUM
      *
           OPEN    INPUT               RECEYMD-FILE
           IF      STS-RECEYMD         =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECEYMD             TO  WRK-ERR-FILE-NM
               MOVE    STS-RECEYMD         TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
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
               MOVE    200                 TO  WRK-RECENUM
           END-IF
      *
           IF      FLG-STS             =   ZERO
              PERFORM 990-RECE41-READ-SEC
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
                  MOVE    "tbl_syskanri"    TO  MCP-TABLE
                  MOVE    "key8"            TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"  TO  MCP-TABLE
                    MOVE    "key8"          TO  MCP-PATHNAME
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
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
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
                  PERFORM 900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL  (FLG-SYSKANRI  =    1)
                            OR    (SYSKAN-IDX   >=  100)
      *
                    MOVE    MCPDATA-REC   TO  SYS-4001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-4001-REC  TO  TBL-SYS4001(SYSKAN-IDX)
      *
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-KANRIMST-NEXT-SEC
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
              IF      SYS-1900-PRTKBN (2)  =   SPACE
                  CONTINUE
              ELSE
                  INITIALIZE                       SYSKANRI-REC
                  MOVE    "1901"               TO  SYS-KANRICD
                  MOVE    SYS-1900-PRTKBN (2)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (2)  TO  SYS-KBNCD
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
                    IF      SYS-1900-PRTKBN (2)  =   SPACE
                        CONTINUE
                    ELSE
                        ADD     1             TO  SYSKAN-IDX
                        MOVE    SYS-1900-REC  TO
                                              TBL-SYS1900 (SYSKAN-IDX)
                        MOVE    1             TO  FLG-SYS-1900
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
      *    パラメタＤＢ読み込み
           INITIALIZE                      BTPARA-REC
           IF      WRK-PARA-SHELLID    NOT =   "RECEPTX"
               MOVE    WRK-PARA-SHELLID    TO  BTPARA-SHELLID
               MOVE    WRK-PARA-JOBID      TO  BTPARA-JOBID
               MOVE    WRK-PARA-HOSPNUM    TO  BTPARA-HOSPNUM
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
           IF      FLG-SYSKANRI     =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-25-REC
           ELSE    
               INITIALIZE                  SYS-25-REC
           END-IF
           IF      SYS-25-INSPECT-CHARACTER-KBN
                                   =   SPACE
               MOVE    ZERO            TO
                                       SYS-25-INSPECT-CHARACTER-KBN
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
           PERFORM         UNTIL   FLG-RECE41     =   1
                           OR      FLG-STS        =   2 
      *            レセプト種別別件数カウント
               MOVE    ZERO            TO  CNT-KENSU
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE41     =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW    NOT =   KEY-OLD
      *
                   ADD     1                      TO  CNT-KENSU
      *                                      
                   PERFORM 2001-SYORI-SEC
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
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1         TO  CNT-PAGE
           INITIALIZE                  WRK-EDIT-AREA
           INITIALIZE                  TABLE-AREA
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<傷病の経過>処理
           IF      RECE41-SRYYM        <   201307
               IF      WRK-PARA-RECECOM    =   "0"
                   PERFORM 360-HC101-COMMENT-HEN-SEC
               END-IF
           ELSE
               PERFORM 360-COMMENT-HENSYU-SEC
           END-IF 
      *
      *    帳票編集<適用（点数）>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<傷病の経過>処理
           IF      RECE41-SRYYM        <   201307
               IF      WRK-PARA-RECECOM    =   "1"
                   PERFORM 360-HC101-COMMENT-HEN-SEC
               END-IF
           ELSE
               PERFORM 360-HC101-COMMENT-HEN-SEC
           END-IF
      *
      *    帳票印刷処理（続紙）
           IF      CNT-PAGE           >   1
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
      *
      *    帳票印刷処理
           IF      FLG-STS            =   ZERO
               MOVE    1                  TO  CNT-PAGE
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
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
       310-HC101-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-LINE    TO  WRK-LINE-MAX
      *     
           INITIALIZE                      HC18
      *    患者番号
           STRING   RECE41-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE41-RECEKA  DELIMITED   BY  SPACE
                                   INTO    HC18-PTNUM
           END-STRING
      *    健康管理手帳番号
           MOVE    RECE41-KOFUNUM      TO  HC18-KOFUNUM
      *    診察年月日
           MOVE    RECE41-TEKSTYMD     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO2       TO  WRK-HENGENGO
           MOVE    WRK-HENYMDG         TO  HC18-SRYYMD 
      *    検査年月日
           IF      RECE41-KENSAKBN     =   "1"
               MOVE    RECE41-TEKSTYMD     TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENGENGO2       TO  WRK-HENGENGO
               MOVE    WRK-HENYMDG         TO  HC18-KENSAYMD
           END-IF     
      *    合計額
           MOVE    ZERO                TO  WRK-GOKEI
           COMPUTE WRK-GOKEI   =   RECE41-TOTALKIN
                               +   RECE41-TOTALKIN2
           MOVE    WRK-GOKEI           TO  WRK-KHNTENZ
           MOVE    WRK-KHNTENZ         TO  HC18-GOKEI
      *    カナ氏名
           MOVE    RECE41-KANANAME     TO  HC18-KANANAME
      *    氏名
           MOVE    RECE41-NAME         TO  HC18-NAME
      *    病院等の名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC18-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC18-HOSPNAME (31:)
               MOVE    SYS-1901-HOSPNAME3  TO  HC18-HOSPNAME (61:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC18-HOSPNAME
           END-IF
           INSPECT HC18-HOSPNAME   REPLACING   ALL "  "    BY  "　"
      *    傷病コード
           MOVE    RECE41-SONSHOUKBN   TO  HC18-SYOBYO
           MOVE    RECE41-SONSHOUKBN-EDANUM
                                       TO  HC18-SYOBYOEDA
      *    病床数
           MOVE    SYS-1001-BEDSU      TO  WRK-BYOSYOSU
           MOVE    WRK-BYOSYOSU        TO  HC18-BYOSYOSU
      *    前回の検査年月日
           IF      RECE41-KENSAYMD-ZEN =   SPACE
               CONTINUE
           ELSE    
               MOVE    RECE41-KENSAYMD-ZEN TO  WRK-SYMD
               PERFORM 31012-SE-WA-YMDEDIT-SEC
               MOVE    WRK-HENGENGO        TO  HC18-ZENKAIG
               MOVE    WRK-HENYY           TO  HC18-ZENKAIY
               MOVE    WRK-HENMM           TO  HC18-ZENKAIM
               MOVE    WRK-HENDD           TO  HC18-ZENKAID
           END-IF     
      *
      *    点検用レセプト作成時
           IF      WRK-PARA-PRTKBN NOT =   "1"
               IF      SYS-25-INSPECT-CHARACTER-KBN
                                           =   "0"
                   MOVE    "点検用レセプト"    TO  HC18-LABEL
                   MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                               TO  HC18-LOPTION
               END-IF
           ELSE
               MOVE    "MonpeLayerIn -L SEN MonpeLayerOut"
                                           TO  HC18-LOPTION
           END-IF
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<続紙ヘッダー部>処理
      *****************************************************************
       310-HC20-HEAD-HEN-SEC          SECTION.
      *
           MOVE    WRK-TEKIYOU-HC20-LINE    TO  WRK-LINE-MAX
      *
           INITIALIZE                 HC20
      *    患者番号
           STRING   RECE41-PTNUM   DELIMITED   BY  SPACE
                    " "            DELIMITED   BY  SIZE
                    RECE41-RECEKA  DELIMITED   BY  SPACE
                    "-"            DELIMITED   BY  SIZE
                    CNT-PAGE (3:1) DELIMITED   BY  SIZE
                                   INTO    HC20-PTNUM
           END-STRING
      *    指定病院等の番号
           MOVE    SYS-4001-RSIIRYOCDN TO  HC20-RISIRYOCD
      *    病院等の名称
           IF      FLG-SYS-1901        =   1
               MOVE    SYS-1901-HOSPNAME1  TO  HC20-HOSPNAME (1:)
               MOVE    SYS-1901-HOSPNAME2  TO  HC20-HOSPNAME (31:)
               MOVE    SYS-1901-HOSPNAME3  TO  HC20-HOSPNAME (61:)
           ELSE 
               MOVE    SYS-1001-HOSPNAME   TO  HC20-HOSPNAME
           END-IF
           INSPECT HC20-HOSPNAME   REPLACING   ALL "  "    BY  "　"
      *
      *    健康管理手帳番号
           MOVE    RECE41-KOFUNUM      TO  HC20-KOFUNUM
      *    氏名
           MOVE    RECE41-NAME         TO  HC20-NAME
           MOVE    RECE41-KANANAME     TO  HC20-KANANAME
      *    受診年月日
           MOVE    RECE41-TEKSTYMD     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO        TO  HC20-SRYYMDH
           MOVE    WRK-HENYY           TO  HC20-SRYYMDY
           MOVE    WRK-HENMM           TO  HC20-SRYYMDM
           MOVE    WRK-HENDD           TO  HC20-SRYYMDD
      *
      *    点検用レセプト作成時
           MOVE    HC18-LABEL          TO  HC20-LABEL
           MOVE    HC18-LOPTION        TO  HC20-LOPTION
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *     
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
      *    診療内容・点数（点）
      *
      *    初診（時間外）                           
           EVALUATE    RECE41-SHOSHINTIME
               WHEN    "1"
                   MOVE    "○"       TO  HC18-JIKANGAI1
               WHEN    "2"
                   MOVE    "休日"     TO  HC18-JIKANGAI11
               WHEN    "3"
                   MOVE    "深夜"     TO  HC18-JIKANGAI11
           END-EVALUATE    
      *    再診（時間外）
           IF      RECE41-KEIZOKU-PRTKBN     =   "2"
             EVALUATE    RECE41-SAISHINTIME
               WHEN    "1"
                   MOVE    "○"       TO  HC18-JIKANGAI3
               WHEN    "2"
                   MOVE    "休日"     TO  HC18-JIKANGAI31
               WHEN    "3"
                   MOVE    "深夜"     TO  HC18-JIKANGAI31
             END-EVALUATE 
           ELSE
             EVALUATE    RECE41-SAISHINTIME
               WHEN    "1"
                   MOVE    "○"       TO  HC18-JIKANGAI2
               WHEN    "2"
                   MOVE    "休日"     TO  HC18-JIKANGAI21
               WHEN    "3"
                   MOVE    "深夜"     TO  HC18-JIKANGAI21
             END-EVALUATE 
           END-IF   
      *
      *
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   32
      *        <回数・単位>
               EVALUATE    IDX
      *            再診
                   WHEN    3
                   WHEN    5
                   WHEN    6
                   WHEN    14
                   WHEN    17
                   WHEN    19
                   WHEN    20
                   WHEN    22
                   WHEN    23
                   WHEN    25
                   WHEN    26
                   WHEN    28
                   WHEN    29  THRU    32
                       CONTINUE
                   WHEN    OTHER    
                       MOVE    RECE41-KAISU(IDX) TO  WRK-KAISUZ
                       MOVE    WRK-KAISUZ        TO  HC18-KAISU   (IDX)
               END-EVALUATE
      *        <点数・公費点数>
               MOVE    RECE41-TENSU(IDX)         TO  WRK-TENSUZ
               MOVE    WRK-TENSUZ                TO  HC18-KOUTEN  (IDX)
           END-PERFORM
      *
      *    小計イ
           IF      RECE41-TOTALTEN        <   100000
               MOVE    RECE41-TOTALTEN        TO  WRK-TOTALTEN
               MOVE    WRK-TOTALTEN           TO  HC18-TOTALTEN
           ELSE
               MOVE    RECE41-TOTALTEN        TO  WRK-TOTALTEN3
               MOVE    WRK-TOTALTEN3          TO  HC18-TOTALTEN
           END-IF
           IF      RECE41-TOTALKIN            >   ZERO
               MOVE    RECE41-TOTALKIN        TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC18-TOTALMONEY
           END-IF
      *
      *    診療内容・金額                     
      *    <初診>
           MOVE    RECE41-KINGAKU2(1)      TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ            TO  HC18-SYOSHINMONEY
           MOVE    RECE41-SYOSAIYMD2 (1)   TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO            TO  HC18-SYOSHING
           MOVE    WRK-HENYY               TO  HC18-SYOSHINY
           MOVE    WRK-HENMM               TO  HC18-SYOSHINM
           MOVE    WRK-HENDD               TO  HC18-SYOSHIND
      *    <再診>
           MOVE    RECE41-KINGAKU2(2)      TO  WRK-KINGAKUZ
           MOVE    WRK-KINGAKUZ            TO  HC18-SAISHINMONEY 
           MOVE    RECE41-SYOSAIYMD2 (2)   TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           MOVE    WRK-HENGENGO            TO  HC18-SAISHING
           MOVE    WRK-HENYY               TO  HC18-SAISHINY
           MOVE    WRK-HENMM               TO  HC18-SAISHINM
           MOVE    WRK-HENDD               TO  HC18-SAISHIND
      *
      *    <小計ロ>
           IF      RECE41-TOTALKIN2           >   ZERO
               MOVE    RECE41-TOTALKIN2       TO  WRK-TOTALTEN2
               MOVE    WRK-TOTALTEN2          TO  HC18-SYOKEI
           END-IF
      *
           .
       330-HC101-TENSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<剤>処理
      *****************************************************************
       340-HC101-ZAI-HEN-SEC      SECTION.
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
           MOVE    ZERO                TO  CNT-RECE47
           MOVE    ZERO                TO  CNT-ZAIMEISAISU
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE47-REC
           MOVE    RECE41-HOSPNUM      TO  RECE47-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECE47-SRYYM
           MOVE    RECE41-NYUGAIKBN    TO  RECE47-NYUGAIKBN
           MOVE    RECE41-PTID         TO  RECE47-PTID
           MOVE    RECE41-RECEKA       TO  RECE47-RECEKA
           MOVE    RECE41-HKNID        TO  RECE47-HKNID
           MOVE    RECE41-TEKSTYMD     TO  RECE47-TEKSTYMD
      *
           START   RECE47-FILE
               KEY IS    >=   RECE47-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE47
               NOT INVALID
                   PERFORM 990-RECE47-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE47           =   1                  )
               OR     ( RECE41-HOSPNUM   NOT =   RECE47-HOSPNUM     )
               OR     ( RECE41-SRYYM     NOT =   RECE47-SRYYM       )
               OR     ( RECE41-NYUGAIKBN NOT =   RECE47-NYUGAIKBN   )
               OR     ( RECE41-PTID      NOT =   RECE47-PTID        )
               OR     ( RECE41-RECEKA    NOT =   RECE47-RECEKA      )
               OR     ( RECE41-HKNID     NOT =   RECE47-HKNID       )
               OR     ( RECE41-TEKSTYMD  NOT =   RECE47-TEKSTYMD    )
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE47-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE47-HENSYU-SEC
      *
               MOVE    RECE47-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE47-SRYKBN        TO  WRK-R061-SRYKBN-O
      *
               PERFORM 990-RECE47-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE47-HENSYU-SEC           SECTION.
      *
      ***  IF  CNT-ZAIMEISAISU           =   1
      *        次の行為明細数を追加すると何行になるか
      ***      COMPUTE   WRK-WLINETEK    =   WRK-LINETEK
      ***                                +   RECE47-ZAIMEISAISU
      ***      IF  CNT-RECE47           >   1
      *            診療区分が変更している場合、診療区切り線分を追加
      ***          IF  RECE47-SRYKBN    NOT =   WRK-R061-SRYKBN-O
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
      ***          PERFORM 34111-SRYKBN-HENSYU-SEC
      ***      ELSE
      *            区切り線
      ***          IF  FLG-KUGIRI    NOT =   ZERO
      ***              PERFORM 34110-KUGIRI-LINE-SEC
      ***          END-IF
      *            診療区分
      ***          ADD     1             TO  WRK-LINETEK
      ***          IF  RECE47-SRYKBN    NOT =   WRK-R061-SRYKBN-O
      ***              PERFORM 34111-SRYKBN-HENSYU-SEC
      ***          END-IF
      ***      END-IF
      ***  ELSE
      ***      ADD     1                 TO  WRK-LINETEK
      ***  END-IF
      *
           IF  CNT-ZAIMEISAISU           =   1
             IF  CNT-RECE47            >   1
      *          診療区分が変更している場合、診療区切り線分を追加
                 IF  RECE47-SRYKBN    NOT =   WRK-R061-SRYKBN-O
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
             PERFORM 34111-SRYKBN-HENSYU-SEC
           END-IF
      *
      *    明細
           INSPECT RECE47-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           IF      CNT-PAGE        =   1
               MOVE    RECE47-TEKIYO-INF  TO  HC18-TEKIYO(WRK-LINETEK)
           ELSE    
               MOVE    RECE47-TEKIYO-INF  TO  HC20-TEKIYO (WRK-LINETEK)
           END-IF    
      *
           .
       3410-RECE47-HENSYU-EXT.
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
               MOVE    WRK-KUGIRI1 TO  HC18-TEKIYO (WRK-LINETEK)
           ELSE    
               MOVE    WRK-KUGIRI1 TO  HC20-TEKIYO  (WRK-LINETEK)
           END-IF    
      *
           .
       34110-KUGIRI-LINE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    区切り線　編集処理
      *****************************************************************
       34111-SRYKBN-HENSYU-SEC     SECTION.
      *
           EVALUATE    RECE47-SRYKBN
               WHEN    "11"
                   MOVE    "初診"         TO  WRK-SRYKBN
               WHEN    "12"
                   MOVE    "再診"         TO  WRK-SRYKBN
               WHEN    "13"
                   MOVE    "指導"         TO  WRK-SRYKBN
               WHEN    "14"
                   MOVE    "在宅"         TO  WRK-SRYKBN
               WHEN    "21"
                   MOVE    "内服"         TO  WRK-SRYKBN
               WHEN    "22"
                   MOVE    "屯服"         TO  WRK-SRYKBN
               WHEN    "23"
                   MOVE    "外用"         TO  WRK-SRYKBN
               WHEN    "25"
                   MOVE    "処方"         TO  WRK-SRYKBN
               WHEN    "26"
                   MOVE    "麻毒"         TO  WRK-SRYKBN
               WHEN    "27"
                   MOVE    "調基"         TO  WRK-SRYKBN
               WHEN    "31"
                   MOVE    "皮下"         TO  WRK-SRYKBN
               WHEN    "32"
                   MOVE    "静脈"         TO  WRK-SRYKBN
               WHEN    "33"
                   MOVE    "他"           TO  WRK-SRYKBN
               WHEN    "40"
                   MOVE    "処置"         TO  WRK-SRYKBN
               WHEN    "50"
      ***          MOVE    "手術"         TO  WRK-SRYKBN
                   MOVE    "麻酔"         TO  WRK-SRYKBN
               WHEN    "60"
                   MOVE    "検査"         TO  WRK-SRYKBN
               WHEN    "70"
                   MOVE    "画像"         TO  WRK-SRYKBN
               WHEN    "80"
                   MOVE    "他"           TO  WRK-SRYKBN
               WHEN    OTHER
                   MOVE    SPACE          TO  WRK-SRYKBN
           END-EVALUATE
      *                  
           IF      CNT-PAGE        =   1
               MOVE    WRK-SRYKBN      TO  HC18-SRYKBN (WRK-LINETEK)
           ELSE
               MOVE    WRK-SRYKBN      TO  HC20-SRYKBN (WRK-LINETEK)
           END-IF                                    
      *
           .
       34111-SRYKBN-HENSYU-EXT.
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
               PERFORM 390-PRINT-OUT-SEC
           END-IF    
      * 
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HC20-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病の経過>処理
      *****************************************************************
       360-COMMENT-HENSYU-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
                                           WRK-KEIKA-LINE
      *     
      *    傷病の経過
           MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
           MOVE    "key17"             TO  WRK-MCP-PATHNAME
           MOVE    "AA"                TO  WRK-RECECOM-SJKBN-X
           PERFORM 3601-RECECOM-HENSYU-SEC
      *     
           .
       360-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       360-HC101-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
                                           WRK-KEIKA-LINE
           MOVE    ZERO                TO  FLG-RECECOM-SJKBN
           MOVE    SPACE               TO  WRK-RECECOM-NAME 
                                           WRK-RECECOM-SJKBN
      *
      *    適用欄に記載するときの設定
           IF    ( WRK-PARA-RECECOM    =   "1"    )
           OR    ( RECE41-SRYYM        >=  201307 )
               MOVE    WRK-KEIKA-HC18-LINE     TO  WRK-KEIKA-LINE
           END-IF
      *     
           IF      RECE41-SRYYM    <   201307
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key11"             TO  WRK-MCP-PATHNAME
               PERFORM 3601-RECECOM-HENSYU-SEC
           ELSE
      *    適用欄コメント     
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key17"             TO  WRK-MCP-PATHNAME
               MOVE    "99"                TO  WRK-RECECOM-SJKBN-X
               PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *        症状詳記コメント（０１〜０６）
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key18"             TO  WRK-MCP-PATHNAME
               PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *        症状詳記コメント（その他）    
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key19"             TO  WRK-MCP-PATHNAME
               PERFORM 3601-RECECOM-HENSYU-SEC
      *     
      *        症状詳記コメント（５０、５１、５２、９０）
               MOVE    "tbl_rececom"       TO  WRK-MCP-TABLE
               MOVE    "key24"             TO  WRK-MCP-PATHNAME
               PERFORM 3601-RECECOM-HENSYU-SEC
           END-IF
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
           MOVE    RECE41-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE41-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE41-PTID         TO  RECECOM-PTID
           MOVE    RECE41-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
           MOVE    RECE41-TEKSTYMD (7:2)
                                       TO  RECECOM-SRYDD
           IF      WRK-MCP-PATHNAME    =   "key17"
               MOVE    WRK-RECECOM-SJKBN-X TO  RECECOM-SJKBN
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
               IF    ( WRK-MCP-PATHNAME    =   "key11" )
               AND   ( RECECOM-SJKBN       =   "AA"    )
                   MOVE    1               TO  FLG-RECECOM
               ELSE
                   IF      RECECOM-HKNCOMBI    =   RECE41-HKNCOMBI
                       PERFORM 3601-COMMENT-HENSYU-SEC                 
                   END-IF
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
      *   （平成２５年７月以降は全て記載）
           IF      RECE41-SRYYM    <   201307
               MOVE    SPACE           TO  RECECOM-SJKBN
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
                       IF      RECE41-SRYYM    <   201412
                           MOVE    TBL-RECECOM-NAME (8)
                                                   TO  WRK-RECECOM-NAME
                       ELSE
                           MOVE    TBL-RECECOM-NAME (12)
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
                   WHEN    "90"
                       MOVE    1               TO  FLG-RECECOM-SJKBN
                       MOVE    TBL-RECECOM-NAME (11)
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
                       IF      WRK-KEIKA-LINE  <   WRK-KEIKA-HC18-LINE
      *                  傷病の経過欄へ症状詳記区分の見出しを記載
                         IF      FLG-RECECOM-SJKBN   =   1
                             ADD     1           TO  WRK-KEIKA-LINE       
                             MOVE    WRK-RECECOM-NAME
                                       TO  HC18-KEIKA (WRK-KEIKA-LINE)
                             MOVE    ZERO        TO  FLG-RECECOM-SJKBN
                         END-IF            
                         IF      WRK-KEIKA-LINE
                                             <   WRK-KEIKA-HC18-LINE
      *                    傷病の経過欄へ編集
                           ADD     1           TO  WRK-KEIKA-LINE       
                           MOVE    WRK-COMMENT (IDX)
                                               TO
                                           HC18-KEIKA (WRK-KEIKA-LINE)
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
                               IF      WRK-LINETEK      >=  WRK-LINE-MAX
                                   PERFORM 34120-PAGE-SEC
                               END-IF
                               ADD     1               TO  WRK-LINETEK
                               IF      CNT-PAGE        =   1   
                                   MOVE    WRK-RECECOM-NAME
                                           TO  HC18-TEKIYO (WRK-LINETEK)
                               ELSE                
                                   MOVE    WRK-RECECOM-NAME
                                           TO  HC20-TEKIYO (WRK-LINETEK)
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
                                           TO  HC18-TEKIYO (WRK-LINETEK)
                           ELSE
                               MOVE    WRK-COMMENT (IDX)
                                           TO  HC20-TEKIYO (WRK-LINETEK)
                           END-IF
                       END-IF                    
                   END-PERFORM
               END-IF  
               MOVE    WRK-COMM1       TO  WRK-COMM
           END-PERFORM   
      *
           .
       3601-COMMENT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票格納処理
      *****************************************************************
       390-PRINT-OUT-SEC               SECTION.
      *
      *    レセプト種別不明のとき続紙は出力しない
           IF    ( RECE41-RECESYUBETU   =   ZERO )
           AND   ( CNT-PAGE             >   1    )
               GO  TO  390-PRINT-OUT-EXT
           END-IF
      *
           ADD     1                   TO  TBL-RECE-MAX
      * 
           MOVE    CNT-PAGE            TO  TBL-RECE-RENNUM(TBL-RECE-MAX)
           IF      CNT-PAGE            =   1
               EVALUATE  WRK-PARA-FRAME
                   WHEN      "0"
                       MOVE    "HCM18V06.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN      "1"
                       MOVE    "HCM18V06P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               END-EVALUATE
               MOVE    HC18                TO
                                       TBL-RECE-PRTDATA (TBL-RECE-MAX)
           ELSE
               EVALUATE  WRK-PARA-FRAME
                   WHEN      "0"
                       MOVE    "HCM20V06.red"      TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
                   WHEN      "1"
                       MOVE    "HCM20V06P.red"     TO
                                       TBL-RECE-FORMID  (TBL-RECE-MAX)
               END-EVALUATE
               MOVE    HC20                TO
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
           MOVE    ZERO                TO  FLG-ERR
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
       391-DB-WRITE-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    レセプト明細管理ＤＢ
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
           MOVE    WRK-RECEYMD-CREYMD  TO  RECE08-CREYMD
           MOVE    WRK-RECEYMD-CREHMS  TO  RECE08-CREHMS
           MOVE    "0"                 TO  RECE08-PREFKBN
           MOVE    "00"                TO  RECE08-PREFNUM
           MOVE    RECE41-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    "3"                 TO  RECE08-TEISYUTUSAKI
      *****IF      RECE41-PTNUM        =   SPACE
      *         MOVE    ZERO                TO  RECE08-RECESYUBETU
      *     ELSE      
               IF      TBL-RECE-RENNUM (IDXY)  =   1                                 
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
               ELSE
                   MOVE    RECE41-RECESYUBETU  TO  RECE08-RECESYUBETU
                   MOVE    "3"                 TO  RECE08-RECESYUBETU
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
               DISPLAY "ORCR0482 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE41-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 900-DB-ERR-ABORT-SEC
           ELSE    
               MOVE    RECE41-KANANAME     TO  RECE08-KANANAME
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
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
                       WHEN    "HCM18V06.red"                  
                       WHEN    "HCM18V06P.red"                  
                           MOVE    RECE08-PRTDATA  TO HC18
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC18-PAGE
                           MOVE    HC18            TO RECE08-PRTDATA
                       WHEN    "HCM20V06.red"                  
                       WHEN    "HCM20V06P.red"                  
                           MOVE    RECE08-PRTDATA  TO HC20
                           MOVE    WRK-HEN-PAGE-AREA
                                                   TO HC20-PAGE
                           MOVE    HC20            TO RECE08-PRTDATA
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
                   DISPLAY "ORCR0482 " MCP-RC " " RECE08-KEY 
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
               DISPLAY "ORCR0482 MCP-RC= "  MCP-RC   
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
           MOVE    3                   TO  RECE10-TEISYUTUSAKI
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
      *****MOVE    ZERO                TO  RECE10-HKNJANUM-S
      *
           MOVE    "3"                 TO  RECE10-HKNKBN
           MOVE    RECE41-KOFUNUM      TO  RECE10-KOFUNUM
           MOVE    RECE41-TEKSTYMD     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           STRING  LNK-DAY2-EDTYMD2-D      DELIMITED   BY  SPACE
                   "日　"                  DELIMITED   BY  SIZE
                   RECE41-COMMENT          DELIMITED   BY  SPACE
                                           INTO        RECE10-COMMENT
           END-STRING
      * 
           IF      RECE41-JNISSU       =   999
               MOVE    ZERO                TO  RECE10-JNISSU       (1)
           ELSE
               MOVE    RECE41-JNISSU       TO  RECE10-JNISSU       (1)
           END-IF
           MOVE    RECE41-TOTALTEN     TO  RECE10-TOTALTEN     (1)
           COMPUTE RECE10-FTNMONEY (1) =   RECE41-TOTALKIN     +
                                           RECE41-TOTALKIN2
           MOVE    RECE41-TOTALKIN     TO  RECE10-FTNMONEY-FTN (1)
           MOVE    RECE41-TOTALKIN2    TO  RECE10-FTNMONEY-KOJ (1)
      *
           MOVE    RECE41-TENSU (1)    TO  RECE10-TENSU (1 1)
           COMPUTE RECE10-TENSU (2 1)  =   RECE41-TENSU (2)    +
                                           RECE41-TENSU (3)    +
                                           RECE41-TENSU (4)
           MOVE    RECE41-TENSU (5)    TO  RECE10-TENSU (3 1)
           MOVE    RECE41-TENSU (6)    TO  RECE10-TENSU (4 1)
           COMPUTE RECE10-TENSU (5 1)  =   RECE41-TENSU (7)    +
                                           RECE41-TENSU (8)    +
                                           RECE41-TENSU (9)    +
                                           RECE41-TENSU (10)   +
                                           RECE41-TENSU (11)   +
                                           RECE41-TENSU (12)   +
                                           RECE41-TENSU (13)   +
                                           RECE41-TENSU (14)
           COMPUTE RECE10-TENSU (6 1)  =   RECE41-TENSU (15)   +
                                           RECE41-TENSU (16)   +
                                           RECE41-TENSU (17)
           COMPUTE RECE10-TENSU (7 1)  =   RECE41-TENSU (18)   +
                                           RECE41-TENSU (19)   +
                                           RECE41-TENSU (20)
           COMPUTE RECE10-TENSU (8 1)  =   RECE41-TENSU (21)   +
                                           RECE41-TENSU (22)   +
                                           RECE41-TENSU (23)
           COMPUTE RECE10-TENSU (9 1)  =   RECE41-TENSU (24)   +
                                           RECE41-TENSU (25)   +
                                           RECE41-TENSU (26)
           COMPUTE RECE10-TENSU (10 1) =   RECE41-TENSU (27)   +
                                           RECE41-TENSU (28)   +
                                           RECE41-TENSU (29)
           COMPUTE RECE10-TENSU (11 1) =   RECE41-TENSU (30)   +
                                           RECE41-TENSU (31)   +
                                           RECE41-TENSU (32)
           MOVE    RECE41-KINGAKU2 (1) TO  RECE10-TENSU (13 1)
           MOVE    RECE41-KINGAKU2 (2) TO  RECE10-TENSU (14 1)
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
               DISPLAY "ORCR0482 SEIKYU MCP-RC= "  MCP-RC
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
      *    西暦→和暦日付編集
      *****************************************************************
       31012-SE-WA-YMDEDIT-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-HENYMDG
                                           WRK-HENGENGO2
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
               EVALUATE    LNK-DAY2-EDTYMD1-G
                   WHEN    "M"
                       MOVE    "1"             TO  WRK-HENGENGO2
                   WHEN    "T"
                       MOVE    "3"             TO  WRK-HENGENGO2
                   WHEN    "S"
                       MOVE    "5"             TO  WRK-HENGENGO2
                   WHEN    "H"
                       MOVE    "7"             TO  WRK-HENGENGO2
                   WHEN    "R"
                       MOVE    "9"             TO  WRK-HENGENGO2
               END-EVALUATE
           ELSE
               MOVE    SPACE               TO  WRK-HENYMDG
                                               WRK-HENGENGO2
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
           STRING  "ORCR0482 "             DELIMITED   BY  SIZE
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
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           OPEN    OUTPUT              RECEYMD-FILE 
           MOVE    WRK-RECENUM         TO  WRK-RECEYMD-RECENUM
           MOVE    WRK-RECEYMD-REC     TO  RECEYMD-REC
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
           CLOSE   RECEYMD-FILE
      *          
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE41-FILE
           CLOSE                       RECE43-FILE
           CLOSE                       RECE47-FILE
      *
      *
      *****IF      CNT-OUT            =   ZERO 
      *****    MOVE    "対象データがありませんでした" TO  WRK-RECEERR
      *****    PERFORM 500-ERR-HENSYU-SEC
      *****END-IF
           DISPLAY "*** ORCR0482 IN   "  CNT-RECE41
           DISPLAY "*** ORCR0482 PAGE "  CNT-OUT
           DISPLAY "*** ORCR0482 END ***"
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
                   IF      RECE41-HKNKBN           =   "3"
                   AND     RECE41-RECESYUBETU  NOT =   ZERO
      *
                       IF    ( RECE41-TOTALKIN     =   ZERO )
                       AND   ( RECE41-TOTALKIN2    =   ZERO )
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
      *
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
                             RECE41-SRYYM       DELIMITED  BY  SIZE
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
                  MOVE    SYS-1900-PRTKBN (2)  TO  SYS-KBNCD
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
                  MOVE    SYS-1900-PRTKBN (2)  TO  SYS-KBNCD
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
                   IF      RECE47-SRYKBN    =   "99"
                       MOVE    SPACE         TO  RECE47-SRYKBN
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
           STRING  "ORCR0482 db err PTNUM="
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
               STRING  "ORCR0482 select err TABLE="
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
