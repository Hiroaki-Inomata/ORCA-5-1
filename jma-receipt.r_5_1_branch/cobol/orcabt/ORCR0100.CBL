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
       PROGRAM-ID.                 ORCR0100.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト
      *  コンポーネント名  : レセプト印刷用ファイル作成
      *                      ２００２年１０月改正以前
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC-太田      新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01.06.29  保険者番号・
      *                                     適用開始年月日の追加
      *                                     KEYが長すぎるので医療機関ＩＤ・
      *                                     診療年月を削除
      *  01.00.02    MCC-藤原     01.06.29  ファイル名を連番に変更
      *  01.00.03    MCC-藤原     01.07.12  給付割合の編集の変更
      *  01.00.04    MCC-藤原     01.07.13  社保のとき県内・県外区分にゼロ
      *                                     をセットする 
      *  01.00.05    MCC-藤原     01.07.27  請求管理ＤＢに更新する  
      *  01.00.06    MCC-藤原     01.08.08  総括用項目の追加
      *  01.00.07    MCC-藤原     01.08.10  保険者番号の編集追加
      *  01.00.08    MCC-藤原     01.10.01  保険種別の記号修正
      *  01.00.09    MCC-藤原     01.10.16  職務上の事由
      *  01.00.09    MCC-藤原     01.11.27  摘要欄の病名の編集の修正
      *  01.00.10    MCC-藤原     01.12.06  レセプト種別不明のとき提出先
      *                                     の表示をしない 
      *  01.00.11    MCC-藤原     01.12.13  病名が二枚以上に印刷できるように
      *                                     修正 
      *  01.00.12    MCC-藤原     02.01.09  レセプト印刷情報により傷病名の
      *                                     の編集をする 
      *  01.00.13    MCC-藤原     02.01.10  診療開始日の編集を変更
      *  01.00.14    MCC-藤原     02.02.06  レセプトコメントマスタの追加
      *  01.00.15    MCC-藤原     02.02.12  帳票管理ファイルに件数の追加
      *  01.00.16    MCC-藤原     02.02.13  レセプトコメントを該当の保険組合せ
      *                                     のときだけ出力する 
      *  01.00.17    MCC-藤原     02.02.14  手術麻酔(診療区分＝５４)を５０
      *                                     にして摘要欄に編集
      *  01.00.18    MCC-藤原     02.02.14  保険組合せを３から１０に変更
      *  01.00.19    MCC-藤原     02.02.15  特記事項の編集の変更
      *  01.00.20    MCC-藤原     02.02.25  患者番号変換でエラーのとき
      *                                     不明分で出力
      *  01.00.21    MCC-藤原     02/02/26  レセプト管理ファイルを
      *                                     索引ファイルからＤＢへ変更
      *  01.00.22    MCC-藤原     02/03/06  レセプトが複数枚印刷するときの
      *                                     人数の計算を修正
      *  01.00.23    MCC-藤原     02/03/07  レセプトコメントの改行コードの編集
      *  01.00.24    MCC-藤原     02/03/13  保険の履歴の編集
      *  01.00.25    MCC-藤原     02/03/20  請求点の編集の条件を修正
      *  01.00.26    MCC-藤原     02/03/26  薬剤一部負担金額がゼロのときの編集
      *  01.00.27    MCC-藤原     02/03/28  国保総括のために給付割合を食事日数に
      *                                     セット
      *  01.00.28    MCC-藤原     02/04/02  県単用項目の処理追加
      *                                     補足項目の削除 
      *  01.00.29    MCC-藤原     02/04/02  県単用項目の処理追加
      *  01.00.30    MCC-小豆沢   02.04.09  初診の時間外／深夜／休日を表示
      *  01.00.31    MCC-小豆沢   02.04.10  テスト患者は請求管理の更新処理を
      *                                     しない
      *  01.00.32    MCC-藤原     02.04.10  病名の疑いフラグからの編集
      *  01.00.33    MCC-藤原     02.04.11  記号の半角アルファベットの変換
      *
      *  ２００２年４月改正対応
      *  01.00.34    MCC-藤原     02.04.19  主病名の編集の修正
      *  01.00.35    MCC-小豆沢   02.04.23  特記事項の名称追加と変更
      *  01.00.36    MCC-藤原　   02.04.26  病名の転帰日の編集修正
      *  01.00.37    MCC-藤原　   02.05.02  半角チェックサブの追加
      *  01.00.38    MCC-藤原　   02.05.07  薬剤一部負担金が同一額のときの
      *                                     編集の修正
      *  01.00.39    MCC-藤原     02.05.14  病名の疑いフラグからの編集の中止
      *  01.00.40    MCC-藤原     02.05.16  個別の時複数診療月で作成するために
      *                                     診療年月を追加
      *  01.00.41    MCC-藤原     02.05.16  保険の履歴の開始年月日の編集の修正
      *  01.00.42    MCC-藤原     02.05.21  レセプト種別不明分の請求管理の更新
      *                                     処理をしない
      *  01.00.43    MCC-藤原     02.05.30  レセプトＤＢへ保険区分の追加
      *  01.00.44    MCC-藤原     02.06.11  転帰編集区分によって転帰日を編集
      *  01.00.44    MCC-藤原     02.07.03  レセプトＤＢへの更新時のエラー処理
      *  01.00.45    NACL-藤原    02.08.26  自費(8888)のとき請求管理の更新
      *                                     処理をしない
      *
      *  ２００２年１０月改正対応
      *  01.01.01    NACL-藤原    02/09/17  県内県外区分・県番号・
      *                                     年齢・給付割合の追加
      *  01.01.02    NACL-藤原    02/10/17  氏名を３２から５０に変更
      *  01.01.03    NACL-門脇    02/10/17  公費２の一部負担金欄の編集修正
      *  01.01.04    NACL-藤原    02/10/24  保険内自費のみのとき出力しないように
      *                                     修正
      *  01.01.05    NACL-藤原    02.11.07  医療機関ＩＤ・診療年月を追加
      *  01.01.06    NACL-藤原    02.11.08  国保の特定の保険者番号のとき
      *                                     県内分とするように修正
      *  01.00.07    NACL-藤原    02/11/28  国保の県内扱い保険者番号をシス管より
      *                                     編集するように修正
      *  01.00.08    NACL-藤原    03/01/27  公費請求書ＤＢを追加
      *  01.00.09    NACL-藤原    03.02.12  補助区分の追加
      *  01.00.10    NACL-門脇    03.03.03  剤番号桁修正対応
      *  01.00.11    NACL-藤原    03.03.05  レセプトコメントＤＢに入外区分を追加
      *  01.00.12    NACL-藤原    03/03/11  種別不明のとき情報を編集
      *
      *  ２００３．４改正対応
      *  01.02.01    NACL-藤原    03/04/17  出力件数をジョブ管理ＤＢへ更新
      *  01.02.02    NACL-藤原    03/04/21  地方公費データのとき主保険が無い
      *                                     ときは不明分としてレセプトを作成する
      *  01.02.03    NACL-藤原    03/04/23  公費請求書ＤＢの給付割合を修正
      *  01.02.04    NACL-藤原    03/06/10  保険履歴の改頁処理の修正
      *  01.02.05    NACL-藤原    03/07/10  治験のとき請求管理ＤＢに
      *                                     ０：請求しないとして更新する
      *  01.02.06    NACL-藤原    03/08/05  月内で保険者が変わったときの
      *                                     件数の集計方法を変更する
      *  01.02.07    NACL-藤原    04/01/19  病床数の記載の修正
      *  01.02.08    NACL-門脇    04/01/20  自費レセプトの自費分
      *                                     摘要欄記載対応
      *  01.02.09    NACL-門脇    04/01/20  自費レセ摘要欄編集修正
      *  01.02.10    NACL-門脇    04/05/13  初診時に時間外・深夜・休日が
      *                                     ２つ以上あった場合の記載対応
      *  01.02.11    NACL-小豆沢  04/05/19  ＤＢ書き込み時のコミット追加
      *  中止処理追加
      *  01.01.01    NACL-藤原    04/09/28  中止処理の追加
      *  01.01.02    NACL-門脇    05/01/13  公費請求テーブル保険組合せ
      *                                     番号情報編集修正
      *  01.01.03    NACL-門脇    05/03/17  公費請求・請求管理の負担金額
      *                                     編集修正
      *  02.06.01    NACL-藤原    05/07/20  レセコメントのコメントの長さ
      *                                     変更による対応
      *
      *  02.07.01    NACL-藤原    05/12/12  国保レセプト編綴順対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/04  平成２０年４月改正対応
      *                                     フラグ、データエリア追加
      *
      *  04.07.01    NACL-藤原    12/01/24  monpe-0.6.20対応
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    レセプト０２データ
           SELECT  RECE02-FILE     ASSIGN  RECE01PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE.
      *
      *    レセプト０２データ（公費請求書）
           SELECT  RECE02X-FILE    ASSIGN  RECE02XPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE02X-KEY
                                   FILE    STATUS  IS  STS-RECE02X.
      *    レセプト０３１データ
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE.
      *    レセプト０６１データ
           SELECT  RECE061-FILE     ASSIGN  RECE061PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE061-KEY
                                   FILE    STATUS  IS  STS-RECE.
      *                             
           SELECT  RECE09-FILE     ASSIGN  RECE09PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE09-KEY
                                   FILE    STATUS  IS  STS-RECE09.
      *    レセプト（県単用項目）データ
           SELECT  RECE11-FILE     ASSIGN  RECE11PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE11-KEY
                                   FILE    STATUS  IS  STS-RECE11.
      *
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    レセプト０２データ
       FD  RECE02-FILE.
       01  RECE02-REC.
           COPY    "CPRCF002.INC".
      *
      *    レセプト０２データ（公費請求書）
       FD  RECE02X-FILE.
       01  RECE02X-REC. 
           COPY    "CPRCF002.INC"  REPLACING  //RECE02//
                                   BY         //RECE02X//.
      *
      *    レセプト０３１データ
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
      *    レセプト０６１データ
       FD  RECE061-FILE.
       01  RECE061-REC. 
           COPY    "CPRCF0061.INC".
      *
      *    レセプト０９データ
       FD  RECE09-FILE.
       01  RECE09-REC. 
           COPY    "CPRCF009.INC".
      *
      *    レセプト（県単用項目）データ
       FD  RECE11-FILE.
       01  RECE11-REC. 
           COPY    "CPRCF011.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200). 
      *
       WORKING-STORAGE             SECTION.
grpsys*
grpsys     COPY    "COMMON-SPA".
      *    シェル用領域
           COPY    "CPCOMMONSHELL.INC".
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE01PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE02XPARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE031PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE061PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE09PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE11PARA//.
      *
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
           COPY    "CPTEMPFL.INC".
      *
           COPY    "HC06.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE            PIC X(02).
           03  STS-RECE09          PIC X(02).
           03  STS-RECE11          PIC X(02).
           03  STS-RECEERR         PIC X(02).
           03  STS-RECE02X         PIC X(02).  
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(03).
           03  FLG-RECE02          PIC 9(01).
           03  FLG-RECE031         PIC 9(01).
           03  FLG-RECE061         PIC 9(01).
           03  FLG-RECE08          PIC 9(01).
           03  FLG-RECE09          PIC 9(01).
           03  FLG-RECE11          PIC 9(01).
           03  FLG-KANRIMST        PIC 9(01).
           03  FLG-HKNNUM          PIC 9(01).
           03  FLG-KUGIRI          PIC 9(01).
           03  FLG-RECE07          PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-RECECOM         PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
      *
           03  FLG-KOHSKY          PIC 9(01).
           03  FLG-RECE02X         PIC 9(01).
           03  FLG-TENKI           PIC 9(01).     
           03  FLG-SET             PIC 9(01).
           03  FLG-STS             PIC 9(04).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
           03  CNT-RECE02          PIC 9(06).
           03  CNT-RECE02X         PIC 9(06).
           03  CNT-RECE03          PIC 9(06).
           03  CNT-RECE061         PIC 9(06).
           03  CNT-RECE09          PIC 9(06).
           03  CNT-OUT             PIC 9(06).
           03  CNT-ZAIMEISAISU     PIC 9(03).
           03  CNT-PAGE            PIC 9(03).
           03  CNT-KENSU           PIC 9(03). 
           03  CNT-COMMIT          PIC 9(03). 
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
           03  IDXP                PIC 9(02).
           03  IDY1                PIC 9(03).
           03  IDY2                PIC 9(03).
           03  IDY3                PIC 9(03).
           03  IDXX                PIC 9(03).
      *     
           03  SEIHIDX             PIC 9(02).
           03  KAKKIDX             PIC 9(02).
           03  SESNIDX             PIC 9(02).
           03  SYSKAN-IDX          PIC 9(03).
      *
      *    
       01  KEY-AREA                          VALUE   LOW-VALUE.          
           03  KEY-NEW.
               05  KEY-NEW1.
grpsys             07  KEY-N-HOSPNUM   PIC  9(02).
                   07  KEY-N-SRYYM     PIC  9(06).
                   07  KEY-N-NYUGAIKBN PIC  X(01).
                   07  KEY-N-PTID      PIC  9(10).
                   07  KEY-N-RECEKA    PIC  X(02).
               05  KEY-N-TEISYUTUSAKI  PIC  9(01).
               05  KEY-N-RECESYUBETU   PIC  9(04).
               05  KEY-N-HKNJANUM-KEY  PIC  X(08).
           03  KEY-OLD.
               05  KEY-OLD1.
grpsys             07  KEY-O-HOSPNUM   PIC  9(02).
                   07  KEY-O-SRYYM     PIC  9(06).
                   07  KEY-O-NYUGAIKBN PIC  X(01).
                   07  KEY-O-PTID      PIC  9(10).
                   07  KEY-O-RECEKA    PIC  X(02).
               05  KEY-O-TEISYUTUSAKI  PIC  9(01).
               05  KEY-O-RECESYUBETU   PIC  9(04).
               05  KEY-O-HKNJANUM-KEY  PIC  X(08).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  10.
               05  TBL-SYS1001         PIC  X(610).
      *    医療機関（住所等情報）退避用領域
       01  TBL-SYS1002-T.
           03  TBL-SYS1002-OCC         OCCURS  10.
               05  TBL-SYS1002         PIC  X(610).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-PARA.
               05  WRK-PARA-SRYYM   PIC  X(06).
               05  WRK-PARA-TERMID  PIC  X(16).
               05  WRK-PARA-SYSYMD  PIC  X(08).
               05  WRK-PARA-SYOKBN  PIC  X(01).
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
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-RECENUM         PIC 9(06). 
      *
           03  WRK-HKNJANUM        PIC X(08).
           03  WRK-KYURATE-9       PIC 9(03).
           03  WRK-KYURATE         PIC Z9.
           03  WRK-BEDSU           PIC ZZZ.
      *
           03  WRK-ERRMSG          PIC X(40).
      *
           03  WRK-1033-TABLE.
               05  WRK-1033-TBL    OCCURS  5.
                   07  WRK-1033-HKNJANUM-G.
                       09  WRK-1033-HKNJANUM
                                   PIC X(08) OCCURS  20.
                   07  WRK-1033-STYUKYMD
                                   PIC X(08).
                   07  WRK-1033-EDYUKYMD 
                                   PIC X(08).
      *
      *    県単用項目ファイル用ワーク     
           COPY    "HC06.INC"      REPLACING  //HC06//
                                   BY         //RECE11-HC06//.
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
      *    全角変換処理用
           03  WRK-CHAR-HENKAN-AREA.
               05  WRK-CHAR-X                  PIC X(80).
               05  WRK-CHAR-N                  PIC X(160).
               05  WRK-CHAR-CNT                PIC 9(03).
           03  WRK-NUM-IDX         PIC 9(01).    
      *
           03  WRK-BYO-OLD.
               05  WRK-BO-SYUBYOFLG            PIC X(01).
               05  WRK-BO-SRYYMD               PIC X(08).
               05  WRK-BO-TENKIKBN             PIC X(01).
               05  WRK-BO-TENKIYMD             PIC X(08).
      *
      *    1桁目:生保 2桁目:結核 3桁目:精神
           03  WRK-3SYU-KBN.
               05  WRK-3SYU        PIC X(03).
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
               05  WRK-LINETEK                 PIC 9(02).
      *        前行の続きのとき（傷病名）         
               05  WRK-CONTINUE                PIC 9(01).
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
               05  WRK-BYOMEI2R-NAMEW  REDEFINES   WRK-BYOMEI2-NAME.
                   07  FILLER                  PIC X(14).
                   07  WRK-BYOMEI2R-ELSE.
                       09  WRK-BYOMEI2R-YMD    PIC X(20).
                       09  WRK-BYOMEI2R-TENKI  PIC X(04).
                       09  WRK-BYOMEI2R-TENKIYMD
                                               PIC X(06).
      *                 
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  50.  
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(12).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(02).                
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).    
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).   
      *
           03  WRK-WLINETEK                    PIC 9(03).
           03  WRK-TNSIDX                      PIC 9(02).
           03  WRK-R061-ZAINUM-O               PIC 9(08).
           03  WRK-R061-SRYKBN-O               PIC X(02).
           03  WRK-R061-KOHBUNRI-O             PIC 9(01).
      *
      *****03  WRK-IN                          PIC X(01).
      *
           03  WRK-SRYYMD                      PIC X(08).
           03  WRK-KGKZ                        PIC ZZZ,ZZZ,ZZZ.
           03  WRK-KGK9                        PIC ZZZ,ZZZ,ZZ9.
           03  WRK-KGK2                        PIC Z9.
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
       01  WRK-BYOMEI-AREA.     
      *    傷病名最大文字数（傷病名欄）
           03  WRK-BYOMEI1-MAX     PIC 9(02)   VALUE   48.
      *    傷病名最大文字数（適用欄）
           03  WRK-BYOMEI2-MAX     PIC 9(02)   VALUE   44.
      *    適用欄最大行数
           03  WRK-TEKIYOU-LINE    PIC 9(02)   VALUE   35.
      *    傷病名欄最大行数
           03  WRK-BYOMEI-LINE     PIC 9(02)   VALUE   5.
      *     
      *   全角数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *   特記事項
       01  TBL-TOKKI-AREA.
           03  FILLER          PIC X(12)   VALUE   "01０１公".
           03  FILLER          PIC X(12)   VALUE   "02０２長".
           03  FILLER          PIC X(12)   VALUE   "03０３長処".
           03  FILLER          PIC X(12)   VALUE   "04０４老保".
           03  FILLER          PIC X(12)   VALUE   "05０５高度".
           03  FILLER          PIC X(12)   VALUE   "07０７老併".
           03  FILLER          PIC X(12)   VALUE   "08０８老健".
           03  FILLER          PIC X(12)   VALUE   "09０９施".
           03  FILLER          PIC X(12)   VALUE   "10１０第三".
           03  FILLER          PIC X(12)   VALUE   "11１１薬治".
           03  FILLER          PIC X(12)   VALUE   "12１２材治".
       01  TBL-TOKKI-R         REDEFINES   TBL-TOKKI-AREA.
           03  TBL-TOKKI       OCCURS   11.
               05  TBL-TOKKICD             PIC X(02).
               05  TBL-TOKKINAME           PIC X(10).
      *
      *   退職者保険番号
       01  WRK-RETIRE-AREA.
      *        組合
           03  WRK-RETIRE1         PIC X(03)   VALUE  "063".
      *        国公
           03  WRK-RETIRE2         PIC X(03)   VALUE  "072".
      *        地公
           03  WRK-RETIRE3         PIC X(03)   VALUE  "073".
      *        警察
           03  WRK-RETIRE4         PIC X(03)   VALUE  "074".
      *        公学・私学
           03  WRK-RETIRE5         PIC X(03)   VALUE  "075".
      *        国保
           03  WRK-RETIRE6         PIC X(03)   VALUE  "067".
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
      *    補足編集
      *01  WRK-HOSOKU-AREA.
      **   03  WRK-HOSOKU1         PIC X(10)   VALUE  "【原爆】".
      *
      *    区切り線
       01  WRK-LINE-AREA.
           03  WRK-KUGIRI1         PIC X(52)   VALUE
               ALL "―".
           03  WRK-KUGIRI2.
               05  FILLER          PIC X(22)   VALUE
                                   "＝＝＝＝＝＝＝　以下　".
               05  WRK-KUGIRI2-KOHNAME         PIC X(10).
               05  FILLER          PIC X(20)   VALUE
                                   "　適用分　＝＝＝＝＝".
           03  WRK-KUGIRI3         PIC X(52)   VALUE
               ALL "―".
      *    
      *    固定端末ＩＤ
           03  WRK-CONS-TERMID     PIC X(32)   VALUE
                                   "0100007FZZZZZZZZZZZZZZZZZZZZZZZZ". 
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
       01  WRK-TENSU-AREA.
           03  WRK-KHNTEN.
               05  WRK-KHNTENZ     PIC  ZZZZZZZ.
           03  WRK-KAISU.
               05  WRK-KAISUZ      PIC  ZZZ.
           03  WRK-KOUTEN.
               05  WRK-KOUTENZ     PIC  ZZZZZZZ.
           03  WRK-TENSU.
               05  WRK-TENSUZ      PIC  ZZZZZZZ.
           03  WRK-JINISSU.
               05  WRK-JINISSUZ    PIC  ZZ.
      *
       01  WRK-CNT                 PIC  9(5).
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
           03  WRK-ERR-MSG-4           PIC X(50)  VALUE
               "地方公費のみ選択されています。主保険がありません".
      *
      *    患者情報（ワーク）
       01  WRK-PTINF-REC.
           COPY    "CPPTINF.INC"      REPLACING  //PTINF//
                                      BY         //WRK-PTINF//.
      *
       01  WRK-RECE07-REC. 
           COPY    "CPRCF007.INC"  REPLACING  //RECE07//
                                   BY         //WRK-RECE07//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK2005.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
           COPY    "CPSK1002.INC".
      *
      *    レセプト作成指示（個別指示）コード
           COPY    "CPSK2002.INC".
      *     
      *    県内扱い保険者情報
           COPY    "CPSK1033.INC".
      *
           COPY    "CPRCF008.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    保険マスタ
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    レセプトコメントマスタ
       01  RECECOM-REC.
           COPY    "CPRECECOM.INC".
      *
      *    レセプト管理     
       01  RECE07-REC. 
           COPY    "CPRCF007.INC".
      *
      *    患者情報
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
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
      *   ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
           COPY    "CPORCSGETTEMP.INC".
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
           STOP    RUN
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
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  SYS-1002-REC
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA
                                               WRK-PARA-HOSPNUM
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
      *    ファイルＯＰＥＮ処理
           PERFORM 110-FILE-OPEN-SEC
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ORCR0100"      TO  JOB-PGID
           MOVE    "印刷データ編集"
                                   TO  JOB-SHELLMSG
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
      *    処理中止設定処理
           IF      FLG-STS             =   ZERO
               PERFORM 500-CANCEL-HENSYU-SEC
           END-IF
      *         
      *    ジョブ管理更新（中止フラグ非活性）処理
           IF      FLG-STS             =   ZERO
               MOVE    "STP"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    3               TO  JOB-STOPFLG
grpsys         PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
      *    システム管理マスタ（医療機関情報）
           IF      FLG-STS             =   ZERO
               PERFORM 120-HOSPINF-GET-SEC
           END-IF
      *
           IF      FLG-STS             =   ZERO
               PERFORM 5001-DB-DELETE-SEC
           END-IF
      *
           IF      FLG-STS             =   ZERO
              PERFORM 990-RECE02-READ-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイルＯＰＥＮ処理
      *****************************************************************
       110-FILE-OPEN-SEC          SECTION.
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE02X"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    WRK-CONS-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE031"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE061"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE090"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           MOVE    "RECE110"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (7)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE01PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE02XPARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE031PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE061PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE09PARA
           MOVE    SGETTEMP-FULLNAMES (7)
                                       TO  RECE11PARA
      *
           OPEN    INPUT               RECE02-FILE
           IF  STS-RECE            NOT =   ZERO
               DISPLAY "*** ORCR0100 RECE02 OPEN ERR  ***"
               MOVE    2               TO  FLG-STS
           END-IF
      *
           OPEN    INPUT               RECE02X-FILE
           IF  STS-RECE02X         NOT =   ZERO
               DISPLAY "*** ORCR0103 RECE02X OPEN ERR  ***"
               MOVE    2               TO  FLG-STS
           END-IF
      *
           OPEN    INPUT               RECE031-FILE
           IF  STS-RECE            NOT =   ZERO
               DISPLAY "*** ORCR0100 RECE031 OPEN ERR  ***"
               MOVE    2               TO  FLG-STS
           END-IF
      *
           OPEN    INPUT               RECE061-FILE
           IF  STS-RECE            NOT =   ZERO
               DISPLAY "*** ORCR0100 RECE061 OPEN ERR  ***"
               MOVE    2               TO  FLG-STS
           END-IF
      *
           OPEN    INPUT               RECE09-FILE
           IF  STS-RECE09            NOT =   ZERO
               DISPLAY "*** ORCR0100 RECE09 OPEN ERR  ***" STS-RECE09
               MOVE    2               TO  FLG-STS
           END-IF
      *
           OPEN    INPUT               RECE11-FILE
           IF  STS-RECE11            NOT =   ZERO
               DISPLAY "*** ORCR0100 RECE11 OPEN ERR  ***" STS-RECE11
               MOVE    2               TO  FLG-STS
           END-IF
           .
       110-FILE-OPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報検索
      *****************************************************************
       120-HOSPINF-GET-SEC            SECTION.
      *
           ACCEPT  SYS-YMD         FROM    DATE
           COMPUTE WRK-SYSYY       =   SYS-YY      +   2000
           MOVE    SYS-MM              TO  WRK-SYSMM
           MOVE    SYS-DD              TO  WRK-SYSDD
           ACCEPT  SYS-TIME            FROM    TIME
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
                  MOVE    MCPDATA-REC  TO  SYS-1001-REC
              ELSE    
                  MOVE    "医療機関情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                  PERFORM 500-ERR-HENSYU-SEC
                  MOVE    2            TO  FLG-STS
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
                  PERFORM  UNTIL   FLG-KANRIMST  =    1
      *
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
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
              MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
              MOVE    "key10"             TO  WRK-MCP-PATHNAME
              PERFORM 960-KANRIMST-READ-SEC
              IF      FLG-KANRIMST     =   ZERO
                  MOVE    MCPDATA-REC  TO  SYS-1002-REC
              ELSE
                  MOVE "医療機関情報（所在地）が取得できませんでした。"
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
grpsys        MOVE    SPA-HOSPNUM         TO  SYS-HOSPNUM
              MOVE    SYSKANRI-REC        TO  MCPDATA-REC
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
grpsys        PERFORM 900-DBSELECT-SEC
              IF      MCP-RC              =   ZERO
                  MOVE    "tbl_syskanri"      TO  MCP-TABLE
                  MOVE    "key8"              TO  MCP-PATHNAME
                  PERFORM  900-KANRIMST-NEXT-SEC
                  PERFORM  UNTIL   FLG-KANRIMST  =    1
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
                   MOVE
                       "医療機関情報（所在地）が取得できませんでした。"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2            TO  FLG-STS
                   GO  TO  120-HOSPINF-GET-EXT
               END-IF
           END-IF
      *
      *    レセプト編集区分情報
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "2005"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
           MOVE    WRK-PARA-SRYYM   TO  SYS-STYUKYMD (1:6)
           MOVE    "01"             TO  SYS-STYUKYMD (7:2)
           MOVE    SYS-STYUKYMD     TO  SYS-EDYUKYMD
           MOVE    "tbl_syskanri"      TO  WRK-MCP-TABLE
           MOVE    "key10"             TO  WRK-MCP-PATHNAME
           PERFORM 960-KANRIMST-READ-SEC
           IF      FLG-KANRIMST     =   ZERO
               MOVE    MCPDATA-REC      TO  SYS-2005-REC
           ELSE    
               MOVE    SPACE            TO  SYS-2005-REC
           END-IF
           IF      SYS-2005-BYOMEIHENKBN    =   SPACE
               MOVE    ZERO             TO  SYS-2005-BYOMEIHENKBN
           END-IF
           IF      SYS-2005-TENKIHENKBN     =   SPACE
               MOVE    ZERO             TO  SYS-2005-TENKIHENKBN
           END-IF
      * 
      *    県内扱い保険者情報
           MOVE    SPACE            TO  WRK-1033-TABLE
      *     
           MOVE    SPACE            TO  SYSKANRI-REC
           INITIALIZE                   SYSKANRI-REC
           MOVE    "1033"           TO  SYS-KANRICD
           MOVE    "*"              TO  SYS-KBNCD
grpsys     MOVE    SPA-HOSPNUM      TO  SYS-HOSPNUM
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           MOVE    ZERO                TO  IDX
           PERFORM             UNTIL   FLG-KANRIMST   =   1
                               OR      IDX            >=  5
               MOVE    MCPDATA-REC         TO  SYS-1033-REC
               IF      SYS-1033-STYUKYMD   >   "20020930"
                   CONTINUE
               ELSE    
                   ADD     1              TO  IDX
                   MOVE    SYS-1033-STYUKYMD
                                          TO  WRK-1033-STYUKYMD   (IDX)
                   MOVE    SYS-1033-EDYUKYMD
                                          TO  WRK-1033-EDYUKYMD   (IDX)
                   MOVE    SYS-1033-TBL   TO  WRK-1033-HKNJANUM-G (IDX)
               END-IF    
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 900-KANRIMST-NEXT-SEC
           END-PERFORM
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       120-HOSPINF-GET-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
           PERFORM         UNTIL   FLG-RECE02     =   1
                           OR      FLG-STS        =   2 
      *
      *        患者情報読込処理      
               PERFORM 2001-PTINF-HEN-SEC 
      *        患者番号別保険履歴情報編集処理      
               PERFORM 2001-RIREKI-HEN-SEC 
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM         UNTIL   FLG-RECE02     =   1
                               OR      FLG-STS        =   2
                               OR      KEY-NEW1   NOT =   KEY-OLD1
      *            レセプト種別別件数カウント
                   MOVE    ZERO            TO  CNT-KENSU
                   MOVE    KEY-NEW         TO  KEY-OLD
                   PERFORM         UNTIL   FLG-RECE02     =   1
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
                       PERFORM 990-RECE02-READ-SEC
                   END-PERFORM
               END-PERFORM    
           END-PERFORM
      *
      *    公費請求書出力
           IF      FLG-STS         =   2
               CONTINUE
           ELSE
               PERFORM 2002-KOHSKY-WRITE-SEC
           END-IF 
      *
           MOVE    2               TO  FLG-STS
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *        患者情報読込処理      
      *****************************************************************
        2001-PTINF-HEN-SEC       SECTION.
      *
grpsys     MOVE    KEY-N-HOSPNUM       TO  PTINF-HOSPNUM
           MOVE    KEY-N-PTID          TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           MOVE    PTINF-REC           TO  WRK-PTINF-REC
           . 
        2001-PTINF-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号別保険履歴情報編集処理
      *****************************************************************
       2001-RIREKI-HEN-SEC      SECTION.
      *
      *    診療年月編集
           MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMST
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    RECE02-SRYYM        TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI     TO  WRK-SYMD 
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3 (11:12)
                                       TO  WRK-SRYYMED
      *
           MOVE    SPACE               TO  WRK-HKN-RIREKI-AREA
           MOVE    ZERO                TO  WRK-RIREKI-MAX
                                           CNT-RECE09
      *     
           INITIALIZE                      RECE09-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE09-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE09-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE09-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE09-PTID
           MOVE    RECE02-RECEKA       TO  RECE09-RECEKA
      *
           START   RECE09-FILE
               KEY IS    >=   RECE09-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE09
               NOT INVALID
                   PERFORM 990-RECE09-NEXT-SEC
           END-START
           PERFORM UNTIL ( FLG-RECE09           =   1                )
grpsys             OR    ( RECE02-HOSPNUM   NOT =   RECE09-HOSPNUM   )
                   OR    ( RECE02-SRYYM     NOT =   RECE09-SRYYM     )
                   OR    ( RECE02-NYUGAIKBN NOT =   RECE09-NYUGAIKBN )
                   OR    ( RECE02-PTID      NOT =   RECE09-PTID      )
                   OR    ( RECE02-RECEKA    NOT =   RECE09-RECEKA    )
      *
               ADD     1                  TO  CNT-RECE09
      *
               ADD     1                  TO  WRK-RIREKI-MAX
               IF      WRK-RIREKI-MAX     >   50
                   MOVE    "保険履歴情報がテーブルオーバーしました"
                                              TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    2               TO  FLG-STS
                   GO  TO  2001-RIREKI-HEN-EXT
               END-IF    
      *
               EVALUATE    RECE09-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "社保"     TO  WRK-HKN-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE      TO  WRK-HKN-TEISYUTUSAKI   
               END-EVALUATE          
               IF      RECE09-NUM         =   SPACE
               AND     RECE09-KIGO        =   SPACE        
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               ELSE
                   MOVE    SPACE               TO  WRK-HKN-KIGO  
                   IF      RECE09-KIGO     NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-KIGO     TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-HKN-KIGO
                       END-IF
                   END-IF      
                   MOVE    SPACE               TO  WRK-HKN-NUM 
                   IF      RECE09-NUM    NOT =   SPACE
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE09-NUM      TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                                   TO  WRK-HKN-NUM
                       END-IF
                   END-IF    
                   STRING  WRK-HKN-TEISYUTUSAKI   DELIMITED  BY  SPACE
                           RECE09-TANSEIDONAME    DELIMITED  BY  SPACE
                           "　"                   DELIMITED  BY  SIZE
                           WRK-HKN-KIGO           DELIMITED  BY  SPACE
                           "―"                   DELIMITED  BY  SIZE
                           WRK-HKN-NUM            DELIMITED  BY  SPACE
                              INTO    WRK-HKN-RIREKI (WRK-RIREKI-MAX)
                   END-STRING
               END-IF                   
               IF      WRK-HKN-RIREKI (WRK-RIREKI-MAX)(13:)
                                          NOT =   SPACE
                   ADD     1                   TO  WRK-RIREKI-MAX
                   IF      WRK-RIREKI-MAX     >   50
                       MOVE    "保険履歴情報がテーブルオーバーしました"
                                                   TO  WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       MOVE    2               TO  FLG-STS
                       GO  TO  2001-RIREKI-HEN-EXT
                   END-IF    
               END-IF                             
      *
               MOVE    RECE09-TEKSTYMDX   TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               IF      RECE09-TEKSTYMDX(1:6)  =   RECE02-SRYYM                      
      *************MOVE    RECE09-TEKSTYMDX   TO  WRK-SYMD
      *************PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(11:12)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(9:)
               ELSE
      *************MOVE    WRK-SRYYMST                TO
      *************               WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)
                   MOVE    LNK-DAY2-EDTYMD3-G(1:2)    TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(1:2)
                   MOVE    LNK-DAY2-EDTYMD3(5:18)     TO  
                                  WRK-HKN-TEKSTYMD(WRK-RIREKI-MAX)(3:)
               END-IF                                    
      *  
               IF      RECE09-TEKEDYMDX(1:6)  =   RECE02-SRYYM                      
                   MOVE    RECE09-TEKEDYMDX   TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3(11:12)    TO
                                      WRK-HKN-TEKEDYMD(WRK-RIREKI-MAX)
                   MOVE    "から"             TO
                                      WRK-HKN-FL1     (WRK-RIREKI-MAX)
                   MOVE    "まで"             TO
                                      WRK-HKN-FL2     (WRK-RIREKI-MAX)                                     
               ELSE
                   MOVE    "から有効"         TO
                                      WRK-HKN-FL      (WRK-RIREKI-MAX) 
               END-IF                                    
      *
               PERFORM 990-RECE09-NEXT-SEC
           END-PERFORM
      *
           IF      CNT-RECE09             =   1
               MOVE    SPACE                  TO  WRK-HKN-RIREKI-AREA
               MOVE    ZERO                   TO  WRK-RIREKI-MAX
           END-IF          
      *
           .
       2001-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       2001-SYORI-SEC                SECTION.
      *
           MOVE    1         TO  CNT-PAGE
      *
      *    医療保険・公費のフラグ
           MOVE    SPACE              TO  WRK-3SYU-KBN
           MOVE    ZERO               TO  SEIHIDX
           MOVE    ZERO               TO  KAKKIDX
           MOVE    ZERO               TO  SESNIDX
           PERFORM   VARYING   IDX  FROM    1   BY  1
                     UNTIL   ( IDX                >    4    )
                     OR      ( RECE02-KOHID(IDX)  =   SPACE )
      *        生保
               IF  RECE02-KOHNUM (IDX)  =   "012"
                   MOVE    "1"          TO  WRK-3SYU (1:1)
                   MOVE     IDX         TO  SEIHIDX
               END-IF
      *        結核
               IF  RECE02-KOHNUM (IDX)  =   "010"  OR  "011"
                   MOVE    "1"          TO  WRK-3SYU (2:1)
                   MOVE     IDX         TO  KAKKIDX
               END-IF
      *        精神
               IF  RECE02-KOHNUM (IDX)  =   "020"  OR  "021"
                   MOVE    "1"          TO  WRK-3SYU (3:1)
                   MOVE     IDX         TO  SESNIDX
               END-IF
           END-PERFORM
      *
      *    県単用項目ファイル読込処理
           PERFORM 301-RECE11-HENSYU-SEC
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票編集<点数>処理
           PERFORM 330-HC101-TENSU-HEN-SEC
      *
      *    帳票編集<補足>処理
           PERFORM 350-HC101-HOSOKU-HEN-SEC
      *
      *    帳票編集<傷病名>処理
           PERFORM 320-HC101-BYOMEI-HEN-SEC
      *
      *    帳票編集<剤>処理
           PERFORM 340-HC101-ZAI-HEN-SEC
      *
      *    帳票編集<コメント>処理
           PERFORM 360-HC101-COMMENT-HEN-SEC 
      *
      *    帳票編集<保険履歴情報>処理
           PERFORM 370-HC101-HKN-RIREKI-HEN-SEC 
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
           .
       2001-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書出力処理
      *****************************************************************
       2002-KOHSKY-WRITE-SEC           SECTION.
      *
           PERFORM 990-RECE02X-READ-SEC
      *
           PERFORM             UNTIL   FLG-RECE02X =   1
                               OR      FLG-ERR     =   1
      *
               IF      RECE02X-RECESYUBETU     =   ZERO
                   IF      RECE02X-HKNNUM      NOT =   SPACE 
                       CONTINUE
                   ELSE
                       PERFORM 20021-KOHSKY-FUMEI-HENSYU-SEC
                   END-IF
               ELSE
                   PERFORM 20022-KOHSKY-WRITE-SEC
               END-IF 
      *
               PERFORM 990-RECE02X-READ-SEC
           END-PERFORM
      *
           .
       2002-KOHSKY-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書不明分レセプト出力処理
      *****************************************************************
       20021-KOHSKY-FUMEI-HENSYU-SEC           SECTION.
      *
           IF   WRK-PARA-SRYYM         =   ZERO
      *        医療機関編集
               MOVE  RECE02X-SRYYM         TO  WRK-SYS-SRYYMD(1:6)
               MOVE  "01"                  TO  WRK-SYS-SRYYMD(7:2)
               PERFORM 900-SYSKANRI-HENSYU-SEC
           END-IF
      *
           MOVE    RECE02X-REC         TO  RECE02-REC 
      *
           MOVE    1                   TO  CNT-PAGE
           MOVE    1                   TO  FLG-RECE11
           MOVE    SPACE               TO  WRK-3SYU-KBN
           MOVE    ZERO                TO  SEIHIDX
           MOVE    ZERO                TO  KAKKIDX
           MOVE    ZERO                TO  SESNIDX
      *
      *    帳票編集<ヘッダー部>処理
           PERFORM 310-HC101-HEAD-HEN-SEC
      *
      *    帳票印刷処理
           PERFORM 390-PRINT-OUT-SEC
      *
           .
       20021-KOHSKY-FUMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求書編集処理
      *****************************************************************
       20022-KOHSKY-WRITE-SEC          SECTION.
      *
           MOVE    SPACE               TO  KOHSKY-REC
           INITIALIZE                      KOHSKY-REC
grpsys     MOVE    RECE02X-HOSPNUM     TO  KOHSKY-HOSPNUM
           MOVE    RECE02X-SRYYM       TO  KOHSKY-SRYYM
           MOVE    RECE02X-NYUGAIKBN   TO  KOHSKY-NYUGAIKBN
           MOVE    RECE02X-PTID        TO  KOHSKY-PTID
           IF      RECE02X-RECEKA      =   SPACE
               MOVE    ZERO                TO  KOHSKY-RECEKA
           ELSE
               MOVE    RECE02X-RECEKA      TO  KOHSKY-RECEKA
           END-IF
           MOVE    RECE02X-TEISYUTUSAKI    TO  KOHSKY-TEISYUTUSAKI
           IF      RECE02X-HKNJANUM-KEY    =   SPACE
               MOVE    ZERO                    TO  KOHSKY-HKNJANUM-KEY
           ELSE
               MOVE    RECE02X-HKNJANUM-KEY    TO  KOHSKY-HKNJANUM-KEY
           END-IF
           IF      RECE02X-HOJOKBN-KEY     =   SPACE
               MOVE    ZERO                    TO  KOHSKY-HOJOKBN-KEY
           ELSE
               MOVE    RECE02X-HOJOKBN-KEY     TO  KOHSKY-HOJOKBN-KEY
           END-IF
           IF      RECE02X-TEKSTYMD    =   SPACE
               MOVE    ZERO                TO  KOHSKY-TEKSTYMD
           ELSE
               MOVE    RECE02X-TEKSTYMD    TO  KOHSKY-TEKSTYMD
           END-IF
      *
      *    レセプト種別を請求管理より編集
           INITIALIZE                      RECE10-REC
           MOVE    KOHSKY-KEY          TO  RECE10-KEY
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key15"             TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    RECE10-RECESYUBETU  TO  KOHSKY-RECESYUBETU
      *****ELSE
      *         STRING  "レセプト種別が設定不能 PTNUM="
      *                                     DELIMITED  BY  SIZE
      *                 RECE02X-PTNUM       DELIMITED  BY  SIZE 
      *                                     INTO  WRK-RECEERR
      *         END-STRING
      *         PERFORM 500-ERR-HENSYU-SEC
      *         MOVE    1                  TO  FLG-ERR
      **********GO  TO  2002-KOHSKY-WRITE-EXT
           END-IF
      *
           PERFORM 900-KOHSKY-INV-SEC
           IF      FLG-KOHSKY          =   ZERO
               MOVE    MCPDATA-REC         TO  KOHSKY-REC
               MOVE    WRK-PARA-SYSYMD     TO  KOHSKY-UPDYMD
               IF      KOHSKY-SKYKBN       =   "2"
                   MOVE    "1"                 TO  KOHSKY-SKYKBN
               END-IF 
               MOVE    "2"                 TO  KOHSKY-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  KOHSKY-SKYKBN
               MOVE    WRK-PARA-SYSYMD     TO  KOHSKY-CREYMD
               MOVE    ZERO                TO  KOHSKY-UPDYMD
               MOVE    ZERO                TO  KOHSKY-SKYYM
                                               KOHSKY-HENREIYM
                                               KOHSKY-TEISEIKBN
           END-IF
           MOVE    RECE02X-PTNUM       TO  KOHSKY-PTNUM
           MOVE    RECE02X-KANANAME    TO  KOHSKY-KANANAME
           MOVE    RECE02X-NAME        TO  KOHSKY-NAME
           MOVE    RECE02X-SEX         TO  KOHSKY-SEX
           MOVE    RECE02X-BIRTHDAY    TO  KOHSKY-BIRTHDAY
           MOVE    RECE02X-AGE         TO  KOHSKY-AGE  
           MOVE    RECE02X-DEATHKBN    TO  KOHSKY-DEATHKBN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       20
               MOVE    RECE02X-SRYKA   (IDX)
                                       TO  KOHSKY-SRYKA (IDX)
           END-PERFORM
           MOVE    RECE02X-HKN         TO  KOHSKY-HKN
      *****IF      RECE02X-KYURATE  >  ZERO
      *         COMPUTE WRK-KYURATE-9   =
      *                        (100     -   RECE02X-KYURATE)
      *                                         / 10 
      *         MOVE    WRK-KYURATE-9   TO  KOHSKY-KYURATE
      *     ELSE
      *         MOVE    ZERO            TO  KOHSKY-KYURATE
      *****END-IF             
           MOVE    RECE02X-KYURATE     TO  KOHSKY-KYURATE
           MOVE    RECE02X-RJNHKN      TO  KOHSKY-RJNHKN
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
               MOVE    RECE02X-KOHID       (IDX) 
                                       TO  KOHSKY-KOHID      (IDX)
               MOVE    RECE02X-KOHNUM      (IDX)
                                       TO  KOHSKY-KOHNUM     (IDX)
               MOVE    RECE02X-KOHHBTNUM   (IDX)
                                       TO  KOHSKY-KOHHBTNUM  (IDX)
               MOVE    RECE02X-KOHFTNJANUM (IDX)
                                       TO  KOHSKY-KOHFTNJANUM(IDX)
               MOVE    RECE02X-KOHJKYSNUM  (IDX)
                                       TO  KOHSKY-KOHJKYSNUM (IDX)
           END-PERFORM
      ***  PERFORM VARYING IDX FROM    1   BY  1
      ***          UNTIL   IDX >       10
      ***      MOVE   RECE02X-HKNCOMBIINF (IDX)
      ***                                  TO KOHSKY-HKNCOMBIINF (IDX)
      ***  END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       10
               MOVE   RECE02X-HKNCOMBI(IDX)   TO  KOHSKY-HKNCOMBI(IDX)
               MOVE   RECE02X-HKNNUMHC(IDX)   TO  KOHSKY-HKNNUMHC(IDX)
               MOVE   RECE02X-KOHNUMHC(IDX 1) TO  KOHSKY-KOHNUMHC(IDX 1)
               MOVE   RECE02X-KOHNUMHC(IDX 2) TO  KOHSKY-KOHNUMHC(IDX 2)
               MOVE   RECE02X-KOHNUMHC(IDX 3) TO  KOHSKY-KOHNUMHC(IDX 3)
               MOVE   RECE02X-KOHNUMHC(IDX 4) TO  KOHSKY-KOHNUMHC(IDX 4)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       50
               MOVE    RECE02X-SRYKBNINF (IDX)
                                       TO  KOHSKY-SRYKBNINF (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02X-JNISSUINF (IDX)
                                       TO  KOHSKY-JNISSUINF (IDX)
               MOVE    RECE02X-TOTALTENINF (IDX)
                                       TO  KOHSKY-TOTALTENINF (IDX)
      *********MOVE    RECE02X-FTNMONEYINF(IDX)
      *********                        TO  KOHSKY-FTNMONEYINF (IDX)
               MOVE    RECE02X-FTNMONEY   (IDX)
                                       TO  KOHSKY-FTNMONEY    (IDX)
               MOVE    RECE02X-SHOKUJIINF (IDX)
                                       TO  KOHSKY-SHOKUJIINF (IDX)
               MOVE    RECE02X-YKZFTNINF  (IDX)
                                       TO  KOHSKY-YKZFTNINF   (IDX)
           END-PERFORM
           MOVE    RECE02X-SHOSHINTIME TO  KOHSKY-SHOSHINTIME
           MOVE    RECE02X-ZAITAKU     TO  KOHSKY-ZAITAKU
           MOVE    RECE02X-ZAITAKU-SAIKEI
                                       TO  KOHSKY-ZAITAKU-SAIKEI
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE02X-SHOKUJIFTNKBN
                                       TO  KOHSKY-SHOKUJIFTNKBN
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-KOHSKY          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0100 KOHSKY MCP-RC= "  MCP-RC   
               IF      FLG-KOHSKY          =   1    
                   MOVE    "公費請求ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
                   MOVE    "公費請求ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
      *
           .
       20022-KOHSKY-WRITE-EXT.
           EXIT.
           .
       2002-KOHSKY-WRITE-EXT.
           EXIT.
      *****************************************************************
      *    県単用項目ファイル読込処理
      *****************************************************************
       301-RECE11-HENSYU-SEC           SECTION.
      *
           MOVE    SPACE               TO  RECE11-HC06
      *       
           INITIALIZE                      RECE11-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE11-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE11-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE11-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE11-PTID
           MOVE    RECE02-RECEKA       TO  RECE11-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE11-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE11-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE11-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE11-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE11-TEKSTYMD
           PERFORM 990-RECE11-READ-SEC
           IF      FLG-RECE11          =   ZERO
               MOVE    RECE11-PRTDATA      TO  RECE11-HC06
           END-IF    
           .
       301-RECE11-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<ヘッダー部>処理
      *****************************************************************
       310-HC101-HEAD-HEN-SEC          SECTION.
      *
           INITIALIZE                      HC06
      *
      *    患者番号
           MOVE    RECE02-PTNUM        TO  HC06-PTNUM
      *
      *    右肩の記号
           PERFORM 3110-MIGIKATA-SET-SEC
      *
      *    提出先
           IF      RECE02-RECESYUBETU  =    "0000"  OR  "8888"
               MOVE    SPACE           TO  HC06-TEISYUTUSAKI
           ELSE    
               EVALUATE    RECE02-TEISYUTUSAKI
                   WHEN    1
                       MOVE    "１社"          TO  HC06-TEISYUTUSAKI
                   WHEN    2
                       MOVE    "２国"          TO  HC06-TEISYUTUSAKI
                   WHEN    OTHER
                       MOVE    SPACE           TO  HC06-TEISYUTUSAKI
               END-EVALUATE
           END-IF    
      *
      *    診療年月（診療年月日として退避）
           MOVE    RECE02-SRYYM        TO  WRK-SYMD
           MOVE    1                   TO  WRK-SDD
           MOVE    WRK-SYMD            TO  WRK-SRYYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  HC06-SRYY (1:2)
           MOVE    LNK-DAY2-EDTYMD1-M  TO  HC06-SRYM (1:2)
      *
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  HC06-PREFNUM
      *
      *    医療機関コード
           MOVE    SYS-1001-HOSPCDN    TO  HC06-HOSPCDN
      *
      *    保険種別１
           PERFORM 3110-HKNSBT1-SET-SEC
      *
      *    保険種別２
           PERFORM 3110-HKNSBT2-SET-SEC
      *
      *    本人・家族
           PERFORM 3110-HONKZK-SET-SEC
      *
      *    老人保険−市町村番号
           PERFORM 3110-RJNFTNJANUM-SET-SEC
      *
      *    老人保険−受給者番号
           PERFORM 3110-RJNJKYSNUM-SET-SEC
      *
      *    保険者番号・公費−負担者番号・受給者番号・診療実日数・記号番号
           PERFORM 3110-KANREN-NUM-SET-SEC
      *
      *    給付割合
           PERFORM 3110-KYURATE-SET-SEC
      *
      *    カナ氏名
           MOVE    RECE02-KANANAME     TO  HC06-KANANAME
      *
      *    氏名
           MOVE    RECE02-NAME         TO  HC06-NAME
      *
      *    性別
           EVALUATE    RECE02-SEX
           WHEN    "1"
               MOVE    "１"            TO  HC06-SEXNUM
               MOVE    "男"            TO  HC06-SEXN
           WHEN    "2"
               MOVE    "２"            TO  HC06-SEXNUM
               MOVE    "女"            TO  HC06-SEXN
           END-EVALUATE
      *
      *    生年月日
           MOVE    RECE02-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SE-WA-YMDEDIT-SEC
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    LNK-DAY1-GENGO      TO  WRK-CHAR-X
           MOVE    1                   TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  HC06-BIRTHDAY(1:)
           MOVE    RECE02-BIRTHDAY     TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD2 (1:2)
                                       TO  HC06-BIRTHDAY(3:2)
           MOVE    LNK-DAY2-EDTYMD2 (5:)
                                       TO  HC06-BIRTHDAY(5:)
      *
      *    特記事項
           PERFORM 3110-TOKKI-SET-SEC
      *
      *    職務上の事由
           IF      RECE02-HKNNUM       =   "002"
               EVALUATE    RECE02-HOJOKBN
                   WHEN    "1"
                       MOVE    "職務上"              TO  HC06-JIYU
                   WHEN    "2"
                       MOVE    "下船後３ヶ月以内"    TO  HC06-JIYU
                   WHEN    "3"
                       MOVE    "通勤災害"            TO  HC06-JIYU
               END-EVALUATE
           END-IF                 
      *
      *    医療機関所在地
           MOVE    SYS-1002-ADRS       TO  HC06-HOSPADRS
      *
      *    医療機関名称
           MOVE    SYS-1001-HOSPNAME   TO  HC06-HOSPNAME
      *
      *    医療機関ＴＥＬ
           MOVE    SYS-1002-TEL        TO  HC06-HOSPTEL
      *
      **    診療科？？？
      *     IF  RECE02-SRYKA        NOT =   SPACE
      *         MOVE    SPACE           TO  HC06-SRYKA
      *     ELSE
               MOVE    SPACE           TO  HC06-SRYKA
      *     END-IF
      *
      *    病床数
           IF    ( SYS-1001-BEDSU      >   ZERO )
           AND   ( SYS-1001-HOSPSBT    =   "1"  ) 
               IF      RECE02-BEDSUFLG     =   "1" 
                   MOVE    SYS-1001-BEDSU      TO  WRK-BEDSU
                   MOVE    WRK-BEDSU           TO  HC06-BEDSU
               ELSE
                   IF      SYS-1001-BEDSUIPN   >=  200
                       MOVE    SYS-1001-BEDSUIPN   TO  WRK-BEDSU
                       MOVE    WRK-BEDSU           TO  HC06-BEDSU
                   END-IF
               END-IF
           END-IF
      *
      *    ＯＣＲ
           IF  CNT-PAGE                =   1
               MOVE    RECE02-OCR3     TO  HC06-OCR1
               MOVE    RECE02-OCR2     TO  HC06-OCR2
               MOVE    RECE02-OCR1     TO  HC06-OCR3
           END-IF
      *
      *    県単用項目
      *    但し、レセプト種別不明のときは原因を編集する
           IF      CNT-PAGE            =   1
               IF      RECE02-RECESYUBETU  =    ZERO
                   PERFORM 3101-FUMEI-HENSYU-SEC
               ELSE 
                   IF      FLG-RECE11          =   ZERO
                       INSPECT RECE11-HC06-HEAD1-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE11-HC06-HEAD2-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       INSPECT RECE11-HC06-HEAD3-G  REPLACING   ALL "  "
                                                        BY  "　" 
                       MOVE    RECE11-HC06-HEAD1-G TO  HC06-HEAD1-G
                       MOVE    RECE11-HC06-HEAD2-G TO  HC06-HEAD2-G
                       MOVE    RECE11-HC06-HEAD3-G TO  HC06-HEAD3-G
                   END-IF
               END-IF
           END-IF
      *
           .
       310-HC101-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別不明のとき不明の原因を編集処理
      *****************************************************************
       3101-FUMEI-HENSYU-SEC              SECTION.
      *
      *
           EVALUATE    RECE02-ERR-ERRKBN
               WHEN    1
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-1
                   END-IF
                   MOVE    WRK-ERR-MSG-1   TO   HC06-HEAD1-G
               WHEN    2
               WHEN    3
                   IF      RECE02-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-2
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-2
                   END-IF
                   MOVE    RECE02-ERR-TEKSTYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKSTYMD-2
                   IF      RECE02-ERR-ERRKBN    =   3
                       MOVE    WRK-ERR-MSG-21  TO   HC06-HEAD1-G
                   ELSE
                       MOVE    WRK-ERR-MSG-2   TO   HC06-HEAD1-G
                   END-IF
               WHEN    4
               WHEN    5
                   IF      RECE02-ERR-DAY  >   ZERO
                       INITIALIZE              ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI 
                       MOVE    RECE02-ERR-DAY  TO  WRK-KGK2
                       MOVE    WRK-KGK2        TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   ZERO
                           MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  WRK-ERR-DAY-3
                       END-IF
                   END-IF
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-ERR-HKNCOMBINUM
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-ERR-HKNCOMBINUM-3
                   END-IF
                   MOVE    RECE02-ERR-TEKEDYMD
                                           TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD2
                                           TO   WRK-ERR-TEKEDYMD-3
                   IF      RECE02-ERR-ERRKBN    =   5
                       MOVE    WRK-ERR-MSG-31  TO   HC06-HEAD1-G
                   ELSE
                       MOVE    WRK-ERR-MSG-3   TO   HC06-HEAD1-G
                   END-IF
               WHEN    6
                   MOVE    WRK-ERR-MSG-4   TO   HC06-HEAD1-G
           END-EVALUATE        
           INSPECT HC06-HEAD1-G  REPLACING ALL "  "    BY  "　"
      *
           .
       3101-FUMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <右肩>編集処理
      *****************************************************************
       3110-MIGIKATA-SET-SEC           SECTION.
      *
           EVALUATE  RECE02-RECESYUBETU 
                             ALSO  RECE02-TEISYUTUSAKI
           WHEN    1111      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  HC06-MSG
               ELSE
                   MOVE    "★"        TO  HC06-MSG
               END-IF
           WHEN    1111      ALSO      1
           WHEN    1111      ALSO      ANY
               MOVE    "★"            TO  HC06-MSG
      *
           WHEN    1112      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  HC06-MSG
               ELSE
                   MOVE    "☆☆"      TO  HC06-MSG
               END-IF
           WHEN    1112      ALSO      1
               MOVE    "☆☆"          TO  HC06-MSG
      *
           WHEN    1115      ALSO      ANY
               MOVE    "★★★"        TO  HC06-MSG
      *
           WHEN    1116      ALSO      ANY
               MOVE    "☆☆☆☆"  TO  HC06-MSG
      ****     MOVE    "★★★★"      TO  HC06-MSG
      *
           WHEN    1121      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  HC06-MSG
               ELSE
                   MOVE    "★"        TO  HC06-MSG
               END-IF
           WHEN    1121      ALSO      1
               MOVE    "★"            TO  HC06-MSG
      *
           WHEN    1122      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  HC06-MSG
               ELSE
                   MOVE    "☆☆"      TO  HC06-MSG
               END-IF
           WHEN    1122      ALSO      1
               MOVE    "☆☆"          TO  HC06-MSG
      *
           WHEN    1125      ALSO      ANY
               MOVE    "★★★"        TO  HC06-MSG
      *
           WHEN    1126      ALSO      ANY
               MOVE    "☆☆☆☆"      TO  HC06-MSG
      *
           WHEN    1131      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  HC06-MSG
               ELSE
                   MOVE    "★"        TO  HC06-MSG
               END-IF
           WHEN    1131      ALSO      1
               MOVE    "★"            TO  HC06-MSG
      *
           WHEN    1132      ALSO      2
               IF  RECE02-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  HC06-MSG
               ELSE
                   MOVE    "☆☆"      TO  HC06-MSG
               END-IF
           WHEN    1132      ALSO      1
               MOVE    "☆☆"          TO  HC06-MSG
      *
           WHEN    1135      ALSO      ANY
               MOVE    "★★★"        TO  HC06-MSG
      *
           WHEN    1136      ALSO      ANY
               MOVE    "☆☆☆☆"      TO  HC06-MSG
      *
           WHEN    1211      ALSO      ANY
           WHEN    1221      ALSO      ANY
           WHEN    1231      ALSO      ANY
               MOVE    "●"            TO  HC06-MSG
      *
           WHEN    1212      ALSO      ANY
           WHEN    1222      ALSO      ANY
           WHEN    1232      ALSO      ANY
               MOVE    "○○"          TO  HC06-MSG
      *
           WHEN    1311      ALSO      ANY
           WHEN    1321      ALSO      ANY
           WHEN    1331      ALSO      ANY
               MOVE    "▲"            TO  HC06-MSG
      *
           WHEN    1312      ALSO      ANY
           WHEN    1322      ALSO      ANY
           WHEN    1332      ALSO      ANY
               MOVE    "△△"          TO  HC06-MSG
      *
           WHEN    1411      ALSO      ANY
           WHEN    1421      ALSO      ANY
           WHEN    1431      ALSO      ANY
               MOVE    "◆"            TO  HC06-MSG
      *
           WHEN    1412      ALSO      ANY
           WHEN    1422      ALSO      ANY
           WHEN    1432      ALSO      ANY
               MOVE    "◇◇"          TO  HC06-MSG
      *
           WHEN    1415      ALSO      ANY
           WHEN    1425      ALSO      ANY
           WHEN    1435      ALSO      ANY
               MOVE    "◆◆◆"        TO  HC06-MSG
      *
           WHEN    1416      ALSO      ANY
           WHEN    1426      ALSO      ANY
           WHEN    1436      ALSO      ANY
               MOVE    "◇◇◇◇"      TO  HC06-MSG
      *
           END-EVALUATE
      *
           .
       3110-MIGIKATA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別１>編集処理
      *****************************************************************
       3110-HKNSBT1-SET-SEC              SECTION.
      *
           EVALUATE    RECE02-RECESYUBETU (2:1)
           WHEN    "1"
               EVALUATE    RECE02-TEISYUTUSAKI
               WHEN    1
                   MOVE    "１社"      TO  HC06-HKNSBT1
               WHEN    2
                   MOVE    "１国"      TO  HC06-HKNSBT1
               END-EVALUATE
           WHEN    "2"
               MOVE    "２公費"        TO  HC06-HKNSBT1
           WHEN    "3"
               MOVE    "３老人"        TO  HC06-HKNSBT1
           WHEN    "4"
               MOVE    "４退職"        TO  HC06-HKNSBT1
           END-EVALUATE
      *
           .
       310-HKNSBT1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <保険種別２>編集処理
      *****************************************************************
       3110-HKNSBT2-SET-SEC              SECTION.
      *
           EVALUATE    RECE02-RECESYUBETU (3:1)
           WHEN    "1"
               MOVE    "１単独"        TO  HC06-HKNSBT2
           WHEN    "2"
               MOVE    "２２併"        TO  HC06-HKNSBT2
           WHEN    "3"
               MOVE    "３３併"        TO  HC06-HKNSBT2
           END-EVALUATE
      *
           .
       310-HKNSBT2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <本人・家族>編集処理
      *****************************************************************
       3110-HONKZK-SET-SEC             SECTION.
      *
      *    国保
           IF  RECE02-TEISYUTUSAKI     =   "2"
      *        国保　＆　老人＆退職者
               IF  ( RECE02-RJNHKNNUM    NOT =   SPACE )  OR
                   ( RECE02-HKNNUM           =   "067" )
                   EVALUATE    RECE02-RECESYUBETU (4:1)
                   WHEN    "1"
                       MOVE    "１本入"    TO  HC06-HONKZK
                   WHEN    "2"
                       MOVE    "２本外"    TO  HC06-HONKZK
                   WHEN    "5"
                       MOVE    "５家入"    TO  HC06-HONKZK
                   WHEN    "6"
                       MOVE    "６家外"    TO  HC06-HONKZK
                   END-EVALUATE
               ELSE
      *            国保　＆　老人＆退職者以外
                   IF  RECE02-HONKZKKBN    =   "1"
                       IF  RECE02-NYUGAIKBN      =   "1"
                           MOVE    "１本入"        TO  HC06-HONKZK
                       ELSE
                           MOVE    "２本外"        TO  HC06-HONKZK
                       END-IF
                   ELSE
                       IF  RECE02-NYUGAIKBN      =   "1"
                           MOVE    "５家入"        TO  HC06-HONKZK
                       ELSE
                           MOVE    "６家外"        TO  HC06-HONKZK
                       END-IF
                   END-IF
               END-IF
           ELSE
      *        国保以外
               EVALUATE    RECE02-RECESYUBETU (4:1)
               WHEN    "1"
                   MOVE    "１本入"    TO  HC06-HONKZK
               WHEN    "2"
                   MOVE    "２本外"    TO  HC06-HONKZK
               WHEN    "5"
                   MOVE    "５家入"    TO  HC06-HONKZK
               WHEN    "6"
                   MOVE    "６家外"    TO  HC06-HONKZK
               END-EVALUATE
           END-IF
      *
           .
       310-HONKZK-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <老人保険−市町村番号８桁>　編集処理
      *****************************************************************
       3110-RJNFTNJANUM-SET-SEC        SECTION.
      *
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-RJNFTNJANUM  TO  WRK-CHAR-X
           MOVE    8                   TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  HC06-RJNFTNJANUM
      *
           .
       310-RJNFTNJANUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <老人保険−受給者番号７桁>　編集処理
      *****************************************************************
       3110-RJNJKYSNUM-SET-SEC        SECTION.
      *
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-RJNJKYSNUM   TO  WRK-CHAR-X
           MOVE    7                   TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N          TO  HC06-RJNJKYSNUM
      *
           .
       3110-RJNJKYSNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  主保険−保険者番号８桁
      *          公費−負担者番号  ８桁
      *          記号・番号　診療実日数     編集処理
      *****************************************************************
       3110-KANREN-NUM-SET-SEC        SECTION.
      *
      *    <保険者番号>
           MOVE    SPACE              TO  WRK-HKNJANUM
           IF  WRK-3SYU-KBN           =   ALL "1" AND 
               RECE02-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               MOVE    RECE02-KOHFTNJANUM(SEIHIDX)  TO  WRK-HKNJANUM
           ELSE
               IF  RECE02-TEISYUTUSAKI    =   "2"
                   IF      RECE02-HKNJANUM (1:2)  =   "67"
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM
                   ELSE
      *            国保の場合（６桁右詰に編集）
                       MOVE    SPACE              TO  WRK-HKNJANUM(1:2)
                       MOVE    RECE02-HKNJANUM    TO  WRK-HKNJANUM(3:)
                   END-IF
               ELSE
                   IF  RECE02-HKNNUM      =   "001"
      *                政府管掌の場合（４桁右詰に編集）
                       MOVE    SPACE            TO  WRK-HKNJANUM(1:4)
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM(5:)
                   ELSE
      *                他の保険の場合（８桁）
                       MOVE    RECE02-HKNJANUM  TO  WRK-HKNJANUM
                   END-IF
               END-IF
           END-IF
      *    全角に変換
      **     PERFORM VARYING     IDX     FROM    1   BY  1
      **             UNTIL       IDX     >   8
      **         IF      WRK-HKNJANUM (IDX:1)  NUMERIC
      **             MOVE    WRK-HKNJANUM(IDX:1) TO  WRK-NUM-IDX
      **             IF      WRK-NUM-IDX       =   ZERO
      **                 MOVE    10              TO  WRK-NUM-IDX
      **             END-IF
      **             MOVE    TBL-SUJI(WRK-NUM-IDX)
      **                                     TO  HC06-HKNJANUM(IDX)
      **         ELSE
      **             MOVE    SPACE           TO  HC06-HKNJANUM(IDX)
      **         END-IF
      **     END-PERFORM
      *
             MOVE   WRK-HKNJANUM    TO    HC06-HKNJANUM
      *
      *
      *    <記号・番号>
           IF  WRK-3SYU-KBN           =   ALL "1" AND 
               RECE02-HKNNUM          =   SPACE            
      *        ３種の場合（８桁）
               IF      RECE02-KOHJKYSNUM(SEIHIDX)
                                      NOT =   SPACE
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KOHJKYSNUM(SEIHIDX)
                                           TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  HC06-KIGO
                   END-IF
               END-IF        
           ELSE
               IF      RECE02-KIGO     NOT =   SPACE 
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-KIGO       TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-KIGO
                   END-IF
               END-IF
               IF      RECE02-NUM      NOT =   SPACE     
                   INITIALIZE              ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI 
                   MOVE    RECE02-NUM      TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   ZERO
                       MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                               TO  HC06-NUM
                   END-IF
               END-IF        
           END-IF
      *
      *    <公費負担者番号・公費受給者番号>
      *    
           IF  WRK-3SYU-KBN           =   ALL "1" AND 
               RECE02-HKNNUM          =   SPACE            
      *        ３種の場合
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE    
      *        ３種以外の場合
               MOVE    1               TO  IDA
               MOVE    2               TO  IDB
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
           END-IF
      *    公負１
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-KOHFTNJANUM(IDA)
                               TO  WRK-CHAR-X
           MOVE    8           TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N  TO  HC06-KOHFTNJANUM1
      *    公負２
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-KOHFTNJANUM(IDB)
                               TO  WRK-CHAR-X
           MOVE    8           TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N  TO  HC06-KOHFTNJANUM2
      *    公受１
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-KOHJKYSNUM(IDA)
                               TO  WRK-CHAR-X
           MOVE    8           TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N  TO  HC06-KOHJKYSNUM1
      *    公受２
           INITIALIZE          WRK-CHAR-HENKAN-AREA
           MOVE    RECE02-KOHJKYSNUM(IDB)
                               TO  WRK-CHAR-X
           MOVE    8           TO  WRK-CHAR-CNT
           PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
           MOVE    WRK-CHAR-N  TO  HC06-KOHJKYSNUM2
      *
      *    <診療実日数>
           IF      CNT-PAGE        =   1
               MOVE    RECE02-JNISSU (IDX1)
                                       TO  WRK-JINISSUZ
               MOVE    WRK-JINISSU     TO  HC06-JINISSU
               IF  RECE02-JNISSU (IDX1)
                                   NOT =   RECE02-JNISSU (IDX2)
                   MOVE    RECE02-JNISSU (IDX2)
                                           TO  WRK-JINISSUZ
                   MOVE    WRK-JINISSU     TO  HC06-JINISSU1
               END-IF
               IF  RECE02-JNISSU (IDX1)
                                   NOT =   RECE02-JNISSU (IDX3)
                   MOVE    RECE02-JNISSU (IDX3)
                                           TO  WRK-JINISSUZ
                   MOVE    WRK-JINISSU     TO  HC06-JINISSU2
               END-IF
           END-IF
      *
           .
       3110-KANREN-NUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <給付割合>　編集処理
      *****************************************************************
       3110-KYURATE-SET-SEC        SECTION.
      *
           MOVE   SPACE              TO  HC06-KYURATE
      *          
      *    国保OR退職者のみ印字
           IF  ((RECE02-TEISYUTUSAKI =   "2"  )       AND
                (RECE02-RJNHKNNUM    =   SPACE))          OR
               ( RECE02-HKNNUM       =   WRK-RETIRE1  OR
                                         WRK-RETIRE2  OR
                                         WRK-RETIRE3  OR
                                         WRK-RETIRE4  OR
                                         WRK-RETIRE5  OR
                                         WRK-RETIRE6 )
               COMPUTE WRK-KYURATE-9   =   (100     -   RECE02-KYURATE)
                                           / 10 
               IF      WRK-KYURATE-9       >   ZERO
                   MOVE    WRK-KYURATE-9       TO  WRK-KYURATE
                   INITIALIZE                  WRK-CHAR-HENKAN-AREA
                   MOVE    WRK-KYURATE         TO  WRK-CHAR-X
                   MOVE    2                   TO  WRK-CHAR-CNT
                   PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
                   MOVE   "給割"               TO  HC06-KYURATE
                   MOVE    WRK-CHAR-N          TO  HC06-KYURATE(5:)
               END-IF
           END-IF
           .
       3110-KYURATE-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票  <特記事項>　編集処理
      *****************************************************************
       3110-TOKKI-SET-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDZ
      *     
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL ( IDX                 >   5     )
                   OR    ( RECE02-TOKKI(IDX)   =   SPACE )
                   OR    ( IDZ                 =   4     ) 
      *
               MOVE    SPACE                   TO  HC06-TOKKI (IDX)
               PERFORM VARYING IDX2    FROM    1   BY  1
                       UNTIL ( IDX2            >   11    )
                       OR    ( HC06-TOKKI (IDX)
                                           NOT =   SPACE )
                   IF      RECE02-TOKKI(IDX)   =   TBL-TOKKICD(IDX2)
                       ADD     1                TO  IDZ
                       MOVE    TBL-TOKKINAME(IDX2)
                                                TO  HC06-TOKKI (IDZ)
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    県単用項目
           IF      FLG-RECE11           =   ZERO
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX                 >   4
                       OR      IDZ                 =   4     
                       OR      RECE11-HC06-TOKKI(IDX)  =   SPACE
                   ADD     1                    TO  IDZ    
                   MOVE    RECE11-HC06-TOKKI (IDX)           
                                                TO  HC06-TOKKI (IDZ)
               END-PERFORM
           END-IF
      *
           .
       3110-TOKKI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       320-HC101-BYOMEI-HEN-SEC      SECTION.
      *
      *    パラメータ０３ファイルより編集
           MOVE    LOW-VALUE           TO  WRK-BYO-OLD
      *     
           INITIALIZE                  WRK-EDIT-AREA
           MOVE    SPACE               TO  WRK-BYOMEI1
                                           WRK-BYOMEI2
           MOVE    ZERO                TO  FLG-TENKI                                
      *     
           INITIALIZE                      RECE031-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE031-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE031-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE031-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE031-PTID
           MOVE    RECE02-RECEKA       TO  RECE031-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE031-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE031-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE031-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE031-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE031-TEKSTYMD
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
               NOT INVALID
                   PERFORM 990-RECE031-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-RECE031          =   1                    )
grpsys         OR     ( RECE02-HOSPNUM   NOT =   RECE031-HOSPNUM      )
               OR     ( RECE02-SRYYM     NOT =   RECE031-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE031-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE031-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE031-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE031-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE031-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE031-HKNJANUM     )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE031-HOJOKBN      )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE031-TEKSTYMD     )
      *        転帰区分:8(移行)以外 処理対象
      ******   IF  RECE031-TENKIKBN     NOT =   "8"
      *
      *********    病名編集処理
      *********    PERFORM 3210-BYOMEI-SET-SEC
      *********END-IF
      *        病名編集処理
               PERFORM 3210-BYOMEI-SET-SEC
      *
      ******** PERFORM 990-RECE031-NEXT-SEC
           END-PERFORM
      *
           MOVE    1               TO  FLG-RECE031     
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
      *
           .
       320-HC101-BYOMEI-HEN-EXT.
           EXIT.
      ****************************************************************
      *     病名編集処理
      ****************************************************************
       3210-BYOMEI-SET-SEC           SECTION.
      *
      *****MOVE    SPACE                TO  WRK-HENFLG1
      *                                     WRK-HENFLG2
      *    疑いフラグがセットされているとき     
      *    IF      RECE031-UTAGAIFLG    NOT =   SPACE
      *        COMPUTE WRK-MOJISU   =   RECE031-BYOMEIMOJI   *   2
      *        IF      RECE031-UTAGAIFLG    =   "1" OR  "3"
      *            IF      WRK-MOJISU       <   7
      *                MOVE    1                TO  IDZ
      *            ELSE    
      *                COMPUTE IDZ     =   WRK-MOJISU    -   7
      *            END-IF    
      *            PERFORM VARYING IDX FROM    IDZ BY  2
      *                    UNTIL   IDX >       WRK-MOJISU
      *                IF      RECE031-BYOMEI (IDX:2)      =   "疑"
      *                    MOVE    "1"              TO  WRK-HENFLG1
      *                    MOVE    WRK-MOJISU       TO  IDX
      *                END-IF
      *            END-PERFORM
      *        END-IF                
      *        IF      RECE031-UTAGAIFLG    =   "2" OR  "3"
      *            PERFORM VARYING IDX FROM    1   BY  4
      *                    UNTIL   IDX >       WRK-MOJISU
      *                IF      RECE031-BYOMEI (IDX:4)      =   "急性"
      *                    MOVE    "1"              TO  WRK-HENFLG2
      *                    MOVE    WRK-MOJISU       TO  IDX
      *                END-IF
      *            END-PERFORM
      *        END-IF
      *****END-IF    
      *    連結表示のとき
           IF      SYS-2005-BYOMEIHENKBN    =   "0"
      *        前のデータと同一主病名フラグ・診療開始日・転帰区分
               IF  ( RECE031-SYUBYOFLG    =   WRK-BO-SYUBYOFLG )  AND
                   ( RECE031-SRYYMD       =   WRK-BO-SRYYMD    )  AND
                   ( RECE031-TENKIKBN     =   WRK-BO-TENKIKBN  )  AND 
                   ( RECE031-TENKIYMD     =   WRK-BO-TENKIYMD  )   
      *
      *            病名をつなげると何桁になるかを計算（，＋病名）
                   IF  WRK-CLM           =   ZERO
                       MOVE    WRK-CLM   TO  WRK-WCLM
                   ELSE
                       COMPUTE WRK-WCLM  =   WRK-CLM   +   2
                   END-IF
                   COMPUTE     WRK-WCLM  =
                              ( RECE031-BYOMEIMOJI  *   2 )
                                                    +   WRK-WCLM
      *************疑い分を追加計算
      *            IF      RECE031-UTAGAIFLG     =   "1" OR  "3"
      *            AND     WRK-HENFLG1           =   SPACE
      *                COMPUTE WRK-WCLM      =   
      *                            WRK-WCLM  +   WRK-CONS-UTAGAI-MOJISU
      *            END-IF
      *            IF      RECE031-UTAGAIFLG     =   "2" OR  "3"
      *            AND     WRK-HENFLG2           =   SPACE
      *                COMPUTE WRK-WCLM      =   
      *                            WRK-WCLM  +   WRK-CONS-KYUSEI-MOJISU
      *************END-IF    
      *
                   EVALUATE    TRUE
      *                傷病名欄に編集
                       WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI1-MAX
                               PERFORM 3221-BYOMEI1-OUT-SEC
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                            前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI1-NAME
                                                           (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
      *                適用欄に編集
                       WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                           IF  WRK-WCLM          >   WRK-BYOMEI2-MAX
                               MOVE    1             TO  FLG-TENKI
                               PERFORM 3222-BYOMEI2-OUT-SEC
                               MOVE    ZERO          TO  FLG-TENKI
                           ELSE
                               IF  WRK-CLM       >   ZERO
      *                        前の病名の直後に編集
                                   COMPUTE WRK-STCLM =   WRK-CLM   +
                                                         1
                                   MOVE    "，"  TO  WRK-BYOMEI2-NAME
                                                       (WRK-STCLM:)
                                   COMPUTE WRK-CLM   =   WRK-CLM   +
                                                         2
                               END-IF
                           END-IF
                   END-EVALUATE
               ELSE
      *            主病名フラグ・診療開始日・転帰区分 が異なる場合
                   IF      WRK-BYO-OLD        =   LOW-VALUE
                       CONTINUE
                   ELSE
                       IF  WRK-LINEBYO       <  WRK-BYOMEI-LINE
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
                   END-IF    
               END-IF
           ELSE
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
           END-IF
      *
      *    主病名との区切り線編集処理     
           PERFORM 3211-SYUBYOFLG-HENSYU-SEC
      *
      *    対象病名を編集
           COMPUTE     WRK-STCLM         =   WRK-CLM   +   1
      *    単病名文字数を計算
           COMPUTE WRK-MOJISU          =   RECE031-BYOMEIMOJI   *   2
           MOVE    RECE031-BYOMEI      TO  WRK-BYOMEI
      *
      *****疑いフラグがセットされているとき     
      *    IF      RECE031-UTAGAIFLG   =   "1" OR  "3"
      *    AND     WRK-HENFLG1         =   SPACE                         
      *        MOVE    SPACE               TO  WRK-BYOMEI
      *        STRING  RECE031-BYOMEI      DELIMITED BY  SPACE
      *                WRK-CONS-UTAGAI     DELIMITED BY  SIZE
      *                INTO                WRK-BYOMEI
      *        COMPUTE WRK-MOJISU          =      
      *                         WRK-MOJISU +   WRK-CONS-UTAGAI-MOJISU
      *    END-IF    
      *    IF      RECE031-UTAGAIFLG   =   "2" OR  "3"
      *    AND     WRK-HENFLG2         =   SPACE                         
      *        MOVE    SPACE               TO  WRK-BYOMEIW
      *        STRING  WRK-CONS-KYUSEI     DELIMITED BY  SIZE
      *                WRK-BYOMEI          DELIMITED BY  SPACE
      *                INTO                WRK-BYOMEIW
      *        COMPUTE WRK-MOJISU          =      
      *                         WRK-MOJISU +   WRK-CONS-KYUSEI-MOJISU
      *        MOVE    WRK-BYOMEIW         TO  WRK-BYOMEI  
      *****END-IF
      *
           EVALUATE    TRUE
      *        傷病名欄に編集
               WHEN    WRK-LINEBYO     <   WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI1-MAX
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM 3221-BYOMEI1-OUT-SEC
                       IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE 
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI1-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE
                       ELSE
                           MOVE    WRK-BYOMEI(49:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                           MOVE    1               TO  WRK-CONTINUE    
                       END-IF                                   
                   END-IF    
      *        適用欄に編集
               WHEN    WRK-LINEBYO     >=  WRK-BYOMEI-LINE
                   MOVE    WRK-BYOMEI      TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   IF      WRK-MOJISU       >   WRK-BYOMEI2-MAX 
                       MOVE    RECE031-SRYYMD  TO  WRK-BO-SRYYMD
                       MOVE    RECE031-TENKIKBN
                                               TO  WRK-BO-TENKIKBN
                       MOVE    RECE031-TENKIYMD
                                               TO  WRK-BO-TENKIYMD
                       PERFORM  3222-BYOMEI2-OUT-SEC
                       MOVE    1               TO  WRK-CONTINUE
                       MOVE    WRK-BYOMEI(45:) TO  WRK-BYOMEI2-NAME
                                                          (WRK-STCLM:)
                   END-IF                                       
           END-EVALUATE
      *
           COMPUTE     WRK-CLM           =   WRK-MOJISU
                                         +   WRK-CLM
      *     
           MOVE    RECE031-SYUBYOFLG     TO  WRK-BO-SYUBYOFLG
           MOVE    RECE031-SRYYMD        TO  WRK-BO-SRYYMD
           MOVE    RECE031-TENKIKBN      TO  WRK-BO-TENKIKBN
           MOVE    RECE031-TENKIYMD      TO  WRK-BO-TENKIYMD
      *
           PERFORM 990-RECE031-NEXT-SEC
      *
           .
       3210-BYOMEI-SET-EXT.
           EXIT.
      ****************************************************************
      *      主病名と副病名の区切り線の編集
      ****************************************************************
       3211-SYUBYOFLG-HENSYU-SEC         SECTION.
      *
      *    主病名との区切り線（副傷病名があるときのみ）     
           IF      WRK-BYO-OLD        =   LOW-VALUE
               CONTINUE
           ELSE
               IF      FLG-RECE031             =   ZERO
               AND     RECE031-SYUBYOFLG   NOT =   WRK-BO-SYUBYOFLG 
               AND     WRK-BO-SYUBYOFLG        =   "1"
                   IF      WRK-LINEBYO     <   WRK-BYOMEI-LINE
                       ADD     1           TO  WRK-LINEBYO
                       MOVE    WRK-KUGIRI3 TO
                                          HC06-BYOMEI(WRK-LINEBYO)(2:52)
                   ELSE
                       IF      WRK-LINETEK >=   WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF    
                       ADD     1           TO  WRK-LINETEK
                       MOVE    WRK-KUGIRI1 TO
                                           HC06-TEKIYO (WRK-LINETEK)
                   END-IF
               END-IF
           END-IF        
      *
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
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE          WRK-CHAR-HENKAN-AREA
               MOVE    WRK-LINE (2:1)  TO  WRK-CHAR-X
               MOVE    1               TO  WRK-CHAR-CNT
               PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
               STRING  "（"            DELIMITED  BY  SIZE
                       WRK-CHAR-N      DELIMITED  BY  SPACE
                       "）"            DELIMITED  BY  SIZE
                                       INTO    WRK-BYOMEI1-NO 
               END-STRING
      *        診療開始日
               MOVE    WRK-BYOMEI1-NO      TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  HC06-SRYYMD
                                                    (WRK-LINEBYO)(7:)
      *        転帰
               MOVE    WRK-BYOMEI1-NO    TO  HC06-TENKI(WRK-LINEBYO)
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "2"
                   MOVE    "死亡"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    "3"
                   MOVE    "中止"    TO  HC06-TENKI(WRK-LINEBYO)(7:)
               WHEN    OTHER
                   MOVE    SPACE     TO  HC06-TENKI(WRK-LINEBYO)
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                           TO  HC06-TENKIYMD
                                                    (WRK-LINEBYO)
               END-IF                                     
           END-IF
      *    病名
           MOVE    WRK-BYOMEI1     TO  HC06-BYOMEI(WRK-LINEBYO)
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI1
           MOVE    ZERO            TO  WRK-CLM
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
      *        病名の項番カウント
               ADD     1                   TO  WRK-LINE
      *        項番
               INITIALIZE          WRK-CHAR-HENKAN-AREA
               IF  WRK-LINE            <   10
                   MOVE    WRK-LINE (2:1)  TO  WRK-CHAR-X
                   MOVE    1               TO  WRK-CHAR-CNT
               ELSE    
                   MOVE    WRK-LINE (1:2)  TO  WRK-CHAR-X
                   MOVE    2               TO  WRK-CHAR-CNT
               END-IF
               PERFORM 31101-NUM-ZENKAKU-HENKAN-SEC
               STRING      "（"            DELIMITED  BY  SIZE
                           WRK-CHAR-N      DELIMITED  BY  SPACE
                           "）"            DELIMITED  BY  SIZE
                                           INTO    WRK-BYOMEI2-NO 
               END-STRING
           END-IF    
      *
      *    診療開始日・転帰を印字するスペースが有る場合
           IF      SYS-2005-TENKIHENKBN    =   ZERO
               IF  WRK-BYOMEI2-ELSE    =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           ELSE
               IF  WRK-BYOMEI2R-ELSE   =   SPACE
      *            診療年月日・転帰　編集処理
                   PERFORM 32221-SRYYMD-TENKI-HEN-SEC
                   MOVE    ZERO            TO  FLG-TENKI
               END-IF
           END-IF    
      *
           MOVE    WRK-BYOMEI2 TO  HC06-TEKIYO (WRK-LINETEK)
      *
      *    最後の病名に印字するスペースがなかったとき 
           IF       FLG-TENKI              =   1     
      *****OR       WRK-BYOMEI2-ELSE    NOT =   SPACE
      *        診療年月日・転帰　編集処理
               MOVE    SPACE           TO  WRK-BYOMEI2
               PERFORM 32221-SRYYMD-TENKI-HEN-SEC
      *        適用欄の行カウント
               IF      WRK-LINETEK     >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               MOVE    WRK-BYOMEI2     TO  HC06-TEKIYO (WRK-LINETEK)
           END-IF
      *
      *
      *    クリア
           MOVE    SPACE           TO  WRK-BYOMEI2
           MOVE    ZERO            TO  WRK-CLM
           MOVE    ZERO            TO  WRK-CONTINUE
      *
           .
       3222-BYOMEI2-OUT-EXT.
           EXIT.
      *****************************************************************
      *    診療年月日・転帰　編集処理
      *****************************************************************
       32221-SRYYMD-TENKI-HEN-SEC    SECTION.
      *
      *    転帰日を編集しないとき
           IF      SYS-2005-TENKIHENKBN    =   ZERO
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2-YMD
      *        転帰
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
           ELSE
      *        診療開始日
               MOVE    WRK-BO-SRYYMD       TO  WRK-SYMD
               PERFORM 31012-SEIWA-HEN-SEC
               MOVE    LNK-DAY2-EDTYMD3 (5:)
                                           TO  WRK-BYOMEI2R-YMD
      *        転帰
               EVALUATE    WRK-BO-TENKIKBN
               WHEN    "1"
                   MOVE    "治ゆ"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "2"
                   MOVE    "死亡"          TO  WRK-BYOMEI2R-TENKI
               WHEN    "3"
                   MOVE    "中止"          TO  WRK-BYOMEI2R-TENKI
               WHEN    OTHER
                   MOVE    SPACE           TO  WRK-BYOMEI2R-TENKI
               END-EVALUATE
      *        転帰日
               IF      WRK-BO-TENKIYMD     =   SPACE
                   CONTINUE 
               ELSE
                   MOVE    WRK-BO-TENKIYMD     TO  WRK-SYMD
                   PERFORM 31012-SEIWA-HEN-SEC
                   MOVE    LNK-DAY2-EDTYMD3 (17:)
                                               TO  WRK-BYOMEI2R-TENKIYMD
               END-IF                                
           END-IF                                     
      *
           .
       32221-SRYYMD-TENKI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票編集<点数>処理
      *****************************************************************
       330-HC101-TENSU-HEN-SEC      SECTION.
      *
           EVALUATE  RECE02-SHOSHINTIME
               WHEN  "1"
                     MOVE  "（時間外）"   TO  HC06-SHOSHINTIME
               WHEN  "2"
                     MOVE  "（深夜）"     TO  HC06-SHOSHINTIME
               WHEN  "3"
                     MOVE  "（休日）"     TO  HC06-SHOSHINTIME
               WHEN  "4"
                     MOVE  "時外・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "5"
                     MOVE  "時外・休日"   TO  HC06-SHOSHINTIME
               WHEN  "6"
                     MOVE  "休日・深夜"   TO  HC06-SHOSHINTIME
               WHEN  "7"
                     MOVE  "外・休・深"   TO  HC06-SHOSHINTIME
           END-EVALUATE
           IF      RECE02-HKNNUM      =   SPACE
               IF      SEIHIDX            >   ZERO 
                   COMPUTE IDX1    =   SEIHIDX   +   1
                   MOVE    2                  TO  IDX2
                   IF      IDX1               =   IDX2
                       COMPUTE  IDX2  =  IDX1  +  1
                   END-IF
               ELSE
                   MOVE    2                  TO  IDX1
                   MOVE    3                  TO  IDX2
               END-IF 
           ELSE 
               MOVE    1                  TO  IDX1
               MOVE    2                  TO  IDX2
           END-IF      

      *            
           PERFORM   VARYING   IDX   FROM    1   BY  1
                     UNTIL     IDX       >   WRK-TEKIYOU-LINE
      *        <単価>
               EVALUATE    IDX
      *        再診
               WHEN    2
               WHEN    3
               WHEN    4
               WHEN    5
               WHEN    6
                   COMPUTE   WRK-TNSIDX          =   IDX   -   1
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN
                                                         (WRK-TNSIDX)
      *        投薬
               WHEN    15
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN1
               WHEN    18
               WHEN    19
                   COMPUTE   WRK-TNSIDX          =   IDX   -   17
                   MOVE    RECE02-KHNTEN (IDX)   TO  WRK-KHNTENZ
                   MOVE    WRK-KHNTEN            TO  HC06-KHNTEN2
                                                          (WRK-TNSIDX)
               WHEN    OTHER
                   CONTINUE
               END-EVALUATE
      *        <回数>
               MOVE    RECE02-KAISU(IDX)         TO  WRK-KAISUZ
               MOVE    WRK-KAISU                 TO  HC06-KAISU   (IDX)
      *        <点数・公費点数>
               MOVE    RECE02-TENSU(IDX IDX1)    TO  WRK-TENSUZ
               MOVE    WRK-TENSU                 TO  HC06-TENSU   (IDX)
               IF      IDX2                      >   ZERO
                   MOVE    RECE02-TENSU(IDX IDX2)
                                                 TO  WRK-KOUTENZ
                   MOVE    WRK-KOUTEN            TO  HC06-KOUTEN  (IDX)
               END-IF
           END-PERFORM
      *
      *        <請求点・薬剤一部負担金・一部負担金>
      *
           IF  WRK-3SYU-KBN           =   ALL "1" AND 
               RECE02-HKNNUM          =   SPACE            
               IF  KAKKIDX         >   SESNIDX
                   MOVE    SESNIDX     TO  IDA
                   MOVE    KAKKIDX     TO  IDB
               ELSE
                   MOVE    KAKKIDX     TO  IDA
                   MOVE    SESNIDX     TO  IDB
               END-IF
               COMPUTE IDX1     =   SEIHIDX    +   1
               COMPUTE IDX2     =   IDA        +   1
               COMPUTE IDX3     =   IDB        +   1
           ELSE
               MOVE    1               TO  IDX1
               MOVE    2               TO  IDX2
               MOVE    3               TO  IDX3
           END-IF    
      *    保険
           IF      RECE02-TOTALTEN (IDX1)     >   ZERO
               MOVE    RECE02-TOTALTEN (IDX1) TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC06-TOTALTEN(1)
               IF      RECE02-YKZFTN   (IDX1) >   ZERO
                   MOVE    RECE02-YKZFTN   (IDX1) TO  WRK-KGK9
                   MOVE    WRK-KGK9               TO  HC06-YKZFTN  (1)
               END-IF    
               IF      RECE02-FTNMONEY (IDX1) >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX1) TO  WRK-KGK9
                   MOVE    WRK-KGK9               TO  HC06-FTNMONEY(1)
               END-IF
           END-IF
      *    公費１
           IF    ( RECE02-TOTALTEN (IDX2) >   ZERO                     )
           AND   ((RECE02-TOTALTEN (IDX2)
                                      NOT =   RECE02-TOTALTEN (IDX1)) OR
                  (RECE02-FTNMONEY (IDX2)
                                      NOT =   RECE02-FTNMONEY (IDX1))  )
      *********IF      RECE02-YKZFTN   (IDX2) >   ZERO
      *             MOVE    RECE02-YKZFTN   (IDX2) TO  WRK-KGK9
      *             MOVE    WRK-KGK9               TO  HC06-YKZFTN  (2)
      *********END-IF    
               IF    RECE02-FTNMONEY (IDX2)   >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX2) TO  WRK-KGK9
                   MOVE    WRK-KGK9               TO  HC06-FTNMONEY(2)
               END-IF
           END-IF
           IF      RECE02-TOTALTEN (IDX2) >   ZERO                     
           AND     RECE02-TOTALTEN (IDX2)
                                      NOT =   RECE02-TOTALTEN (IDX1)   
               MOVE    RECE02-TOTALTEN (IDX2) TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC06-TOTALTEN(2)
           END-IF
           IF      RECE02-YKZFTN   (IDX2) >   ZERO
           AND     RECE02-YKZFTN   (IDX2) 
                                      NOT =   RECE02-YKZFTN (IDX1)
               MOVE    RECE02-YKZFTN   (IDX2) TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC06-YKZFTN  (2)
           END-IF    
      *    公費２
           IF    (RECE02-TOTALTEN (IDX3) >   ZERO                      )
           AND   ((RECE02-TOTALTEN (IDX3)
                                      NOT =   RECE02-TOTALTEN (IDX1)) OR
                  (RECE02-FTNMONEY (IDX3)
                                      NOT =   RECE02-FTNMONEY (IDX1))  )
      *
             IF  ( RECE02-TOTALTEN (IDX3) =   RECE02-TOTALTEN (IDX1)   )
             AND ( RECE02-FTNMONEY (IDX3) =   RECE02-FTNMONEY (IDX2)   )
               CONTINUE
             ELSE
      **       MOVE    RECE02-TOTALTEN (IDX3) TO  WRK-KGK9
      **       MOVE    WRK-KGK9               TO  HC06-TOTALTEN(3)
      *********IF      RECE02-YKZFTN   (IDX3) >   ZERO
      *             MOVE    RECE02-YKZFTN   (IDX3) TO  WRK-KGK9
      *             MOVE    WRK-KGK9               TO  HC06-YKZFTN  (3)
      *********END-IF    
               IF    RECE02-FTNMONEY (IDX3)   >   ZERO
                   MOVE    RECE02-FTNMONEY (IDX3) TO  WRK-KGK9
                   MOVE    WRK-KGK9               TO  HC06-FTNMONEY(3)
               END-IF
             END-IF
      * 
           END-IF
           IF      RECE02-TOTALTEN (IDX3) >   ZERO                      
           AND     RECE02-TOTALTEN (IDX3)
                                      NOT =   RECE02-TOTALTEN (IDX1)
               MOVE    RECE02-TOTALTEN (IDX3) TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC06-TOTALTEN(3)
           END-IF
           IF      RECE02-YKZFTN   (IDX3) >   ZERO
           AND     RECE02-YKZFTN   (IDX3) 
                                      NOT =   RECE02-YKZFTN (IDX1)
               MOVE    RECE02-YKZFTN   (IDX3) TO  WRK-KGK9
               MOVE    WRK-KGK9               TO  HC06-YKZFTN  (3)
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
      *    摘要欄に病名が表示されている場合、１行あける
           IF  WRK-LINETEK             >   ZERO
      *
               IF  WRK-LINETEK          >=   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF    
               ADD     1               TO  WRK-LINETEK
               MOVE    WRK-KUGIRI1 TO  HC06-TEKIYO (WRK-LINETEK)
           END-IF
      *
           MOVE    ZERO                TO  WRK-R061-ZAINUM-O
           MOVE    SPACE               TO  WRK-R061-SRYKBN-O
           MOVE    ZERO                TO  WRK-R061-KOHBUNRI-O
           MOVE    ZERO                TO  CNT-RECE061
      *
      *    パラメータ０６ファイルより編集
           INITIALIZE                      RECE061-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE061-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE061-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE061-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE061-PTID
           MOVE    RECE02-RECEKA       TO  RECE061-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE061-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE061-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE061-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE061-HOJOKBN
           MOVE    RECE02-TEKSTYMD     TO  RECE061-TEKSTYMD
      *
           START   RECE061-FILE
               KEY IS    >=   RECE061-KEY
               INVALID
                   MOVE    1            TO  FLG-RECE061
               NOT INVALID
                   PERFORM 990-RECE061-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE061           =   1 )
grpsys         OR     ( RECE02-HOSPNUM   NOT =   RECE061-HOSPNUM     )
               OR     ( RECE02-SRYYM     NOT =   RECE061-SRYYM       )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE061-NYUGAIKBN   )
               OR     ( RECE02-PTID      NOT =   RECE061-PTID        )
               OR     ( RECE02-RECEKA    NOT =   RECE061-RECEKA      )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE061-TEISYUTUSAKI)
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE061-RECESYUBETU )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE061-HKNJANUM    )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE061-HOJOKBN     )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE061-TEKSTYMD    )
      *
               MOVE    ZERO                  TO  FLG-KUGIRI
               IF  RECE061-ZAINUM        NOT =   WRK-R061-ZAINUM-O
                   MOVE    ZERO              TO  CNT-ZAIMEISAISU
               END-IF
               ADD     1                     TO  CNT-ZAIMEISAISU
      *
      *        レセプト０６ファイル編集処理
               PERFORM 3410-RECE061-HENSYU-SEC
      *
               MOVE    RECE061-ZAINUM        TO  WRK-R061-ZAINUM-O
               MOVE    RECE061-SRYKBN        TO  WRK-R061-SRYKBN-O
               MOVE    RECE061-KOHBUNRI      TO  WRK-R061-KOHBUNRI-O
      *
               PERFORM 990-RECE061-NEXT-SEC
           END-PERFORM
      *
           .
       340-HC101-ZAI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    レセプト０６ファイル編集処理
      *****************************************************************
       3410-RECE061-HENSYU-SEC           SECTION.
      *
           IF  CNT-ZAIMEISAISU           =   1
      *        次の行為明細数を追加すると何行になるか
               COMPUTE   WRK-WLINETEK    =   WRK-LINETEK
                                         +   RECE061-ZAIMEISAISU
               IF  CNT-RECE061           >   1
      *            公費分離区分が変更している場合、公費区切り線分を追加
                   IF  RECE061-KOHBUNRI  NOT =   WRK-R061-KOHBUNRI-O
                       ADD     1             TO  WRK-WLINETEK
                       MOVE    2             TO  FLG-KUGIRI
                   ELSE
      *                診療区分が変更している場合、診療区切り線分を追加
                       IF  RECE061-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                           ADD     1             TO  WRK-WLINETEK
                           MOVE    1             TO  FLG-KUGIRI
                       END-IF
                   END-IF
               END-IF
      *
               IF  WRK-WLINETEK          >   WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
      *
      *            区切り線
                   IF  FLG-KUGIRI        =   2
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1               TO  WRK-LINETEK
                   MOVE    RECE061-SRYKBN  TO  HC06-SRYKBN
                                                       (WRK-LINETEK)
               ELSE
      *            区切り線
                   IF  FLG-KUGIRI    NOT =   ZERO
                       PERFORM 34110-KUGIRI-LINE-SEC
                   END-IF
      *            診療区分
                   ADD     1             TO  WRK-LINETEK
                   IF  RECE061-SRYKBN    NOT =   WRK-R061-SRYKBN-O
                       MOVE    RECE061-SRYKBN
                                   TO  HC06-SRYKBN  (WRK-LINETEK)
                   END-IF
               END-IF
           ELSE
               ADD     1                 TO  WRK-LINETEK
           END-IF
      *
      *    明細
           INSPECT RECE061-TEKIYO-INF
                           REPLACING    ALL   "  "   BY   "　"
           MOVE    RECE061-TEKIYO-INF  TO  HC06-TEKIYO(WRK-LINETEK)
      *
           .
       3410-RECE061-HENSYU-EXT.
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
           WHEN    2
               PERFORM 34110-KOHNAME-SET-SEC
               MOVE    WRK-KUGIRI2 TO  HC06-TEKIYO (WRK-LINETEK)
           WHEN    1
               MOVE    WRK-KUGIRI1 TO  HC06-TEKIYO (WRK-LINETEK)
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
      *    帳票編集<ヘッダー部>処理
           ADD     1             TO  CNT-PAGE
           PERFORM 310-HC101-HEAD-HEN-SEC
           MOVE    ZERO          TO  WRK-LINETEK
      *
           .
       34120-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    区切り線　公費名セット処理
      *****************************************************************
       34110-KOHNAME-SET-SEC          SECTION.
      *
           MOVE    SPACE              TO  WRK-KUGIRI2-KOHNAME
      *
           MOVE    SPACE              TO  HKNNUM-REC
grpsys     MOVE    SPA-HOSPNUM        TO  HKN-HOSPNUM
           MOVE    RECE02-KOHNUM (RECE061-KOHBUNRI)
                                      TO  HKN-HKNNUM
           MOVE    WRK-SRYYMD         TO  HKN-TEKSTYMD
                                          HKN-TEKEDYMD
      *
           MOVE    HKNNUM-REC         TO  MCPDATA-REC
           MOVE   "tbl_hknnum"     TO  MCP-TABLE
           MOVE   "key5"           TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE   "tbl_hknnum"     TO  MCP-TABLE
               MOVE   "key5"           TO  MCP-PATHNAME
               PERFORM 910-HKNMST-NEXT-SEC
           ELSE
               MOVE    1              TO  FLG-HKNNUM
           END-IF
           MOVE    "tbl_hknnum"    TO  MCP-TABLE
           MOVE    "key5"          TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           IF      FLG-HKNNUM         =   ZERO   
               MOVE    HKN-TANSEIDONAME  TO  WRK-KUGIRI2-KOHNAME
               INSPECT WRK-KUGIRI2-KOHNAME
                       REPLACING    ALL   "  "   BY   "　"
           END-IF
      *
           .
       34110-KOHNAME-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<補足>処理
      *****************************************************************
       350-HC101-HOSOKU-HEN-SEC      SECTION.
      *
      *****IF      RECE02-HOSOKUKBN  =   "1"
      *****    MOVE    WRK-HOSOKU1       TO  HC06-HOSOKU
      *****END-IF      
      *
      *    県単用項目
           IF      FLG-RECE11           =   ZERO
               MOVE    RECE11-HC06-BODY     TO  HC06-BODY
               MOVE    RECE11-HC06-FOOT-G   TO  HC06-FOOT-G
           END-IF    
      *
           .
       350-HC101-HOSOKU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<コメント>処理
      *****************************************************************
       360-HC101-COMMENT-HEN-SEC      SECTION.
      *
           MOVE    ZERO                TO  WRK-RECECOM
      *     
           INITIALIZE                      RECECOM-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECECOM-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECECOM-SRYYM
           MOVE    RECE02-PTID         TO  RECECOM-PTID
           MOVE    RECE02-NYUGAIKBN    TO  RECECOM-NYUGAIKBN
      * 
           MOVE    RECECOM-REC         TO  MCPDATA-REC
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           ELSE
               INITIALIZE                  RECECOM-REC
               MOVE    1               TO  FLG-RECECOM
           END-IF
      *
           PERFORM             UNTIL   FLG-RECECOM   =   1
               PERFORM VARYING IDY FROM    1   BY  1
                       UNTIL   IDY >       10
                       OR      RECE02-HKNCOMBI (IDY) =   ZERO
                   IF      RECECOM-HKNCOMBI    =   RECE02-HKNCOMBI(IDY)
                       PERFORM 3601-COMMENT-HENSYU-SEC                 
                       MOVE    10                  TO  IDY
                   END-IF
               END-PERFORM
      *                 
               MOVE    "tbl_rececom"       TO  MCP-TABLE
               MOVE    "key1"              TO  MCP-PATHNAME
               PERFORM 900-RECECOM-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_rececom"       TO  MCP-TABLE
           MOVE    "key1"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       360-HC101-COMMENT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    コメントの改行コード削除処理
      *****************************************************************
       3601-COMMENT-HENSYU-SEC    SECTION.
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
                   IF      WRK-COMMENT-G   >   SPACE
      *                摘要欄に１行あける
                     IF      WRK-RECECOM         =   ZERO
                       MOVE    1               TO  WRK-RECECOM
                       IF  WRK-LINETEK         >   ZERO
                           IF      WRK-LINETEK >=  WRK-TEKIYOU-LINE
                               PERFORM 34120-PAGE-SEC
                           ELSE
                               ADD     1           TO  WRK-LINETEK
                               MOVE    WRK-KUGIRI1     TO 
                                               HC06-TEKIYO(WRK-LINETEK)
                           END-IF
                       END-IF
                     END-IF
      *                 
                     PERFORM VARYING IDW FROM    1   BY  1
                             UNTIL   IDW >       16
                           OR      WRK-COMMENT (IDW)   =   SPACE
                       IF      WRK-LINETEK      >=
                                                WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       END-IF
                       ADD     1               TO  WRK-LINETEK
                       MOVE    WRK-COMMENT (IDW)
                                       TO  HC06-TEKIYO(WRK-LINETEK)
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
      *    帳票編集<保険履歴情報>処理
      *****************************************************************
       370-HC101-HKN-RIREKI-HEN-SEC      SECTION.
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       WRK-RIREKI-MAX  
      *        摘要欄に１行あける
               IF      IDXX                =   1
                   IF  WRK-LINETEK             >   ZERO
                       IF      WRK-LINETEK         >=  WRK-TEKIYOU-LINE
                           PERFORM 34120-PAGE-SEC
                       ELSE
                           ADD     1               TO  WRK-LINETEK
                           MOVE    WRK-KUGIRI1     TO 
                                               HC06-TEKIYO(WRK-LINETEK)
                       END-IF
                   END-IF
               END-IF
      *
      *
               IF      WRK-LINETEK        >=  WRK-TEKIYOU-LINE
                   PERFORM 34120-PAGE-SEC
               END-IF
               ADD     1               TO  WRK-LINETEK
               MOVE    WRK-HKN-RIREKI-TBL (IDXX)
                                       TO  HC06-TEKIYO(WRK-LINETEK)
           END-PERFORM
      *
           .
       370-HC101-HKN-RIREKI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC                SECTION.
      *
           MOVE    ZERO                 TO  FLG-ERR
      *
           ADD     1                    TO  WRK-RECENUM
      * 
      *    レセプト明細管理ＤＢ
           PERFORM 3901-RECE08-INSERT-SEC
      *
      *    レセプト帳票管理ファイル出力
           IF      FLG-ERR              =   ZERO
               PERFORM 3903-RECE07-WRITE-SEC
           END-IF
      *
      *    請求管理ＤＢ更新
           IF      FLG-ERR              =   ZERO   AND 
                   CNT-PAGE             =   1
               PERFORM 3904-RECE10-HENSYU-SEC
           END-IF
      *
           IF      FLG-ERR              =   1
               MOVE    2                    TO  FLG-STS
           END-IF
           .
       390-PRINT-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細管理ＤＢ
      *****************************************************************
       3901-RECE08-INSERT-SEC        SECTION.
      *
           INITIALIZE                  RECE08-REC
grpsys     MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
           MOVE    "HC06"              TO  RECE08-PRTID
      *****MOVE    WRK-PARA-SRYYM      TO  RECE08-SRYYM
           MOVE    WRK-PARA-SRYYM      TO  RECE08-RECEYM
      *****MOVE    WRK-PARA-SYSYMD     TO  RECE08-CREYMD
           MOVE    WRK-SYSYMD          TO  RECE08-CREYMD
           MOVE    SYS-TIME (1:6)      TO  RECE08-CREHMS
           IF      RECE02-TEISYUTUSAKI =   "2"
               IF      RECE02-HKNJANUM-KEY (1:2)   =   "67"
                   MOVE    RECE02-HKNJANUM-KEY (3:2)
                                           TO  RECE08-PREFNUM
               ELSE
                   MOVE    RECE02-HKNJANUM-KEY (1:2)
                                           TO  RECE08-PREFNUM
               END-IF
               IF      RECE08-PREFNUM  =   SYS-1001-PREFNUM
                   MOVE    "1"             TO  RECE08-PREFKBN
               ELSE
                   MOVE    "2"             TO  RECE08-PREFKBN
                   MOVE    RECE02-SRYYM        TO  WRK-SYMD (1:6)
                   MOVE    "01"                TO  WRK-SYMD (7:2)
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       5
                           OR      WRK-1033-STYUKYMD (IDX) =   SPACE
                       IF      WRK-1033-STYUKYMD   (IDX) <=  WRK-SYMD
                       AND     WRK-1033-EDYUKYMD   (IDX) >=  WRK-SYMD      
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       20
                                   OR      WRK-1033-HKNJANUM (IDX IDY)
                                                       =   SPACE     
                               IF      RECE02-HKNJANUM-KEY = 
                                       WRK-1033-HKNJANUM (IDX IDY)
                                   MOVE    "1"     TO  RECE08-PREFKBN
                                   MOVE    20      TO  IDY
                                   MOVE    5       TO  IDX
                               END-IF
                           END-PERFORM
                       END-IF            
                   END-PERFORM
               END-IF        
           ELSE
               MOVE    "0"             TO  RECE08-PREFKBN
               MOVE    "00"            TO  RECE08-PREFNUM
           END-IF
           MOVE    RECE02-HKNJANUM-KEY TO  RECE08-HKNJANUM   
           MOVE    RECE02-TEKSTYMD     TO  RECE08-TEKSTYMD
           MOVE    RECE02-TEISYUTUSAKI TO  RECE08-TEISYUTUSAKI
           IF      RECE02-PTNUM        =   SPACE
               MOVE    ZERO                TO  RECE08-RECESYUBETU
           ELSE      
               MOVE    RECE02-RECESYUBETU  TO  RECE08-RECESYUBETU
           END-IF    
           MOVE    RECE02-PTNUM        TO  RECE08-PTNUM
           PERFORM VARYING IDX FROM    20  BY  -1
                   UNTIL   IDX <       1
               IF      RECE08-PTNUM (IDX:1)    NOT =   SPACE
                   MOVE    1               TO  IDX 
               ELSE  
                   MOVE    "Z"             TO  RECE08-PTNUM (IDX:1)
               END-IF
           END-PERFORM 
           MOVE    RECE02-SRYYM        TO  RECE08-SRYYM
           MOVE    CNT-PAGE            TO  RECE08-RENNUM
           MOVE    WRK-RECENUM         TO  RECE08-RECENUM
           MOVE    "00"               TO  RECE08-SRYKA
      *    編綴順項目の設定（初期値）
           MOVE    "99"                TO  RECE08-PREFKBN-SRT
           MOVE    "9"                 TO  RECE08-HKNJANUM-SRT
                                           RECE08-ZAITAKU
                                           RECE08-CHOUKI
           MOVE    RECE08-REC          TO  MCPDATA-REC
           PERFORM 900-RECE08-READ-SEC
           IF      FLG-RECE08          =   ZERO
               DISPLAY "ORCR0481 MCP-RC=" MCP-RC " " RECE08-KEY 
               STRING   "レセプト明細に更新できません　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE02-PTNUM           DELIMITED  BY  SIZE  
                                               INTO  WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           ELSE    
               IF      RECE08-TEISYUTUSAKI =   "1"
                   EVALUATE    RECE02-HKNNUM                   
                       WHEN    "001"
                           MOVE    "1"             TO  RECE08-HKNKBN
                       WHEN    "002"
                           MOVE    "2"             TO  RECE08-HKNKBN
                       WHEN    "003"
                           MOVE    "3"             TO  RECE08-HKNKBN
                       WHEN    "004"
                           MOVE    "4"             TO  RECE08-HKNKBN
                       WHEN    "031"  THRU  "034"
                           MOVE    "5"             TO  RECE08-HKNKBN
                       WHEN    "006"
                           MOVE    "6"             TO  RECE08-HKNKBN
                       WHEN    "007"               
                           MOVE    "7"             TO  RECE08-HKNKBN
                       WHEN    "063"
                       WHEN    "072"  THRU  "075"
                           MOVE    "8"             TO  RECE08-HKNKBN
                   END-EVALUATE
               END-IF                      
               MOVE    RECE02-KANANAME     TO  RECE08-KANANAME
      *********MOVE    WRK-RECENUM         TO  RECE08-RECENUM
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
      *********MOVE    "HC06"              TO  RECE08-FORMID
               MOVE    "HC06V01"           TO  RECE08-FORMID
               MOVE    WRK-PARA-TERMID     TO  RECE08-TERMID
               MOVE    ZERO                TO  RECE08-PRTJYUN
                                               RECE08-PRTRENNUM
               MOVE    HC06                TO  RECE08-PRTDATA
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    RECE08-REC          TO  MCPDATA-REC
grpsys         PERFORM 900-ORCDBMAIN-SEC
      *
               IF      MCP-RC     =   ZERO
                   ADD     1                 TO  CNT-OUT
               ELSE 
                   DISPLAY "ORCR0100 " MCP-RC " " RECE08-KEY 
                   STRING
                        "レセプト明細に更新できませんでした　患者番号＝"
                                               DELIMITED  BY  SIZE
                        RECE02-PTNUM           DELIMITED  BY  SIZE  
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
      *    出力処理
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
      *****MOVE    RECE08-SRYYM        TO  RECE07-SRYYM
           MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
           MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
           MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
           MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
           MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
           MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN
           MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
           MOVE    "99999999"          TO  RECE07-HKNJANUM
           MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
           MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
           MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
           MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
           MOVE    RECE07-REC          TO  MCPDATA-REC 
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
      *****    MOVE    RECE08-SRYYM        TO  RECE07-SRYYM
               MOVE    WRK-PARA-RECEYM     TO  RECE07-SRYYM
               MOVE    RECE08-CREYMD       TO  RECE07-CREYMD
               MOVE    RECE08-CREHMS       TO  RECE07-CREHMS
               MOVE    RECE08-TEISYUTUSAKI TO  RECE07-TEISYUTUSAKI
               MOVE    RECE08-RECESYUBETU  TO  RECE07-RECESYUBETU
               MOVE    1                   TO  RECE07-PAGE
               MOVE    1                   TO  RECE07-KENSU
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE08-NYUGAIKBN    TO  RECE07-NYUGAIKBN 
               MOVE    RECE08-TERMID       TO  RECE07-TERMID
               MOVE    RECE08-OPID         TO  RECE07-OPID
               MOVE    RECE08-SRYKA        TO  RECE07-SRYKA
               MOVE    "99999999"          TO  RECE07-HKNJANUM
               MOVE    RECE08-PREFKBN-SRT  TO  RECE07-PREFKBN-SRT
               MOVE    RECE08-ZAITAKU      TO  RECE07-ZAITAKU
               MOVE    RECE08-CHOUKI       TO  RECE07-CHOUKI
               MOVE    RECE08-HKNJANUM-SRT TO  RECE07-HKNJANUM-SRT
           ELSE
               ADD     1                   TO  RECE07-PAGE
               IF      CNT-KENSU           =   1
               AND     CNT-PAGE            =   1
                   ADD     1                   TO  RECE07-KENSU
               END-IF
           END-IF
      *
           MOVE    RECE07-REC          TO  MCPDATA-REC
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-RECE07          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC 
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0100 MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
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
      *    請求管理ＤＢ更新
      *****************************************************************
       3904-RECE10-HENSYU-SEC        SECTION.
      * 
      *    テスト患者のとき更新しない
           IF      WRK-PTINF-TSTPTNUMKBN   =   "1"
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      * 
      *    レセプト種別不明または自費のとき更新しない
           IF      RECE02-RECESYUBETU      =   "0000"  OR  "8888"
               GO  TO  3904-RECE10-HENSYU-EXT
           END-IF
      *          
           INITIALIZE                  RECE10-REC
grpsys     MOVE    RECE02-HOSPNUM      TO  RECE10-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE10-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE10-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE10-PTID
           IF      RECE02-RECEKA       =   SPACE
               MOVE    ZERO            TO  RECE10-RECEKA
           ELSE
               MOVE    RECE02-RECEKA   TO  RECE10-RECEKA
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO  RECE10-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE10-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO            TO  RECE10-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY
                                       TO  RECE10-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY =   SPACE
               MOVE    ZERO            TO  RECE10-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY
                                       TO  RECE10-HOJOKBN-KEY
           END-IF
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECE10-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECE10-TEKSTYMD
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
               MOVE    WRK-PARA-SYSYMD     TO  RECE10-UPDYMD
               IF      RECE10-SKYKBN       =   "2"
                   MOVE    "1"                 TO  RECE10-SKYKBN
               END-IF 
               MOVE    "2"                 TO  RECE10-TEISEIKBN
           ELSE
               MOVE    "1"                 TO  RECE10-SKYKBN
               MOVE    RECE02-PTNUM        TO  RECE10-PTNUM
               MOVE    RECE02-HKNNUM       TO  RECE10-HKNNUM
                                               RECE10-HKNNUM-S
               MOVE    RECE02-RJNHKNNUM    TO  RECE10-RJNHKNNUM
               MOVE    RECE02-HONKZKKBN    TO  RECE10-HONKZKKBN
               MOVE    RECE02-HOJOKBN      TO  RECE10-HOJOKBN
               MOVE    RECE02-CONTKBN      TO  RECE10-CONTKBN
               MOVE    RECE02-KIGO         TO  RECE10-KIGO
               MOVE    RECE02-NUM          TO  RECE10-NUM
               MOVE    ZERO                TO  RECE10-SKYYM
                                               RECE10-HENREIYM
                                               RECE10-TEISEIKBN
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       4
                   MOVE    RECE02-KOHNUM (IDX) TO
                                       RECE10-KOHNUM (IDX)
               END-PERFORM
               MOVE    RECE02-HKNNUM   TO  RECE10-HKNNUM-S
               IF      RECE02-HKNJANUM-KEY =   SPACE
                   MOVE    ZERO            TO  RECE10-HKNJANUM-S
               ELSE
                   IF      RECE02-HKNJANUM-KEY(1:2)    =   "67"
                       MOVE    RECE02-HKNJANUM-KEY(3:)
                                           TO  RECE10-HKNJANUM-S
                       MOVE    "067"       TO  RECE10-HKNNUM-S
                   ELSE
                       MOVE    RECE02-HKNJANUM-KEY
                                           TO  RECE10-HKNJANUM-S
                   END-IF
               END-IF
               MOVE    RECE08-PREFKBN      TO  RECE10-PREFKBN
               MOVE    RECE08-PREFNUM      TO  RECE10-PREFNUM
               MOVE    RECE02-AGE          TO  RECE10-AGE  
               MOVE    WRK-PARA-SYSYMD     TO  RECE10-CREYMD
               MOVE    ZERO                TO  RECE10-UPDYMD
           END-IF
      *
      *    治験は請求区分を０：請求しないとする
           IF      RECE10-HKNNUM       >=  "900"
           AND     RECE10-HKNNUM       <=  "969"
                   MOVE    ZERO            TO  RECE10-SKYKBN
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       5
               MOVE    RECE02-JNISSUINF (IDX)  TO
                                       RECE10-JNISSUINF (IDX)
               MOVE    RECE02-TOTALTENINF (IDX)    TO 
                                       RECE10-TOTALTENINF (IDX)
               MOVE    RECE02-FTNMONEYINF(IDX) TO
                                       RECE10-FTNMONEYINF (IDX)
               MOVE    RECE02-SHOKUJIINF (IDX) TO
                                       RECE10-SHOKUJIINF (IDX)
               MOVE    RECE02-YKZFTNINF  (IDX) TO
                                       RECE10-YKZFTNINF   (IDX)
           END-PERFORM
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       45
               MOVE    RECE02-SRYKBNINF (IDX)  TO
                                       RECE10-SRYKBNINF (IDX)
           END-PERFORM
      *    給付割合
           IF  ((RECE02-TEISYUTUSAKI =   "2"  )       AND
                (RECE02-RJNHKNNUM    =   SPACE))          OR
               ( RECE02-HKNNUM       =   WRK-RETIRE1  OR
                                         WRK-RETIRE2  OR
                                         WRK-RETIRE3  OR
                                         WRK-RETIRE4  OR
                                         WRK-RETIRE5  OR
                                         WRK-RETIRE6 )
               COMPUTE WRK-KYURATE-9   =
                              (100     -   RECE02-KYURATE)
                                           / 10 
      *********MOVE    WRK-KYURATE-9   TO  RECE10-SHOKUJINISSU (1)
               MOVE    WRK-KYURATE-9   TO  RECE10-KYURATE
           ELSE
      *********MOVE    ZERO            TO  RECE10-SHOKUJINISSU (1)
               IF      RECE02-TEISYUTUSAKI =   "1"
                   MOVE    ZERO            TO  RECE10-KYURATE
               ELSE
                   MOVE    10              TO  RECE10-KYURATE
               END-IF        
           END-IF             
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       4
                   OR      RECE02-ETC-KOHNUM (IDX) =   SPACE
               MOVE    RECE02-ETC-KOHNUM (IDX) TO
                                       RECE10-ETC-KOHNUM (IDX)
           END-PERFORM
      *    一部負担金・食事療養費標準負担額区分
           MOVE    RECE02-SHOKUJIFTNKBN
                                       TO  RECE10-SHOKUJIFTNKBN
      *
           MOVE    "00000000000000000000000000000000000000000000000000"
                                       TO  RECE10-RESERVE-AREA
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           IF      FLG-SEIKYU          =   1    
               MOVE    "DBINSERT"          TO  MCP-FUNC
           ELSE
               MOVE    "DBUPDATE"          TO  MCP-FUNC
           END-IF 
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
           IF      MCP-RC     =   ZERO
               CONTINUE
           ELSE
               DISPLAY "ORCR0100 MCP-RC= "  MCP-RC   
               IF      FLG-SEIKYU          =   1    
               MOVE    "請求管理ＤＢに追加できませんでした"
                                               TO  WRK-RECEERR
               ELSE
               MOVE    "請求管理ＤＢに更新できませんでした"
                                               TO  WRK-RECEERR
               END-IF 
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                  TO  FLG-ERR
           END-IF
           .
       3904-RECE10-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数値半角→全角変換処理
      *****************************************************************
       31101-NUM-ZENKAKU-HENKAN-SEC    SECTION.
      *
           MOVE    1                   TO  IDZ
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-CHAR-CNT
               EVALUATE    WRK-CHAR-X (IDX:1)
                   WHEN    "0"   
                       MOVE    TBL-SUJI (10)
                                       TO  WRK-CHAR-N (IDZ:)
                   WHEN    "1" THRU    "9"                    
                       MOVE    WRK-CHAR-X (IDX:1)
                                       TO  WRK-NUM-IDX
                       MOVE    TBL-SUJI (WRK-NUM-IDX)
                                       TO  WRK-CHAR-N (IDZ:)
                   WHEN    OTHER
                       MOVE    "  "    TO  WRK-CHAR-N (IDZ:)
               END-EVALUATE
               ADD     2               TO  IDZ
           END-PERFORM
      *
           .
       31101-NUM-ZENKAKU-HENKAN-EXT.
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
           MOVE    SPACE               TO  LNK-DAY1-AREA
           MOVE    "11"                TO  LNK-DAY1-IRAI
           MOVE    WRK-SYMD            TO  LNK-DAY1-YMD
           CALL    "ORCSDAY"   USING   STS-AREA-DAY  LNK-DAY1-AREA
           IF  STS-DAY-RC1             =   ZERO
               MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
           ELSE
               MOVE    SPACE               TO  WRK-SYMD
           END-IF
           .
       31012-SE-WA-YMDEDIT-EXT.
           EXIT.
057400*
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
      *        ジョブ管理開始処理
               MOVE    "JBE"           TO  SJOBKANRI-MODE
               INITIALIZE                  JOBKANRI-REC
               MOVE    WRK-RECEERR     TO  JOB-YOBI
               MOVE    "9999"          TO  JOB-ERRCD
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *                             
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    処理中止処理
      *****************************************************************
       500-CANCEL-HENSYU-SEC                SECTION.
      *
           MOVE    "CHK"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
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
      *            ジョブ管理終了処理
                   MOVE    "CAN"           TO  SJOBKANRI-MODE
                   INITIALIZE                  JOBKANRI-REC
                   MOVE    WRK-RECEERR     TO  JOB-YOBI
                   MOVE    "8888"          TO  JOB-ERRCD
                   MOVE    2               TO  JOB-STOPFLG
grpsys             PERFORM 900-CALL-ORCSJOB-SEC
               END-IF
      *                             
               MOVE    1                   TO  FLG-END
               MOVE    2                   TO  FLG-STS
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
      *        診療年月の請求管理削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECE10-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    1                   TO  RECE10-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del6"              TO  MCP-PATHNAME
                   MOVE    "DBDELETE"          TO  MCP-FUNC
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（社保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の請求管理削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                      RECE10-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECE10-SRYYM
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    2                   TO  RECE10-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECE10-NYUGAIKBN
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del6"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（国保分）でエラーとなりました"
                                           TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月の公費請求書削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                      KOHSKY-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    1                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "2"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月公費請求書削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                      KOHSKY-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    2                   TO  KOHSKY-TEISYUTUSAKI
                   MOVE    "2"                 TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（社保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "1"          
                   INITIALIZE                  RECEDEN-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    1                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（社保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月のレセ電削除処理（国保分）
               IF      WRK-PARA-RECEKBN    =   "0" OR  "2"          
                   INITIALIZE                  RECEDEN-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    WRK-PARA-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    2                   TO  RECEDEN-TEISYUTUSAKI
                   MOVE    "2"                 TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "レセ電削除処理（国保分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
               END-IF      
      *
      *        診療年月レセプト管理削除処理（一括分）
               INITIALIZE                  RECE07-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC06"              TO  RECE07-PRTID
               MOVE    WRK-PARA-SRYYM      TO  RECE07-SRYYM
               MOVE    "1"                 TO  RECE07-SYOKBN
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    WRK-PARA-RECEKBN    TO  RECE07-RECEKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "tbl_recekanri"     TO  WRK-MCP-TABLE
               MOVE    "key7"              TO  WRK-MCP-PATHNAME
               PERFORM 900-RECE07-INV-SEC
               IF      FLG-RECE07          =   ZERO
                   MOVE    RECE07-REC          TO  WRK-RECE07-REC
                   MOVE    RECE07-REC          TO  MCPDATA-REC 
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_recekanri"     TO  MCP-TABLE
                   MOVE    "del1"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
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
grpsys             MOVE    WRK-RECE07-HOSPNUM  TO  RECE08-HOSPNUM
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
grpsys             PERFORM 900-ORCDBMAIN-SEC
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
               MOVE    TBL-SYS1001 (1)     TO  SYS-1001-REC
      *
      *        レセプト管理削除処理（個別分）
               INITIALIZE                  RECE07-REC
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE07-HOSPNUM
               MOVE    "HC06"              TO  RECE07-PRTID  
               MOVE    "2"                 TO  RECE07-NYUGAIKBN
               MOVE    RECE07-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
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
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  RECE08-HOSPNUM
               MOVE    "HC06"              TO  RECE08-PRTID
               MOVE    "2"                 TO  RECE08-NYUGAIKBN
               MOVE    RECE08-REC          TO  MCPDATA-REC 
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_receprt"       TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-ORCDBMAIN-SEC
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
grpsys         MOVE    WRK-PARA-HOSPNUM    TO  SYS-2002-HOSPNUM
               MOVE    ZERO                TO  SYS-STYUKYMD
               MOVE    ZERO                TO  SYS-EDYUKYMD
               MOVE    SYS-2002-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-KANRIMST-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-KANRIMST
               END-IF
      *
               PERFORM             UNTIL       FLG-KANRIMST    =   1
                   MOVE    MCPDATA-REC         TO  SYS-2002-REC
      *            診療年月の請求管理削除処理
                   INITIALIZE                  RECE10-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECE10-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  RECE10-PTID
                   MOVE    SYS-2002-SRYYM      TO  RECE10-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  RECE10-NYUGAIKBN
                   MOVE    1                   TO  RECE10-TEISYUTUSAKI
                   MOVE    2                   TO  RECE10-XXTEISYUTUSAKI 
                   MOVE    RECE10-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_seikyu"        TO  MCP-TABLE
                   MOVE    "del4"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                       "請求管理削除処理（個別分）でエラーとなりました"
                                               TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月の公費請求書削除処理
                   INITIALIZE                  KOHSKY-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  KOHSKY-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  KOHSKY-PTID
                   MOVE    SYS-2002-SRYYM      TO  KOHSKY-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  KOHSKY-NYUGAIKBN
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "del2"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
                               " MCP-RC=" MCP-RC
                       MOVE
                      "公費請求書削除処理（個別分）でエラーとなりました"
                                                   TO    WRK-RECEERR
                       PERFORM 500-ERR-HENSYU-SEC
                       GO  TO  5001-DB-DELETE-EXT
                   END-IF
      *
      *            診療年月のレセ電削除処理
                   INITIALIZE                  RECEDEN-REC
grpsys             MOVE    WRK-PARA-HOSPNUM    TO  RECEDEN-HOSPNUM
                   MOVE    SYS-2002-PTID       TO  RECEDEN-PTID
                   MOVE    SYS-2002-SRYYM      TO  RECEDEN-SRYYM
                   MOVE    SYS-2002-NYUGAIKBN  TO  RECEDEN-NYUGAIKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_receden"       TO  MCP-TABLE
                   MOVE    "del2"              TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC           NOT =   ZERO
                       DISPLAY "DBPATH=" MCP-TABLE " " MCP-PATHNAME
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
      *    ファイルＣＬＯＳＥ処理
           CLOSE                       RECE02-FILE
           CLOSE                       RECE02X-FILE
           CLOSE                       RECE031-FILE
           CLOSE                       RECE061-FILE
                                       RECE09-FILE
                                       RECE11-FILE 
      *
      *****IF      CNT-OUT            =   ZERO 
      *         MOVE    "対象データがありませんでした" TO  WRK-RECEERR
      *         PERFORM 500-ERR-HENSYU-SEC
      *****END-IF
           DISPLAY "RECE02  CNT-IN  :"  CNT-RECE02
           DISPLAY "RECE02X CNT-IN  :"  CNT-RECE02X
           DISPLAY "        PAGE    :"  CNT-OUT
           DISPLAY "*** ORCR0100 END ***"
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
grpsys     PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時ファイル名取得処理
      *****************************************************************
       800-FILENM-SET-SEC        SECTION.
      *
           MOVE    WRK-PARA-TERMID     TO  RECEDAT1PARA-TERMID
           MOVE    WRK-PARA-HOSPNUM    TO  RECEDAT1PARA-HOSPNUM
           .
       800-FILENM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ順検索処理
      *****************************************************************
       990-RECE02-READ-SEC           SECTION.
      *
           READ    RECE02-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02
                   MOVE    HIGH-VALUE          TO  KEY-NEW
               NOT AT  END
                   MOVE    ZERO                TO  FLG-RECE02
                   IF      RECE02-TOTALTEN (1) =   ZERO
                   AND     RECE02-TOTALTEN (2) =   ZERO 
                   AND     RECE02-TOTALTEN (3) =   ZERO 
                   AND     RECE02-TOTALTEN (4) =   ZERO
                   AND     RECE02-TOTALTEN (5) =   ZERO
                   AND     RECE02-RECESYUBETU  NOT =  8888
                       GO  TO  990-RECE02-READ-SEC
                   ELSE
                     IF      RECE02-TOTALTEN (1) =   ZERO
                     AND     RECE02-TOTALTEN (2) =   ZERO 
                     AND     RECE02-TOTALTEN (3) =   ZERO 
                     AND     RECE02-TOTALTEN (4) =   ZERO
                     AND     RECE02-TOTALTEN (5) =   ZERO
                     AND     RECE02-RECESYUBETU  =   8888
                     AND     RECE02-JIHTEKFLG    =   SPACE
                       GO  TO  990-RECE02-READ-SEC
                     ELSE
                       DISPLAY "RECE02  PTNUM=" RECE02-PTNUM     
                       ADD     1                   TO  CNT-RECE02
grpsys                 MOVE    RECE02-HOSPNUM      TO  KEY-N-HOSPNUM
                       MOVE    RECE02-SRYYM        TO  KEY-N-SRYYM
                       MOVE    RECE02-NYUGAIKBN    TO  KEY-N-NYUGAIKBN
                       MOVE    RECE02-PTID         TO  KEY-N-PTID
                       MOVE    RECE02-RECEKA       TO  KEY-N-RECEKA
                       MOVE    RECE02-TEISYUTUSAKI TO
                                                   KEY-N-TEISYUTUSAKI
                       MOVE    RECE02-RECESYUBETU  TO  KEY-N-RECESYUBETU
                       MOVE    RECE02-HKNJANUM-KEY TO
                                                   KEY-N-HKNJANUM-KEY
                       IF      WRK-PARA-SRYYM      =  ZERO
      *                    医療機関ＩＤ編集
                           MOVE  RECE02-SRYYM         TO
                                                   WRK-SYS-SRYYMD(1:6)
                           MOVE  "01"                 TO
                                                   WRK-SYS-SRYYMD(7:2)
                           PERFORM 900-SYSKANRI-HENSYU-SEC
                       END-IF
                     END-IF
                   END-IF    
           END-READ
      *
           .
       990-RECE02-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト０２データ（公費請求書）順検索処理
      *****************************************************************
       990-RECE02X-READ-SEC           SECTION.
      *
           READ    RECE02X-FILE       NEXT
               AT  END
                   MOVE    1                   TO  FLG-RECE02X
               NOT AT  END
      *            テスト患者・レセプト種別不明のとき読み飛ばす
                   IF    ( RECE02X-TSTPTNUMKBN     =   "1"    )
      *************OR    ( RECE02X-RECESYUBETU     =   "0000" )
                       GO  TO  990-RECE02X-READ-SEC
                   END-IF
      *            点数が全てゼロのとき読み飛ばす
      *************IF      RECE02X-TOTALTEN (1) =   ZERO
      *            AND     RECE02X-TOTALTEN (2) =   ZERO 
      *            AND     RECE02X-TOTALTEN (3) =   ZERO 
      *            AND     RECE02X-TOTALTEN (4) =   ZERO
      *            AND     RECE02X-TOTALTEN (5) =   ZERO
      *                GO  TO  990-RECE02X-READ-SEC
      *************END-IF
                   DISPLAY "RECE02X PTNUM=" RECE02X-PTNUM     
      *
                   MOVE    ZERO                TO  FLG-RECE02X
                   ADD     1                   TO  CNT-RECE02X
           END-READ
      *
           .
       990-RECE02X-READ-EXT.
           EXIT.
      *
      ****************************************************************
      *    診療年月の対象となるシス管理を取得（個別処理のみ
      ****************************************************************
       900-SYSKANRI-HENSYU-SEC         SECTION.
      *
      *    医療機関情報（基本情報）
           MOVE    ZERO                TO  FLG-SET
      *
           PERFORM VARYING SYSKAN-IDX  FROM    1   BY  1
                   UNTIL ( SYSKAN-IDX              >  10   )
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
                   UNTIL ( SYSKAN-IDX              >  10   )
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
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE031-NEXT-SEC           SECTION.
      *
           READ    RECE031-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE031
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE031
                   IF      RECE031-TENKIKBN     =   "8"
                       GO  TO  990-RECE031-NEXT-SEC
                   END-IF
           END-READ
      *
           .
       990-RECE031-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０６１データ読み
      ****************************************************************
       990-RECE061-NEXT-SEC           SECTION.
      *
           READ    RECE061-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE061
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE061
                   ADD     1         TO  CNT-RECE061
                   IF      RECE061-SRYKBN    =   "54"
                       MOVE    "50"          TO  RECE061-SRYKBN
                   END-IF    
           END-READ
      *
           .
       990-RECE061-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０９データ読み
      ****************************************************************
       990-RECE09-NEXT-SEC           SECTION.
      *
           READ    RECE09-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE09
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE09
           END-READ
      *
           .
       990-RECE09-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト１１データ読み
      ****************************************************************
       990-RECE11-READ-SEC           SECTION.
      *
           READ    RECE11-FILE
               INVALID
                   MOVE    1           TO  FLG-RECE11
               NOT INVALID
                   MOVE    ZERO        TO  FLG-RECE11
           END-READ
      *
           .
       990-RECE11-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込処理
      *****************************************************************
       900-RECE07-READ-SEC           SECTION.
      *
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_recekanri"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
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
           MOVE    "tbl_recekanri"     TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECE07-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト管理読込（キー）
      *****************************************************************
       900-RECE07-INV-SEC         SECTION.
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
           .
       900-RECE07-INV-EXT.
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
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KANRIMST
           ELSE
               MOVE    1               TO  FLG-KANRIMST
           END-IF
      *
           .
       900-KANRIMST-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    保険番号マスタ次検索処理
      *****************************************************************
       910-HKNMST-NEXT-SEC              SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE              HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
           .
       910-HKNMST-NEXT-EXT.
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
grpsys     PERFORM 900-DBSELECT-SEC
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
      *
           .
       900-SEIKYU-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SEIKYU
           ELSE
               MOVE    1               TO  FLG-SEIKYU
           END-IF
      *
           .
       900-SEIKYU-READ-EXT.
           EXIT.
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
           .
       900-RECECOM-READ-EXT.
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
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
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
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費請求マスター読込（主キー）
      *****************************************************************
       900-KOHSKY-INV-SEC         SECTION.
      *
           MOVE    KOHSKY-REC          TO  MCPDATA-REC
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_kohsky"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-KOHSKY
               ELSE
                   MOVE    1               TO  FLG-KOHSKY
               END-IF
           ELSE
               MOVE    1                   TO  FLG-KOHSKY
           END-IF
      *
           MOVE    "tbl_kohsky"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-KOHSKY-INV-EXT.
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
      *
      *****************************************************************
      *    ＤＢ　ＣＯＭＭＩＴ処理
      *****************************************************************
       900-DBCOMMIT-SEC            SECTION.
      *
           MOVE    LOW-VALUE           TO  MCP-TABLE
                                           MCP-PATHNAME
           MOVE    "DBCOMMIT"          TO  MCP-FUNC
grpsys     PERFORM 900-ORCDBMAIN-SEC
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
grpsys     PERFORM 900-ORCDBMAIN-SEC
           .
       900-DBSTART-EXT.
           EXIT.
