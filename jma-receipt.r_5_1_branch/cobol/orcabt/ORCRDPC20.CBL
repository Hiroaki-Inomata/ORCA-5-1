       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORCRDPC20.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : レセプト（入院外）
      *  コンポーネント名  : ＤＰＣファイル作成
      *                    : Ｅ、Ｆファイル作成
      *  管理者            : 
      *  作成日付    作業者        記述
      *  07/01/23    NACL−伊藤    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *              NACL-小豆沢  xx/xx/xx  グループ診療対応
      *  平成20年度仕様対応
      *              NACL-伊藤    08/06/20  出来高・包括フラグ
      *                                       特定入院料１セット廃止
      *                                       診療区分21,22,23の場合
      *                                       退院日であれば１をセット
      *              NACL-伊藤    08/07/08  診療行為名称
      *                                       入力側の有効桁数が2->3に
      *                                       変更されたため作業領域の
      *                                       有効桁数を変更
      *              NACL-小豆沢  08/07/17  データ識別番号
      *                                       患者IDから患者番号へ変更
      *              NACL-伊藤    08/07/18  出来高点数
      *                                       手術の自動吻合器加算など
      *                                       時間外加算の所定点数に該
      *                                       当しない点数が含まれてい
      *                                       た
      *              NACL-伊藤    08/07/28  入院日
      *                                       自院入院歴を作成されてい
      *                                       た場合の考慮を行った
      *              NACL-伊藤    08/07/29  出来高点数
      *                                       内視鏡検査＋検査逓減＋乳
      *                                       幼児加算のようなケースで
      *                                       乳幼児加算が０点
      *              NACL-伊藤    08/07/30  出来高点数
      *                                       ２以上の手術の５０％併施
      *                                       加算の点数計算
      *              NACL-伊藤    08/08/06  出来高点数
      *                                       手術の通則加算の点数は所
      *                                       定点数に含めない
      *              NACL-小豆沢  08/08/21  入退院日
      *                                       退院同日入院の場合入退院
      *                                       歴が同日入院であったため
      *                                       退院の歴として記録する
      *              NACL-伊藤    08/10/06  出来高点数
      *                                       閉鎖循環式全身麻酔計算
      *              NACL-伊藤    09/01/13  出来高点数
      *                                       通則加算対象外できざみ
      *                                       加算のマスタの出来高点数
      *              NACL-伊藤    09/01/21  出来高・包括フラグ
      *                                       退院時処方の判断を退院日
      *                                       からコメントコードに変更
      *              NACL-伊藤    10/08/19  特定入院料包括対応
      *              NACL-伊藤    11/01/06  退院日
      *                                       当該月末時点での入院状態
      *                                       により退院日を決定する
      *                                     退院時投薬のコメント
      *                                       ユーザーが作成したコメン
      *                                       トコードに対応
      *              NACL-小豆沢            外来EF対応のため入院EF対応
      *                                     から複写して作成
      *              NACL-伊藤    12/06/14  出来高・包括フラグ
      *                                       原則「1」をセット
      *                                       院外処方は「9」をセット
      *                                       システム管理1030の設定と
      *                                       点数マスタの設定から
      *                                       一般名処方は「8」をセット
      *                                       一般名記載、銘柄名記載
      *                                       システム予約コード
      *              NACL-伊藤    14/04/18  データ区分
      *                                       9999包括保険で病理診断を
      *                                       算定した場合64で記録され
      *                                       ていた
      *              NACL-伊藤    14/04/21  出来高点数
      *                                       検査で包括区分が０でない
      *                                       場合薬剤器材の点数を引く
      *              NACL-伊藤    14/07/16  行為明細点数、出来高点数
      *                                       端数処理
      *
      *  04.08.01    NACL-藤原    14/07/07  一時ディレクトリ対応
      *
      *  平成２６年度改定対応
      *              NACL-小豆沢  14/09/03  向精神薬多剤投与の薬剤料逓減
      *                                     対応
      *              NACL-小豆沢  14/09/09  出来高・包括フラグの"6"を
      *                                     "1"とするよう修正
      *              NACL-小豆沢  14/09/10  Ｅファイル編集処理
      *                                     包括分は行為点数、行為薬剤料、
      *                                     行為材料料にゼロ点をセット
      *                                     する
      *              NACL-伊藤    14/10/17  レセプト科区分クリア漏れ
      *              NACL-小豆沢  14/11/05  COMMITカウント
      *                                     300 => 10
      *              NACL-小豆沢  14/11/07  データ識別番号への患者番号
      *                                     セット修正
      *              NACL-小豆沢  15/03/25  Ｆファイル編集処理
      *                                     行為明細点数をきざみ点数を
      *                                     加算した点数に修正
      *
      *              NACL-伊藤    15/06/23  後発品変更不可（処方単位）
      *  04.08.01    NACL-藤原    15/08/24  公費のみの診療（医療保険のな
      *                                     い診療）のＥＦ出力可否対応
      *              NACL-伊藤    15/10/20  閉麻計算見直し
      *              NACL-小豆沢  15/10/20  処置の出来高実績点数セット修正
      *
      *              NACL-小豆沢  16/07/05  平成２８年度対応
      *              NACL-伊藤    16/10/18  閉麻計算見直し
      *              NACL-小豆沢  16/12/20  外来の包括分は出来高実績点数
      *                                     をゼロ点で記録する
      *              NACL-門脇    17/03/22  後発品変更可（処方単位）
      *              NACL-小豆沢  17/09/08  きざみ計算の出来高実績点数
      *                                     端数処理追加
      *  04.08.02    NACL-藤原    18/04/17  傷病名レコードの作成対応
      *  04.08.02    NACL-小豆沢  18/06/21  胃瘻造設時嚥下機能評価加算は
      *                                     通則加算対象外区分を０（所定
      *                                     点数の対象となる診療行為）に
      *                                     読み替え
      *              NACL-小豆沢  19/03/07  医薬品マスターの金額種別対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION           SECTION.
       INPUT-OUTPUT            SECTION.
       FILE-CONTROL.
      *
      *    レセプト明細書
           SELECT  RECE02-FILE     ASSIGN  RECE02PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE02-KEY
                                   FILE    STATUS  IS  STS-RECE02.
      *
      *    レセプト０４データ
           SELECT  RECE04-FILE    ASSIGN  RECE04PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE04-KEY
                                   FILE    STATUS  IS  STS-RECE04.
      *    レセプト０５データ
           SELECT  RECE05-FILE     ASSIGN  RECE05PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE05-KEY
                                   FILE    STATUS  IS  STS-RECE05.
      *
           SELECT  RECE031-FILE     ASSIGN  RECE031PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  SEQUENTIAL
                                   RECORD  KEY     IS  RECE031-KEY
                                   FILE    STATUS  IS  STS-RECE03.
      *
      *    順序番号情報
           SELECT  RECE900-FILE    ASSIGN  RECE900PARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  RECE900-KEY
                                   FILE    STATUS  IS  STS-RECE900.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                    DIVISION.
       FILE                    SECTION.
      *
      *    レセプト明細書
       FD  RECE02-FILE.
       01  RECE02-REC. 
           COPY    "CPRCF002.INC".
      *
      *    レセプト０４データ
       FD  RECE04-FILE.
       01  RECE04-REC.
           COPY    "CPRCF004.INC".
      *
      *    レセプト０５データ
       FD  RECE05-FILE.
       01  RECE05-REC.
           COPY    "CPRCF005.INC".
      *
       FD  RECE031-FILE.
       01  RECE031-REC.
           COPY    "CPRCF0031.INC".
      *
      *    順序番号データ
       FD  RECE900-FILE.
       01  RECE900-REC. 
           COPY    "CPRCF900.INC".
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
      *     レセプト明細ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE02PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE04PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE05PARA//.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE031PARA//.
      *
      *     順序番号情報ファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECE900PARA//.
      *    エラーファイル 名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //RECEERR//.
      *
      *    ファイル名取得
           COPY    "CPRECEDAT1.INC".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-RECE02              PIC X(02).
           03  STS-RECE04              PIC X(02).
           03  STS-RECE05              PIC X(02).
           03  STS-RECE03              PIC X(02).
           03  STS-RECE900             PIC X(02).
           03  STS-RECEERR             PIC X(02).
      *
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-BYOMEI              PIC 9(01).
           03  FLG-TENSU               PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-RECE02              PIC 9(01).
           03  FLG-RECE04              PIC 9(01).
           03  FLG-RECE05              PIC 9(01).
           03  FLG-RECE031             PIC 9(01).
           03  FLG-RECE900             PIC 9(01).
      *
           03  FLG-READ                PIC 9(01).
      *
           03  FLG-TEIGEN              PIC 9(01).
           03  FLG-KOKUJISKBKBN1       PIC 9(01).
           03  FLG-TBLMAX              PIC 9(01).
           03  FLG-SRYSYUKBN721        PIC 9(01).
           03  FLG-SRYKBN70-TEIGEN     PIC 9(01).
           03  FLG-KENSA-KASAN         PIC 9(01).
           03  FLG-KENSA-TEIGEN        PIC 9(01).
           03  FLG-SET                 PIC 9(01).
           03  FLG-HIT                 PIC 9(01).
           03  FLG-CHK                 PIC 9(01).
           03  FLG-DAYSET              PIC 9(01).
           03  FLG-SRYACCT             PIC 9(01).
           03  FLG-SRYACT              PIC 9(01).
           03  FLG-HOUKATU-RECE04      PIC 9(01).
           03  FLG-TOKUTEINYUIN        PIC 9(01).
           03  FLG-GE-TARGET           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-IN                  PIC 9(06).
           03  CNT-OUT                 PIC 9(06).
           03  CNT-OUT-E               PIC 9(06).
           03  CNT-OUT-F               PIC 9(06).
           03  CNT-RECE04              PIC 9(06).
           03  CNT-RECE03              PIC 9(06).
           03  CNT-RECE900             PIC 9(06).
           03  CNT-COMMIT              PIC 9(03).
      *
       01  INDEX-AREA.
           03  IDW0                    PIC 9(03).
           03  IDW                     PIC 9(03).
           03  IDX                     PIC 9(03).
           03  IDY                     PIC 9(03).
           03  IDZ                     PIC 9(03).
           03  IDY1                    PIC 9(03).
           03  IDY2                    PIC 9(03).
           03  IDY3                    PIC 9(03).
           03  IDXW                    PIC 9(03).
           03  IDXX                    PIC 9(03).
           03  IDXY                    PIC 9(03).
           03  IDXZ                    PIC 9(03).
           03  IDW1                    PIC 9(03).
           03  IDW2                    PIC 9(04).
           03  IDW3                    PIC 9(03).
           03  IDW4                    PIC 9(04).
           03  IDZ1                    PIC 9(03).
           03  IDZ2                    PIC 9(03).
           03  IDZ3                    PIC 9(03).
           03  IDZ4                    PIC 9(03).
           03  IDZ5                    PIC 9(03).
           03  IDZ6                    PIC 9(03).
           03  SYSKAN-IDX              PIC 9(03).
           03  SYSKAN-IDX2             PIC 9(03).
           03  II                      PIC 9(01).
           03  IJ                      PIC 9(02).
           03  IDX-HOUKATU             PIC 9(02).
      *
       01  KEY-AREA                          VALUE   LOW-VALUE.
           03  KEY-NEW.
               05  KEY-NEW1.
                   07  KEY-N-HOSPNUM   PIC 9(02).
                   07  KEY-N-SRYYM     PIC 9(06).
                   07  KEY-N-NYUGAIKBN PIC X(01).
                   07  KEY-N-PTID      PIC 9(10).
                   07  KEY-N-RECEKA    PIC X(02).
               05  KEY-N-TEISYUTUSAKI  PIC 9(01).
           03  KEY-OLD.
               05  KEY-OLD1.
                   07  KEY-O-HOSPNUM   PIC 9(02).
                   07  KEY-O-SRYYM     PIC 9(06).
                   07  KEY-O-NYUGAIKBN PIC X(01).
                   07  KEY-O-PTID      PIC 9(10).
                   07  KEY-O-RECEKA    PIC X(02).
               05  KEY-O-TEISYUTUSAKI  PIC 9(01).
      *
      *    医療機関（基本情報）退避用領域
       01  TBL-SYS1001-T.
           03  TBL-SYS1001-OCC         OCCURS  10.
               05  TBL-SYS1001         PIC  X(610).
      *
      *    診療科情報退避用領域
       01  TBL-SRYKA-AREA.
           03  TBL-SRYKA-OCC           OCCURS  100.
               05  TBL-SRYKA           PIC X(02).
               05  TBL-RECESRYKA       PIC X(02).
           03  TBL-SRYKA-MAX           PIC 9(03).
      *
      *    一時領域
       01  WRK-AREA.
      *    パラメタエリア
           03  WRK-PARA.
               05  WRK-PARA1.
                   07  WRK-PARA-SRYYM  PIC X(06).
                   07  WRK-PARA-TERMID PIC X(16).
                   07  WRK-PARA-SYSYMD PIC X(08).
               05  WRK-PARA-TEISYUTUSAKI
                                       PIC X(01).
               05  WRK-PARA-JOBID      PIC 9(07).
               05  WRK-PARA-SHELLID    PIC X(08).
               05  WRK-PARA-SYARECEKBN PIC X(01).
               05  WRK-PARA-KKHRECEKBN PIC X(01).
               05  WRK-PARA-CREYMD     PIC X(08).
               05  WRK-PARA-CREHMS     PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *
           03  WRK-SYS-SRYYMD          PIC X(08).
      *
           03  WRK-KIZAI-SRYCD         PIC X(09).
      *     右ずめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-SRYKBN              PIC X(02).
           03  WRK-REC                 PIC X(2500).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NUM-X.
                   07  WRK-NUM         PIC 9999999999.999.
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02).
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
      *    傷病名レコード編集用
           03  WRK-BYOMEI-AREA.
               05  WRK-BYOMEICD        PIC X(07).
               05  WRK-RECECD-G.
                   07  WRK-RECECD      PIC X(04)   OCCURS  20.
      *    給付割合 
           03  WRK-KYURATE             PIC 9(03).
      *****レセプト番号
      *****03  WRK-RECENUM             PIC 9(06).
      *
           03  WRK-RENNUM-E            PIC 9(04).
           03  WRK-RENNUM-F            PIC 9(04).
      *  WRK-RENNUM と WRK-MEISAI-RENNUM は桁数変更不可
           03  WRK-RENNUM              PIC 9(04).
           03  WRK-MEISAI-RENNUM       PIC 9(03).
           03  WRK-RENNUM-SY           PIC 9(04).
           03  WRK-MEISAI-RENNUM-SY    PIC 9(03).
           03  WRK-COM-RENNUM          PIC 9(04).
      *
           03  WRK-TAIHI-FTNKBN        PIC X(01).
           03  WRK-SJ-KBN              PIC X(02).
           03  WRK-RECECOM-SJKBN       PIC X(02).
           03  WRK-SJ-CHK              PIC 9(01).
      *
      *    摘要レコード編集用
           03  WRK-ZAIWMAX             PIC 9(03).
           03  WRK-TBLMAX1             PIC 9(03).
           03  WRK-TBLMAX6             PIC 9(03).
           03  WRK-TBLMAX7             PIC 9(03).
           03  WRK-TBLMAX8             PIC 9(03).
           03  WRK-SURYO               PIC 9(10).
           03  WRK-TENSU               PIC 9(07).
           03  WRK-KAISU               PIC 9(03).
           03  WRK-KAISU-70            PIC 9(03)V9(01).
           03  WRK-KAISU-70T           PIC 9(03)V9(01).
           03  WRK-TANKA               PIC 9(07)V9(02).
           03  WRK-SRYSURYO            PIC 9(05)V9(05).
           03  WRK-FTNKBN-AREA.
               05  WRK-FTNKBN          PIC X(01).
               05  WRK-KOHNUM          PIC 9(01)
                                           OCCURS  3.
               05  WRK-SEHFLG          PIC 9(01).
           03  WRK-SRYCD1-MAX          PIC 9(03).
           03  WRK-SRYCD71-MAX         PIC 9(03).
           03  WRK-SRYCD72-MAX         PIC 9(03).
           03  WRK-KOKUJISKBKBN1-KOUI-TEN
                                       PIC 9(09)V9(02).
           03  WRK-RECE04-ZAIKAISU     PIC 9(04).
           03  WRK-KENSASU             PIC 9(04).
      *    日付変換用
           03  WRK-SYMD                PIC X(08).
      *
           03  WRK-HENYMDG             PIC 9(08).
           03  WRK-HENYMDGR            REDEFINES   WRK-HENYMDG.
               05  FILLER              PIC 9(01).
               05  WRK-HENYMD          PIC 9(07).
      *
           03  WRK-MCP-TABLE           PIC X(64).
           03  WRK-MCP-PATHNAME        PIC X(64).
      *
      *    きざみ計算用
           03  FLG-KZM                 PIC 9(01).
           03  WRK-KZM-TEN             PIC 9(08)V9(3).
           03  WRK-KZM-TEN1            PIC 9(08)V9(3).
           03  WRK-KZM-TEN2            PIC 9(08).
           03  WRK-KZM-SURYO           PIC 9(05)V9(5).
           03  WRK-KZM-SURYO-R         REDEFINES  WRK-KZM-SURYO.
               05  WRK-KZM-SURYO-S1    PIC 9(05).
               05  WRK-KZM-SURYO-S2    PIC 9(05).
           03  WRK-30M-SURYO           PIC 9(05)V9(5).
      *
      *    保険履歴編集（適用欄）
           03  WRK-HKN-RIREKI-AREA.
               05  WRK-HKN-RIREKI-TBL          OCCURS  50.
                   07  WRK-HKN-RIREKI          PIC X(52).
                   07  WRK-HKN-RIREKI-W   REDEFINES    WRK-HKN-RIREKI.
                       09  WRK-HKN-MEISYO      PIC X(04).
                       09  WRK-HKN-TEKSTYMD.
                           11  WRK-HKN-TEKSTYY PIC X(08).
                           11  WRK-HKN-TEKSTMD PIC X(12).
                       09  WRK-HKN-FL.
                           11  WRK-HKN-FL1     PIC X(04).
                           11  WRK-HKN-TEKEDYMD.
                               13  WRK-HKN-TEKEDYY
                                               PIC X(08).
                               13  WRK-HKN-TEKEDMD
                                               PIC X(12).
                           11  WRK-HKN-FL2     PIC X(04).
           03  WRK-RIREKI-MAX                  PIC 9(02).
           03  WRK-HKN-RIREKI-X                PIC X(60).
           03  WRK-HKN-HKNJANUM                PIC X(16).
           03  WRK-HKN-NUM                     PIC X(50).
           03  WRK-HKN-KIGO                    PIC X(50).
           03  WRK-HKN-TEISYUTUSAKI            PIC X(04).
           03  WRK-SRYYMST                     PIC X(12).
           03  WRK-SRYYMED                     PIC X(12).
      *
      *    レセプトコメント編集
           03  WRK-COMMENT-G.
               05  WRK-COMMENT                 PIC X(50)
                                               OCCURS  16.
           03  WRK-COMMENT-R               REDEFINES   WRK-COMMENT-G.
               05  WRK-COMMENT1                PIC X(100)
                                               OCCURS  8.
           03  WRK-COMM                        PIC X(800).
           03  WRK-COMM1                       PIC X(800).
           03  WRK-COL                         PIC 9(03). 
           03  WRK-RECECOM         PIC 9(01).
           03  WRK-RECEHEN         PIC 9(01).
           03  WRK-SJKBN-X.
               05  WRK-SJKBN       PIC 9(02).
      *
      *    入退院履歴編集（自院歴−入院中区分）
           03  WRK-NYUINCHUKBN                 PIC X(01).
      *
      *    区分番号編集（点数表）
           03  WRK-KBNNUM-EDIT.
               05  WRK-KBNNUM-KBN              PIC X(01).
               05  WRK-KBNNUM-KBNNUM           PIC 9(03).
               05  WRK-KBNNUM-HAIFUN           PIC X(01).
               05  WRK-KBNNUM-KBNNUM-EDA       PIC 9(02).
      *
      *    データ識別番号への患者番号編集用
           03  WRK-ZEROKETA                    PIC 9(02).
           03  WRK-PTNUMKETA                   PIC 9(02).
      *
      *    医薬品・特定器材用テーブル
       01  TBL-ZAIW-AREA.
           03  TBL-ZAIW-TABLE          OCCURS  2.
               05  TBL-ZAIW-REC        OCCURS 100.
                   07  TBL-ZAIW-SRYACTINF.
      *                診療コード
                       09  TBL-ZAIW-SRYCD           PIC  X(09).
      *                診療種別区分
                       09  TBL-ZAIW-SRYSYUKBN       PIC  X(03).
      *                数量
                       09  TBL-ZAIW-SRYSURYO        PIC  9(05)V9(05).
      *                回数
                       09  TBL-ZAIW-SRYKAISU        PIC  9(03).
      *                単位コード
                       09  TBL-ZAIW-TANICD          PIC  9(03).
      *                単位名称有効桁数
                       09  TBL-ZAIW-TANIMOJISU      PIC  9(01).
      *                単位名称
                       09  TBL-ZAIW-TANINAME        PIC  X(24).
      *                点数識別区分
                       09  TBL-ZAIW-TENSIKIBETU     PIC  9(01).
      *                点数
                       09  TBL-ZAIW-TEN             PIC  9(09)V9(03).
      *                明細請求フラグ
                       09  TBL-ZAIW-MEISKYFLG       PIC  X(01).
      *                自動算定区分
                       09  TBL-ZAIW-AUTOKBN         PIC  X(01).
      *                名称入力番号
                       09  TBL-ZAIW-INPUTNUM        PIC  9(03).
      *                点数欄集計先識別
                       09  TBL-ZAIW-TENTTLKBN       PIC  9(03).
      *                データ区分
                       09  TBL-ZAIW-DATAKBN         PIC  9(01).
      *                包括対象検査
                       09  TBL-ZAIW-HOUKNSKBN       PIC  9(02).
      *                検査等実施判断
                       09  TBL-ZAIW-KNSJISKBN       PIC  9(01).
      *                検査等実施判断グループ
                       09  TBL-ZAIW-KNSJISGRPKBN    PIC  9(02).
      *                各種番号等
                       09  TBL-ZAIW-CDKBNINF.
                           11  TBL-ZAIW-CDKBN-KBN   PIC  X(01).
                           11  TBL-ZAIW-CDKBN-SYO   PIC  9(01).
                           11  TBL-ZAIW-CDKBN-BU    PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KBNNUM     PIC  9(03).
                           11  TBL-ZAIW-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  TBL-ZAIW-CDKBN-KOUBAN     PIC  9(02).
                           11  TBL-ZAIW-KOUHYOJYUNNUM    PIC  9(09).
      *                告示等識別区分１
                       09  TBL-ZAIW-KOKUJISKBKBN1   PIC  9(01).
      *                告示等識別区分２
                       09  TBL-ZAIW-KOKUJISKBKBN2   PIC  9(01).
      *                通則加算対象外区分
                       09  TBL-ZAIW-TUSOKUGAIKBN    PIC  9(01).
      *                きざみ値計算識別
                       09  TBL-ZAIW-KZMCOMPSIKIBETU PIC  9(01).
      *                注加算コード
                       09  TBL-ZAIW-CHUKSNINF.
      *                    コード
                           11  TBL-ZAIW-CHUKSNCD    PIC  X(04).
      *                    通番
                           11  TBL-ZAIW-CHUKSNTUBAN PIC  X(01).
      *                検体検査コメント
                       09  TBL-ZAIW-KENTAICOMMENT   PIC  9(01).
      *                入力コメント
                       09  TBL-ZAIW-INPUTCOMENT     PIC  X(80).
      *            名称情報
                   07  TBL-ZAIW-NAMEINF.
      *                漢字有効桁数
                       09  TBL-ZAIW-YUKOKETA        PIC  9(03).
      *                漢字名称
                       09  TBL-ZAIW-NAME            PIC  X(200).
      *            データ識別(１：フィルム ２：特定機材)
                   07  TBL-ZAIW-DATSKB              PIC 9(01).
      *
      *    診療行為・コメント用テーブル
       01  TBL-KOUI-AREA.
           03  TBL-KOUI-REC                     OCCURS  100.
               05  TBL-KOUI-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                   07  TBL-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                   07  TBL-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                   07  TBL-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                   07  TBL-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                   07  TBL-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                   07  TBL-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                   07  TBL-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                   07  TBL-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                   07  TBL-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                   07  TBL-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                   07  TBL-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                   07  TBL-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                   07  TBL-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                   07  TBL-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                   07  TBL-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                   07  TBL-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                   07  TBL-KOUI-CDKBNINF.
                       09  TBL-KOUI-CDKBN-KBN   PIC  X(01).
                       09  TBL-KOUI-CDKBN-SYO   PIC  9(01).
                       09  TBL-KOUI-CDKBN-BU    PIC  9(02).
                       09  TBL-KOUI-CDKBN-KBNNUM     PIC  9(03).
                       09  TBL-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                       09  TBL-KOUI-CDKBN-KOUBAN     PIC  9(02).
                       09  TBL-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                   07  TBL-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                   07  TBL-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                   07  TBL-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                   07  TBL-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                   07  TBL-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                   07  TBL-KOUI-CHUKSNINF.
      *                コード
                       09  TBL-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                       09  TBL-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                   07  TBL-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                   07  TBL-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
               05  TBL-KOUI-NAMEINF.
      *            漢字有効桁数
                   07  TBL-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                   07  TBL-KOUI-NAME            PIC  X(200).
      *
      *    コメントデータ用テーブル
       01  TBL-COMMENT-AREA.
           03  TBL-COMMENT-REC                  OCCURS  100.
               05  TBL-COMMENT-SRYCD            PIC  X(09).
               05  TBL-COMMENT-NAME             PIC  X(200).
      *
      *    閉麻計算用
       01  TBL-HEIMA-AREA.
           03  WRK-HEIMA-IDX                    PIC 9(02).
           03  WRK-HEIMA-SURYO                  PIC 9(05).
           03  WRK-HEIMA-SURYO2                 PIC 9(05).
           03  TBL-HEIMA-MAX                    PIC 9(02).
           03  TBL-HEIMA-TOT-SURYO              PIC 9(05)V9(5).
           03  TBL-HEIMA-TBL                    OCCURS  10.
               05  TBL-HEIMA-TEN                PIC 9(07).
               05  TBL-HEIMA-KZMKGN             PIC 9(08)V9(3).
               05  TBL-HEIMA-KZMJGN             PIC 9(08)V9(3).
               05  TBL-HEIMA-KZMTEN             PIC 9(07).
               05  TBL-HEIMA-KZMERR             PIC 9(01).
               05  TBL-HEIMA-SURYO              PIC 9(08)V9(5).
               05  TBL-HEIMA-ZAN-SURYO          PIC 9(08)V9(5).
               05  TBL-HEIMA-COM-SURYO          PIC 9(08)V9(5).
               05  TBL-HEIMA-30M-SURYO          PIC 9(08)V9(5).
               05  TBL-HEIMA-30M-IDX            PIC 9(02).
               05  TBL-HEIMA-30M-TMP            PIC 9(08)V9(5).
      *
      *    診療行為データ用テーブル（告示等識別区分による記録順）
       01  WRK-KOUI-TAIHI-AREA.
           03  WRK-KOUI-TAIHI-TABLE             OCCURS  100.
               05  WRK-KOUI-TAIHI-REC           PIC X(500).
      *        診療コード
               05  WRK-KOUI-TAIHI-SRYCD         PIC  X(09).
      *        告示等識別区分１
               05  WRK-KOUI-TAIHI-KOKUJISKBKBN1 PIC  9(01).
      *        注加算コード
               05  WRK-KOUI-TAIHI-CHUKSNCD      PIC  X(04).
      *        点数識別区分
               05  WRK-KOUI-TAIHI-TENSIKIBETU   PIC  9(01).
      *
       01  WRK-KOUI-AREA.
           03  WRK-KOUI-TABLE                   OCCURS  4.
               05  WRK-KOUI-TBLMAX              PIC 9(03).
               05  WRK-KOUI-REC                     OCCURS  100.
                   07  WRK-KOUI-SRYACTINF.
      *            診療コード
                       09  WRK-KOUI-SRYCD           PIC  X(09).
      *            診療種別区分
                       09  WRK-KOUI-SRYSYUKBN       PIC  X(03).
      *            数量
                       09  WRK-KOUI-SRYSURYO        PIC  9(05)V9(05).
      *            回数
                       09  WRK-KOUI-SRYKAISU        PIC  9(03).
      *            単位コード
                       09  WRK-KOUI-TANICD          PIC  9(03).
      *            単位名称有効桁数
                       09  WRK-KOUI-TANIMOJISU      PIC  9(01).
      *            単位名称
                       09  WRK-KOUI-TANINAME        PIC  X(24).
      *            点数識別区分
                       09  WRK-KOUI-TENSIKIBETU     PIC  9(01).
      *            点数
                       09  WRK-KOUI-TEN             PIC  9(09)V9(02).
      *            明細請求フラグ
                       09  WRK-KOUI-MEISKYFLG       PIC  X(01).
      *            自動算定区分
                       09  WRK-KOUI-AUTOKBN         PIC  X(01).
      *            名称入力番号
                       09  WRK-KOUI-INPUTNUM        PIC  9(03).
      *            点数欄集計先識別
                       09  WRK-KOUI-TENTTLKBN       PIC  9(03).
      *            データ区分
                       09  WRK-KOUI-DATAKBN         PIC  9(01).
      *            包括対象検査
                       09  WRK-KOUI-HOUKNSKBN       PIC  9(02).
      *            検査等実施判断
                       09  WRK-KOUI-KNSJISKBN       PIC  9(01).
      *            検査等実施判断グループ
                       09  WRK-KOUI-KNSJISGRPKBN    PIC  9(02).
      *            各種番号等
                       09  WRK-KOUI-CDKBNINF.
                           11  WRK-KOUI-CDKBN-KBN   PIC  X(01).
                           11  WRK-KOUI-CDKBN-SYO   PIC  9(01).
                           11  WRK-KOUI-CDKBN-BU    PIC  9(02).
                           11  WRK-KOUI-CDKBN-KBNNUM     PIC  9(03).
                           11  WRK-KOUI-CDKBN-KBNNUM-EDA PIC  9(02).
                           11  WRK-KOUI-CDKBN-KOUBAN     PIC  9(02).
                           11  WRK-KOUI-KOUHYOJYUNNUM    PIC  9(09).
      *            告示等識別区分１
                       09  WRK-KOUI-KOKUJISKBKBN1   PIC  9(01).
      *            告示等識別区分２
                       09  WRK-KOUI-KOKUJISKBKBN2   PIC  9(01).
      *            通則加算対象外区分
                       09  WRK-KOUI-TUSOKUGAIKBN    PIC  9(01).
      *            時間外加算区分
                       09  WRK-KOUI-TIMEKSNKBN      PIC  9(01).
      *            きざみ値計算識別
                       09  WRK-KOUI-KZMCOMPSIKIBETU PIC  9(01).
      *            注加算コード
                       09  WRK-KOUI-CHUKSNINF.
      *                コード
                           11  WRK-KOUI-CHUKSNCD    PIC  X(04).
      *                通番
                           11  WRK-KOUI-CHUKSNTUBAN PIC  X(01).
      *            検体検査コメント
                       09  WRK-KOUI-KENTAICOMMENT   PIC  9(01).
      *            入力コメント
                       09  WRK-KOUI-INPUTCOMENT     PIC  X(80). 
      *        名称情報
                   07  WRK-KOUI-NAMEINF.
      *            漢字有効桁数
                       09  WRK-KOUI-YUKOKETA        PIC  9(03).
      *            漢字名称
                       09  WRK-KOUI-NAME            PIC  X(200).
      *
      *    並び替え用ワーク
       01  TBL-KOUI-SORT                        PIC X(500).
      *
      *    診療行為データ用テーブル（再診 時間外）
       01  TBL-KOUI-SRYKBN12-AREA.
           03  TBL-KOUI-SRYKBN12-REC                     OCCURS  3.
               05  TBL-KOUI-SRYKBN12-SRYACTINF.
      *            診療コード
                   07  TBL-KOUI-SRYKBN12-SRYCD  PIC  X(09).
      *            回数
                   07  TBL-KOUI-SRYKBN12-KAISU  PIC  9(04).
      *            点数
                   07  TBL-KOUI-SRYKBN12-TEN    PIC  9(09)V9(02).
      *
      *    負担区分編集用エリア
       01  WRK-RECE04-REC.
           COPY    "CPRCF004.INC" REPLACING  //RECE04//
                                   BY         //WRK-RECE04//.
      *
       01  WRK-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-RECE05//.
      *
      *    画像診断用退避エリア
       01  WRK-721-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-721-RECE05//.
       01  WRK-700-RECE05-AREA.
           02  WRK-SRYSYUKBN700-MAX           PIC 9(01).
           02  FLG-SRYSYUKBN700               PIC 9(01)  OCCURS  5.
           02  WRK-700-RECE05-REC                        OCCURS  5.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-700-RECE05//.
       01  WRK-SRYKBN70-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-SRY70-RE05//.
      *    生体検査・加算用退避エリア
       01  WRK-KASAN-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-KASAN-RE05//.
      *    生体検査・逓減用退避エリア
       01  WRK-TEIGEN-RECE05-REC.
           COPY    "CPRCF005.INC"  REPLACING  //RECE05//
                                   BY         //WRK-TEIGEN-RE5//.
      *    後発変更可不可（処方単位）退避用エリア
       01  WRK-GEN10-RECE04-REC.
           COPY    "CPRCF004.INC" REPLACING  //RECE04//
                                   BY        //WRK-GEN10//.
       01  WRK-GEN11-RECE04-REC.
           COPY    "CPRCF004.INC" REPLACING  //RECE04//
                                   BY        //WRK-GEN11//.
      *
       01  WRK-CONS-AREA.
      *    コンマ
           03  WRK-KUGIRI             PIC X(01)   VALUE   ",".
      *    ＴＡＢ
           03  WRK-KUGIRI-TAB         PIC X(01)   VALUE   X"09".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)   VALUE   X"0D".
      *    薬剤料逓減（９０／１００）（内服薬）
           03  WRK-CONS-TEIGEN        PIC X(09)   VALUE   "630010002".
      *    薬剤料逓減（８０／１００）（向精神薬多剤投与）
           03  WRK-CONS-TEIGEN2       PIC X(09)   VALUE   "630010005".
      *    薬剤料逓減（６０／１００）（紹介率が低い大病院３０日以上投薬）
           03  WRK-CONS-TEIGEN3       PIC X(09)   VALUE   "630010006".
      *    薬剤料減点（湿布薬薬剤料上限超）
           03  WRK-CONS-TEIGEN4       PIC X(09)   VALUE   "630010007".
      *
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）
           03  WRK-CONS-SANSO1        PIC X(09)   VALUE   "739210000".
      *    液体酸素・可搬式液体酸素容器（ＬＧＣ）（離島）
           03  WRK-CONS-SANSO2        PIC X(09)   VALUE   "739250000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）
           03  WRK-CONS-SANSO3        PIC X(09)   VALUE   "739200000".
      *    液体酸素・定置式液体酸素貯槽（ＣＥ）（離島）
           03  WRK-CONS-SANSO4        PIC X(09)   VALUE   "739240000".
      *    酸素ボンベ・大型
           03  WRK-CONS-SANSO5        PIC X(09)   VALUE   "739220000".
      *    酸素ボンベ・大型（離島）
           03  WRK-CONS-SANSO6        PIC X(09)   VALUE   "739260000".
      *    酸素ボンベ・小型
           03  WRK-CONS-SANSO7        PIC X(09)   VALUE   "739230000".
      *    酸素ボンベ・小型（離島）
           03  WRK-CONS-SANSO8        PIC X(09)   VALUE   "739270000".
      *
      *    新生児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN1  PIC X(09)   VALUE   "160155290".
      *    乳幼児加算（生体検査）
           03  WRK-CONS-KENSA-KASAN2  PIC X(09)   VALUE   "160155390".
      *
      *    検査逓減
           03  WRK-CONS-KENSA-TEIGEN  PIC X(09)   VALUE   "160000190".
      *    間接撮影の５０％逓減
           03  WRK-CONS-SRYKBN70-TEIGEN
                                      PIC X(09)   VALUE   "170020570".
      *
      *    超音波内視鏡加算
           03  WRK-CONS-CHOONPA-KASAN
                                      PIC X(09)   VALUE   "160148670".
      *
      *    施設基準不適合（検査）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-KENSA
                                      PIC X(09)   VALUE   "160174470".
      *
      *    施設基準不適合（画像）（１００分の８０）
           03  WRK-CONS-SISETUKIJYUN-GAZOU
                                      PIC X(09)   VALUE   "170025170".
      *
      *    施設基準不適合（放射線）（１００分の７０）
           03  WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                      PIC X(09)   VALUE   "180025270".
      *
      *    酸素補正率１．３（１気圧）
           03  WRK-CONS-SANSO-HOSEI   PIC X(09)   VALUE   "770020070".
      *
           03  WRK-CONS-SRYYM-201804   PIC 9(06)   VALUE   201804.
      *
      *    改行コード
       01  WRK-CONS-RETURN          PIC X(01)   VALUE   X"0A".
      *
       01  WRK-FEDT-AREA.
           03  WRK-FEDT-1ST-FLG        PIC 9(01).
           03  WRK-FEDT-REC-FLG        PIC X(01).
           03  WRK-FEDT-NYUINYMD       PIC X(08).
           03  WRK-FEDT-TAIINYMD       PIC X(08).
           03  WRK-FEDT-SRYCD          PIC X(09).
           03  WRK-FEDT-KBNNUM-FLG     PIC 9(01).
           03  WRK-FEDT-NAME           PIC X(200).
           03  WRK-FEDT-SRYKBN-SAVE    PIC X(02).
           03  WRK-FEDT-SRYKBN         PIC X(02).
           03  WRK-FEDT-ZAITEN         PIC S9(08).
           03  WRK-FEDT-YKZTEN         PIC S9(07).
           03  WRK-FEDT-KIZAITEN       PIC 9(07).
           03  WRK-FEDT-TENKBN         PIC X(01).
           03  WRK-FEDT-ZAIKAISU       PIC 9(04).
           03  WRK-FEDT-HKNJANUM       PIC X(08).
           03  WRK-FEDT-RECESYUBETU    PIC 9(04).
           03  WRK-FEDT-YMD.
               05  WRK-FEDT-SRYYM      PIC 9(06).
               05  WRK-FEDT-DD         PIC 9(02).
           03  WRK-FEDT-RECESRYKA      PIC X(02).
           03  WRK-FEDT-SRYKA          PIC X(03).
           03  WRK-FEDT-DRCD           PIC X(05).
           03  WRK-FEDT-BYOTOCD        PIC X(10).
           03  WRK-FEDT-BYOTOKBN       PIC X(01).
           03  WRK-FEDT-NYUGAIKBN      PIC X(01).
           03  WRK-FEDT-SRYSURYO       PIC 9(05)V9(05).
           03  WRK-FEDT-TANICD         PIC 9(03).
           03  WRK-FEDT-MEISAI-TEN     PIC S9(08).
           03  WRK-FEDT-MEISAI-YKZ     PIC S9(09)V9(03).
           03  WRK-FEDT-MEISAI-KIZ     PIC 9(09)V9(03).
           03  WRK-FEDT-DEKIDAKA-TEN   PIC S9(08).
           03  WRK-FEDT-DEKIDAKA-SUM   PIC 9(08).
           03  WRK-FEDT-DEKIDAKA-SUB   PIC 9(01).
           03  WRK-FEDT-HOUKATU-FLG    PIC X(01).
           03  WRK-FEDT-PREV-TEN       PIC 9(08).
           03  WRK-FEDT-PREV-TEN50     PIC 9(08).
           03  WRK-FEDT-PREV-TEN54     PIC 9(08).
           03  WRK-FEDT-PREV-TEN721    PIC 9(08).
           03  WRK-FEDT-KOUI-MEISAI.
             05  WRK-FEDT-KOUI-MEISAI1   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI2   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI3   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI4   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI5   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI6   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI7   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI8   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI9   PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI10  PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI11  PIC 9(01).
             05  WRK-FEDT-KOUI-MEISAI12  PIC 9(01).
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
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    点数マスタ退避ワーク
           COPY    "CPTTNS.INC".
      *
           COPY    "CPDPCCONST.INC".
      *
      *    退院時投薬コメント
      **     COPY    "CPDPCTTOYAKU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSK1009.INC".
      *
           COPY    "CPSK2009.INC".
      *
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
      *    診療科情報
           COPY    "CPSK1005.INC".
      *
      *    帳票編集区分情報
           COPY    "CPSK1030.INC".
      *
           COPY    "CPSK9102.INC".
      *
      *    病名マスタ−
       01  BYOMEI-REC.
           COPY    "CPBYOMEI.INC".
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
       01  TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    診療会計
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療行為
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
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
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *
      *    病名組立サブ
           COPY    "CPORCSMKBYOMEI.INC".
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
       LINKAGE                 SECTION.
       01  COMMAND-PARAM.
           02  FILLER      PIC X(256).
      ****************************************************************
       PROCEDURE                   DIVISION
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
                                       TTNS-AREA
      *
           UNSTRING    COMMAND-PARAM   DELIMITED  BY  ","
                                       INTO    WRK-PARA1
                                               WRK-PARA-TEISYUTUSAKI
                                               WRK-PARA-SYARECEKBN
                                               WRK-PARA-KKHRECEKBN
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               WRK-PARA-CREYMD
                                               WRK-PARA-CREHMS
                                               RECEERR
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    "ＥＦファイル作成"
                                   TO  JOB-SHELLMSG
           MOVE    "ORCRDPC20"     TO  JOB-PGID
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 110-SYSKANRI-HENSYU-SEC
      *
           PERFORM 110-PTNUM-KETA-SEC
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "RECE020"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (2)
           MOVE    "RECE041"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (3)
           MOVE    "RECE051"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (4)
           MOVE    "RECE900"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (5)
           MOVE    "RECE031"           TO  RECEDAT1PARA-FILE-ID
           PERFORM 800-FILENM-SET-SEC
           MOVE    RECEDAT1PARA-BASENAME
                                       TO  SGETTEMP-BASENAMES  (6)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECE02PARA
           MOVE    SGETTEMP-FULLNAMES (3)
                                       TO  RECE04PARA
           MOVE    SGETTEMP-FULLNAMES (4)
                                       TO  RECE05PARA
           MOVE    SGETTEMP-FULLNAMES (5)
                                       TO  RECE900PARA
           MOVE    SGETTEMP-FULLNAMES (6)
                                       TO  RECE031PARA
      *
           MOVE   SGETTEMP-ST          TO  WRK-SGETTEMP-ST
      *
           OPEN    I-O                 RECE02-FILE
           IF      STS-RECE02          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE02PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE02          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    I-O                 RECE04-FILE
           IF      STS-RECE04          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE04PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE04          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
           OPEN    I-O                 RECE05-FILE
           IF      STS-RECE05          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE05PARA          TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE05          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    INPUT               RECE031-FILE
           IF      STS-RECE03          =   "00"
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    1                   TO  WRK-ERR-FLG
               MOVE    RECE031PARA         TO  WRK-ERR-FILE-NM
               MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
               PERFORM 500-FILE-ERR-ABORT-SEC
           END-IF
      *
           OPEN    OUTPUT              RECE900-FILE
           CLOSE                       RECE900-FILE
           OPEN    I-O                 RECE900-FILE
      *
           IF      FLG-END             =   ZERO
      *        ファイル読み込み
               PERFORM 900-RECE02-READ-SEC
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ取得
      *****************************************************************
       110-SYSKANRI-HENSYU-SEC      SECTION.
      *
           IF   WRK-PARA-SRYYM    NOT =  ZERO
      *       医療機関ＩＤ編集
              INITIALIZE                  SYS-1001-REC
              MOVE    "1001"              TO  SYS-1001-KANRICD
              MOVE    "*"                 TO  SYS-1001-KBNCD
              MOVE    WRK-PARA-SRYYM      TO  SYS-1001-STYUKYMD(1:6)
              MOVE    "01"                TO  SYS-1001-STYUKYMD(7:2)
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
                  MOVE    "tbl_syskanri"  TO  MCP-TABLE
                  MOVE    "key8"          TO  MCP-PATHNAME
                  PERFORM  900-SYSKANRI-READ-SEC
                  PERFORM  UNTIL   FLG-SYSKANRI  =    1
                    MOVE    MCPDATA-REC   TO  SYS-1001-REC
                    ADD     1             TO  SYSKAN-IDX
                    MOVE    SYS-1001-REC  TO  TBL-SYS1001(SYSKAN-IDX)
                    MOVE    "tbl_syskanri"    TO  MCP-TABLE
                    MOVE    "key8"            TO  MCP-PATHNAME
                    PERFORM 900-SYSKANRI-READ-SEC
                  END-PERFORM
              END-IF
      *
              MOVE    "tbl_syskanri"      TO  MCP-TABLE
              MOVE    "key8"              TO  MCP-PATHNAME
              PERFORM 900-CLOSE-SEC
              IF      SYSKAN-IDX          =   ZERO
                   MOVE    "医療機関情報が取得できませんでした。"
                                          TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
              END-IF
           END-IF
      *
      *    診療科情報
           MOVE    SPACE               TO  TBL-SRYKA-AREA
           INITIALIZE                      TBL-SRYKA-AREA
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                      SYS-1005-REC
           MOVE    ZERO                TO  SYSKAN-IDX2
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1005-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1005-STYUKYMD(7:2)
           MOVE    SYS-1005-STYUKYMD   TO  SYS-1005-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key7"          TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               PERFORM  UNTIL   FLG-SYSKANRI  =    1
                   MOVE    MCPDATA-REC TO  SYS-1005-REC
                   ADD     1           TO  SYSKAN-IDX2
                   MOVE    SYS-1005-KBNCD (1:2)
                                       TO  TBL-SRYKA (SYSKAN-IDX2)
                   MOVE    SYS-1005-RECESRYKA
                                       TO  TBL-RECESRYKA (SYSKAN-IDX2)
                   MOVE    "tbl_syskanri"  TO  MCP-TABLE
                   MOVE    "key7"          TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
           MOVE    SYSKAN-IDX2         TO  TBL-SRYKA-MAX
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           IF      SYSKAN-IDX2         =   ZERO
                   MOVE    "診療科情報が取得できませんでした。"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
      *    レセプト編集区分情報
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-2009-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-2009-STYUKYMD(7:2)
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2009-REC
           ELSE
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-SYAHO
                                   =   SPACE )
               MOVE    "1"             TO  SYS-2009-SYUKA-SYAHO
           END-IF      
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOKUHO
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOKUHO
           END-IF
      *
      *    帳票編集区分情報
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                      SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-1030-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-1030-STYUKYMD(7:2)
           MOVE    SYS-1030-STYUKYMD   TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-1030-REC
           ELSE
               INITIALIZE                  SYS-1030-REC
           END-IF
      *
           MOVE    SPACE               TO  SYS-9102-REC
           INITIALIZE                      SYS-9102-REC
           MOVE    "9102"              TO  SYS-9102-KANRICD
           MOVE    "*"                 TO  SYS-9102-KBNCD
           MOVE    WRK-PARA-SRYYM      TO  SYS-9102-STYUKYMD(1:6)
           MOVE    "01"                TO  SYS-9102-STYUKYMD(7:2)
           MOVE    SYS-9102-STYUKYMD   TO  SYS-9102-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-9102-HOSPNUM
           MOVE    SYS-9102-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-9102-REC
           ELSE
               INITIALIZE                      SYS-9102-REC
           END-IF
           IF      SYS-9102-NUMBER     =   SPACE
               MOVE    "0"                 TO  SYS-9102-NUMBER
           END-IF
           IF      SYS-9102-KOH-RECEIPT
                                       =   SPACE
               MOVE    "0"                 TO  SYS-9102-KOH-RECEIPT
           END-IF
      *
           DISPLAY "SYS-9102-NUMBER     =" SYS-9102-NUMBER
           DISPLAY "SYS-9102-KOH-RECEIPT=" SYS-9102-KOH-RECEIPT
           .
       110-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理より患者番号桁数取得
      *****************************************************************
       110-PTNUM-KETA-SEC      SECTION.
      *
      *
           INITIALIZE                      SYS-1009-REC
           MOVE    "1009"              TO  SYS-1009-KANRICD
           MOVE    "*"                 TO  SYS-1009-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-1009-HOSPNUM
           MOVE    SYS-1009-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key8"          TO  MCP-PATHNAME
               PERFORM  900-SYSKANRI-READ-SEC
               IF       FLG-SYSKANRI   =   ZERO
                 MOVE    MCPDATA-REC   TO  SYS-1009-REC
                 DISPLAY "SYS-1009-PTNUMKSIKBN = "
                          SYS-1009-PTNUMKSIKBN
                 EVALUATE  SYS-1009-PTNUMKSIKBN
                     WHEN  "1"
                         DISPLAY "SYS-1009-JIYKSIKETA = "
                                  SYS-1009-JIYKSIKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-JIYKSIKETA
                     WHEN  "2"
                         DISPLAY "SYS-1009-HJNKSIRENNUMKETA = "
                                  SYS-1009-HJNKSIRENNUMKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-HJNKSIRENNUMKETA
                     WHEN  "3"
                         DISPLAY "SYS-1009-KKCKSIMAEKETA = "
                                  SYS-1009-KKCKSIMAEKETA
                         DISPLAY "SYS-1009-KKCKSIRENNUMKETA = "
                                  SYS-1009-KKCKSIRENNUMKETA
                         DISPLAY "SYS-1009-KKCKSIATOKETA = "
                                  SYS-1009-KKCKSIATOKETA
                         COMPUTE  WRK-ZEROKETA  =   10  -
                                  SYS-1009-KKCKSIMAEKETA  -
                                  SYS-1009-KKCKSIRENNUMKETA -
                                  SYS-1009-KKCKSIATOKETA
                 END-EVALUATE
                 DISPLAY "WRK-ZEROKETA = " WRK-ZEROKETA
                 COMPUTE  WRK-PTNUMKETA  = WRK-ZEROKETA  +  1
                 DISPLAY "WRK-PTNUMKETA = " WRK-PTNUMKETA
               ELSE
                 MOVE    "患者番号構成が取得できませんでした。"
                                       TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       110-PTNUM-KETA-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    診療年月・患者ID・レセ科別処理
           MOVE    KEY-NEW         TO  KEY-OLD
      *
      *    傷病名の順序番号の取り込み
           MOVE    ZERO            TO  WRK-RENNUM-SY
           IF      KEY-N-SRYYM         >=  WRK-CONS-SRYYM-201804
               INITIALIZE                      RECE900-REC
               MOVE    KEY-N-HOSPNUM       TO  RECE900-HOSPNUM
               MOVE    KEY-N-SRYYM         TO  RECE900-SRYYM
               MOVE    KEY-N-PTID          TO  RECE900-PTID
               MOVE    "SY"                TO  RECE900-SRYKBN
               PERFORM 900-RECE900-READ-SEC
               IF      FLG-RECE900     =   1
                   MOVE    ZERO            TO  WRK-RENNUM-SY
               ELSE
                   MOVE    RECE900-RENNUM  TO  WRK-RENNUM-SY
               END-IF
           END-IF
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      KEY-NEW1
                                                   NOT =   KEY-OLD1
      *
      *        診療年月・患者ID・レセ科・提出先別処理
               MOVE    KEY-NEW         TO  KEY-OLD
               PERFORM                 UNTIL   FLG-END     =   1
                                       OR      KEY-NEW NOT =   KEY-OLD
      *
                   MOVE    ZERO                TO  WRK-RENNUM
                   MOVE    ZERO                TO  WRK-MEISAI-RENNUM
                   MOVE    SPACE               TO  WRK-FEDT-SRYKBN-SAVE
                   MOVE    ZERO                TO  WRK-COM-RENNUM
                                                   FLG-TOKUTEINYUIN
      *
                   MOVE    ZERO               TO  WRK-RENNUM-E
                                                  WRK-RENNUM-F 
      *
      *            包括分データ作成
      **AZU
      **             PERFORM     9999-HOUKATU-SEC
      *            摘要レコード
                   PERFORM     2007-HENSYU-SEC
      *            傷病名レコード
                   PERFORM     2006-HENSYU-SEC
      *
                   ADD     1           TO  CNT-COMMIT
                   IF      CNT-COMMIT      =    10
                       MOVE  ZERO      TO      CNT-COMMIT
                       PERFORM 900-DBCOMMIT-SEC
                      PERFORM 900-DBSTART-SEC
                   END-IF
      *
                   PERFORM     900-RECE02-READ-SEC
               END-PERFORM
           END-PERFORM
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病名レコード編集処理
      *****************************************************************
       2006-HENSYU-SEC              SECTION.
      *
           IF      RECE02-SRYYM     <   WRK-CONS-SRYYM-201804
               GO  TO  2006-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  WRK-MEISAI-RENNUM-SY
      *
      *    傷病名情報
           INITIALIZE                      RECE031-REC
           MOVE    RECE02-KEY          TO  RECE031-KEY1
      *
           START   RECE031-FILE
               KEY IS    >=   RECE031-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE031
               NOT INVALID
                   PERFORM 990-RECE03-NEXT-SEC
           END-START
      *
           PERFORM UNTIL  ( FLG-RECE031  =   1            )
                   OR     ( RECE02-KEY
                                     NOT =   RECE031-KEY1 )
      *        転帰区分:8(移行)以外 処理対象
               IF      RECE031-TENKIKBN     =   "8"
                   CONTINUE
               ELSE    
                   PERFORM 20061-BYOMEI-HENSYU-SEC
               END-IF
      *             
               PERFORM 990-RECE03-NEXT-SEC
           END-PERFORM
           .
       2006-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    帳票編集<傷病名>処理
      *****************************************************************
       20061-BYOMEI-HENSYU-SEC         SECTION.
      *     
      *    廃止年月日が設定されているとき廃止年月日＜診療開始日なら
      *    未傷病名コードにする
      *    保険請求対象外のときは未傷病名コードにする
      *    保険請求対象外の傷病名コードのみのときはそのまま記録する
      *
           IF      RECE031-BYOMEIHENFLG =   "1"
                CONTINUE
           ELSE    
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE031-BYOMEICD(IDX)   =   SPACE
                   IF      RECE031-BYOMEICD(IDX)   =   ALL "9"
                       CONTINUE
                   ELSE  
                       IF      RECE031-BYOMEICD(IDX)(1:3)
                                                   NOT =   "ZZZ"
                           MOVE    RECE031-BYOMEICD(IDX)
                                                   TO  BYO-BYOMEICD
                           PERFORM 900-BYOMEI-READ-SEC
                           IF      BYO-HKNSKYKBN   =   1
                               IF      RECE031-BYOMEICD (2)
                                                   NOT =   SPACE
                                   MOVE    "1" TO  RECE031-BYOMEIHENFLG
                               END-IF
                               MOVE    21      TO  IDX
                           ELSE
                               IF      BYO-HAISIYMD    NOT =   SPACE
                                   IF      BYO-HAISIYMD    <
                                                       RECE031-SRYYMD
                                       MOVE    "1"     TO
                                                   RECE031-BYOMEIHENFLG
                                       MOVE    21      TO  IDX
                                   END-IF 
                               END-IF
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    疑いフラグより病名を編集
           IF      RECE031-UTAGAIFLG   NOT =   SPACE
               INITIALIZE                      ORCSMKBYOMEIAREA
               MOVE    RECE031-UTAGAIFLG   TO  SMKBYO-UTAGAIFLG
               MOVE    RECE031-IN-BYOMEI   TO  SMKBYO-BYOMEI
               MOVE    RECE031-BYOMEIHENFLG
                                           TO  SMKBYO-BYOMEIHENFLG
               MOVE    RECE031-BYOMEICD-G  TO  SMKBYO-BYOMEICD-G
               CALL    "ORCSMKBYOMEI"      USING   ORCSMKBYOMEIAREA
               MOVE    SMKBYO-BYOMEI       TO  RECE031-BYOMEI
               MOVE    SMKBYO-BYOMEIHENFLG TO  RECE031-BYOMEIHENFLG
               MOVE    SMKBYO-BYOMEICD-G   TO  RECE031-BYOMEICD-G
           ELSE
               MOVE    RECE031-IN-BYOMEI   TO  RECE031-BYOMEI 
           END-IF
      *
      *    傷病名コード／修飾語コードの取得
           MOVE    SPACE           TO  WRK-BYOMEI-AREA
      *
           IF      RECE031-BYOMEIHENFLG =   "1"
      *        編集病名のとき未傷病名コードにする
               MOVE    "0000999"           TO  WRK-BYOMEICD
           ELSE
               MOVE    ZERO                TO  IDY
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       21
                       OR      RECE031-BYOMEICD(IDX)   =   SPACE
                 IF      RECE031-BYOMEICD(IDX) =   ALL "9"
                     CONTINUE
                 ELSE
                   IF      RECE031-BYOMEICD(IDX)(1:3)  NOT =   "ZZZ"
                       IF      WRK-BYOMEICD                =   SPACE
                           MOVE    RECE031-BYOMEICD(IDX)
                                                   TO  WRK-BYOMEICD
                       END-IF
                   END-IF
                 END-IF
               END-PERFORM
           END-IF
      *
           ADD     1                   TO  WRK-RENNUM-SY
           MOVE    ZERO                TO  WRK-MEISAI-RENNUM-SY
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
           PERFORM 200611-BYOMEI-WRITE-SEC
      * 
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
           PERFORM 200611-BYOMEI-WRITE-SEC
           .
       20061-BYOMEI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    傷病名レコード出力処理
      *****************************************************************
       200611-BYOMEI-WRITE-SEC         SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
      *
      *    施設コード
           STRING  SYS-1001-PREFNUM    DELIMITED   BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED   BY  SIZE
                   WRK-KUGIRI-TAB      DELIMITED   BY  SIZE
                   INTO                WRK-REC
           END-STRING
           MOVE    10                  TO  WRK-REC-LENGTH
      *    データ識別番号
           IF      SYS-9102-NUMBER     =   "0"
               MOVE    11                  TO  IDW3
               MOVE    "0000000000"        TO  WRK-DATA
               PERFORM VARYING IDW4    FROM    20  BY  -1
                       UNTIL   IDW4    <       1
                   IF      RECE02-PTNUM (IDW4:1)    NOT =   SPACE
                       IF      IDW3        =   1
                           MOVE    SPACE               TO  WRK-ERR-AREA
                           INITIALIZE                      WRK-ERR-AREA
                           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
                           STRING "患者番号の桁数が１０桁を超えています"
                                               DELIMITED   BY  SIZE
                           " 患者番号＝"           DELIMITED   BY  SIZE
                           RECE02-PTNUM        DELIMITED   BY  SIZE
                                               INTO    WRK-RECEERR
                           END-STRING
                           PERFORM 500-ERR-HENSYU-SEC
                           MOVE   "ptnum err"     TO  WRK-ERRMSG
                           PERFORM 500-COBABORT-SEC
                       ELSE
                           COMPUTE IDW3    =   IDW3    -   1
                           MOVE    RECE02-PTNUM (IDW4:1)
                                               TO  WRK-DATA (IDW3:1)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               MOVE    RECE02-PTID         TO  WRK-DATA
           END-IF
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    生年月日
           MOVE    RECE02-BIRTHDAY     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    外来受診年月日
           MOVE    RECE02-SRYYM        TO  WRK-DATA
           MOVE    "00"                TO  WRK-DATA (7:2)
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    データ区分（診療識別）
           MOVE    "SY"                TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    順序番号
           MOVE    WRK-RENNUM-SY       TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           IF      WRK-FEDT-REC-FLG    =   "F"
      *      行為明細番号
             ADD   1                       TO  WRK-MEISAI-RENNUM-SY
             MOVE  WRK-MEISAI-RENNUM-SY    TO  WRK-DATA
             MOVE  3                       TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *
      *    病院点数マスタコード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    レセプト電算処理システム用コード
           MOVE    WRK-BYOMEICD        TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    解釈番号（基本）
           PERFORM 5004-MOJI-HENSYU-SEC
      *    診療行為名称
           MOVE    RECE031-BYOMEI      TO  WRK-DATA
           MOVE    254                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           IF      WRK-FEDT-REC-FLG    =   "F"
      *        使用量
               MOVE    "0000000.000"       TO  WRK-DATA
               MOVE    11                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        基準単位
               MOVE    "000"               TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *
      *    行為点数・行為明細点数
           MOVE    "00000000"          TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *    行為薬剤料・行為明細薬剤料
           MOVE    "00000000.000"      TO  WRK-DATA
           IF      WRK-FEDT-REC-FLG    =   "F"
               MOVE    12                  TO  WRK-NAGASA
           ELSE
               MOVE    8                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    行為材料料・行為明細材料料
           IF      WRK-FEDT-REC-FLG    =   "F"
               MOVE    "00000000.000"      TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
           ELSE
               MOVE    "00000000"          TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
           END-IF
           PERFORM 5004-MOJI-HENSYU-SEC
      *    円・点区分
           MOVE    "1"                 TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *
           IF      WRK-FEDT-REC-FLG    =   "E"
      *        行為回数
               MOVE    "001"               TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        保険者番号
               MOVE    WRK-FEDT-HKNJANUM   TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        レセプト種別
               MOVE    WRK-FEDT-RECESYUBETU
                                           TO  WRK-NUM
               MOVE    4                   TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
      *        診療開始日
               MOVE    RECE031-SRYYMD      TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        レセプト科区分
               MOVE    WRK-FEDT-RECESRYKA  TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        診療科区分
               MOVE    WRK-FEDT-SRYKA      TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        医師コード
               MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        病棟コード
               MOVE    WRK-FEDT-BYOTOCD    TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        病棟区分
               MOVE    WRK-FEDT-BYOTOKBN   TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        入外区分
               MOVE    "1"                 TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        施設タイプ
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-WRITE-SEC
           END-IF
      *
           IF      WRK-FEDT-REC-FLG    =   "F"
      *        出来高実績点数
               MOVE    "00000000"          TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *        行為明細区分情報（性別＋転記区分＋主病名）
               MOVE    "000000000000"      TO  WRK-DATA (1:12)
               MOVE    RECE02-SEX          TO  WRK-DATA (3:1)
               EVALUATE    RECE031-TENKIKBN
                   WHEN    "1"
                       MOVE    "2"         TO  WRK-DATA (4:1)
                   WHEN    "2"
                       MOVE    "3"         TO  WRK-DATA (4:1)
                   WHEN    "3"
                       MOVE    "4"         TO  WRK-DATA (4:1)
                   WHEN    SPACE
                       MOVE    "1"         TO  WRK-DATA (4:1)
               END-EVALUATE
               IF      RECE031-SYUBYOFLG   =   "1"
                   MOVE    "1"                TO  WRK-DATA (5:1)
               ELSE
                   MOVE    "0"                TO  WRK-DATA (5:1)
               END-IF 
               MOVE    12                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               PERFORM 5001-FILE-F-WRITE-SEC
           END-IF
           .
       200611-BYOMEI-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要レコード編集処理
      *****************************************************************
       2007-HENSYU-SEC              SECTION.
      *
      *
      *    摘要情報（診察料）
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-KEY          TO  RECE04-KEY1
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   DISPLAY "RECE04 START INVALID"
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   DISPLAY "RECE04 START NOT INVALID"
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
      *
           PERFORM
               UNTIL  ( FLG-END              =   1           )
               OR     ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
      **AZU
             IF     ( RECE04-SRYKBN       =  "11"   OR   "12")   AND
                    ( RECE04-SRYCDTOTAL   NOT =  112011010)
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *        摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   PERFORM 20072-ZAI-HENSYU-SEC
      *
               END-IF
             END-IF
             PERFORM 990-RECE04-NEXT-SEC
           END-PERFORM
      *
      *    摘要情報（外来管理加算）
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-KEY          TO  RECE04-KEY1
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   DISPLAY "RECE04 START INVALID"
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   DISPLAY "RECE04 START NOT INVALID"
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1           )
               OR     ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
      **AZU
             IF     ( RECE04-SRYKBN       =     "12")   AND
                    ( RECE04-SRYCDTOTAL   =  112011010)
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *        摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   PERFORM 20072-ZAI-HENSYU-SEC
      *
               END-IF
             END-IF
             PERFORM 990-RECE04-NEXT-SEC
           END-PERFORM
      *
      *    後発品変更負荷処方単位情報
           INITIALIZE                      WRK-GEN10-RECE04-REC
           INITIALIZE                      WRK-GEN11-RECE04-REC
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-KEY          TO  RECE04-KEY1
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   DISPLAY "RECE04 START INVALID"
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   DISPLAY "RECE04 START NOT INVALID"
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1           )
               OR     ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
             IF     ( RECE04-SRYKBN       =  "98" )
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
             END-IF
             PERFORM 990-RECE04-NEXT-SEC
           END-PERFORM
      *
      *    摘要情報（診察料、外来管理加算以外）
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-KEY          TO  RECE04-KEY1
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   DISPLAY "RECE04 START INVALID"
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   DISPLAY "RECE04 START NOT INVALID"
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END              =   1           )
               OR     ( FLG-RECE04           =   1           )
               OR     ( RECE02-KEY       NOT =   RECE04-KEY1 )
      **AZU
             IF       RECE04-SRYKBN  =  "11" OR "12" OR "98"
                 CONTINUE
             ELSE
      *
      *        明細テーブル退避処理
               PERFORM 20071-ZAI-TAIHI-SEC
      *        摘要データ編集処理
               IF      FLG-END           =   ZERO
                   PERFORM 20074-ZAI-KOUI-SORT-SEC
                   PERFORM 20072-ZAI-HENSYU-SEC
               END-IF
             END-IF
             PERFORM 990-RECE04-NEXT-SEC
           END-PERFORM
      *
           .
       2007-HENSYU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    剤　明細テーブル退避処理
      *****************************************************************
       20071-ZAI-TAIHI-SEC    SECTION.
      *
           INITIALIZE                      TBL-ZAIW-AREA
                                           TBL-KOUI-AREA
                                           TBL-COMMENT-AREA
           INITIALIZE                      WRK-RECE05-REC
                                           WRK-KASAN-RECE05-REC
                                           WRK-TEIGEN-RECE05-REC
                                           WRK-700-RECE05-AREA
                                           WRK-721-RECE05-REC
                                           WRK-SRYKBN70-RECE05-REC
      *
           MOVE    ZERO                TO  WRK-ZAIWMAX
           MOVE    ZERO                TO  WRK-TBLMAX1
                                           WRK-TBLMAX6
                                           WRK-TBLMAX7
                                           WRK-TBLMAX8
                                           FLG-TEIGEN
                                           FLG-KOKUJISKBKBN1
                                           FLG-SRYSYUKBN721
                                           FLG-SRYKBN70-TEIGEN
                                           FLG-KENSA-KASAN
                                           FLG-KENSA-TEIGEN
                                           WRK-SRYCD1-MAX
                                           WRK-SRYCD71-MAX
                                           WRK-SRYCD72-MAX
                                           WRK-KOKUJISKBKBN1-KOUI-TEN
      *
           MOVE    RECE04-SRYKBN      TO  WRK-SRYKBN
      *
           INITIALIZE                       RECE05-REC
           MOVE    RECE04-HOSPNUM      TO  RECE05-HOSPNUM
           MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE04-PTID         TO  RECE05-PTID
           MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE04-KOHID        TO  RECE05-KOHID
           MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE04-RECEIPT-KBN  TO  RECE05-RECEIPT-KBN
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-END               =   1                   )
               OR     ( FLG-RECE05            =   1                   )
               OR     ( RECE04-HOSPNUM    NOT =   RECE05-HOSPNUM      )
               OR     ( RECE04-SRYYM      NOT =   RECE05-SRYYM        )
               OR     ( RECE04-NYUGAIKBN  NOT =   RECE05-NYUGAIKBN    )
               OR     ( RECE04-PTID       NOT =   RECE05-PTID         )
               OR     ( RECE04-RECEKA     NOT =   RECE05-RECEKA       )
               OR     ( RECE04-TEISYUTUSAKI
                                          NOT =   RECE05-TEISYUTUSAKI )
               OR     ( RECE04-RECESYUBETU
                                          NOT =   RECE05-RECESYUBETU  )
               OR     ( RECE04-HKNJANUM   NOT =   RECE05-HKNJANUM     )
               OR     ( RECE04-HOJOKBN    NOT =   RECE05-HOJOKBN      )
               OR     ( RECE04-KOHID      NOT =   RECE05-KOHID        )
               OR     ( RECE04-TEKSTYMD   NOT =   RECE05-TEKSTYMD     )
               OR     ( RECE04-RECEIPT-KBN NOT =  RECE05-RECEIPT-KBN  )
               OR     ( RECE04-ZAINUM     NOT =   RECE05-ZAINUM       )
      *
      *        9999包括分剤は自保区分に１がセットされている
               IF       RECE04-JIHOKBN        =   "1"
                   MOVE      "6"               TO  WRK-FEDT-HOUKATU-FLG
               ELSE
                   MOVE      "1"               TO  WRK-FEDT-HOUKATU-FLG
               END-IF
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
                         OR      ( FLG-END           =   1     )
      *
      *         入院料はコメントのみ対象とする
                IF    ( RECE04-SRYKBN               >=  "90"        )
                AND   ( RECE05-SRYCD (IDX) (1:1) NOT =   "8"   AND
                        RECE05-SRYCD (IDX) (1:3) NOT =   "008" AND
                        RECE05-SRYCD (IDX)       NOT =  "190128310" AND
                        RECE05-SRYCD (IDX)       NOT =  "190128410" AND
                        RECE05-SRYCD (IDX)       NOT =  "190128570" AND
                        RECE05-SRYCD (IDX)       NOT =  "099209910" AND
                        RECE05-SRYCD (IDX)       NOT =  "099209911" )
                 CONTINUE
                ELSE
      *
                 IF    (WRK-PARA-SRYYM          >=  "201004")   AND
                       (RECE05-SRYCD (IDX)       =  "190128310" OR
                                                    "190128410" OR
                                                    "190128570")
                     CONTINUE
                 ELSE
      *
                 IF      RECE05-RECEDENFLG (IDX)     =   SPACE
                   MOVE    SPACE                TO  WRK-KIZAI-SRYCD
                   IF      RECE05-SRYCD (IDX) (1:3) = "059"
                       MOVE    RECE05-SRYCD (IDX)
                                                TO  WRK-KIZAI-SRYCD
                       MOVE    "777770000"      TO  RECE05-SRYCD (IDX)
                   END-IF
      *
                   EVALUATE    RECE05-SRYCD (IDX) (1:1)
                       WHEN    "0"
                       WHEN    "8"
                           IF   (RECE05-SRYKBN = "21" OR "22" OR "23" OR
                                                 "14")
                           AND ((RECE05-SRYCD(IDX)
                                               = "840000006")
                           OR   (RECE05-SRYCD(IDX)
                                               = SYS-9102-SRYCD(1))
                           OR   (RECE05-SRYCD(IDX)
                                               = SYS-9102-SRYCD(2)))
                               MOVE  "1"      TO WRK-FEDT-HOUKATU-FLG
      *            テーブルが３件なので繰り返しは保留にしIF文のみとした 
      *                        PERFORM VARYING TBL-TAITOYAKU-IDX
      *                            FROM  1  BY  1
      *                            UNTIL (TBL-TAITOYAKU-IDX
      *                                  >  TBL-TAITOYAKU-MAX)
      *                              OR  (TBL-TAITOYAKU
      *                                   (TBL-TAITOYAKU-IDX) 
      *                                  =  "000000000")
      *                            IF  RECE05-SRYCD(IDX)
      *                                  =  TBL-TAITOYAKU
      *                                      (TBL-TAITOYAKU-IDX) 
      *                                MOVE "1" TO WRK-FEDT-HOUKATU-FLG
      *                            END-IF
      *                        END-PERFORM
                           END-IF
                           IF    RECE05-SRYCD(IDX)  =  "099209910"
                               MOVE RECE04-REC TO WRK-GEN10-RECE04-REC
                           END-IF
                           IF    RECE05-SRYCD(IDX)  =  "099209911"
                               MOVE RECE04-REC TO WRK-GEN11-RECE04-REC
                           END-IF
                       WHEN    "1"
                           PERFORM 200711-KOUI-TAIHI-SEC
                       WHEN    "6"
                           PERFORM 200711-YKZ-TAIHI-SEC
                       WHEN    "7"
                           PERFORM 200711-KIZAI-TAIHI-SEC
                   END-EVALUATE
                 END-IF
                 END-IF
                END-IF
               END-PERFORM
      *
               PERFORM 990-RECE05-NEXT-SEC
           END-PERFORM
      *
      *    画像診断のとき順番を変える
      *
           IF      FLG-SRYSYUKBN721    >   ZERO
               MOVE     FLG-SRYSYUKBN721   TO  IDX
               MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *
               IF      FLG-SRYKBN70-TEIGEN >   ZERO
                   MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX
                   MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               END-IF
      *
               IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX
                       MOVE     FLG-SRYSYUKBN700  (IDXY)  TO  IDX
                       MOVE    WRK-700-RECE05-REC (IDXY)  TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   END-PERFORM
               END-IF
           END-IF
      *
      *    生体検査とき順番を変える
           IF      FLG-KENSA-TEIGEN    >   ZERO
               MOVE    FLG-KENSA-TEIGEN        TO  IDX
               MOVE    WRK-TEIGEN-RECE05-REC   TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
           IF      FLG-KENSA-KASAN     >   ZERO
               MOVE     FLG-KENSA-KASAN        TO  IDX
               MOVE    WRK-KASAN-RECE05-REC    TO  RECE05-REC
               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
           END-IF
           .
       20071-TBL-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ退避処理
      *****************************************************************
       200711-KOUI-TAIHI-SEC          SECTION.
      *
      *    画像診断・生体検査のとき順番を変える
           IF      RECE05-SRYSYUKBN-X (IDX)
                                       =   "721"
               MOVE    RECE05-REC          TO  WRK-RECE05-REC
               MOVE    IDX                 TO  IDW1
      *
               IF      FLG-SRYSYUKBN721    >   ZERO
                   MOVE     FLG-SRYSYUKBN721   TO  IDX
                   MOVE    WRK-721-RECE05-REC  TO  RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                   MOVE    ZERO                TO  FLG-SRYSYUKBN721
                   INITIALIZE                  WRK-721-RECE05-REC
      *
                   IF      FLG-SRYKBN70-TEIGEN >   ZERO
                       MOVE     FLG-SRYKBN70-TEIGEN
                                               TO  IDX
                       MOVE    WRK-SRYKBN70-RECE05-REC
                                               TO  RECE05-REC
                       PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       MOVE    ZERO            TO  FLG-SRYKBN70-TEIGEN
                       INITIALIZE              WRK-SRYKBN70-RECE05-REC
                   END-IF
      *
                   IF      WRK-SRYSYUKBN700-MAX    >   ZERO
                       PERFORM VARYING IDXY    FROM    1   BY  1
                               UNTIL   IDXY    >   WRK-SRYSYUKBN700-MAX
                           MOVE     FLG-SRYSYUKBN700  (IDXY)
                                                           TO  IDX
                           MOVE    WRK-700-RECE05-REC (IDXY)
                                                           TO RECE05-REC
                           PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
                       END-PERFORM
      *****************MOVE    ZERO                TO  FLG-SRYSYUKBN700
                       INITIALIZE                  WRK-700-RECE05-AREA
                   END-IF
               END-IF
      *
               MOVE    WRK-RECE05-REC      TO  RECE05-REC
               MOVE    IDW1                TO  IDX
      *
               MOVE    IDX                 TO  FLG-SRYSYUKBN721
               MOVE    RECE05-REC          TO WRK-721-RECE05-REC
           ELSE
               IF    ( RECE05-SRYCD (IDX)  =  WRK-CONS-SRYKBN70-TEIGEN )
               AND   ( FLG-SRYSYUKBN721    >  ZERO                     )
                   MOVE    IDX         TO  FLG-SRYKBN70-TEIGEN
                   MOVE    RECE05-REC  TO  WRK-SRYKBN70-RECE05-REC
                   PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
               ELSE
                   IF    ( RECE05-SRYSYUKBN-X (IDX)
                                               =   "700" )
                   AND   ( RECE05-DATAKBN   (IDX)
                                               =   "2"   )
                   AND   ( FLG-SRYSYUKBN721    >   ZERO  )
                       ADD     1               TO  WRK-SRYSYUKBN700-MAX
                       MOVE    IDX             TO  FLG-SRYSYUKBN700
                                               (WRK-SRYSYUKBN700-MAX)
                       MOVE    RECE05-REC      TO WRK-700-RECE05-REC
                                               (WRK-SRYSYUKBN700-MAX)
                   ELSE
      *****************IF    ( RECE05-SRYCD (IDX)
      *****************                    =   WRK-CONS-KENSA-KASAN1
      *****************                    OR  WRK-CONS-KENSA-KASAN2 )
      *****************    MOVE    IDX         TO  FLG-KENSA-KASAN
      *****************    MOVE    RECE05-REC  TO  WRK-KASAN-RECE05-REC
      *****************ELSE
      *****************    IF      RECE05-SRYCD (IDX)
      *****************                        =   WRK-CONS-KENSA-TEIGEN
      *****************        MOVE    IDX         TO  FLG-KENSA-TEIGEN
      *****************        MOVE    RECE05-REC  TO
      *****************                            WRK-TEIGEN-RECE05-REC
      *****************    ELSE
                               PERFORM 2007111-KOUI-TAIHI-HENSYU-SEC
      *****************    END-IF
      *****************END-IF
                   END-IF
               END-IF
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）退避編集処理
      *****************************************************************
       2007111-KOUI-TAIHI-HENSYU-SEC SECTION.
      *
           IF      WRK-TBLMAX1         >=  100
               MOVE    "*** ORCRDPC20 KOUI TABLE OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               ADD     1                   TO  WRK-TBLMAX1
      *
               MOVE    WRK-TBLMAX1         TO  WRK-SRYCD1-MAX
      *
               MOVE    RECE05-SRYACTINF (IDX)  TO
                                     TBL-KOUI-SRYACTINF (WRK-TBLMAX1)
               MOVE    RECE05-NAMEINF (IDX)    TO
                                     TBL-KOUI-NAMEINF   (WRK-TBLMAX1)
      *        皮内、皮下及び筋肉内注射と静脈内注射は名称の再セット
               IF      RECE05-NAME (IDX)  =   SPACE
                   MOVE    RECE05-INPUTCOMENT (IDX)    TO
                                     TBL-KOUI-NAME   (WRK-TBLMAX1)
               END-IF
      *
               IF    ( RECE04-SRYKBN      =   "13"  OR  "14" )
                   IF      TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                           NOT =   "1"
                       MOVE    IDX             TO  FLG-KOKUJISKBKBN1
                   END-IF
                   IF    ( TBL-KOUI-KOKUJISKBKBN1(WRK-TBLMAX1)
                                                   =   "1" )
                   AND   ( TBL-KOUI-DATAKBN (WRK-TBLMAX1)
                                                   =   2 )
                       ADD     TBL-KOUI-TEN (WRK-TBLMAX1)
                                       TO  WRK-KOKUJISKBKBN1-KOUI-TEN
                   END-IF
               END-IF
           END-IF
           .
       200711-KOUI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（医薬品）退避処理
      *****************************************************************
       200711-YKZ-TAIHI-SEC          SECTION.
      *
           IF      RECE05-SRYCD (IDX)  =   (WRK-CONS-TEIGEN  OR
                                            WRK-CONS-TEIGEN2 OR
                                            WRK-CONS-TEIGEN3 OR
                                            WRK-CONS-TEIGEN4)
               CONTINUE
           ELSE
      *         IF      RECE04-YKZTEN    =   ZERO
      *             GO  TO  200711-YKZ-TAIHI-EXT
      *         END-IF
      *         院外処方も記録する為、上記の判定文を削除
                CONTINUE
           END-IF
      *
           ADD     1                   TO  WRK-TBLMAX6
      *
           IF      WRK-TBLMAX6         =   1
               IF      RECE05-SRYCD (IDX)
                                       =  (WRK-CONS-TEIGEN  OR
                                           WRK-CONS-TEIGEN2 OR
                                           WRK-CONS-TEIGEN3 OR
                                           WRK-CONS-TEIGEN4)
                   MOVE    1               TO  FLG-TEIGEN
               END-IF
           END-IF
      *
           IF      WRK-TBLMAX6         >   100
               MOVE    "*** ORCRDPC20 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (1 WRK-TBLMAX6)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (1 WRK-TBLMAX6)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (1 WRK-TBLMAX6)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(1 WRK-TBLMAX6)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
      *        点数識別が４（除算金額）のときは１０で除した値に置き換える
               IF      RECE05-SRYCD (IDX)(1:1)  =   "6"   AND
                       RECE05-TENSIKIBETU (IDX) =    4
                   COMPUTE   TBL-ZAIW-TEN (1 WRK-TBLMAX6)
                          =  RECE05-TEN (IDX)   /   10
               END-IF
      *        点数識別が５（乗算金額）のときは１０で乗じた値に置き換える
               IF      RECE05-SRYCD (IDX)(1:1)  =   "6"   AND
                       RECE05-TENSIKIBETU (IDX) =    5
                   COMPUTE   TBL-ZAIW-TEN (1 WRK-TBLMAX6)
                          =  RECE05-TEN (IDX)   *   10
               END-IF
               IF      RECE05-SRYCD (IDX)  =  (WRK-CONS-TEIGEN  OR
                                               WRK-CONS-TEIGEN2 OR
                                               WRK-CONS-TEIGEN3 OR
                                               WRK-CONS-TEIGEN4)
                       MOVE   RECE04-ZAITEN    TO
                                   TBL-ZAIW-TEN        (1 WRK-TBLMAX6)
               END-IF
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (1 WRK-TBLMAX6)
               MOVE    RECE05-NAME       (IDX)  TO
                                   TBL-ZAIW-NAME       (1 WRK-TBLMAX6)
               MOVE    RECE05-YUKOKETA   (IDX)  TO
                                   TBL-ZAIW-YUKOKETA   (1 WRK-TBLMAX6)
           END-IF
           .
       200711-YKZ-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（その他）退避処理
      *****************************************************************
       200711-KIZAI-TAIHI-SEC          SECTION.
      *
           IF    ( RECE04-SRYKBN  NOT =   "70" )
           AND   ( RECE04-KIZAITEN    =   ZERO )
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *
      *    酸素補正率１．３（１気圧）のとき
           IF     RECE05-SRYCD (IDX)    =   WRK-CONS-SANSO-HOSEI
               GO  TO  200711-KIZAI-TAIHI-EXT
           END-IF
      *
           ADD     1                   TO  WRK-TBLMAX7
      *
           IF      WRK-TBLMAX7         >   100
               MOVE    "*** ORCRDPC20 TABLE 100 OVER  ***"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
               MOVE    1                   TO  FLG-END
           ELSE
               MOVE    RECE05-SRYCD      (IDX) TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
               MOVE    RECE05-SRYSURYO   (IDX) TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
               MOVE    RECE05-TANICD     (IDX) TO
                                   TBL-ZAIW-TANICD     (2 WRK-TBLMAX7)
               MOVE    RECE05-TENSIKIBETU(IDX) TO
                                   TBL-ZAIW-TENSIKIBETU(2 WRK-TBLMAX7)
               MOVE    RECE05-TEN        (IDX) TO
                                   TBL-ZAIW-TEN        (2 WRK-TBLMAX7)
               MOVE    RECE05-DATAKBN    (IDX) TO
                                   TBL-ZAIW-DATAKBN    (2 WRK-TBLMAX7)
               MOVE    RECE05-NAMEINF   (IDX)  TO
                                   TBL-ZAIW-NAMEINF    (2 WRK-TBLMAX7)
      *
               IF    ( RECE05-DATAKBN (IDX)
                                           =   "3"  )
               AND   ( RECE04-SRYKBN      =   "70" )
                   MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX
               ELSE
                   IF      RECE05-SRYCD (IDX)
                                              =   "799990070"
                       MOVE    1                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD71-MAX
                   ELSE
                       MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                       MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
                   END-IF
               END-IF
           END-IF
      *
      *    酸素のとき補正率を編集
           IF    ( RECE05-SRYCD (IDX)  =   WRK-CONS-SANSO1
                                       OR  WRK-CONS-SANSO2
                                       OR  WRK-CONS-SANSO3
                                       OR  WRK-CONS-SANSO4
                                       OR  WRK-CONS-SANSO5
                                       OR  WRK-CONS-SANSO6
                                       OR  WRK-CONS-SANSO7
                                       OR  WRK-CONS-SANSO8 )
               ADD     1                   TO  WRK-TBLMAX7
      *
               IF      WRK-TBLMAX7         >   100
                   MOVE    "*** ORCRDPC20 TABLE 100 OVER  ***"
                                               TO  WRK-RECEERR
                   PERFORM 500-ERR-HENSYU-SEC
                   MOVE    1                   TO  FLG-END
               ELSE
                   MOVE    WRK-CONS-SANSO-HOSEI TO
                                   TBL-ZAIW-SRYCD      (2 WRK-TBLMAX7)
                   MOVE    1                   TO
                                   TBL-ZAIW-SRYSURYO   (2 WRK-TBLMAX7)
                   MOVE    2                  TO
                                   TBL-ZAIW-DATSKB     (2 WRK-TBLMAX7)
                   MOVE    WRK-TBLMAX7        TO  WRK-SRYCD72-MAX
               END-IF
           END-IF
           .
       200711-KIZAI-TAIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ（診療行為）並び替え処理
      *****************************************************************
       20074-ZAI-KOUI-SORT-SEC          SECTION.
      *
           INITIALIZE                   WRK-KOUI-AREA
                                        WRK-KOUI-TAIHI-AREA
      *
           IF      RECE04-SRYKBN       =   "50"    OR  "54"    OR
                                            "60"    OR  "40"    OR
                                            "80"
               CONTINUE
           ELSE
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           IF      WRK-TBLMAX1          =   1
               GO  TO  20074-ZAI-KOUI-SORT-EXT
           END-IF
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       WRK-TBLMAX1
               MOVE    TBL-KOUI-REC          (IDX) TO
                                       WRK-KOUI-TAIHI-REC          (IDX)
               MOVE    TBL-KOUI-SRYCD        (IDX) TO
                                       WRK-KOUI-TAIHI-SRYCD        (IDX)
               MOVE    TBL-KOUI-TENSIKIBETU  (IDX) TO
                                       WRK-KOUI-TAIHI-TENSIKIBETU  (IDX)
               MOVE    TBL-KOUI-CHUKSNCD     (IDX) TO
                                       WRK-KOUI-TAIHI-CHUKSNCD     (IDX)
               MOVE    TBL-KOUI-KOKUJISKBKBN1(IDX) TO
                                       WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDX)
           END-PERFORM
      *
           INITIALIZE                      TBL-KOUI-AREA
           MOVE    ZERO                TO  WRK-TBLMAX1
      *
           MOVE    1                   TO  IDX
           PERFORM UNTIL   IDX >=      100
                   OR      WRK-KOUI-TAIHI-SRYCD (IDX)  =   SPACE
               EVALUATE    WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                   WHEN    "0"
                   WHEN    "8"
                       IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                       OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                           PERFORM 200741-KASAN-HENSYU-SEC
                       END-IF
                       ADD     1           TO  WRK-TBLMAX1
                       MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
                       COMPUTE IDX     =   IDX     +   1
                   WHEN    OTHER
      *                基本項目・加算項目・通則加算項目に分ける
                       PERFORM UNTIL   IDX >=      100
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                       =   SPACE
                               OR      WRK-KOUI-TAIHI-SRYCD (IDX) (1:1)
                                                   NOT =   "1"
                           IF  WRK-KOUI-TAIHI-SRYCD (IDX) = "150268970"
                              MOVE    1      TO
                                      WRK-KOUI-TAIHI-KOKUJISKBKBN1 (IDX)
                           END-IF
                           EVALUATE    WRK-KOUI-TAIHI-KOKUJISKBKBN1(IDX)
                               WHEN    "1"
                               WHEN    "3"
                               WHEN    "5"
                                   IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
                                   OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
                                       PERFORM 200741-KASAN-HENSYU-SEC
                                   END-IF
                                   MOVE    1               TO  IDZ
                               WHEN    "7"
                                   IF      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                           =
                                                 WRK-CONS-CHOONPA-KASAN
                                     MOVE    4               TO  IDZ
                                   ELSE
                                     IF      WRK-KOUI-TAIHI-SRYCD (IDX)
                                                           =
                                         WRK-CONS-SISETUKIJYUN-KENSA  OR
                                         WRK-CONS-SISETUKIJYUN-GAZOU  OR
                                         WRK-CONS-SISETUKIJYUN-HOUSYASEN
                                       MOVE    2               TO  IDZ
                                     ELSE
                                      IF    ( WRK-KOUI-TAIHI-TENSIKIBETU
                                                               (IDX)
                                                         =   5   OR  6 )
                                      AND   ( WRK-KOUI-TAIHI-CHUKSNCD
                                                                 (IDX)
                                                         =   "0000"    )
                                      AND   ( RECE04-SRYKBN
                                                     NOT =   "54"      )
                                        MOVE    3               TO  IDZ
                                      ELSE
                                        MOVE    2               TO  IDZ
                                      END-IF
                                     END-IF
                                   END-IF
                               WHEN    "9"
                                   MOVE    4               TO  IDZ
                           END-EVALUATE
                           ADD     1           TO  WRK-KOUI-TBLMAX (IDZ)
                           MOVE    WRK-KOUI-TBLMAX (IDZ)
                                               TO  IDY
                           MOVE    WRK-KOUI-TAIHI-REC (IDX)
                                               TO
                                               WRK-KOUI-REC (IDZ IDY)
                           COMPUTE IDX     =   IDX     +   1
                       END-PERFORM
               END-EVALUATE
           END-PERFORM
      *
           IF    ( WRK-KOUI-TBLMAX (1) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (2) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (3) >  ZERO )
           OR    ( WRK-KOUI-TBLMAX (4) >  ZERO )
               PERFORM 200741-KASAN-HENSYU-SEC
           END-IF
           .
       20074-ZAI-KOUI-SORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       200741-KASAN-HENSYU-SEC          SECTION.
      *
      *    加算項目の記録順
           IF      WRK-KOUI-TBLMAX (2)         >   ZERO
               COMPUTE IDY     =   WRK-KOUI-TBLMAX (2) -   1
               PERFORM VARYING IDY1    FROM    1   BY  1
                       UNTIL   IDY1    >       IDY     
                   COMPUTE IDZ     =   IDY1    +   1
                   PERFORM VARYING IDY2    FROM   IDZ  BY  1
                           UNTIL   IDY2    >   WRK-KOUI-TBLMAX (2)
                       IF      WRK-KOUI-CHUKSNINF (2 IDY1)
                                       > WRK-KOUI-CHUKSNINF (2 IDY2)
                           MOVE    WRK-KOUI-REC (2 IDY1)  TO
                                                 TBL-KOUI-SORT
                           MOVE    WRK-KOUI-REC (2 IDY2)  TO
                                                 WRK-KOUI-REC (2 IDY1)
                           MOVE    TBL-KOUI-SORT         TO
                                                 WRK-KOUI-REC (2 IDY2)
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-IF
      *
      *    基本項目・加算項目・通則加算項目の順に編集
           PERFORM VARYING IDY1    FROM    1   BY  1
                   UNTIL   IDY1    >       4
               MOVE    WRK-KOUI-TBLMAX (IDY1)  TO  IDY
               PERFORM VARYING IDY2    FROM    1   BY  1
                       UNTIL   IDY2    >       IDY
                   ADD     1               TO  WRK-TBLMAX1
                   MOVE    WRK-KOUI-REC (IDY1 IDY2)
                                           TO  TBL-KOUI-REC(WRK-TBLMAX1)
               END-PERFORM
           END-PERFORM
      *
           INITIALIZE                   WRK-KOUI-AREA
           .
       200741-KASAN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    摘要データ編集処理
      *****************************************************************
       20072-ZAI-HENSYU-SEC          SECTION.
      *
           IF      RECE04-SRYKBN  NOT =   WRK-FEDT-SRYKBN-SAVE
      *    順序番号リセット
      *            順序番号の取り込み
                   INITIALIZE                        RECE900-REC
                   MOVE    RECE04-HOSPNUM       TO  RECE900-HOSPNUM
                   MOVE    RECE04-SRYYM         TO  RECE900-SRYYM
                   MOVE    RECE04-PTID          TO  RECE900-PTID
                   MOVE    RECE04-SRYKBN        TO  RECE900-SRYKBN
                   PERFORM   900-RECE900-READ-SEC
                   IF      FLG-RECE900         =   1
                       MOVE    ZERO        TO  WRK-RENNUM
                   ELSE
                       MOVE    RECE900-RENNUM   TO   WRK-RENNUM
                   END-IF
                   MOVE    RECE04-SRYKBN
                                       TO  WRK-FEDT-SRYKBN-SAVE
           END-IF
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       31
      *    診療行為データ・コメントデータの編集
               IF      RECE04-KAISU (1 IDXX)  >   ZERO
                   MOVE    ZERO        TO  WRK-FEDT-1ST-FLG
      *    順序番号
                   ADD     1           TO  WRK-RENNUM
      *    順序番号ファイルの更新
                   INITIALIZE                        RECE900-REC
                   MOVE    RECE04-HOSPNUM       TO  RECE900-HOSPNUM
                   MOVE    RECE04-SRYYM         TO  RECE900-SRYYM
                   MOVE    RECE04-PTID          TO  RECE900-PTID
                   MOVE    RECE04-SRYKBN        TO  RECE900-SRYKBN
                   PERFORM   900-RECE900-READ-SEC
                   IF    FLG-RECE900   =    1
                         MOVE    RECE04-HOSPNUM TO  RECE900-HOSPNUM
                         MOVE    RECE04-SRYYM   TO  RECE900-SRYYM
                         MOVE    RECE04-PTID    TO  RECE900-PTID
                         MOVE    RECE04-SRYKBN  TO  RECE900-SRYKBN
                         MOVE       WRK-RENNUM   TO  RECE900-RENNUM
                         WRITE      RECE900-REC
                   ELSE
                         MOVE       WRK-RENNUM   TO  RECE900-RENNUM
                         REWRITE    RECE900-REC
                   END-IF
      *    行為明細番号リセット
                   MOVE    ZERO        TO  WRK-MEISAI-RENNUM
                   MOVE    IDXX        TO  WRK-FEDT-DD
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX1
      *
      *****    診療行為最終データ又は次がコメントデータの場合
      *****    MOVE    ZERO            TO  FLG-TBLMAX
      *****    IF      IDX             =   WRK-SRYCD1-MAX
      *****        CONTINUE
      *****    ELSE
      *****        COMPUTE IDY1    =   IDX     +   1
      *****        IF      TBL-KOUI-SRYCD (IDY1) (1:1)
      *****                                =   "0" OR  "8"
      *****            MOVE    1               TO  FLG-TBLMAX
      *****        END-IF
      *****    END-IF
      *
                       EVALUATE    TBL-KOUI-SRYCD (IDX) (1:1)
                           WHEN    "0"
                           WHEN    "8"
                               CONTINUE
                           WHEN    OTHER
                               IF    ((IDX               =  1)
                                AND   (WRK-FEDT-1ST-FLG  =  ZERO))
                               OR    ((RECE04-SRYKBN    =  "70")
                                AND   (IDX               =  2)
                                AND   (WRK-FEDT-1ST-FLG  =  ZERO))
                                   MOVE    1    TO    WRK-FEDT-1ST-FLG
                                   PERFORM 200721-ZAI-E-HENSYU-SEC
                                   PERFORM 5001-FILE-WRITE-SEC
                               END-IF
                               PERFORM 200721-ZAI-F-HENSYU-SEC
                               PERFORM 5001-FILE-F-WRITE-SEC
                       END-EVALUATE
                   END-PERFORM
               END-IF
      *
      *    医薬品データの編集
               IF      RECE04-KAISU (1 IDXX)  >   ZERO
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       WRK-TBLMAX6
      *ito.TAITOYAKU   IF    IDX               =  1
                       IF   (WRK-FEDT-1ST-FLG  =  ZERO)
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  "840000006")
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  SYS-9102-SRYCD(1))
                       AND  (TBL-ZAIW-SRYCD (1 IDX)
                                           NOT =  SYS-9102-SRYCD(2))
                             MOVE    1    TO    WRK-FEDT-1ST-FLG
                             PERFORM 200722-YKZ-E-HENSYU-SEC
                             PERFORM 5001-FILE-WRITE-SEC
                       END-IF
                       PERFORM 200722-YKZ-F-HENSYU-SEC
                       PERFORM 5001-FILE-F-WRITE-SEC
                   END-PERFORM
               END-IF
      *
      *    特定器材データの編集
               IF      RECE04-KAISU (1 IDXX)  >   ZERO
                   PERFORM VARYING IDXY    FROM    1   BY  1
                           UNTIL   IDXY    >       2
                     PERFORM VARYING IDX FROM    1   BY  1
                             UNTIL   IDX >       WRK-TBLMAX7
                       IF      IDXY        =   TBL-ZAIW-DATSKB (2 IDX)
                         IF    IDX               =  1
                         AND   WRK-FEDT-1ST-FLG  =  ZERO
                               MOVE    1     TO    WRK-FEDT-1ST-FLG
                               PERFORM 200723-KIZAI-E-HENSYU-SEC
                               PERFORM 5001-FILE-WRITE-SEC
                         END-IF
                         PERFORM 200723-KIZAI-F-HENSYU-SEC
                         PERFORM 5001-FILE-F-WRITE-SEC
                       END-IF
                     END-PERFORM
                   END-PERFORM
               END-IF
           END-PERFORM
           .
       20072-ZAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：診療行為  編集処理
      *****************************************************************
       200721-ZAI-E-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN50
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN54
           MOVE    ZERO                TO  WRK-FEDT-PREV-TEN721
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUM
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-TEN
           MOVE    ZERO                TO  TBL-HEIMA-AREA
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-KOUI-SRYCD(IDX) TO  WRK-FEDT-SRYCD
           MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-FEDT-NAME
           MOVE    1                   TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE04-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           MOVE    RECE04-ZAITEN      TO  WRK-FEDT-ZAITEN
      *行為薬剤料
           MOVE    RECE04-YKZTEN      TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    RECE04-KIZAITEN    TO  WRK-FEDT-KIZAITEN
           IF      RECE04-SRYKBN      =   "70"
             COMPUTE  WRK-FEDT-KIZAITEN    =   RECE04-KIZAITEN
                                           +   RECE04-SYUTEN2
           END-IF
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE04-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *****MOVE    RECE04-ZAIKAISU    TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE04-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE04-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE04-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE04-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE04-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE04-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード用に受診履歴検索
           INITIALIZE                   JYURRK-REC
           MOVE    RECE04-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    RECE04-PTID      TO  JYURRK-PTID
           MOVE    RECE04-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    RECE04-SRYKA     TO  JYURRK-SRYKA
           MOVE    WRK-FEDT-YMD     TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"     TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =   ZERO
              MOVE    "tbl_jyurrk"     TO  MCP-TABLE
              MOVE    "key4"          TO  MCP-PATHNAME
              PERFORM 900-DBFETCH-SEC
              IF      MCP-RC           =   ZERO
                  MOVE    MCPDATA-REC  TO  JYURRK-REC
                  MOVE    JYURRK-DRCD  TO  WRK-FEDT-DRCD
      *            MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
      *            MOVE    5                   TO  WRK-NAGASA
      *            PERFORM 5004-MOJI-HENSYU-SEC
              ELSE
                  INITIALIZE               JYURRK-REC
              END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key4"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *病棟コード
           MOVE    "9999999999"     TO  WRK-FEDT-BYOTOCD
      *病棟区分
           MOVE    "9"              TO  WRK-FEDT-BYOTOKBN
      *入外区分
           MOVE    RECE04-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200721-ZAI-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科区分  編集処理
      *****************************************************************
       200721-SRYKAKBN-HENSYU-SEC      SECTION.
      *
           SET     TBL-DPCSRYKACD-IDX  TO  1
           SEARCH  TBL-DPCSRYKACD-OCC
                               VARYING TBL-DPCSRYKACD-IDX
               AT  END
                   MOVE  "000"         TO  WRK-FEDT-SRYKA
               WHEN  WRK-FEDT-RECESRYKA =  TBL-RECEDEN-SRYKACD
                                           (TBL-DPCSRYKACD-IDX)
                   MOVE  TBL-DPC-SRYKACD (TBL-DPCSRYKACD-IDX)
                                       TO  WRK-FEDT-SRYKA
           END-SEARCH
           .
       200721-SRYKAKBN-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜診療明細情報＞：診療行為  編集処理
      *****************************************************************
       200721-ZAI-F-HENSYU-SEC       SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
           INITIALIZE                      TNS-TENSU-REC
           MOVE    TBL-KOUI-SRYCD(IDX) TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-KOUI-SRYCD(IDX) TO  WRK-FEDT-SRYCD
           MOVE    TBL-KOUI-NAME(IDX)  TO  WRK-FEDT-NAME
           MOVE    1                   TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           MOVE    TBL-KOUI-SRYSURYO (IDX)
                                       TO  WRK-FEDT-SRYSURYO
      *基準単位
           MOVE    TBL-KOUI-TANICD (IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           IF      TBL-KOUI-TENSIKIBETU (IDX)  =   3
             COMPUTE WRK-FEDT-MEISAI-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       +   0.5
           ELSE
             MOVE  ZERO                TO  WRK-FEDT-MEISAI-TEN
           END-IF
      *行為明細薬剤料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-YKZ
      *行為明細材料料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      WRK-FEDT-SRYKBN     =   "60"
           AND     TBL-KOUI-HOUKNSKBN (IDX)
                                   NOT =   ZERO
             IF    IDX                 =   WRK-TBLMAX1
      **           MOVE    WRK-FEDT-ZAITEN
      **                               TO  WRK-FEDT-DEKIDAKA-TEN
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-ZAITEN
                                       -   WRK-FEDT-YKZTEN
                                       -   WRK-FEDT-KIZAITEN
             ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
             END-IF
           ELSE
             IF    WRK-FEDT-SRYKBN     =   "70"
             AND   WRK-FEDT-SRYSURYO   >   1
                   MOVE  WRK-FEDT-SRYSURYO    TO  WRK-KAISU-70T
                   IF    WRK-KAISU-70T >  5
                     MOVE  5           TO  WRK-KAISU-70T
                   END-IF
                   COMPUTE WRK-KAISU-70 =  (WRK-KAISU-70T - 1)
                                       *   0.5
                                       +   1
                   IF  (TBL-KOUI-TEN (IDX)               =  ZERO)
                   AND (TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1)
                       PERFORM 200799-KZM-DEKIDAKA-SEC
                   ELSE
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       *   WRK-KAISU-70
                                       +   0.5
                   END-IF
                   COMPUTE WRK-FEDT-DEKIDAKA-SUM
                                       =   WRK-FEDT-DEKIDAKA-SUM
                                       +   WRK-FEDT-DEKIDAKA-TEN
             ELSE
              IF   TBL-KOUI-TENSIKIBETU (IDX)  =  5
                IF    WRK-FEDT-SRYKBN          =  "70"
                AND   WRK-FEDT-DEKIDAKA-SUM    >  ZERO
                AND  (TBL-KOUI-SRYCD(IDX)  NOT =  "170017170"
                                           AND    "170017270"
                                           AND    "170034170")
                   COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   RECE04-ZAITEN
                                       -   WRK-FEDT-DEKIDAKA-SUM
                ELSE
      *!           COMPUTE WRK-FEDT-DEKIDAKA-TEN
      *!                               =   RECE04-ZAITEN
      *!                               -   TBL-KOUI-TEN (IDX - 1)
                   IF  (TBL-KOUI-SRYCD(IDX) =   "170017170"
                                            OR  "170017270"
                                            OR  "170034170")
                   AND (WRK-FEDT-PREV-TEN721 >  ZERO)
                       MOVE WRK-FEDT-PREV-TEN721
                                       TO   WRK-FEDT-PREV-TEN
                   END-IF
      *閉麻年齢加算対応
                   IF  (TBL-KOUI-SRYCD(IDX) =   "150231590"
                                            OR  "150231690"
                                            OR  "150265390"
                                            OR  "150286690")
                   AND (WRK-FEDT-PREV-TEN54 >   ZERO)
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                           =   WRK-FEDT-PREV-TEN54
                                           *   TBL-KOUI-TEN (IDX)
                                           /   100
                                           +   0.5
                       MOVE    ZERO        TO  WRK-FEDT-PREV-TEN54
                   ELSE
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                           =   WRK-FEDT-PREV-TEN
                                           *   TBL-KOUI-TEN (IDX)
                                           /   100
                                           +   0.5
                   END-IF
      *--- AZU-MOD START 2015.10.20
      **             IF      WRK-FEDT-SRYKBN     NOT =  "50"
      **                 MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
      **             END-IF
                   IF      WRK-FEDT-SRYKBN     =  "40"
                       COMPUTE WRK-FEDT-PREV-TEN = WRK-FEDT-PREV-TEN
                                                +  WRK-FEDT-DEKIDAKA-TEN
                   END-IF
                   IF      WRK-FEDT-SRYKBN     =  "50"  OR  "40"
                       CONTINUE
                   ELSE
                       MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
                   END-IF
      *--- AZU-MOD END 2015.10.20
                   MOVE    ZERO        TO  WRK-FEDT-PREV-TEN50
                   MOVE    ZERO        TO  WRK-FEDT-PREV-TEN721
                END-IF
              ELSE
               IF  TBL-KOUI-TENSIKIBETU (IDX)  =  6
                   MOVE    1           TO  WRK-FEDT-DEKIDAKA-SUB
      *!           COMPUTE WRK-FEDT-DEKIDAKA-TEN
      *!                               =   TBL-KOUI-TEN (IDX - 1)
      *!                               -   RECE04-ZAITEN
                   IF  TBL-KOUI-SRYCD (IDX) = "150268970"
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-PREV-TEN50
                                       *   TBL-KOUI-TEN (IDX)
                                       /   100
                       COMPUTE WRK-FEDT-PREV-TEN
                                       =   WRK-FEDT-PREV-TEN
                                       -   WRK-FEDT-DEKIDAKA-TEN
                   ELSE
                       COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   WRK-FEDT-PREV-TEN
                                       *   TBL-KOUI-TEN (IDX)
                                       /   100
                       IF  TNS-TEIGENKBN   NOT =   1
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN50
                           MOVE    ZERO        TO  WRK-FEDT-PREV-TEN721
                       END-IF
                   END-IF
               ELSE
      *         きざみ値計算識別判定
                 IF  ((TBL-KOUI-DATAKBN (IDX)          =   1)    AND
                      (TBL-KOUI-CDKBN-KBN (IDX)        =   "L")  AND
                      (TBL-KOUI-CDKBN-KBNNUM (IDX)     =   8)    AND
                      (TBL-KOUI-CDKBN-KBNNUM-EDA (IDX) =   0))
                       IF  WRK-MEISAI-RENNUM   =   1
                           PERFORM 200721-HEIMA-SET-SEC
                           PERFORM 200721-HEIMA-COM1-SEC
                       ELSE
                           PERFORM 200721-HEIMA-COM2-SEC
                       END-IF
                 ELSE
                   IF    TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1
                         PERFORM 200799-KZM-DEKIDAKA-SEC
                   ELSE
                     COMPUTE WRK-FEDT-DEKIDAKA-TEN
                                       =   TBL-KOUI-TEN (IDX)
                                       +   0.5
                   END-IF
                 END-IF
               END-IF
              END-IF
             END-IF
           END-IF
      *    行為明細点数を再セット（きざみ）
           IF     (TBL-KOUI-TENSIKIBETU (IDX)      =   3)  AND
                  (TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1)  AND
                  (WRK-FEDT-MEISAI-TEN         NOT =
                                               WRK-FEDT-DEKIDAKA-TEN)
               MOVE    WRK-FEDT-DEKIDAKA-TEN
                                       TO  WRK-FEDT-MEISAI-TEN
           END-IF
      *出来高・包括フラグ
           IF      WRK-FEDT-HOUKATU-FLG    NOT =  "6"
               MOVE    "1"                 TO  WRK-FEDT-HOUKATU-FLG
           END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
      *加算・減算のため行為点数を累積する
      *    手術で通則加算は通則加算対象外とする
           IF     (WRK-FEDT-SRYKBN             =   "50")  AND
                  (TBL-KOUI-TENSIKIBETU (IDX)  =   3)  AND
                  (TNS-KOKUJISKBKBN2           =   "9")  AND
                  (TNS-TUSOKUGAIKBN            =   "0")
               MOVE     1                      TO  TNS-TUSOKUGAIKBN
           END-IF
           IF     (TBL-KOUI-TENSIKIBETU (IDX)  =   3)
           OR     (TNS-TEIGENKBN               =   1)
             IF    TNS-TUSOKUGAIKBN      NOT =   "1"
               IF    TBL-KOUI-KZMCOMPSIKIBETU (IDX)  =   1
                   COMPUTE WRK-FEDT-PREV-TEN   =   WRK-FEDT-PREV-TEN
                                               +   WRK-FEDT-DEKIDAKA-TEN
                   IF    WRK-FEDT-SRYKBN       =  "70"
                   AND   TBL-KOUI-SRYSYUKBN(IDX)   =  "721"
                       COMPUTE WRK-FEDT-PREV-TEN721
                                               =   WRK-FEDT-PREV-TEN721
                                               +   WRK-FEDT-DEKIDAKA-TEN
                   END-IF
      *--- AZU-ADD START 2016.04.22
                   IF    WRK-FEDT-SRYKBN       =  "50"
                      MOVE WRK-FEDT-DEKIDAKA-TEN TO  WRK-FEDT-PREV-TEN50
                   END-IF
      *--- AZU-ADD END 2016.04.22
               ELSE
                   IF  TBL-KOUI-TENSIKIBETU (IDX)  =  6
                       COMPUTE WRK-FEDT-PREV-TEN
                                        =   WRK-FEDT-PREV-TEN
                                        *  (100  -  TBL-KOUI-TEN (IDX))
                                        /   100
                                        +   0.5
                   ELSE
                       IF  TNS-MASUISKBKBN     NOT =   7
                           COMPUTE WRK-FEDT-PREV-TEN   =
                                   WRK-FEDT-PREV-TEN   +
                                   TBL-KOUI-TEN (IDX)
                           MOVE TBL-KOUI-TEN (IDX)     TO
                                                   WRK-FEDT-PREV-TEN50
                       ELSE
      *閉麻年齢加算対応
                           COMPUTE WRK-FEDT-PREV-TEN54 =
                                   WRK-FEDT-PREV-TEN   +
                                   TBL-KOUI-TEN (IDX)
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-IF
           .
       200721-ZAI-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    閉麻テーブルセット処理
      *****************************************************************
       200721-HEIMA-SET-SEC            SECTION.
      *
           PERFORM VARYING IDXW FROM IDX  BY  1
                   UNTIL   TBL-KOUI-SRYCD (IDXW)  =  SPACE
             IF  ((TBL-KOUI-DATAKBN (IDXW)          =  1)    AND
                  (TBL-KOUI-CDKBN-KBN (IDXW)        =  "L")  AND
                  (TBL-KOUI-CDKBN-KBNNUM (IDXW)     =  8)    AND
                  (TBL-KOUI-CDKBN-KBNNUM-EDA (IDXW) =  0))
                   COMPUTE TBL-HEIMA-MAX  =  TBL-HEIMA-MAX + 1
                   MOVE  TBL-KOUI-SRYSURYO (IDXW)  TO
                                  TBL-HEIMA-SURYO (TBL-HEIMA-MAX)
                   MOVE  TBL-KOUI-SRYSURYO (IDXW)  TO
                                  TBL-HEIMA-ZAN-SURYO (TBL-HEIMA-MAX)
                   COMPUTE TBL-HEIMA-TOT-SURYO
                                       =  TBL-HEIMA-TOT-SURYO
                                       +  TBL-KOUI-SRYSURYO (IDXW)
             END-IF
           END-PERFORM
      *    基本となる数量計算
           IF      TBL-HEIMA-TOT-SURYO     >=  120
               IF  TBL-HEIMA-SURYO (1)     >=  120
                   COMPUTE WRK-HEIMA-SURYO  =  TBL-HEIMA-SURYO(1) - 120
                   COMPUTE WRK-HEIMA-SURYO2 =  WRK-HEIMA-SURYO / 30
                   COMPUTE TBL-HEIMA-ZAN-SURYO (1)
                                            =  WRK-HEIMA-SURYO
                                            - (WRK-HEIMA-SURYO2 * 30)
                   COMPUTE WRK-KZM-SURYO    =  TBL-HEIMA-SURYO (1)
                                            -  TBL-HEIMA-ZAN-SURYO (1)
               ELSE
                   MOVE    TBL-HEIMA-SURYO (1)  TO  WRK-KZM-SURYO
                   MOVE    ZERO            TO  TBL-HEIMA-ZAN-SURYO (1)
                   PERFORM VARYING IDXW FROM 2  BY  1
                           UNTIL   ( IDXW > TBL-HEIMA-MAX )
                                OR ( WRK-KZM-SURYO = 120 )
                     COMPUTE WRK-HEIMA-SURYO  =  120  -  WRK-KZM-SURYO
                     IF      TBL-HEIMA-ZAN-SURYO (IDXW)
                                              >=   WRK-HEIMA-SURYO
                         COMPUTE WRK-KZM-SURYO  =  WRK-KZM-SURYO
                                                +  WRK-HEIMA-SURYO
                         COMPUTE TBL-HEIMA-ZAN-SURYO (IDXW)
                                      =  TBL-HEIMA-ZAN-SURYO (IDXW)
                                      -  WRK-HEIMA-SURYO
                     ELSE
                         COMPUTE WRK-KZM-SURYO  =  WRK-KZM-SURYO
                                      +  TBL-HEIMA-ZAN-SURYO (IDXW)
                         MOVE    ZERO    TO  TBL-HEIMA-ZAN-SURYO (IDXW)
                     END-IF
                   END-PERFORM
               END-IF
           ELSE
               MOVE    TBL-HEIMA-TOT-SURYO TO  WRK-KZM-SURYO
               PERFORM VARYING IDXW FROM 1  BY  1
                       UNTIL   IDXW >    TBL-HEIMA-MAX
                 MOVE    ZERO       TO  TBL-HEIMA-ZAN-SURYO(IDXW)
               END-PERFORM
           END-IF
           MOVE    WRK-KZM-SURYO    TO  TBL-HEIMA-COM-SURYO (1)
           COMPUTE TBL-HEIMA-TOT-SURYO =   TBL-HEIMA-TOT-SURYO
                                       -   WRK-KZM-SURYO
      *    30分以上があるか？
           PERFORM VARYING IDXW FROM 1  BY  1
                   UNTIL   ( IDXW > TBL-HEIMA-MAX )
             IF      TBL-HEIMA-ZAN-SURYO (IDXW)  >=  30
               MOVE    TBL-HEIMA-ZAN-SURYO (IDXW)
                                           TO  WRK-HEIMA-SURYO
               COMPUTE WRK-HEIMA-SURYO2
                       =  TBL-HEIMA-ZAN-SURYO (IDXW) / 30
               COMPUTE WRK-KZM-SURYO        =  WRK-HEIMA-SURYO2 * 30
               COMPUTE TBL-HEIMA-ZAN-SURYO (IDXW)
                                            =  WRK-HEIMA-SURYO
                                            -  WRK-KZM-SURYO
               COMPUTE TBL-HEIMA-COM-SURYO (IDXW)
                                          =  TBL-HEIMA-COM-SURYO (IDXW)
                                          +   WRK-KZM-SURYO
               COMPUTE TBL-HEIMA-TOT-SURYO  =  TBL-HEIMA-TOT-SURYO
                                            -  WRK-KZM-SURYO
             END-IF
             MOVE    TBL-HEIMA-ZAN-SURYO (IDXW)
                                       TO  TBL-HEIMA-30M-SURYO (IDXW)
           END-PERFORM
      *    30分以下を寄せ集め
      *    数量の多い順になるのでここで多い順にインデックスを決定
      *    TBL-HEIMA-30M-IDX()へ格納
      *    一時使用の配列へ複写
           PERFORM VARYING IDXW    FROM    1   BY  1
                   UNTIL  (IDXW    >   TBL-HEIMA-MAX)
               MOVE    TBL-HEIMA-30M-SURYO (IDXW)
                                       TO  TBL-HEIMA-30M-TMP (IDXW)
           END-PERFORM
           PERFORM VARYING IDZ3    FROM    1   BY  1
                   UNTIL  (IDZ3    >   TBL-HEIMA-MAX)
               MOVE    ZERO                TO  WRK-30M-SURYO
               PERFORM VARYING IDXW    FROM    1   BY  1
                       UNTIL  (IDXW    >   TBL-HEIMA-MAX)
                   IF     (TBL-HEIMA-30M-TMP (IDXW)  >
                                               WRK-30M-SURYO)
                       MOVE    IDXW    TO  TBL-HEIMA-30M-IDX (IDZ3)
                       MOVE    TBL-HEIMA-30M-TMP (IDXW)
                                       TO  WRK-30M-SURYO
                   END-IF
               END-PERFORM
               MOVE    TBL-HEIMA-30M-IDX (IDZ3)    TO  IDXW
               IF      IDXW                >   ZERO
                   MOVE    ZERO            TO  TBL-HEIMA-30M-TMP (IDXW)
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  IDZ3
           PERFORM UNTIL   ( TBL-HEIMA-TOT-SURYO = ZERO )
             MOVE    ZERO              TO  WRK-HEIMA-IDX
             MOVE    ZERO              TO  WRK-KZM-SURYO
             COMPUTE IDZ3              =   IDZ3    +   1
             MOVE    TBL-HEIMA-30M-IDX (IDZ3)
                                           TO  WRK-HEIMA-IDX
             IF      WRK-HEIMA-IDX   NOT =   ZERO
                 PERFORM VARYING IDXW FROM 1 BY 1
                         UNTIL ( IDXW > TBL-HEIMA-MAX )
                     MOVE    TBL-HEIMA-30M-IDX (IDXW)
                                           TO  IDW
                     IF      IDW           NOT =   ZERO
                         COMPUTE WRK-KZM-SURYO =   WRK-KZM-SURYO
                                               +   TBL-HEIMA-ZAN-SURYO
                                                   (IDW)
                         IF      WRK-KZM-SURYO >=  30
                             COMPUTE TBL-HEIMA-ZAN-SURYO (IDW) =
                                     WRK-KZM-SURYO -   30
                             MOVE    30        TO  WRK-KZM-SURYO
                         ELSE 
                             MOVE    ZERO      TO  TBL-HEIMA-ZAN-SURYO
                                                   (IDW)
                             MOVE    ZERO      TO  TBL-HEIMA-30M-SURYO
                                                   (IDW)
                         END-IF
                     END-IF
                 END-PERFORM
             END-IF
             COMPUTE TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                 =  TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                 +  WRK-KZM-SURYO
             COMPUTE TBL-HEIMA-TOT-SURYO  =  TBL-HEIMA-TOT-SURYO
                                          -   WRK-KZM-SURYO
      **       DISPLAY TBL-KOUI-SRYCD (1) " ZAN:COM:30M(1) "
      **               TBL-HEIMA-ZAN-SURYO(1) " : "
      **               TBL-HEIMA-COM-SURYO(1) " : "
      **               TBL-HEIMA-30M-SURYO(1)
      **       DISPLAY TBL-KOUI-SRYCD (2) " ZAN:COM:30M(2) "
      **               TBL-HEIMA-ZAN-SURYO(2) " : "
      **               TBL-HEIMA-COM-SURYO(2) " : "
      **               TBL-HEIMA-30M-SURYO(2)
      **       DISPLAY TBL-KOUI-SRYCD (3) " ZAN:COM:30M(3) "
      **               TBL-HEIMA-ZAN-SURYO(3) " : "
      **               TBL-HEIMA-COM-SURYO(3) " : "
      **               TBL-HEIMA-30M-SURYO(3)
           END-PERFORM
           MOVE    ZERO                TO  WRK-HEIMA-IDX
           .
       200721-HEIMA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     閉麻計算１
      *****************************************************************
       200721-HEIMA-COM1-SEC           SECTION.
      *
           ADD     1             TO  WRK-HEIMA-IDX
           MOVE    TNS-TEN       TO  TBL-HEIMA-TEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMKGN    TO  TBL-HEIMA-KZMKGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMJGN    TO  TBL-HEIMA-KZMJGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMTEN    TO  TBL-HEIMA-KZMTEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMERR    TO  TBL-HEIMA-KZMERR (WRK-HEIMA-IDX)
      *
           MOVE    TBL-HEIMA-COM-SURYO (1) TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
           MOVE    ZERO                    TO  FLG-KZM
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-KZM-SURYO       <=  (TNS-KZMKGN - TNS-KZM )
                   IF      TNS-KZMERR      =   2  OR  3
                       MOVE  1             TO  FLG-KZM
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <  WRK-KZM-SURYO ) AND
                        (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                           -   TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                           + ( WRK-KZM-TEN2
                                           *   TNS-KZMTEN)
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            警告エラーとする
                       MOVE    2           TO  FLG-KZM
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF    ((FLG-KZM             =   ZERO )  OR
                  (FLG-KZM             =   2   )) AND
                  (WRK-KZM-TEN2        >   ZERO )
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               MOVE    TNS-TEN         TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           .
       200721-HEIMA-COM1-EXT.
           EXIT.
      *
      *****************************************************************
      *     閉麻計算２
      *****************************************************************
       200721-HEIMA-COM2-SEC           SECTION.
      *
           ADD     1             TO  WRK-HEIMA-IDX
           MOVE    TNS-TEN       TO  TBL-HEIMA-TEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMKGN    TO  TBL-HEIMA-KZMKGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMJGN    TO  TBL-HEIMA-KZMJGN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMTEN    TO  TBL-HEIMA-KZMTEN (WRK-HEIMA-IDX)
           MOVE    TNS-KZMERR    TO  TBL-HEIMA-KZMERR (WRK-HEIMA-IDX)
      *
           MOVE    TBL-HEIMA-COM-SURYO (WRK-HEIMA-IDX)
                                           TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
      *    きざみ値判定
           EVALUATE    TRUE
               WHEN    WRK-KZM-SURYO       =   ZERO
                   CONTINUE
      *
      *            データ≦上限値
               WHEN    (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =   WRK-KZM-SURYO
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                           *   TNS-KZMTEN
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =   WRK-KZM-SURYO
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                               *   TNS-KZMTEN
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   WRK-KZM-TEN2
                                               *   TNS-KZMTEN
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
           IF      WRK-KZM-TEN2        >   ZERO
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               MOVE    ZERO            TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
           .
       200721-HEIMA-COM2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：医薬品  編集処理
      *****************************************************************
       200722-YKZ-E-HENSYU-SEC         SECTION.
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (1 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE04-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           IF      IDX                 =   WRK-TBLMAX6
             IF    TBL-ZAIW-SRYCD (1 IDX)
                                       =  (WRK-CONS-TEIGEN  OR
                                           WRK-CONS-TEIGEN2 OR
                                           WRK-CONS-TEIGEN3 OR
                                           WRK-CONS-TEIGEN4)
               MOVE  RECE04-ZAITEN    TO  WRK-FEDT-ZAITEN
               COMPUTE  WRK-FEDT-ZAITEN =  WRK-FEDT-ZAITEN  *  -1
             ELSE
               MOVE  RECE04-YKZTEN    TO  WRK-FEDT-ZAITEN
               ADD   RECE04-KIZAITEN  TO  WRK-FEDT-ZAITEN
             END-IF
           ELSE
             MOVE  RECE04-ZAITEN      TO  WRK-FEDT-ZAITEN
           END-IF
      *行為薬剤料
           MOVE    RECE04-YKZTEN      TO  WRK-FEDT-YKZTEN
      *薬剤料逓減
           IF    TBL-ZAIW-SRYCD (1 IDX)  =  (WRK-CONS-TEIGEN  OR
                                             WRK-CONS-TEIGEN2 OR
                                             WRK-CONS-TEIGEN3 OR
                                             WRK-CONS-TEIGEN4)
               MOVE  RECE04-ZAITEN    TO  WRK-FEDT-YKZTEN
               COMPUTE   WRK-FEDT-YKZTEN  =  WRK-FEDT-YKZTEN  *  -1
           END-IF
      *行為材料料
           MOVE    RECE04-KIZAITEN    TO  WRK-FEDT-KIZAITEN
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE04-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *****MOVE    RECE04-ZAIKAISU    TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE04-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE04-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE04-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE04-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE04-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *    退院時処方の出来高・包括フラグセット
      *      退院日を無条件にセットは廃止
      *      明細データ退避処理時にフラグをセットするようにした
      **** IF     (WRK-FEDT-SRYKBN     =   "21" OR "22" OR "23")
      ****    AND (WRK-FEDT-YMD        =   WRK-FEDT-TAIINYMD)
      ****      MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
      **** ELSE
      ****      MOVE    "0"                TO  WRK-FEDT-HOUKATU-FLG
      **** END-IF
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE04-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード用に受診履歴検索
           INITIALIZE                   JYURRK-REC
           MOVE    RECE04-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    RECE04-PTID      TO  JYURRK-PTID
           MOVE    RECE04-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    RECE04-SRYKA     TO  JYURRK-SRYKA
           MOVE    WRK-FEDT-YMD     TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"     TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =   ZERO
              MOVE    "tbl_jyurrk"     TO  MCP-TABLE
              MOVE    "key4"          TO  MCP-PATHNAME
              PERFORM 900-DBFETCH-SEC
              IF      MCP-RC           =   ZERO
                  MOVE    MCPDATA-REC  TO  JYURRK-REC
                  MOVE    JYURRK-DRCD  TO  WRK-FEDT-DRCD
      *            MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
      *            MOVE    5                   TO  WRK-NAGASA
      *            PERFORM 5004-MOJI-HENSYU-SEC
              ELSE
                  INITIALIZE               JYURRK-REC
              END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key4"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *病棟コード
           MOVE    "9999999999"     TO  WRK-FEDT-BYOTOCD
      *病棟区分
           MOVE    "9"              TO  WRK-FEDT-BYOTOKBN
      *入外区分
           MOVE    RECE04-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200722-YKZ-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜行為明細情報＞：医薬品  編集処理
      *****************************************************************
       200722-YKZ-F-HENSYU-SEC         SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (1 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (1 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =  (WRK-CONS-TEIGEN  OR
                                           WRK-CONS-TEIGEN2 OR
                                           WRK-CONS-TEIGEN3 OR
                                           WRK-CONS-TEIGEN4)
                   MOVE    ZERO        TO  WRK-FEDT-SRYSURYO
           ELSE
                   MOVE    TBL-ZAIW-SRYSURYO (1 IDX)
                                       TO  WRK-FEDT-SRYSURYO
           END-IF
      *基準単位
           MOVE    TBL-ZAIW-TANICD (1 IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-TEN
      *行為明細薬剤料
           COMPUTE WRK-FEDT-MEISAI-YKZ =   TBL-ZAIW-TEN (1 IDX)
                                       *   WRK-FEDT-SRYSURYO
      *薬剤料逓減
           IF      TBL-ZAIW-SRYCD (1 IDX)
                                       =  (WRK-CONS-TEIGEN  OR
                                           WRK-CONS-TEIGEN2 OR
                                           WRK-CONS-TEIGEN3 OR
                                           WRK-CONS-TEIGEN4)
                   COMPUTE   WRK-FEDT-MEISAI-YKZ
                                       =   TBL-ZAIW-TEN (1 IDX) * -1
           END-IF
      *行為明細材料料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *円・点区分
           MOVE    "1"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      IDX                 =   WRK-TBLMAX6
                   MOVE    WRK-FEDT-YKZTEN
                                       TO  WRK-FEDT-DEKIDAKA-TEN
      *         IF      TBL-ZAIW-SRYCD (1 IDX)
      *                                 =   WRK-CONS-TEIGEN
      *             COMPUTE   WRK-FEDT-DEKIDAKA-TEN
      *                                 =   WRK-FEDT-DEKIDAKA-TEN * -1
      *          DISPLAY "WRK-FEDT-DEKIDAKA-TEN = " WRK-FEDT-DEKIDAKA-TEN
      *         END-IF
           ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
      *出来高・包括フラグ
      *    退院日処方の出来高・包括フラグセット
      *      退院日を無条件にセットは廃止
      **** IF     (WRK-FEDT-SRYKBN     =   "21" OR "22" OR "23")
      ****    AND (WRK-FEDT-YMD        =   WRK-FEDT-TAIINYMD)
      ****      MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
      **** ELSE
      ****      MOVE    "0"                TO  WRK-FEDT-HOUKATU-FLG
      **** END-IF
           IF     (WRK-FEDT-SRYKBN     =   "21" OR "22" OR "23")
      *       IF (WRK-FEDT-DEKIDAKA-TEN    NOT =   ZERO)
              IF (WRK-FEDT-ZAITEN    NOT =   ZERO)    OR
                 (TBL-ZAIW-SRYCD (1 IDX)  =  WRK-CONS-TEIGEN2)
                MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
              ELSE
      *         院外処方
      ****      MOVE    "9"                TO  WRK-FEDT-HOUKATU-FLG
                PERFORM 200722-YKZ-F-HENSYU-GENERIC-SEC
              END-IF
           ELSE
                MOVE    "1"                TO  WRK-FEDT-HOUKATU-FLG
           END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
           .
       200722-YKZ-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方せん一般名記載により出来高・包括フラグ決定
      *****************************************************************
       200722-YKZ-F-HENSYU-GENERIC-SEC SECTION.
      *
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-FEDT-SRYCD      TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数附加情報検索
           IF     (TNS-SRYCD(1:1)      =   "6" ) AND
                  (FLG-TENSU           =   ZERO)
               PERFORM 900-TENSUPLUS-READ-SEC
           ELSE
               INITIALIZE                      TENSUPLUS-REC
           END-IF
      *
           IF     (TNS-SRYCD(1:1)      =   "6" )
      *        システム管理の変更可によって一般名記載とする
               IF    ( TNSP-IPN-KISAIKBN       =   ZERO   )
               AND   ((SYS-1030-KOUHATUKA      =   "1"   ) OR
                      (WRK-GEN11-KAISU(1 IDXX) >   ZERO  ))
               AND   ( TNS-KOUHATUKBN  =  1  OR  2  OR  7 )
                   MOVE    1               TO  TNSP-IPN-KISAIKBN
                   IF   WRK-GEN10-KAISU(1 IDXX)  >  ZERO
                      MOVE    ZERO         TO  TNSP-IPN-KISAIKBN
                   END-IF
               END-IF
           END-IF
      *
      *    システム予約により銘柄名へ変更か調べる
           PERFORM 200722-SRYACCT-CHECK-SEC
      *
           IF      TNSP-IPN-KISAIKBN   =   1  OR  3
               MOVE    "8"                 TO  WRK-FEDT-HOUKATU-FLG
           ELSE
               MOVE    "9"                 TO  WRK-FEDT-HOUKATU-FLG
           END-IF
           .
       200722-YKZ-F-HENSYU-GENERIC-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為データを読みシステム予約マスタが入力されていないか
      *    調べる
      *****************************************************************
       200722-SRYACCT-CHECK-SEC        SECTION.
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    RECE04-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    RECE04-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    RECE04-PTID         TO  SRY-PTID
           MOVE    RECE04-SRYKA        TO  SRY-SRYKA
           MOVE    RECE04-SRYYM        TO  SRY-SRYYM
           MOVE    RECE04-ZAINUM       TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  FLG-GE-TARGET
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 200722-SRYACCT-CHECK-2-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       200722-SRYACCT-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為データを読みシステム予約マスタが入力されていないか
      *    調べる
      *****************************************************************
       200722-SRYACCT-CHECK-2-SEC      SECTION.
      *
           PERFORM VARYING II      FROM    1   BY  1
                   UNTIL   II      >   5
                   OR      SRY-SRYCD (II)      =   SPACE
               IF    ( SRY-SRYCD (II)          =   WRK-FEDT-SRYCD )
                   MOVE    1                       TO  FLG-GE-TARGET
               ELSE
                   IF    ( FLG-GE-TARGET           =   1 )
                       EVALUATE    SRY-SRYCD (II)
                           WHEN    "099209903"
      *                            後発医薬品へ変更不可
                               MOVE    ZERO        TO  TNSP-IPN-KISAIKBN
      *                    WHEN    "099209905"
      *                            含量規格変更不可
      *                    WHEN    "099209906"
      *                            剤型変更不可
                           WHEN    "099209907"
      *                            銘柄名記載
                               MOVE    ZERO        TO  TNSP-IPN-KISAIKBN
                           WHEN    "099209908"
      *                            一般名記載
                               MOVE    1           TO  TNSP-IPN-KISAIKBN
                         WHEN    OTHER
                           CONTINUE
                       END-EVALUATE
      *
                       MOVE    5                       TO  II
                       MOVE    1                       TO  FLG-SRYACT
                   END-IF
               END-IF
           END-PERFORM
           .
       200722-SRYACCT-CHECK-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル＜診療明細情報＞：特定器材  編集処理
      *****************************************************************
       200723-KIZAI-E-HENSYU-SEC       SECTION.
      *
           MOVE    "E"                 TO  WRK-FEDT-REC-FLG
      *退院年月日、入院年月日のセット
           PERFORM 200799-ZAI-HENSYU-0-SEC
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (2 IDX)
                                       TO  WRK-FEDT-NAME
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *順序番号
      *****ADD     1                   TO  WRK-RENNUM
      *データ区分
           MOVE    RECE04-SRYKBN      TO  WRK-FEDT-SRYKBN
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *病院点数マスタコードから診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *行為点数
           MOVE    ZERO                TO  WRK-FEDT-ZAITEN
           IF    ( IDXY                =   1               )
           AND   ( RECE04-SRYKBN      =   "70"            )
               MOVE  RECE04-SYUTEN2   TO  WRK-FEDT-ZAITEN
           END-IF
           IF    ( IDXY                =   2               )
               MOVE  RECE04-KIZAITEN  TO  WRK-FEDT-ZAITEN
           END-IF
      *行為薬剤料
           MOVE    RECE04-YKZTEN      TO  WRK-FEDT-YKZTEN
      *行為材料料
           MOVE    WRK-FEDT-ZAITEN     TO  WRK-FEDT-KIZAITEN
      *****MOVE    RECE04-KIZAITEN    TO  WRK-FEDT-KIZAITEN
      *円・点区分
           MOVE    "0"                 TO  WRK-FEDT-TENKBN
      *行為回数
           MOVE    RECE04-KAISU (1 IDXX)
                                       TO  WRK-FEDT-ZAIKAISU
      *保険者番号
           MOVE    SPACE               TO  WRK-FEDT-HKNJANUM
           IF      RECE04-HKNJANUM (5:4)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:4)
                                       TO  WRK-FEDT-HKNJANUM (5:)
           ELSE
             IF    RECE04-HKNJANUM (7:2)  =  SPACE
                   MOVE    RECE04-HKNJANUM (1:6)
                                       TO  WRK-FEDT-HKNJANUM (3:)
             ELSE
                   MOVE    RECE04-HKNJANUM
                                       TO  WRK-FEDT-HKNJANUM
             END-IF
           END-IF
      *レセプト種別
           MOVE    RECE04-RECESYUBETU TO  WRK-FEDT-RECESYUBETU
      *実施年月日
           MOVE    RECE04-SRYYM       TO  WRK-FEDT-SRYYM
      *    WRK-FEDT-DD はすでにセット済み
      *レセプト科区分
           MOVE    SPACE               TO  WRK-FEDT-RECESRYKA
           PERFORM VARYING SYSKAN-IDX2 FROM  1  BY  1
                   UNTIL   SYSKAN-IDX2 >   TBL-SRYKA-MAX
             IF    RECE04-SRYKA       =   TBL-SRYKA (SYSKAN-IDX2)
               MOVE    TBL-RECESRYKA (SYSKAN-IDX2)
                                       TO  WRK-FEDT-RECESRYKA
               MOVE    TBL-SRYKA-MAX   TO  SYSKAN-IDX2
             END-IF
           END-PERFORM
      *診療科区分
           PERFORM 200721-SRYKAKBN-HENSYU-SEC
      *医師コード用に受診履歴検索
           INITIALIZE                   JYURRK-REC
           MOVE    RECE04-HOSPNUM   TO  JYURRK-HOSPNUM
           MOVE    RECE04-PTID      TO  JYURRK-PTID
           MOVE    RECE04-NYUGAIKBN TO  JYURRK-NYUGAIKBN
           MOVE    RECE04-SRYKA     TO  JYURRK-SRYKA
           MOVE    WRK-FEDT-YMD     TO  JYURRK-SRYYMD
           MOVE    JYURRK-REC       TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"     TO  MCP-TABLE
           MOVE    "key4"           TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC           =   ZERO
              MOVE    "tbl_jyurrk"     TO  MCP-TABLE
              MOVE    "key4"          TO  MCP-PATHNAME
              PERFORM 900-DBFETCH-SEC
              IF      MCP-RC           =   ZERO
                  MOVE    MCPDATA-REC  TO  JYURRK-REC
                  MOVE    JYURRK-DRCD  TO  WRK-FEDT-DRCD
      *            MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
      *            MOVE    5                   TO  WRK-NAGASA
      *            PERFORM 5004-MOJI-HENSYU-SEC
              ELSE
                  INITIALIZE               JYURRK-REC
              END-IF
           END-IF
      *
           MOVE    "tbl_jyurrk"    TO  MCP-TABLE
           MOVE    "key4"         TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *病棟コード
           MOVE    "9999999999"     TO  WRK-FEDT-BYOTOCD
      *病棟区分
           MOVE    "9"              TO  WRK-FEDT-BYOTOKBN
      *入外区分
           MOVE    RECE04-NYUGAIKBN   TO  WRK-FEDT-NYUGAIKBN
      *行為点数から施設タイプまで
           PERFORM 200799-ZAI-HENSYU-E-SEC
           .
       200723-KIZAI-E-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル＜行為明細情報＞：特定器材  編集処理
      *****************************************************************
       200723-KIZAI-F-HENSYU-SEC       SECTION.
      *
           MOVE    "F"                 TO  WRK-FEDT-REC-FLG
      *診療コード、名称、解釈番号編集のセット
           MOVE    TBL-ZAIW-SRYCD (2 IDX)
                                       TO  WRK-FEDT-SRYCD
           MOVE    TBL-ZAIW-NAME (2 IDX)
                                       TO  WRK-FEDT-NAME
           IF      WRK-FEDT-SRYCD      =   WRK-CONS-SANSO-HOSEI
             MOVE  "酸素補正率１．３（１気圧）"
                                       TO  WRK-FEDT-NAME
           END-IF
           MOVE    ZERO                TO  WRK-FEDT-KBNNUM-FLG
      *退院年月日から順序番号まで
           PERFORM 200799-ZAI-HENSYU-1-SEC
      *行為明細番号から診療行為名称まで
           PERFORM 200799-ZAI-HENSYU-2-SEC
      *使用量
           MOVE    TBL-ZAIW-SRYSURYO (2 IDX)
                                       TO  WRK-FEDT-SRYSURYO
      *基準単位
           MOVE    TBL-ZAIW-TANICD (2 IDX)
                                       TO  WRK-FEDT-TANICD
      *行為明細点数
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-TEN
      *行為明細薬剤料
           MOVE    ZERO                TO  WRK-FEDT-MEISAI-YKZ
      *行為明細材料料
      *****IF    ( TBL-ZAIW-TENSIKIBETU (2 IDX)    =   2               )
      *****OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   "777770000"     )
      *****OR    ( TBL-ZAIW-SRYCD       (2 IDX)    =   WRK-CONS-SANSO1
      *****                                        OR  WRK-CONS-SANSO2
      *****                                        OR  WRK-CONS-SANSO3
      *****                                        OR  WRK-CONS-SANSO4
      *****                                        OR  WRK-CONS-SANSO5
      *****                                        OR  WRK-CONS-SANSO6
      *****                                        OR  WRK-CONS-SANSO7
      *****                                        OR  WRK-CONS-SANSO8 )
             COMPUTE WRK-FEDT-MEISAI-KIZ   =   TBL-ZAIW-TEN (2 IDX)
                                           *   WRK-FEDT-SRYSURYO
      *****ELSE
      *****  MOVE  ZERO                TO  WRK-FEDT-MEISAI-KIZ
      *****END-IF
      *円・点区分
           MOVE    "1"                 TO  WRK-FEDT-TENKBN
      *出来高実績点数
           MOVE    ZERO                TO  WRK-FEDT-DEKIDAKA-SUB
           IF      IDX                 =   WRK-TBLMAX7
                   MOVE    WRK-FEDT-KIZAITEN
                                       TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
                   MOVE    ZERO        TO  WRK-FEDT-DEKIDAKA-TEN
           END-IF
      *出来高・包括フラグ
           IF      WRK-FEDT-HOUKATU-FLG    NOT =  "6"
               MOVE    "1"                 TO  WRK-FEDT-HOUKATU-FLG
           END-IF
      *使用量から出来高・包括フラグまで
           PERFORM 200799-ZAI-HENSYU-F-SEC
           .
       200723-KIZAI-F-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（退院日、入院日セット）
      *****************************************************************
       200799-ZAI-HENSYU-0-SEC     SECTION.
      *
      *退院年月日
           MOVE   "00000000"      TO   WRK-FEDT-TAIINYMD
      *入院年月日
           MOVE    RECE04-SRYYM   TO   WRK-FEDT-NYUINYMD(1:6)
           MOVE    WRK-FEDT-DD    TO   WRK-FEDT-NYUINYMD(7:2)
           .
       200799-ZAI-HENSYU-0-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（施設コードから順序番号）
      *****************************************************************
       200799-ZAI-HENSYU-1-SEC     SECTION.
      *
           MOVE    SPACE               TO  WRK-REC
      *施設コード
           STRING  SYS-1001-PREFNUM    DELIMITED  BY  SIZE
                   SYS-1001-HOSPCD     DELIMITED  BY  SIZE
                   WRK-KUGIRI-TAB      DELIMITED  BY  SIZE
                   INTO                    WRK-REC
           END-STRING
           MOVE    10                  TO  WRK-REC-LENGTH
      *データ識別番号
           IF      SYS-9102-NUMBER     =   "0"
               MOVE    11                  TO  IDW3
               MOVE    "0000000000"        TO  WRK-DATA
               PERFORM VARYING IDW4    FROM    20  BY  -1
                       UNTIL   IDW4    <       1
                   IF      RECE02-PTNUM (IDW4:1)    NOT =   SPACE
                       IF      IDW3        =   1
                           MOVE    SPACE               TO  WRK-ERR-AREA
                           INITIALIZE                      WRK-ERR-AREA
                           MOVE    MCP-RC              TO  WRK-MCP-RC
      *
                           STRING "患者番号の桁数が１０桁を超えています"
                                               DELIMITED   BY  SIZE
                           " 患者番号＝"           DELIMITED   BY  SIZE
                           RECE02-PTNUM        DELIMITED   BY  SIZE
                                               INTO    WRK-RECEERR
                           END-STRING
                           PERFORM 500-ERR-HENSYU-SEC
                           MOVE   "ptnum err"     TO  WRK-ERRMSG
                           PERFORM 500-COBABORT-SEC
                       ELSE
                           COMPUTE IDW3    =   IDW3    -   1
                           MOVE    RECE02-PTNUM (IDW4:1)
                                               TO  WRK-DATA (IDW3:1)
                       END-IF
                   END-IF    
               END-PERFORM
      ***      MOVE    ALL"0"              TO  WRK-DATA(1:WRK-ZEROKETA)
      ***      MOVE    RECE02-PTNUM        TO  WRK-DATA
      ***                                          (WRK-PTNUMKETA:10)
           ELSE
               MOVE    RECE02-PTID         TO  WRK-DATA
           END-IF
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *退院年月日
           IF      RECE02-SRYYM        >=  WRK-CONS-SRYYM-201804
               MOVE    RECE02-BIRTHDAY     TO  WRK-DATA
           ELSE
               MOVE    WRK-FEDT-TAIINYMD   TO  WRK-DATA
           END-IF
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           MOVE    "00000000"          TO  WRK-FEDT-TAIINYMD
      *入院年月日
           MOVE    WRK-FEDT-NYUINYMD   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *データ区分（診療識別）
           MOVE    WRK-FEDT-SRYKBN     TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *順序番号
           MOVE    WRK-RENNUM          TO  WRK-DATA
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル編集処理（行為明細番号から診療行為名称）
      *****************************************************************
       200799-ZAI-HENSYU-2-SEC     SECTION.
      *
      *行為明細番号（Ｆレコードのみ）
           IF      WRK-FEDT-REC-FLG    =   "F"
             ADD   1                   TO  WRK-MEISAI-RENNUM
             MOVE  WRK-MEISAI-RENNUM   TO  WRK-DATA
             MOVE  3                   TO  WRK-NAGASA
             PERFORM 5004-MOJI-HENSYU-SEC
           END-IF
      *病院点数マスタコード
           MOVE    WRK-FEDT-SRYCD      TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト電算処理システム用コード
           MOVE    WRK-FEDT-SRYCD      TO  WRK-DATA
           MOVE    9                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *解釈番号（基本）
           IF      WRK-FEDT-KBNNUM-FLG NOT =  3
             MOVE  SPACE               TO  WRK-KBNNUM-EDIT
           END-IF
           IF      WRK-FEDT-KBNNUM-FLG =   1
             MOVE  TBL-KOUI-CDKBN-KBN (IDX)
                                       TO  WRK-KBNNUM-KBN
             MOVE  TBL-KOUI-CDKBN-KBNNUM (IDX)
                                       TO  WRK-KBNNUM-KBNNUM
             IF    TBL-KOUI-CDKBN-KBNNUM-EDA (IDX)
                                   NOT =   ZERO
               MOVE    "-"             TO  WRK-KBNNUM-HAIFUN
               MOVE    TBL-KOUI-CDKBN-KBNNUM-EDA (IDX)
                                       TO  WRK-KBNNUM-KBNNUM-EDA
             END-IF
           END-IF
           IF      WRK-FEDT-KBNNUM-FLG =   2
             MOVE  TNS-CDKBN-KBN       TO  WRK-KBNNUM-KBN
             MOVE  TNS-CDKBN-KBNNUM    TO  WRK-KBNNUM-KBNNUM
             IF    TNS-CDKBN-KBNNUM-EDA
                                   NOT =   ZERO
               MOVE    "-"             TO  WRK-KBNNUM-HAIFUN
               MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  WRK-KBNNUM-KBNNUM-EDA
             END-IF
           END-IF
           MOVE    WRK-KBNNUM-EDIT     TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *診療行為名称
           MOVE    WRK-FEDT-NAME       TO  WRK-DATA
           MOVE    254                 TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-2-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｅファイル編集処理（行為点数から施設タイプ）
      *****************************************************************
       200799-ZAI-HENSYU-E-SEC     SECTION.
      *
      *行為点数
      **     MOVE    WRK-FEDT-ZAITEN     TO  WRK-TENSU
      **     MOVE    WRK-TENSU           TO  WRK-NUM
      **     MOVE    8                   TO  WRK-NAGASA
      **     PERFORM 5005-NUM-HENSYU-SEC
      *行為点数
           IF      WRK-FEDT-HOUKATU-FLG   =    "6"
               MOVE    ZERO            TO  WRK-NUM
           ELSE
               MOVE    WRK-FEDT-ZAITEN TO  WRK-NUM
           END-IF
           MOVE    WRK-NUM-X (3:8)     TO  WRK-DATA
           IF      WRK-FEDT-ZAITEN     <  0
               MOVE   "-"              TO  WRK-DATA (1:1)
           END-IF
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為薬剤料
      **     MOVE    WRK-FEDT-YKZTEN     TO  WRK-TENSU
      **     MOVE    WRK-TENSU           TO  WRK-NUM
      **     MOVE    8                   TO  WRK-NAGASA
      **     PERFORM 5005-NUM-HENSYU-SEC
      *行為薬剤料
           IF      WRK-FEDT-HOUKATU-FLG   =    "6"
               MOVE    ZERO            TO  WRK-NUM
           ELSE
               MOVE    WRK-FEDT-YKZTEN TO  WRK-NUM
           END-IF
           MOVE    WRK-NUM-X (3:8)     TO  WRK-DATA
           IF      WRK-FEDT-YKZTEN     <  0
               MOVE   "-"              TO  WRK-DATA (1:1)
           END-IF
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為材料料
           IF      WRK-FEDT-HOUKATU-FLG   =    "6"
               MOVE    ZERO            TO  WRK-NUM
           ELSE
               MOVE    WRK-FEDT-KIZAITEN  TO  WRK-TENSU
               MOVE    WRK-TENSU          TO  WRK-NUM
           END-IF
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *円・点区分
           MOVE    WRK-FEDT-TENKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為回数
           MOVE    WRK-FEDT-ZAIKAISU   TO  WRK-KAISU
           MOVE    WRK-KAISU           TO  WRK-NUM
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *保険者番号
           MOVE    WRK-FEDT-HKNJANUM   TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト種別
           MOVE    WRK-FEDT-RECESYUBETU
                                       TO  WRK-NUM
           MOVE    4                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *実施年月日
           MOVE    WRK-FEDT-YMD        TO  WRK-DATA
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *レセプト科区分
           MOVE    WRK-FEDT-RECESRYKA  TO  WRK-DATA
           MOVE    2                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *診療科区分
           MOVE    WRK-FEDT-SRYKA      TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *医師コード
           MOVE    WRK-FEDT-DRCD       TO  WRK-DATA
           MOVE    5                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *病棟コード
           MOVE    WRK-FEDT-BYOTOCD    TO  WRK-DATA
           MOVE    10                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *病棟区分
           MOVE    WRK-FEDT-BYOTOKBN   TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *入外区分
           IF      WRK-FEDT-NYUGAIKBN  =   "2"
             MOVE  "1"                 TO  WRK-DATA
           ELSE
             MOVE  "0"                 TO  WRK-DATA
           END-IF
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *施設タイプ
           MOVE    SPACE               TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           MOVE    1                   TO  WRK-END
           PERFORM 5004-MOJI-HENSYU-SEC
           .
       200799-ZAI-HENSYU-E-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｆファイル編集処理（使用量から出来高・包括フラグ）
      *****************************************************************
       200799-ZAI-HENSYU-F-SEC     SECTION.
      *
      *使用量
           IF      WRK-FEDT-TANICD     =   "000"
             MOVE  ZERO                TO  WRK-NUM
           ELSE
             MOVE  WRK-FEDT-SRYSURYO   TO  WRK-SRYSURYO
             MOVE  WRK-SRYSURYO        TO  WRK-NUM
           END-IF
           MOVE    11                  TO  WRK-NAGASA
           MOVE    3                   TO  WRK-SYOSUU
           PERFORM 5005-NUM-HENSYU-SEC
      *基準単位
           MOVE    WRK-FEDT-TANICD     TO  WRK-DATA
           MOVE    3                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為明細点数
           MOVE    WRK-FEDT-MEISAI-TEN TO  WRK-TENSU
           MOVE    WRK-TENSU           TO  WRK-NUM
           MOVE    8                   TO  WRK-NAGASA
           PERFORM 5005-NUM-HENSYU-SEC
      *行為明細薬剤料
           MOVE    WRK-FEDT-MEISAI-YKZ TO  WRK-NUM
           MOVE    WRK-NUM-X (3:12)    TO  WRK-DATA
           IF      WRK-FEDT-MEISAI-YKZ   <  0
               MOVE   "-"             TO  WRK-DATA (1:1)
           END-IF
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *行為明細材料料
           MOVE    WRK-FEDT-MEISAI-KIZ TO  WRK-NUM
           MOVE    WRK-NUM-X (3:12)    TO  WRK-DATA
           MOVE    12                  TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *円・点区分
           MOVE    WRK-FEDT-TENKBN     TO  WRK-DATA
           MOVE    1                   TO  WRK-NAGASA
           PERFORM 5004-MOJI-HENSYU-SEC
      *出来高実績点数
           IF      WRK-FEDT-HOUKATU-FLG   =    "6"
               MOVE    0                   TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5005-NUM-HENSYU-SEC
           ELSE
               MOVE    WRK-FEDT-DEKIDAKA-TEN
                                           TO  WRK-NUM
               MOVE    8                   TO  WRK-NAGASA
               IF     (WRK-FEDT-DEKIDAKA-SUB   =  1)   OR
                      (WRK-FEDT-DEKIDAKA-TEN   <  0)
                   MOVE    WRK-NUM-X (3:8) TO  WRK-DATA
                   MOVE    "-"             TO  WRK-DATA (1:1)
                   PERFORM 5004-MOJI-HENSYU-SEC
               ELSE
                   PERFORM 5005-NUM-HENSYU-SEC
               END-IF
           END-IF
      *--- AZU-MOD START 2016.07.05
      *出来高・包括フラグ
           IF     WRK-FEDT-SRYYM     <   201604
               IF      WRK-FEDT-HOUKATU-FLG   =    "6"
                   MOVE   "1"              TO  WRK-FEDT-HOUKATU-FLG
               END-IF
               MOVE    WRK-FEDT-HOUKATU-FLG
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
           ELSE
               IF    WRK-FEDT-HOUKATU-FLG   =  "8"  OR  "9"
                    MOVE    1              TO  WRK-FEDT-KOUI-MEISAI1
               ELSE
                    MOVE    ZERO           TO  WRK-FEDT-KOUI-MEISAI1
               END-IF
               IF    WRK-FEDT-HOUKATU-FLG   =  "8"
                    MOVE    1              TO  WRK-FEDT-KOUI-MEISAI2
               ELSE
                    MOVE    ZERO           TO  WRK-FEDT-KOUI-MEISAI2
               END-IF
               IF     WRK-FEDT-SRYYM       >=  WRK-CONS-SRYYM-201804
                   EVALUATE    RECE02-SEX
                       WHEN    "1"
                           MOVE   1            TO  WRK-FEDT-KOUI-MEISAI3
                       WHEN    "2"
                           MOVE   2            TO  WRK-FEDT-KOUI-MEISAI3
                   END-EVALUATE
               ELSE
                   MOVE    ZERO                TO  WRK-FEDT-KOUI-MEISAI3
               END-IF
               MOVE    ZERO                TO  WRK-FEDT-KOUI-MEISAI4
                                               WRK-FEDT-KOUI-MEISAI5
                                               WRK-FEDT-KOUI-MEISAI6
                                               WRK-FEDT-KOUI-MEISAI7
                                               WRK-FEDT-KOUI-MEISAI8
                                               WRK-FEDT-KOUI-MEISAI9
                                               WRK-FEDT-KOUI-MEISAI10
                                               WRK-FEDT-KOUI-MEISAI11
                                               WRK-FEDT-KOUI-MEISAI12
               MOVE    WRK-FEDT-KOUI-MEISAI
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    "000000000000"      TO  WRK-FEDT-KOUI-MEISAI
           END-IF
      *--- AZU-MOD END   2016.07.05
           .
       200799-ZAI-HENSYU-F-EXT.
           EXIT.
      *
      *****************************************************************
      *     きざみ値計算・編集処理
      *****************************************************************
       200799-KZM-DEKIDAKA-SEC         SECTION.
      *
      *    きざみ値計算
           MOVE    WRK-FEDT-SRYSURYO       TO  WRK-KZM-SURYO
           MOVE    ZERO                    TO  WRK-KZM-TEN2
           MOVE    ZERO                    TO  FLG-KZM
           INITIALIZE                      TNS-TENSU-REC
           MOVE    WRK-FEDT-SRYCD          TO  TNS-SRYCD
           PERFORM 900-TENSU-READ-SEC
      *    きざみ値判定
           EVALUATE    TRUE
      *            データ≦（下限値−きざみ値）
               WHEN    WRK-KZM-SURYO       <=  (TNS-KZMKGN - TNS-KZM )
                   IF      TNS-KZMERR      =   2  OR  3
                       MOVE  1             TO  FLG-KZM
                   END-IF
      *
      *            下限値＜データ≦上限値
               WHEN     (TNS-KZMKGN        <  WRK-KZM-SURYO ) AND
                        (TNS-KZMJGN        >= WRK-KZM-SURYO )
      *            点数計算式１
                   COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                           -   TNS-KZMKGN)
                                           /   TNS-KZM
                                           +   0.999
                   MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                   COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                           + ( WRK-KZM-TEN2
                                           *   TNS-KZMTEN)
                   COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN    +  0.5
      *
      *            上限値＜データ
               WHEN    TNS-KZMJGN          <   WRK-KZM-SURYO
                   IF      TNS-KZMERR      =   0  OR  2
      *            警告エラーとする
                       MOVE    2           TO  FLG-KZM
      *            点数計算式１
                       COMPUTE WRK-KZM-TEN1    =  (WRK-KZM-SURYO
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   ELSE
      *            点数計算式２
                       COMPUTE WRK-KZM-TEN1    =  (TNS-KZMJGN
                                               -   TNS-KZMKGN)
                                               /   TNS-KZM
                                               +   0.999
                       MOVE    WRK-KZM-TEN1    TO  WRK-KZM-TEN2
                       COMPUTE WRK-KZM-TEN     =   TNS-TEN
                                               + ( WRK-KZM-TEN2
                                               *   TNS-KZMTEN)
                       COMPUTE WRK-KZM-TEN2    =   WRK-KZM-TEN  +  0.5
                   END-IF
           END-EVALUATE
      *
      *    数量は整数であること
           IF      WRK-KZM-SURYO-S2            >   ZERO
                   MOVE    1                   TO  FLG-KZM
           END-IF
      *
           IF    ((FLG-KZM             =   ZERO )  OR
                  (FLG-KZM             =   2   )) AND
                  (WRK-KZM-TEN2        >   ZERO )
               MOVE    WRK-KZM-TEN2    TO  WRK-FEDT-DEKIDAKA-TEN
           ELSE
               COMPUTE    WRK-FEDT-DEKIDAKA-TEN   =  TNS-TEN  + 0.5
           END-IF
           .
       200799-KZM-DEKIDAKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-TERM-SEC                SECTION.
      *
           CLOSE   RECE02-FILE
                   RECE04-FILE
                   RECE05-FILE
                   RECE900-FILE
                   RECE031-FILE
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM   900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
           DISPLAY "RECE02  CNT-IN  :" CNT-IN
           DISPLAY "RECE04  CNT-IN  :" CNT-RECE04
           DISPLAY "RECE03  CNT-IN  :" CNT-RECE03
           DISPLAY "DB      CNT-OUT :" CNT-OUT
           DISPLAY "DB         E    :" CNT-OUT-E
           DISPLAY "DB         F    :" CNT-OUT-F
           DISPLAY "*** ORCRDPC20 END ***"
      *     ACCEPT  WRK-IN FROM CONSOLE
           .
       300-TERM-EXT.
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
               PERFORM   900-CALL-ORCSJOB-SEC
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
           STRING  "ORCRDPC10"             DELIMITED   BY  SIZE
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
      *    ファイル出力処理（Ｅファイル）
      *****************************************************************
       5001-FILE-WRITE-SEC        SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           ADD     1                   TO  WRK-RENNUM-E
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    Ｅファイルの区分設定
           MOVE    "E"                 TO  RECEDEN-RECKBN
           MOVE    WRK-RENNUM-E        TO  RECEDEN-RENNUM
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT-E
           .
       5001-FILE-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ファイル出力処理（Ｆファイル）
      *****************************************************************
       5001-FILE-F-WRITE-SEC       SECTION.
      *
      *    改行コード編集
           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                   WRK-KAIGYO                DELIMITED  BY  SIZE
                   INTO        WRK-REC
           END-STRING
      *
           ADD     1                   TO  WRK-RENNUM-F
      *          
      *    キー編集処理
           PERFORM 50011-KEY-HENSYU-SEC
      *
      *    Ｅファイルの区分設定
           MOVE    "F"                 TO  RECEDEN-RECKBN
           MOVE    WRK-RENNUM-F        TO  RECEDEN-RENNUM
           MOVE    WRK-REC             TO  RECEDEN-RECEDATA
      *
           PERFORM 50012-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT-F
           .
       5001-FILE-F-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    キー編集処理
      *****************************************************************
       50011-KEY-HENSYU-SEC        SECTION.
      *          
           INITIALIZE                      RECEDEN-REC
           MOVE    RECE02-HOSPNUM      TO  RECEDEN-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECEDEN-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECEDEN-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECEDEN-PTID
           IF    ( SYS-2009-SYUKA-KBN  =   "1" )
               IF  (     ( RECE02-TEISYUTUSAKI   =   "1" )
                     AND ( SYS-2009-SYUKA-SYAHO  =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "2" )
                     AND ( SYS-2009-SYUKA-KOKUHO =   "1" ) )
               OR  (     ( RECE02-TEISYUTUSAKI   =   "6" )
                     AND ( SYS-2009-SYUKA-KOUIKI =   "1" ) )
                   MOVE    RECE02-SYUKA        TO  RECEDEN-RECEKA
               ELSE
                   IF      RECE02-RECEKA       =   SPACE
                       MOVE    ZERO            TO  RECEDEN-RECEKA
                   ELSE
                       MOVE    RECE02-RECEKA   TO  RECEDEN-RECEKA
                   END-IF
               END-IF
           ELSE
               IF      RECE02-RECEKA       =   SPACE
                   MOVE    ZERO            TO  RECEDEN-RECEKA
               ELSE
                    MOVE    RECE02-RECEKA  TO  RECEDEN-RECEKA
               END-IF
           END-IF
           MOVE    RECE02-TEISYUTUSAKI TO  RECEDEN-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECEDEN-RECESYUBETU
           IF      RECE02-HKNJANUM-KEY =   SPACE
               MOVE    ZERO                TO  RECEDEN-HKNJANUM
           ELSE
               MOVE    RECE02-HKNJANUM-KEY TO  RECEDEN-HKNJANUM
           END-IF
           IF      RECE02-HOJOKBN-KEY      =   SPACE
               MOVE    ZERO                TO  RECEDEN-HOJOKBN-KEY
           ELSE
               MOVE    RECE02-HOJOKBN-KEY  TO  RECEDEN-HOJOKBN-KEY
           END-IF   
           MOVE    RECE02-KOHID-KEY    TO  RECEDEN-KOHID-KEY
           IF      RECE02-TEKSTYMD     =   SPACE
               MOVE    ZERO            TO  RECEDEN-TEKSTYMD
           ELSE
               MOVE    RECE02-TEKSTYMD TO  RECEDEN-TEKSTYMD
           END-IF
           MOVE    RECE02-RECEIPT-KBN  TO  RECEDEN-RECEIPT-KBN
           .
       50011-KEY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       50012-DBINSERT-SEC          SECTION.
      *
           MOVE    WRK-PARA-CREYMD     TO  RECEDEN-CREYMD
           MOVE    WRK-PARA-CREHMS (1:6)
                                       TO  RECEDEN-CREHMS
      *
           MOVE    RECEDEN-REC         TO  MCPDATA-REC
           MOVE    "tbl_efout"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBINSERT-SEC
      *
           ADD     1                   TO  CNT-OUT
           .
       50012-DBINSERT-EXT.
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
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                               WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       ADD     IDW2                TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI-TAB            DELIMITED  BY  SIZE
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
               PERFORM VARYING IDW2    FROM    WRK-ST    BY  1
                       UNTIL   IDW2    >       10
                   IF      WRK-NUM (IDW2:1)  NOT =   SPACE
                        COMPUTE IDZ     =    11  +   WRK-SYOSUU
                                                 -   IDW2
                        IF      WRK-SYOSUU       >   ZERO
                            ADD     1                TO  IDZ
                        END-IF
                        STRING  WRK-REC(1:WRK-REC-LENGTH)
                                                 DELIMITED  BY  SIZE
                               WRK-NUM(IDW2:IDZ)  DELIMITED  BY  SIZE
                               INTO        WRK-REC
                       END-STRING
                       COMPUTE WRK-REC-LENGTH    =   WRK-REC-LENGTH   +
                                                     IDZ
                       MOVE    10                TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI-TAB            DELIMITED  BY  SIZE
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
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM   900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM   900-DBFETCH-SEC
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
           .
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM   900-DBFETCH-SEC
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
      *    点数マスタ検索処理
      *****************************************************************
       900-TENSU-READ-SEC               SECTION.
      *
           MOVE    ZERO                TO  FLG-TENSU
      *
           MOVE    TNS-SRYCD           TO  TTNS-CHK-SRYCD
           MOVE    RECE02-SRYYM        TO  TTNS-CHK-YMD (1:6)
           MOVE    "01"                TO  TTNS-CHK-YMD (7:2)
      *
      *    点数マスタ退避ワークチェック
           PERFORM 9001-TTNS-CHK-SEC
      *
      *    退避ワークに存在しない場合はＤＢを検索
           IF    ( TTNS-FLG            =   ZERO )
               INITIALIZE                      TNS-TENSU-REC
               MOVE    TTNS-CHK-SRYCD  TO  TNS-SRYCD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOSTYMD
               MOVE    TTNS-CHK-YMD    TO  TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM   900-DBSELECT-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM   900-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
                       MOVE    ZERO            TO  FLG-TENSU
      *
      *                胃瘻造設時嚥下機能評価加算は通則加算対象外区分を
      *                ０（所定点数の対象となる診療行為）に読み替え
                       IF      TNS-SRYCD       =   "150380510"
                           MOVE   0            TO  TNS-TUSOKUGAIKBN
                       END-IF
      *
      *                点数マスタ退避ワークに編集する
                       PERFORM 9001-TTNS-HEN-SEC
                   ELSE
                       INITIALIZE                  TNS-TENSU-REC
                       MOVE    1                   TO  FLG-TENSU
                   END-IF
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
      *
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
           END-IF
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワークチェック処理
      *****************************************************************
       9001-TTNS-CHK-SEC               SECTION.
      *
           MOVE    ZERO                TO  TTNS-FLG
           INITIALIZE                      TNS-TENSU-REC
      *
           PERFORM VARYING TTNS-IDX    FROM    1   BY  1
                   UNTIL ( TTNS-IDX    >   TTNS-MAX )
                    OR   ( TTNS-FLG    =   1 )
               IF    ( TTNS-KEY-SRYCD (TTNS-IDX)
                                       =   TTNS-CHK-SRYCD )
                 AND ( TTNS-KEY-YUKOSTYMD (TTNS-IDX)
                                      <=   TTNS-CHK-YMD )
                 AND ( TTNS-KEY-YUKOEDYMD (TTNS-IDX)
                                      >=   TTNS-CHK-YMD )
                   MOVE    1           TO  TTNS-FLG
                   MOVE    TTNS-DAT (TTNS-IDX)
                                       TO  TNS-TENSU-REC
               END-IF
           END-PERFORM
           .
       9001-TTNS-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ退避ワーク編集処理
      *****************************************************************
       9001-TTNS-HEN-SEC               SECTION.
      *
           IF    ( TTNS-CONST-MAX  >   TTNS-MAX )
               COMPUTE TTNS-MAX    =   TTNS-MAX    +   1
               MOVE    TNS-SRYCD       TO  TTNS-KEY-SRYCD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOSTYMD   TO  TTNS-KEY-YUKOSTYMD
                                                   (TTNS-MAX)
               MOVE    TNS-YUKOEDYMD   TO  TTNS-KEY-YUKOEDYMD
                                                   (TTNS-MAX)
               MOVE    TNS-TENSU-REC   TO  TTNS-DAT
                                                   (TTNS-MAX)
           END-IF
           .
       9001-TTNS-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト明細書読込
      *****************************************************************
       900-RECE02-READ-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-READ
      *
           PERFORM                 UNTIL   FLG-END     =   1
                                   OR      FLG-READ    =   1
               READ    RECE02-FILE         NEXT
                   AT  END
                       MOVE    1           TO  FLG-END
                       MOVE    HIGH-VALUE  TO  KEY-NEW
                   NOT AT  END
      **               IF  ( ( WRK-PARA-SYARECEKBN =   ZERO
      **                                           OR  "1" )   AND
      **                     ( RECE02-TEISYUTUSAKI
      **                                           =   "1"  )      )
      **               OR  ( ( WRK-PARA-KKHRECEKBN =   ZERO
      **                                           OR  "1"  )  AND
      **                     ( RECE02-TEISYUTUSAKI
      **                                           =   "2"  )      )
                       IF  (   RECE02-PTNUM        =   SPACE       )
                       OR  (   RECE02-TSTPTNUMKBN  =   "1"         )
                       OR  (   RECE02-RECESYUBETU  =   ZERO   OR 
                                                       8888   OR 
                                                       6666        )
      *??              OR  (   RECE02-SRYYM    NOT =   WRK-PARA-SRYYM)
                           MOVE    2               TO  FLG-READ
                       ELSE
      *                    主科設定のとき未設定なら更新しない
                           IF    ( SYS-2009-SYUKA-KBN  =   "1"  )
                           AND   ( RECE02-SYUKA        =   "XX" )
                             IF  (     ( RECE02-TEISYUTUSAKI   = "1" )
                                   AND ( SYS-2009-SYUKA-SYAHO  = "1" ) )
                             OR  (     ( RECE02-TEISYUTUSAKI   = "2" )
                                   AND ( SYS-2009-SYUKA-KOKUHO = "1" ) )
                               MOVE    2               TO  FLG-READ
                             END-IF
                           END-IF
                       END-IF
      *
      *                公費のみのレセプト出力可否のとき対象としない
                       IF      FLG-READ            =   ZERO
                           IF    ( SYS-9102-KOH-RECEIPT
                                                       =   "1"  )
                           AND   ( RECE02-RECESYUBETU (1:2)
                                                       =   "12" )
                               MOVE    2               TO  FLG-READ
                           END-IF
                       END-IF 
      *
                       IF      FLG-READ            =   ZERO
      *                    医療機関情報取得
                           IF      WRK-PARA-SRYYM  =   ZERO
                               MOVE  RECE02-SRYYM      TO
                                                   WRK-SYS-SRYYMD(1:6)
                               MOVE  "01"              TO
                                                   WRK-SYS-SRYYMD(7:2)
                               PERFORM 900-SYSKANRI-HENSYU-SEC
                           END-IF
                           MOVE    1               TO  FLG-READ
                       END-IF
      *
                       IF      FLG-READ            =   1
                           ADD     1               TO  CNT-IN
                           MOVE    RECE02-HOSPNUM  TO  KEY-N-HOSPNUM
                           MOVE    RECE02-SRYYM    TO  KEY-N-SRYYM
                           MOVE    RECE02-NYUGAIKBN
                                                   TO  KEY-N-NYUGAIKBN
                           MOVE    RECE02-PTID     TO  KEY-N-PTID
                           MOVE    RECE02-RECEKA   TO  KEY-N-RECEKA
                           MOVE    RECE02-TEISYUTUSAKI
                                                   TO KEY-N-TEISYUTUSAKI
      *
                           DISPLAY "RECE02  PTNUM = " RECE02-PTNUM
                                   "  PTID = " RECE02-PTID
                                   "  RECESYUBETU = " RECE02-RECESYUBETU
                                   "  KEY = " RECE02-KEY
                       ELSE
                           MOVE    ZERO        TO  FLG-READ
                       END-IF
               END-READ
           END-PERFORM
           .
       910-RECE02-READ-EXT.
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
           END-IF
           .
       900-SYSKANRI-HENSYU-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０４データ読み
      ****************************************************************
       990-RECE04-NEXT-SEC           SECTION.
      *
           READ    RECE04-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE04
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE04
                   ADD     1         TO  CNT-RECE04
                   IF    RECE04-SRYKBN   =  "64"
                       MOVE   "60"   TO  RECE04-SRYKBN
                   END-IF
           END-READ
           .
       990-RECE04-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０５データ読み
      ****************************************************************
       990-RECE05-NEXT-SEC           SECTION.
      *
           READ    RECE05-FILE       NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE05
               NOT AT  END
                   MOVE    ZERO      TO  FLG-RECE05
                   IF    RECE05-SRYKBN   =  "64"
                       MOVE   "60"   TO  RECE05-SRYKBN
                   END-IF
           END-READ
           .
       990-RECE05-NEXT-EXT.
           EXIT.
      *
      ****************************************************************
      *    順序番号情報データ読み
      ****************************************************************
       900-RECE900-READ-SEC           SECTION.
      *
           MOVE    ZERO              TO  FLG-RECE900
      *
           READ    RECE900-FILE
               INVALID
                   MOVE    1         TO  FLG-RECE900
               NOT INVALID
                   MOVE    ZERO      TO  FLG-RECE900
           END-READ
           .
       900-RECE900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-SEC              SECTION.
      *
           INITIALIZE                      RECE04-REC
           MOVE    RECE02-HOSPNUM      TO  RECE04-HOSPNUM
           MOVE    RECE02-SRYYM        TO  RECE04-SRYYM
           MOVE    RECE02-NYUGAIKBN    TO  RECE04-NYUGAIKBN
           MOVE    RECE02-PTID         TO  RECE04-PTID
           MOVE    RECE02-RECEKA       TO  RECE04-RECEKA
           MOVE    RECE02-TEISYUTUSAKI TO  RECE04-TEISYUTUSAKI
           MOVE    RECE02-RECESYUBETU  TO  RECE04-RECESYUBETU
           MOVE    RECE02-HKNJANUM-KEY TO  RECE04-HKNJANUM
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE04-HOJOKBN
           MOVE    RECE02-KOHID-KEY    TO  RECE04-KOHID
           MOVE    RECE02-TEKSTYMD     TO  RECE04-TEKSTYMD
           MOVE    RECE02-RECEIPT-KBN  TO  RECE04-RECEIPT-KBN
           MOVE    "90"                TO  RECE04-SRYKBN
      *
           START   RECE04-FILE
               KEY IS    >=   RECE04-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE04
               NOT INVALID
                   PERFORM 990-RECE04-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE04           =   1                    )
               OR     ( RECE02-HOSPNUM   NOT =   RECE04-HOSPNUM      )
               OR     ( RECE02-SRYYM     NOT =   RECE04-SRYYM        )
               OR     ( RECE02-NYUGAIKBN NOT =   RECE04-NYUGAIKBN    )
               OR     ( RECE02-PTID      NOT =   RECE04-PTID         )
               OR     ( RECE02-RECEKA    NOT =   RECE04-RECEKA       )
               OR     ( RECE02-TEISYUTUSAKI
                                         NOT =   RECE04-TEISYUTUSAKI )
               OR     ( RECE02-RECESYUBETU
                                         NOT =   RECE04-RECESYUBETU  )
               OR     ( RECE02-HKNJANUM-KEY
                                         NOT =   RECE04-HKNJANUM     )
               OR     ( RECE02-HOJOKBN-KEY
                                         NOT =   RECE04-HOJOKBN      )
               OR     ( RECE02-KOHID-KEY NOT =   RECE04-KOHID        )
               OR     ( RECE02-TEKSTYMD  NOT =   RECE04-TEKSTYMD     )
               OR     ( RECE02-RECEIPT-KBN
                                         NOT =   RECE04-RECEIPT-KBN  )
               OR     ( RECE04-SRYKBN       >=  "93"                 )
      *
               PERFORM 9999-HOUKATU-1-SEC
      *
               IF      FLG-TOKUTEINYUIN    =   ZERO
                   PERFORM 990-RECE04-NEXT-SEC
               ELSE
                   MOVE    1               TO  FLG-RECE04
               END-IF
           END-PERFORM
      *
           IF      FLG-TOKUTEINYUIN    =   1
                   PERFORM 9999-HOUKATU-SAKUSEI-SEC
           END-IF
           .
       9999-HOUKATU-EXT.
           EXIT. 
      *
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-1-SEC              SECTION.
      *
           INITIALIZE                       RECE05-REC
           MOVE    RECE04-HOSPNUM      TO  RECE05-HOSPNUM
           MOVE    RECE04-SRYYM        TO  RECE05-SRYYM
           MOVE    RECE04-NYUGAIKBN    TO  RECE05-NYUGAIKBN
           MOVE    RECE04-PTID         TO  RECE05-PTID
           MOVE    RECE04-RECEKA       TO  RECE05-RECEKA
           MOVE    RECE04-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
           MOVE    RECE04-RECESYUBETU  TO  RECE05-RECESYUBETU
           MOVE    RECE04-HKNJANUM     TO  RECE05-HKNJANUM
           MOVE    RECE04-HOJOKBN      TO  RECE05-HOJOKBN
           MOVE    RECE04-KOHID        TO  RECE05-KOHID
           MOVE    RECE04-TEKSTYMD     TO  RECE05-TEKSTYMD
           MOVE    RECE04-RECEIPT-KBN  TO  RECE05-RECEIPT-KBN
           MOVE    RECE04-ZAINUM       TO  RECE05-ZAINUM
           START   RECE05-FILE
               KEY IS    >=   RECE05-KEY
               INVALID
                   MOVE    1           TO  FLG-RECE05
               NOT INVALID
                   PERFORM 990-RECE05-NEXT-SEC
           END-START
           PERFORM
               UNTIL  ( FLG-RECE05             =   1                   )
               OR     ( RECE04-HOSPNUM    NOT =   RECE05-HOSPNUM      )
               OR     ( RECE04-SRYYM      NOT =   RECE05-SRYYM        )
               OR     ( RECE04-NYUGAIKBN  NOT =   RECE05-NYUGAIKBN    )
               OR     ( RECE04-PTID       NOT =   RECE05-PTID         )
               OR     ( RECE04-RECEKA     NOT =   RECE05-RECEKA       )
               OR     ( RECE04-TEISYUTUSAKI
                                           NOT =   RECE05-TEISYUTUSAKI )
               OR     ( RECE04-RECESYUBETU
                                           NOT =   RECE05-RECESYUBETU  )
               OR     ( RECE04-HKNJANUM   NOT =   RECE05-HKNJANUM     )
               OR     ( RECE04-HOJOKBN    NOT =   RECE05-HOJOKBN      )
               OR     ( RECE04-RECEIPT-KBN
                                          NOT =   RECE05-RECEIPT-KBN  )
               OR     ( RECE04-TEKSTYMD   NOT =   RECE05-TEKSTYMD     )
               OR     ( RECE04-ZAINUM     NOT =   RECE05-ZAINUM       )
      *
               PERFORM   VARYING   IDX   FROM    1   BY  1
                         UNTIL   ( IDX               >   5     )
                         OR      ( RECE05-SRYCD(IDX) =   SPACE )
      *
                   IF    RECE05-SRYYM   <=   "201403"
                       PERFORM 9999-TOKUTEINYUIN-22-CHK-SEC
                   ELSE
                       PERFORM 9999-TOKUTEINYUIN-26-CHK-SEC
                   END-IF
                   IF      FLG-TOKUTEINYUIN    =   1
                        DISPLAY "## target srycd exist!! "
                                RECE05-SRYCD(IDX)
                        MOVE    5                   TO  IDX
                   END-IF
               END-PERFORM
      *
               IF      FLG-TOKUTEINYUIN    =   ZERO
                   PERFORM 990-RECE05-NEXT-SEC
               ELSE
                   MOVE    1               TO  FLG-RECE05
               END-IF
           END-PERFORM
           .
       9999-HOUKATU-1-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（該当特定入院料チェック）平成２６年３月まで
      *****************************************************************
       9999-TOKUTEINYUIN-22-CHK-SEC       SECTION.
      *
           SET     TBL-TOKUTEINYUIN22-IDX    TO    1
           SEARCH  TBL-TOKUTEINYUIN22-OCC
                               VARYING TBL-TOKUTEINYUIN22-IDX
               AT  END
                   MOVE    ZERO                TO  FLG-TOKUTEINYUIN
               WHEN        RECE05-SRYCD(IDX)
                                       =   TBL-TOKUTEINYUIN22-SRYCD
                                            (TBL-TOKUTEINYUIN22-IDX)
                   MOVE    1                   TO  FLG-TOKUTEINYUIN
           END-SEARCH
           .
       9999-TOKUTEINYUIN-22-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（該当特定入院料チェック）平成２６年４月から
      *****************************************************************
       9999-TOKUTEINYUIN-26-CHK-SEC       SECTION.
      *
           SET     TBL-TOKUTEINYUIN26-IDX    TO    1
           SEARCH  TBL-TOKUTEINYUIN26-OCC
                               VARYING TBL-TOKUTEINYUIN26-IDX
               AT  END
                   MOVE    ZERO                TO  FLG-TOKUTEINYUIN
               WHEN        RECE05-SRYCD(IDX)
                                       =   TBL-TOKUTEINYUIN26-SRYCD
                                            (TBL-TOKUTEINYUIN26-IDX)
                   MOVE    1                   TO  FLG-TOKUTEINYUIN
           END-SEARCH
           .
       9999-TOKUTEINYUIN-26-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理
      *****************************************************************
       9999-HOUKATU-SAKUSEI-SEC       SECTION.
      *
           PERFORM     VARYING   IDX-HOUKATU  FROM   1   BY  1
             UNTIL    (IDX-HOUKATU  >   20)   OR
                      (RECE02-SRYKA(IDX-HOUKATU)   =   SPACE)
      *
             INITIALIZE                      SRYACCT-REC
             MOVE    RECE02-HOSPNUM      TO  ACCT-HOSPNUM
             MOVE    RECE02-SRYYM        TO  ACCT-SRYYM
             MOVE    RECE02-NYUGAIKBN    TO  ACCT-NYUGAIKBN
             MOVE    RECE02-PTID         TO  ACCT-PTID
             DISPLAY "RECE02-SRYKA = " RECE02-SRYKA(IDX-HOUKATU)
             MOVE    RECE02-SRYKA(IDX-HOUKATU)
                                         TO  ACCT-SRYKA
             MOVE    9999                TO  ACCT-HKNCOMBI
             MOVE    SRYACCT-REC         TO  MCPDATA-REC
             MOVE    "DBSELECT"          TO  MCP-FUNC
             MOVE    "tbl_sryacct"       TO  MCP-TABLE
             MOVE    "key24"             TO  MCP-PATHNAME
             PERFORM 900-ORCDBMAIN-SEC
             IF      MCP-RC              =   ZERO
                 MOVE    "tbl_sryacct"   TO  MCP-TABLE
                 MOVE    "key24"         TO  MCP-PATHNAME
                 PERFORM 900-SRYACCT-READ-SEC
             ELSE
                 INITIALIZE                  SRYACCT-REC
                 MOVE    1               TO  FLG-SRYACCT
             END-IF
      *
             PERFORM UNTIL       FLG-SRYACCT     =   1
                 PERFORM 9999-SRYACCT-HENSYU-SEC
                 MOVE    "tbl_sryacct"      TO  MCP-TABLE
                 MOVE    "key24"            TO  MCP-PATHNAME
                 PERFORM 900-SRYACCT-READ-SEC
             END-PERFORM
      *
             MOVE    "tbl_sryacct"      TO  MCP-TABLE
             MOVE    "key24"            TO  MCP-PATHNAME
             PERFORM 900-CLOSE-SEC
      *
           END-PERFORM
           .
       9999-HOUKATU-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成　診療会計・診療行為データ編集処理
      *****************************************************************
       9999-SRYACCT-HENSYU-SEC         SECTION.
      *
           PERFORM VARYING IJ  FROM  1  BY  1
                   UNTIL   IJ  >  31
               IF      RECE02-JNISSUTBL(IJ)    =   SPACE
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               -   ACCT-DAY(1 IJ)
                   MOVE    ZERO                TO  ACCT-DAY(1 IJ)
               END-IF
           END-PERFORM
           IF      ACCT-ZAIKAISU       =   ZERO
                   GO      TO        9999-SRYACCT-HENSYU-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-HOUKATU-RECE04
      *
      *    診療行為データ読み込み
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           PERFORM             UNTIL   FLG-SRYACT    =    1
               PERFORM 9999-SRYACT-HENSYU-SEC
               MOVE    "tbl_sryact"    TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       9999-SRYACCT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成　診療会計・診療行為データ編集処理
      *****************************************************************
       9999-SRYACT-HENSYU-SEC               SECTION.
      *
           PERFORM VARYING II      FROM    1   BY  1
                   UNTIL   II      >   5
                   OR      SRY-SRYCD (II)      =   SPACE
      *
               INITIALIZE                      TNS-TENSU-REC
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    SRY-SRYCD(II)       TO  TNS-SRYCD
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               PERFORM 900-TENSU-READ-SEC
      *
               IF    ( FLG-HOUKATU-RECE04      =   ZERO )  AND
                     ( II                      =   1 )
                   PERFORM 9999-HOUKATU-SAKUSEI-RECE04-SEC
                   WRITE   RECE04-REC
                   IF      STS-RECE04      NOT =   ZERO
                       DISPLAY "RECE04 WRITE ERROR STS= "
                               STS-RECE04
                   END-IF
                   MOVE    1                   TO  FLG-HOUKATU-RECE04
               END-IF
               PERFORM 9999-HOUKATU-SAKUSEI-RECE05-SEC
           END-PERFORM
      *
           WRITE   RECE05-REC
           IF      STS-RECE05      NOT =   ZERO
                       DISPLAY "RECE05 WRITE ERROR STS= "
                               STS-RECE05
           END-IF
           .
       9999-SRYACT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（レセプト０４）
      *****************************************************************
       9999-HOUKATU-SAKUSEI-RECE04-SEC SECTION.
      *
           INITIALIZE                      RECE04-REC
      *医療機関ＩＤ
           MOVE    RECE02-HOSPNUM      TO  RECE04-HOSPNUM
      *診療年月
           MOVE    RECE02-SRYYM        TO  RECE04-SRYYM
      *入外区分
           MOVE    RECE02-NYUGAIKBN    TO  RECE04-NYUGAIKBN
      *患者ＩＤ
           MOVE    RECE02-PTID         TO  RECE04-PTID
      *レセプト科
           MOVE    RECE02-RECEKA       TO  RECE04-RECEKA
      *審査支払機関
           MOVE    RECE02-TEISYUTUSAKI TO  RECE04-TEISYUTUSAKI
      *レセプト種別
           MOVE    RECE02-RECESYUBETU  TO  RECE04-RECESYUBETU
      *保険者番号
           MOVE    RECE02-HKNJANUM-KEY TO  RECE04-HKNJANUM
      *補助区分
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE04-HOJOKBN
      *公費ＩＤ
           MOVE    RECE02-KOHID-KEY    TO  RECE04-KOHID
      *適用開始年月日
           MOVE    RECE02-TEKSTYMD     TO  RECE04-TEKSTYMD
      *分割区分
           MOVE    RECE02-RECEIPT-KBN  TO  RECE04-RECEIPT-KBN
      *診療区分
           IF      ACCT-SRYKBN         =   "64"
               MOVE    "60"                TO  RECE04-SRYKBN
           ELSE
               MOVE    ACCT-SRYKBN         TO  RECE04-SRYKBN
           END-IF
      *コード表用番号−区分番号
      ***     MOVE    TNS-CDKBN-KBNNUM    TO  RECE04-CDKBN-KBNNUM
      *剤番号
           MOVE    ACCT-ZAINUM         TO  RECE04-ZAINUM
      *公費分離区分(???)
           MOVE    RECE02-KOHBUNRI-MIN TO  RECE04-KOHBUNRI
      *患者番号
           MOVE    RECE02-PTNUM        TO  RECE04-PTNUM
      *診療種別区分
           IF      ACCT-SRYKBN         =   "64"
               MOVE    "600"               TO  RECE04-SRYSYUKBN
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  RECE04-SRYSYUKBN
           END-IF
      *診療科
           MOVE    RECE02-SRYKA(1)     TO  RECE04-SRYKA
      *保険組合せ番号(???)
           MOVE    RECE02-HKNCOMBI(1)  TO  RECE04-HKNCOMBI
      *自保区分
           MOVE    ACCT-JIHOKBN        TO  RECE04-JIHOKBN
           IF      ACCT-JIHOKBN        =   SPACE
               MOVE    "1"                 TO  RECE04-JIHOKBN
           ELSE
               DISPLAY "JIHOKBN ? PTNUM= " RECE02-PTNUM
                       "JIHOKBN= " ACCT-JIHOKBN
           END-IF
      *剤点数
           MOVE    ACCT-ZAITEN         TO  RECE04-ZAITEN
      *診療コード計
           MOVE    ACCT-SRYCDTOTAL     TO  RECE04-SRYCDTOTAL
      *数量計
           MOVE    ACCT-SURYOUTOTAL    TO  RECE04-SURYOUTOTAL
      *診療コード＊数量の累計(???)
      *     MOVE                        TO  RECE04-RUIKEITOTAL
      *明細数
           MOVE    ACCT-MEISAISU       TO  RECE04-MEISAISU
      *剤回数
           MOVE    ACCT-ZAIKAISU       TO  RECE04-ZAIKAISU
      *手技点数１
           MOVE    ACCT-SYUTEN1        TO  RECE04-SYUTEN1
      *手技点数２
           MOVE    ACCT-SYUTEN2        TO  RECE04-SYUTEN2
      *薬剤点数
           MOVE    ACCT-YKZTEN         TO  RECE04-YKZTEN
      *器材点数
           MOVE    ACCT-KIZAITEN       TO  RECE04-KIZAITEN
      *ドクターコード
           MOVE    ACCT-DRCD           TO  RECE04-DRCD
      *剤請求フラグ
           MOVE    ACCT-ZAIREQFLG      TO  RECE04-ZAIREQFLG
      *逓減率
           MOVE    ACCT-TEIGENRATE     TO  RECE04-TEIGENRATE
      *投薬剤点０（１又は２（コメントのみ））(???)
      *     MOVE                        TO  RECE04-TOYAKUFLG
      *関係コメント存在フラグ(???)
      *     MOVE                        TO  RECE04-SETCOMFLG
      *剤区分
            MOVE    ACCT-ZAIKBN        TO  RECE04-ZAIKBN
      *検査等実施判断
           MOVE    TNS-KNSJISKBN       TO  RECE04-KNSJISKBN
      *検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  RECE04-KNSJISGRPKBN
      *レセ電用入院料加算フラグ(???)
      *      MOVE                       TO  RECE04-NYUKSNFLG
      *日数回数テーブル
           MOVE    ACCT-DAY-AREA       TO  RECE04-DAY-AREA
           .
       9999-HOUKATU-SAKUSEI-RECE04-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分データ作成処理（レセプト０５）
      *****************************************************************
       9999-HOUKATU-SAKUSEI-RECE05-SEC SECTION.
      *
           IF      II                  =   1
      *
           INITIALIZE                      RECE05-REC
      *医療機関ＩＤ
           MOVE    RECE02-HOSPNUM      TO  RECE05-HOSPNUM
      *診療年月
           MOVE    RECE02-SRYYM        TO  RECE05-SRYYM
      *入外区分
           MOVE    RECE02-NYUGAIKBN    TO  RECE05-NYUGAIKBN
      *患者ＩＤ
           MOVE    RECE02-PTID         TO  RECE05-PTID
      *レセプト科
           MOVE    RECE02-RECEKA       TO  RECE05-RECEKA
      *審査支払機関
           MOVE    RECE02-TEISYUTUSAKI TO  RECE05-TEISYUTUSAKI
      *レセプト種別
           MOVE    RECE02-RECESYUBETU  TO  RECE05-RECESYUBETU
      *保険者番号
           MOVE    RECE02-HKNJANUM-KEY TO  RECE05-HKNJANUM
      *補助区分
           MOVE    RECE02-HOJOKBN-KEY  TO  RECE05-HOJOKBN
      *公費ＩＤ
           MOVE    RECE02-KOHID-KEY    TO  RECE05-KOHID
      *適用開始年月日
           MOVE    RECE02-TEKSTYMD     TO  RECE05-TEKSTYMD
      *分割区分
           MOVE    RECE02-RECEIPT-KBN  TO  RECE05-RECEIPT-KBN
      *剤番号
           MOVE    SRY-ZAINUM          TO  RECE05-ZAINUM
      *連番号
           MOVE    SRY-RENNUM          TO  RECE05-RENNUM
      *公費分離区分(???)
           MOVE    RECE02-KOHBUNRI-MIN TO  RECE05-KOHBUNRI
      *患者番号
           MOVE    RECE02-PTNUM        TO  RECE05-PTNUM
      *診療科
           MOVE    RECE02-SRYKA(1)     TO  RECE05-SRYKA
      *診療区分
           IF      SRY-SRYKBN          =   "64"
               MOVE    "60"                TO  RECE05-SRYKBN
           ELSE
               MOVE    SRY-SRYKBN          TO  RECE05-SRYKBN
           END-IF
      *診療種別区分
           IF      SRY-SRYKBN          =   "64"
               MOVE    "600"               TO  RECE05-SRYSYUKBN
           ELSE
               MOVE    SRY-SRYSYUKBN       TO  RECE05-SRYSYUKBN
           END-IF
      *自費金額
           MOVE    SRY-JIHIMONEYTOTAL  TO  RECE05-JIHIMONEY
      *
           END-IF
      *
      *診療コード
           MOVE    SRY-SRYCD (II)      TO  RECE05-SRYCD (II)
      *診療種別区分
           MOVE    SRY-SRYSYUKBN       TO  RECE05-SRYSYUKBN-X (II)
      *数量
           MOVE    SRY-SRYSURYO (II)   TO  RECE05-SRYSURYO (II)
      *回数
           MOVE    SRY-SRYKAISU (II)   TO  RECE05-SRYKAISU (II)
      *単位コード
           MOVE    TNS-TANICD          TO  RECE05-TANICD (II)
      *単位名称有効桁数
           MOVE    TNS-TANIMOJISU      TO  RECE05-TANIMOJISU (II)
      *単位名称
           MOVE    TNS-TANINAME        TO  RECE05-TANINAME (II)
      *点数識別区分
           MOVE    TNS-TENSIKIBETU     TO  RECE05-TENSIKIBETU (II)
      *点数
           MOVE    TNS-TEN             TO  RECE05-TEN (II)
      *明細請求フラグ
           MOVE    SRY-MEISKYFLG (II)  TO  RECE05-MEISKYFLG (II)
      *自動算定区分
           MOVE    SRY-AUTOKBN (II)    TO  RECE05-AUTOKBN (II)
      *名称入力番号
           MOVE    SRY-INPUTNUM (II)   TO  RECE05-INPUTNUM (II)
      *点数欄集計先識別
           MOVE    TNS-NYUTENTTLKBN    TO  RECE05-TENTTLKBN (II)
      *データ区分
           MOVE    TNS-DATAKBN         TO  RECE05-DATAKBN (II)
      *包括対象検査
           MOVE    TNS-HOUKNSKBN       TO  RECE05-HOUKNSKBN (II)
      *検査等実施判断
           MOVE    TNS-KNSJISKBN       TO  RECE05-KNSJISKBN (II)
      *検査等実施判断グループ
           MOVE    TNS-KNSJISGRPKBN    TO  RECE05-KNSJISGRPKBN (II)
      *各種番号等
           MOVE    TNS-CDKBN-KBN       TO  RECE05-CDKBN-KBN (II)
           MOVE    TNS-CDKBN-SYO       TO  RECE05-CDKBN-SYO (II)
           MOVE    TNS-CDKBN-BU        TO  RECE05-CDKBN-BU (II)
           MOVE    TNS-CDKBN-KBNNUM    TO  RECE05-CDKBN-KBNNUM (II)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  RECE05-CDKBN-KBNNUM-EDA (II)
           MOVE    TNS-CDKBN-KOUBAN    TO  RECE05-CDKBN-KOUBAN (II)
           MOVE    TNS-KOUHYOJYUNNUM   TO  RECE05-KOUHYOJYUNNUM (II)
      *告示等識別区分１
           MOVE    TNS-KOKUJISKBKBN1   TO  RECE05-KOKUJISKBKBN1 (II)
      *告示等識別区分２
           MOVE    TNS-KOKUJISKBKBN2   TO  RECE05-KOKUJISKBKBN2 (II)
      *通則加算対象外区分
           MOVE    TNS-TUSOKUGAIKBN    TO  RECE05-TUSOKUGAIKBN (II)
      *時間外加算区分
           MOVE    TNS-TIMEKSNKBN      TO  RECE05-TIMEKSNKBN (II)
      *きざみ値計算識別
           MOVE    TNS-KZMCOMPSIKIBETU TO  RECE05-KZMCOMPSIKIBETU (II)
      *注加算コード
           MOVE    TNS-CHUKSNCD        TO  RECE05-CHUKSNCD (II)
      *通番
           MOVE    TNS-CHUKSNTUBAN     TO  RECE05-CHUKSNTUBAN (II)
      *検体検査コメント
           MOVE    TNS-KENTAICOMMENT   TO  RECE05-KENTAICOMMENT (II)
      *入力コメント
           MOVE    SPACE               TO  RECE05-INPUTCOMENT (II)
      *漢字有効桁数
           MOVE    TNS-YUKOKETA        TO  RECE05-YUKOKETA (II)
      *漢字名称
           MOVE    TNS-NAME            TO  RECE05-NAME (II)
      *特定器材種別１
           MOVE    TNS-TOKUKIZAISBT1   TO  RECE05-TOKUKIZAISBT1 (II)
      *酸素等区分
           MOVE    TNS-SANSOKBN        TO  RECE05-SANSOKBN (II)
      *レセプト電算対象外フラグ(???)
      *     MOVE                        TO  RECE05-RECEDENFLG(II)
      *レセプト対象外フラグ(???)
      *     MOVE                        TO  RECE05-RECEPTFLG(II)
      *レセ電用特定器材商品名(???)
      *     MOVE                        TO  RECE05-RECEDENNAME(II)
      *レセ用特定器材商品名編集フラグ(???)
      *     MOVE                        TO  RECE05-RECEHENFLG(II)
      *関係コメント関連設定フラグ(???)
      *     MOVE                        TO  RECE05-SETCOMATFLG(II)
           .
       9999-HOUKATU-SAKUSEI-RECE05-EXT.
           EXIT.
      *
      ****************************************************************
      *    レセプト０３１データ読み
      ****************************************************************
       990-RECE03-NEXT-SEC           SECTION.
      *
           READ    RECE031-FILE      NEXT
               AT  END
                   MOVE    1         TO  FLG-RECE031
               NOT AT  END
                   IF      STS-RECE03          =   "00"
                       CONTINUE
                   ELSE
                       MOVE    SPACE               TO  WRK-ERR-AREA
                       INITIALIZE                      WRK-ERR-AREA
                       MOVE    2                   TO  WRK-ERR-FLG
                       MOVE    RECE031PARA         TO  WRK-ERR-FILE-NM
                       MOVE    STS-RECE03          TO  WRK-ERR-FILE-STS
                       PERFORM 500-FILE-ERR-ABORT-SEC
                   END-IF
      *
                   MOVE    ZERO      TO  FLG-RECE031
                   ADD     1         TO  CNT-RECE03
           END-READ
      *
           .
       990-RECE03-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタＲＥＡＤ
      *****************************************************************
       900-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SRYACCT
               MOVE    MCPDATA-REC     TO  SRYACCT-REC
           ELSE
               MOVE    1               TO  FLG-SRYACCT
           END-IF
           .
       900-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為読込
      *****************************************************************
       900-SRYACT-READ-SEC           SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
           .
       900-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       900-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               INITIALIZE                      TENSUPLUS-REC
               MOVE    1                   TO  FLG-TENSUPLUS
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       900-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名マスター読込
      *****************************************************************
       900-BYOMEI-READ-SEC         SECTION.
      *
           MOVE    BYOMEI-REC          TO  MCPDATA-REC
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_byomei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  BYOMEI-REC
                   MOVE    ZERO                TO  FLG-BYOMEI
               ELSE
                   MOVE    1                   TO  FLG-BYOMEI
                   INITIALIZE                  BYOMEI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-BYOMEI
               INITIALIZE                  BYOMEI-REC
           END-IF
      *
           MOVE    "tbl_byomei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-BYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブルクローズ処理
      *****************************************************************
       900-CLOSE-SEC               SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
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
      *    ＤＢＩＮＳＥＲＴ処理
      *****************************************************************
       900-DBINSERT-SEC                SECTION.
      *
           MOVE    "DBINSERT"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           IF      MCP-RC              =   ZERO
               CONTINUE
           ELSE
               MOVE    SPACE               TO  WRK-ERR-AREA
               INITIALIZE                      WRK-ERR-AREA
               MOVE    MCP-RC              TO  WRK-MCP-RC
      *
               STRING  "ＤＢ更新処理エラー　TABLE="
                                           DELIMITED  BY  SIZE
                       MCP-TABLE           DELIMITED  BY  SPACE
                       " RC="              DELIMITED   BY  SIZE
                       WRK-MCP-RC          DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
               END-STRING
               PERFORM 500-ERR-HENSYU-SEC
      *
               STRING  "ORCRDPC20 insert err TABLE="
                                           DELIMITED   BY  SIZE
                      MCP-TABLE            DELIMITED   BY  SPACE
                      " PATHNAME="         DELIMITED   BY  SIZE
                      MCP-PATHNAME         DELIMITED   BY  SPACE
                      " RC="               DELIMITED   BY  SIZE
                      WRK-MCP-RC           DELIMITED   BY  SIZE
                      LOW-VALUE            DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
               END-STRING
               PERFORM 500-COBABORT-SEC
           END-IF
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
               STRING  "ORCRDPC20 select err TABLE="
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
           .
       900-DBFETCH-EXT.
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
