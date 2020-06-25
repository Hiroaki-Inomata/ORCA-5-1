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
       PROGRAM-ID.             ORCGH01.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 請求管理−手書き訂正（Ｈ０１）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-藤原     01/09/14  クリアボタン追加
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/10/01  請求区分、訂正区分の修正
      *  01.01.03    MCC-藤原     01/10/26  保険種別の記号修正
      *  01.01.04    MCC-藤原     01/11/28  氏名検索への遷移の修正
      *  01.01.05    MCC-藤原     02/02/19  記号番号が長いとき表示する
      *                                     ように修正
      *  01.01.06    MCC-藤原     02/03/28  氏名検索画面から戻ったときの
      *                                     チェックを中止
      *  ２００２．１０改正対応 
      *  01.02.01    NACL-藤原    02/10/7   県内県外区分・県番号・
      *                                     年齢・給付割合の追加
      *  01.02.02    NACL-藤原    02/10/10  レセプト種別の出力の修正
      *                                     在宅寝たきり等の表示の追加       
      *  01.02.03    NACL-藤原    03/02/05  公費請求書ＤＢを追加
      *                                     入院対応
      *                                     科コードの入力の修正
      *  01.02.04    NACL-藤原    03/07/05  患者氏名での検索を追加
      *  01.02.05    NACL-藤原    04/01/26  排他制御処理追加
      *
      *  02.06.01    NACL-藤原    05/04/11  本人家族区分の追加
      *  02.06.02    NACL-藤原    05/09/29  補助区分からの編集の修正
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/31  MONFUNC対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *２００６年４月改正対応
      *  02.09.01    NACL-藤原    06/04/14  ２００６年４月改正対応
      *                                     「１３指導」「１３医学管理」
      *                                     「食事日数」「食事回数」
      *                                      の診療年月による表示の変更
      *                                     「在宅寝たきり等」の表示を
      *                                     「在医総等」に変更
      *
      *  03.03.01    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *                                     更新前チェックを追加 
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *                                     一括入力対応
      *  04.03.02    NACL-藤原    08/06/06  レセプト種別名称、作成日の追加
      *  04.03.03    NACL-藤原    08/06/18  公費請求分の請求年月等の追加
      *
      *  04.04.01    NACL-藤原    08/12/10  返戻対応
      *  04.04.02    NACL-藤原    09/08/10  合計点数が不一致となる現象の修正
      *                                     返戻年月入力後のカーソル位置
      *
      *  04.05.01    NACL-藤原    09/04/16  主科設定時の主科未設定のチェ
      *                                     ック追加
      *  04.05.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.05.03    NACL-藤原    11/03/25  災害対応
      *  04.05.04    NACL-藤原    11/04/22  災害対応
      *                                     災害区分の追加
      *  04.05.05    NACL-藤原    12/06/05  レセ電データ無し時のエラー表示
      *  04.05.06    NACL-藤原    12/07/18  公費単独の負担者番号、受給者
      *                                     番号の表示に自立支援を追加
      *
      *  04.06.01    NACL-藤原    10/10/12  公費単独のレセ分離対応
      *  04.06.02    NACL-藤原    10/10/12  キーにkohid 追加
      *                                     調査用にcrehms,updhms 追加
      *  04.06.03    NACL-藤原    10/12/20  保険者からの返戻対応
      *  04.06.04    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.05    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    12/11/19  主科情報の取得方法変更
      *  04.07.03    NACL-藤原    13/02/07  労災レセ電対応
      *  04.07.04    NACL-藤原    13/09/20  第三者行為対応
      *  04.07.05    NACL-藤原    13/10/07  労災レセ電返戻対応
      *
      *  04.08.01    NACL-藤原    15/08/25  作成時間の表示追加
      *  04.08.02    NACL-藤原    15/09/09  返戻取り込み済データ一覧対応
      *  04.08.03    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.04    NACL-藤原    16/10/07  自賠責ＰＤＦファイル作成対応
      *  04.08.05    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *  04.08.06    NACL-藤原    18/03/28  平成３０年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
       WORKING-STORAGE             SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    請求管理スパ領域
           COPY    "H01COMMON-SPA".
      *
      *    画面用ＳＰＡ
           COPY    "H01SCR-SPA".
      *
      *    設定済返戻データ
           02  SPA-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //SPA-HENREI-B-//.
      *
      *    対象一覧用返戻データ
           02  TBL-HENREI-B-REC    OCCURS  30.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //TBL-HENREI-B-//.
           02  TBL-HENREI-B-ERR    PIC 9(01)   OCCURS  30.
           02  TBL-HENREI-B-MAX    PIC 9(02).
      *
      *    更新対象返戻データ
           02  WRK-HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC"
                                   REPLACING   //HENREI-B-//
                                   BY          //WRK-HENREI-B-//.
      *
           COPY    "ENUM-VALUE".
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SEIKYU          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KOHSKY          PIC 9(01).
           03  FLG-HENREI-B        PIC 9(01).
           03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
           03  FLG-LABOR-SIO       PIC 9(01).
           03  FLG-RECEDEN         PIC 9(01).
           03  FLG-JIBAI-PRT       PIC 9(01).
      *
           03  FLG-TOUROKU         PIC 9(01).
           03  FLG-HENREI-B-CHK    pic 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR             PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDWW                PIC 9(04).
           03  IDYY                PIC 9(04).
           03  IDXX                PIC 9(04).
           03  IDZZ                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDB                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-MOTOPG          PIC X(08).
           03  WRK-Z9              PIC Z9.
           03  WRK-HIDMSG          PIC X(70).
      *
           03  WRK-SYO             PIC 9(02).
           03  WRK-AMARI           PIC 9(02).
           03  WRK-WIDGET          PIC 9(02).
           03  WRK-WIDGET-X        REDEFINES   WRK-WIDGET
                                   PIC X(02).
           03  WRK-WIDGET1         PIC 9(01).
      *
           03  WRK-NYUGAIKBN       PIC X(06).
           03  WRK-TEISYUTUSAKI    PIC X(10).
           03  WRK-RECESYUBETU     PIC X(14).
           03  WRK-YM              PIC X(07). 
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  WRK-SKYYM-OLD       PIC 9(08).
      *
           03  WRK-TITLE           PIC X(10).
           03  WRK-ZAITAKU         PIC X(08).
      *
           03  WRK-SYS-SRYYMD      PIC X(08).
      *
      *    入力項目名
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-GMN-CUR         PIC 9(03).
      *
       01  WRK-HEN-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HMM         PIC 9(02).
               05  FILLER          PIC X(01)   VALUE   ".".
               05  WRK-HDD         PIC 9(02).
      *
      *    スパ領域（ワーク）
           COPY    "COMMON-SPA"    REPLACING   //SPA-//
                                   BY          //WRK-SPA-//.
      * 
        01  WRK-CONS-SRYYM-200610  PIC 9(06)   VALUE   200610. 
        01  WRK-CONS-HENREI-B-MAX  PIC 9(02)   VALUE   30. 
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報
           COPY    "CPSK1001.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK2005.INC".
           COPY    "CPSK200501.INC".
      *
           COPY    "CPSK2009.INC".
      *
           COPY    "CPSK4001.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    公費請求書
           COPY    "CPKOHSKY.INC".
      *
       01  HENREI-B-REC.
           COPY    "CPHENREI-BODY.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
      *
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *    レセプト電算
           COPY    "CPRECEDEN.INC".
      *
       01  JIBAI-PRT-REC.
           COPY    "CPJIBAI-PRT.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *   画面日付変換サブ
          COPY    "CPORCSGDAY.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "H01.INC".
           COPY    "H02.INC".
           COPY    "H03.INC".
           COPY    "H99.INC".
           COPY    "HERR.INC".
           COPY    "HID1.INC".
      *
       PROCEDURE                   DIVISION    USING
           MCPAREA
           SPAAREA
           LINKAREA
           SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-H01FREE
           MOVE    SPA-WORK        TO  SPA-H01KYOUTU
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *
           MOVE    SPA-H01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-H01FREE     TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "HERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
               IF      FLG-END             =   1
                   GO  TO  100-INIT-EXT
               END-IF
      *        画面編集
               PERFORM 500-GMNHEN-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM 510-ERRSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
               IF      SPA-HIDCD       NOT =   SPACE
                   PERFORM 520-RIDSET-SEC
                   GO  TO  100-INIT-EXT
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  MCP-PUTTYPE
           MOVE    "H01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *    他のＬＤより
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *
      *        元の画面へ戻す
               IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"  OR
                                               "P97"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-H01FREE
                   MOVE    SPA-WORK        TO  SPA-H01KYOUTU
               END-IF
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-U01SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               ELSE
                   MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
               END-IF
           END-IF
      *
           EVALUATE    SPA-MOTOPG
      *    検索処理
               WHEN    "H99"
                   PERFORM 320-H99SPASET-SEC
                   GO      TO      300-SCREEN-EXT
      *    更新処理
               WHEN    "HID1"
                   PERFORM 330-HID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
      *    返戻情報設定
               WHEN    "H02"
                   PERFORM 340-H02-SET-SEC
                   GO      TO      300-SCREEN-EXT
      *    返戻一覧
               WHEN    "H03"
                   PERFORM 350-H03-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           PERFORM 310-SPASET-SEC
      *
           MOVE    1               TO  SPA-H01GMN-PAGE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-U01SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-H01GMN-PTNUM
               CONTINUE
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
                   MOVE    WRK-SPA-PTNUM       TO  H01-PTNUM
      *
                   PERFORM 420-CLEAR-SEC
                   MOVE    H01-PTNUM           TO  SPA-H01GMN-PTNUM
      *
                   INITIALIZE                  ORCSPTIDAREA
                   MOVE    SPA-HOSPNUM         TO  SPTID-HOSPNUM
                   MOVE    SPA-H01GMN-PTNUM    TO  SPTID-PTNUM
                   CALL    "ORCSPTID"          USING   ORCSPTIDAREA
                                                       SPA-AREA
                   IF      SPTID-RC        NOT =   ZERO
                       MOVE    "0004"          TO  SPA-ERRCD
                   ELSE
                       INITIALIZE              SPA-H01GMN-TABLE
                                               SPA-H01GMN-KOUMOKU
                                               SPA-H01NAI-KOUMOKU1
                       MOVE    SPTID-PTNUM     TO  SPA-H01GMN-PTNUM
                       MOVE    SPTID-PTID      TO  SPA-H01NAI-PTID
      *                患者マスター存在チェック
                       MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
                       MOVE    SPA-H01NAI-PTID TO  PTINF-PTID
                       PERFORM 900-PTINF-READ-SEC
                       IF      FLG-PTINF   NOT =   ZERO
                           MOVE    "0001"      TO  SPA-ERRCD
                       ELSE
      *
      *                    排他制御処理
                           PERFORM 990-LOCK-IN-SEC
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    PTINF-NAME  TO  SPA-H01GMN-NAME
                           END-IF
                       END-IF
                   END-IF     
               END-IF    
               MOVE    2                   TO  SPA-H01GMN-CUR
           END-IF
           .
       3101-U01SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *
           INITIALIZE              SPA-H01FREE
                                   SPA-H01KYOUTU 
      *
      *    医療機関情報取得
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"              TO  SYS-1001-KANRICD
           MOVE    "*"                 TO  SYS-1001-KBNCD
           MOVE    ZERO                TO  SYS-1001-STYUKYMD
                                           SYS-1001-EDYUKYMD
grpsys     MOVE    SPA-HOSPNUM         TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1001-REC
           END-IF
           MOVE    SYS-1001-HOSPSBT1   TO  SPA-SYS-1001-HOSPSBT1
      *
      *    見出し編集
           MOVE    "2"                 TO  SPA-RECE10-NYUGAIKBN
           MOVE    SPA-SYSYMD (1:6)    TO  SPA-RECE10-SRYYM
           MOVE    1                   TO  SPA-RECE10-TEISYUTUSAKI 
           PERFORM 3101-MIDASI-HENSYU-SEC
           MOVE    SPACE               TO  SPA-RECE10-NYUGAIKBN
           MOVE    ZERO                TO  SPA-RECE10-SRYYM
                                           SPA-RECE10-TEISYUTUSAKI
      *
           MOVE    "0"                 TO  SPA-H01GMN-SKYKBNL    (1)
           MOVE    "請求しない"        TO  SPA-H01GMN-SKYKBNMEIL (1)
           MOVE    "1"                 TO  SPA-H01GMN-SKYKBNL    (2)
           MOVE    "未請求"            TO  SPA-H01GMN-SKYKBNMEIL (2)
           MOVE    "2"                 TO  SPA-H01GMN-SKYKBNL    (3)
           MOVE    "請求済"            TO  SPA-H01GMN-SKYKBNMEIL (3)
      *         
      *    レセプト編集情報
           MOVE    SPACE               TO  SYS-2005-REC
           INITIALIZE                      SYS-2005-REC
           MOVE    SPA-HOSPNUM         TO  SYS-2005-HOSPNUM
           MOVE    "2005"              TO  SYS-2005-KANRICD
           MOVE    "*"                 TO  SYS-2005-KBNCD
           MOVE    ZERO                TO  SYS-2005-STYUKYMD 
           MOVE    SYS-2005-STYUKYMD   TO  SYS-2005-EDYUKYMD
           MOVE    SYS-2005-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-2005-REC
           ELSE    
               INITIALIZE                  SYS-2005-REC
           END-IF
           IF      SYS-2005-KKHRECEKBN =   SPACE
               MOVE    ZERO            TO  SYS-2005-KKHRECEKBN
           END-IF
           IF      SYS-2005-SYARECEKBN =   SPACE
               MOVE    ZERO            TO  SYS-2005-SYARECEKBN
           END-IF
           MOVE    SYS-2005-SYARECEKBN TO  SPA-NAI-2005-SYARECEKBN
           MOVE    SYS-2005-KKHRECEKBN TO  SPA-NAI-2005-KKHRECEKBN
      *
           MOVE    SPACE               TO  SYS-200501-REC
           INITIALIZE                      SYS-200501-REC
           MOVE    SPA-HOSPNUM         TO  SYS-200501-HOSPNUM
           MOVE    "2005"              TO  SYS-200501-KANRICD
           MOVE    "01"                TO  SYS-200501-KBNCD
           MOVE    ZERO                TO  SYS-200501-STYUKYMD 
           MOVE    SYS-200501-STYUKYMD TO  SYS-200501-EDYUKYMD
           MOVE    SYS-200501-REC      TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI    =   ZERO
               MOVE    MCPDATA-REC     TO  SYS-200501-REC
           ELSE    
               INITIALIZE                  SYS-200501-REC
           END-IF
           IF      SYS-200501-KOUIKIRECEKBN
                                       =   SPACE
               MOVE    ZERO                TO  SYS-200501-KOUIKIRECEKBN
           END-IF
           IF     SYS-200501-RECEDEN-DISASTER
                                       =   SPACE
               MOVE    "1"             TO  SYS-200501-RECEDEN-DISASTER
           END-IF
           IF     SYS-200501-ROURECEKBN
                                       =   SPACE
               MOVE    "0"             TO  SYS-200501-ROURECEKBN
           END-IF
           IF      SYS-200501-JIBAI-PDF
                                   =   SPACE
               MOVE    "0"             TO  SYS-200501-JIBAI-PDF
           END-IF
           MOVE    SYS-200501-KOUIKIRECEKBN
                                       TO  SPA-NAI-200501-KOUIKIRECEKBN
           MOVE    SYS-200501-RECEDEN-DISASTER
                                       TO
                                       SPA-NAI-200501-RECEDEN-DISASTER
           MOVE    SYS-200501-ROURECEKBN
                                       TO  SPA-NAI-200501-ROURECEKBN
           MOVE    SYS-200501-JIBAI-PDF
                                       TO  SPA-NAI-200501-JIBAI-PDF
      *
      *    レセプト編集区分情報（主科情報）
           MOVE    SPACE               TO  SYS-2009-REC
           INITIALIZE                      SYS-2009-REC
           MOVE    "2009"              TO  SYS-2009-KANRICD
           MOVE    "*"                 TO  SYS-2009-KBNCD
           MOVE    ZERO                TO  SYS-2009-STYUKYMD 
           MOVE    SYS-2009-STYUKYMD   TO  SYS-2009-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-2009-HOSPNUM
           MOVE    SYS-2009-REC        TO  MCPDATA-REC
           PERFORM 910-SYSKANRI-INV-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-2009-REC
           ELSE
               INITIALIZE                  SYS-2009-REC
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   SPACE
               MOVE    "0"             TO  SYS-2009-SYUKA-KBN
           END-IF
           IF      SYS-2009-SYUKA-KBN
                                   =   "0"
               MOVE    "0"             TO  SYS-2009-SYUKA-SYAHO
                                           SYS-2009-SYUKA-KOKUHO
                                           SYS-2009-SYUKA-KOUIKI
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
           IF    ( SYS-2009-SYUKA-KBN
                                   =   "1"   )
           AND   ( SYS-2009-SYUKA-KOUIKI
                                   =   SPACE )
               MOVE    "0"             TO  SYS-2009-SYUKA-KOUIKI
           END-IF
           MOVE    SYS-2009-SYUKA-SYAHO
                                       TO  SPA-NAI-2009-SYUKA-SYAHO
           MOVE    SYS-2009-SYUKA-KOKUHO
                                       TO  SPA-NAI-2009-SYUKA-KOKUHO
           MOVE    SYS-2009-SYUKA-KOUIKI
                                       TO  SPA-NAI-2009-SYUKA-KOUIKI
      *
      *    労災医療機関情報
           INITIALIZE                      SYS-4001-REC
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               PERFORM UNTIL ( FLG-SYSKANRI        =   1   )
                       OR    ( SPA-NAI-4001-MAX    >=  100 )
                   MOVE    MCPDATA-REC         TO  SYS-4001-REC
                   IF      SYS-4001-EDYUKYMD   <   "20130701"
                       CONTINUE
                   ELSE
                       IF      SYS-4001-RSIIRYOKBN =   "1"
                           MOVE    1           TO
                                           SPA-NAI-4001-RSIIRYOKBN-FLG
                       END-IF
                       ADD     1               TO  SPA-NAI-4001-MAX
                       MOVE    SYS-4001-STYUKYMD
                                               TO  SPA-NAI-4001-STYUKYMD
                                                      (SPA-NAI-4001-MAX)
                       MOVE    SYS-4001-EDYUKYMD
                                               TO  SPA-NAI-4001-EDYUKYMD
                                                      (SPA-NAI-4001-MAX)
                       MOVE    SYS-4001-RSIIRYOKBN
                                               TO
                                               SPA-NAI-4001-RSIIRYOKBN
                                                      (SPA-NAI-4001-MAX)
                   END-IF
      *
                   MOVE    "tbl_syskanri"    TO  MCP-TABLE
                   MOVE    "key8"            TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               END-PERFORM
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
      *****IF      SYS-2005-??         =   "0"
               MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
      *****ELSE
      *****    MOVE    WIDGET-NORMAL       TO
      *****                            SPA-H01GMN-HENREI-SIJI-FLG-STE
      *****END-IF
           MOVE    "F"                 TO  SPA-H01GMN-HENREI-SIJI-FLG
           MOVE    ZERO                TO  SPA-H01NAI-HENREI-MSG
      *                                 
           MOVE    1                   TO  SPA-H01GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    見出し編集処理
      *****************************************************************
       3101-MIDASI-HENSYU-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-H01GMN-MIDASI-AREA
      *
           EVALUATE    SPA-RECE10-NYUGAIKBN
                                       ALSO    SPA-RECE10-TEISYUTUSAKI
               WHEN    "1"             ALSO    1
               WHEN    "1"             ALSO    2
               WHEN    "1"             ALSO    6
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       35
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE  "          初    診"  TO SPA-H01GMN-MIDASI(01)
                   IF      SPA-RECE10-SRYYM    >=  "200604"
                       MOVE  "          医学管理"
                                               TO SPA-H01GMN-MIDASI(02)
                   ELSE 
                       MOVE  "          指    導"
                                               TO SPA-H01GMN-MIDASI(02)
                   END-IF 
                   MOVE  "          在    宅"  TO SPA-H01GMN-MIDASI(03)
                   MOVE  "          内    服"  TO SPA-H01GMN-MIDASI(04)
                   MOVE  "          屯    服"  TO SPA-H01GMN-MIDASI(05)
                   MOVE  "          外    用"  TO SPA-H01GMN-MIDASI(06)
                   MOVE  "          調    剤"  TO SPA-H01GMN-MIDASI(07)
                   MOVE  "          麻    毒"  TO SPA-H01GMN-MIDASI(08)
                   MOVE  "          調    基"  TO SPA-H01GMN-MIDASI(09)
                   MOVE  "        皮下筋肉内"  TO SPA-H01GMN-MIDASI(10)
                   MOVE  "            静脈内"  TO SPA-H01GMN-MIDASI(11)
                   MOVE  "            その他"  TO SPA-H01GMN-MIDASI(12)
                   MOVE  "            処  置"  TO SPA-H01GMN-MIDASI(13)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(14)
                   MOVE  "        手術・麻酔"  TO SPA-H01GMN-MIDASI(15)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(16)
                   IF      SPA-RECE10-SRYYM    >=  "200804"
                       MOVE  "          病理検査"
                                               TO SPA-H01GMN-MIDASI(17)
                   ELSE
                       MOVE  "            検  査"
                                               TO SPA-H01GMN-MIDASI(17)
                   END-IF
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(18)
                   MOVE  "          画像診断"  TO SPA-H01GMN-MIDASI(19)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(20)
                   MOVE  "            その他"  TO SPA-H01GMN-MIDASI(21)
                   MOVE  "          薬    剤"  TO SPA-H01GMN-MIDASI(22)
                   MOVE  "  入院基本料・加算"  TO SPA-H01GMN-MIDASI(23)
                   MOVE  "特定入院料・その他"
                                               TO SPA-H01GMN-MIDASI(28)
                   MOVE  "        〃        "  TO SPA-H01GMN-MIDASI(24)
                                                  SPA-H01GMN-MIDASI(25)
                                                  SPA-H01GMN-MIDASI(26)
                                                  SPA-H01GMN-MIDASI(27)
                                                  SPA-H01GMN-MIDASI(29)
                                                  SPA-H01GMN-MIDASI(30)
                                                  SPA-H01GMN-MIDASI(31)
                                                  SPA-H01GMN-MIDASI(32)
                   MOVE  SPACE              TO    SPA-H01GMN-MIDASI(33)
                                                  SPA-H01GMN-MIDASI(34)
                                                  SPA-H01GMN-MIDASI(35)
                   MOVE  "食事療養費"       TO SPA-H01GMN-SHOMIDASI (1)
                   MOVE  "食事標準負担額"   TO SPA-H01GMN-SHOMIDASI (2) 
                   IF      SPA-RECE10-SRYYM    >=  "200604"
                       MOVE  "食事回数"     TO SPA-H01GMN-SHOMIDASI (3)
                   ELSE
                       MOVE  "食事日数"     TO SPA-H01GMN-SHOMIDASI (3)
                   END-IF 
                   MOVE  WIDGET-NORMAL      TO SPA-H01GMN-SHO-STATE 
                   MOVE    "合計点数"       TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "一部負担金"     TO SPA-H01GMN-TENMIDASI (2)
                   IF      SPA-RECE10-SRYYM    <   "200304"
                       MOVE    "薬剤一部負担金"
                                            TO SPA-H01GMN-TENMIDASI (3)
                   END-IF
                   MOVE    "実日数"         TO SPA-H01GMN-TENMIDASI (4)
               WHEN    "2"             ALSO    1
               WHEN    "2"             ALSO    2
               WHEN    "2"             ALSO    6
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       35
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(01)
                   MOVE    "        再    診"  TO SPA-H01GMN-MIDASI(02)
                   MOVE    "    外来管理加算"  TO SPA-H01GMN-MIDASI(03)
                   MOVE    "          時間外"  TO SPA-H01GMN-MIDASI(04)
                   MOVE    "        休    日"  TO SPA-H01GMN-MIDASI(05)
                   MOVE    "        深    夜"  TO SPA-H01GMN-MIDASI(06)
                   IF      SPA-RECE10-SRYYM    >=  "200604"
                       MOVE  "        医学管理"
                                               TO SPA-H01GMN-MIDASI(07)
                   ELSE 
                       MOVE  "        指    導"
                                               TO SPA-H01GMN-MIDASI(07)
                   END-IF 
                   MOVE    "        往    診"  TO SPA-H01GMN-MIDASI(08)
                   MOVE    "        夜    間"  TO SPA-H01GMN-MIDASI(09)
                   MOVE    "      深夜・緊急"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "在宅患者訪問診療"  TO SPA-H01GMN-MIDASI(11)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(12)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(13)
                   MOVE    "        内服薬剤"  TO SPA-H01GMN-MIDASI(14)
                   MOVE    "        内服調剤"  TO SPA-H01GMN-MIDASI(15)
                   MOVE    "        屯服薬剤"  TO SPA-H01GMN-MIDASI(16)
                   MOVE    "        外用薬剤"  TO SPA-H01GMN-MIDASI(17)
                   MOVE    "        外用調剤"  TO SPA-H01GMN-MIDASI(18)
                   MOVE    "        処    方"  TO SPA-H01GMN-MIDASI(19)
                   MOVE    "        麻    毒"  TO SPA-H01GMN-MIDASI(20)
                   MOVE    "        調    基"  TO SPA-H01GMN-MIDASI(21)
                   MOVE    "      皮下筋肉内"  TO SPA-H01GMN-MIDASI(22)
                   MOVE    "          静脈内"  TO SPA-H01GMN-MIDASI(23)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(24)
                   MOVE    "          処  置"  TO SPA-H01GMN-MIDASI(25)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(26)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(27)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(28)
                   IF      SPA-RECE10-SRYYM    >=  "200804"
                       MOVE  "        病理検査"
                                               TO SPA-H01GMN-MIDASI(29)
                   ELSE
                       MOVE  "          検  査"
                                               TO SPA-H01GMN-MIDASI(29)
                   END-IF
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(30)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(31)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(32)
                   MOVE    "        処方せん"  TO SPA-H01GMN-MIDASI(33)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(34)
                   MOVE    "        薬    剤"  TO SPA-H01GMN-MIDASI(35)
                   MOVE    SPACE               TO
                                               SPA-H01GMN-SHOMIDASI (1)
                                               SPA-H01GMN-SHOMIDASI (2)
                                               SPA-H01GMN-SHOMIDASI (3)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "合計点数"       TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "一部負担金"     TO SPA-H01GMN-TENMIDASI (2)
                   IF      SPA-RECE10-SRYYM    <   "200304"
                       MOVE    "薬剤一部負担金"
                                            TO SPA-H01GMN-TENMIDASI (3)
                   END-IF
                   MOVE    "実日数"         TO SPA-H01GMN-TENMIDASI (4)
               WHEN    "1"             ALSO    5
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       31
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE  "          初    診"  TO SPA-H01GMN-MIDASI(01)
                   MOVE  "          医学管理"  TO SPA-H01GMN-MIDASI(02)
                   MOVE  "          在    宅"  TO SPA-H01GMN-MIDASI(03)
                   MOVE  "          内    服"  TO SPA-H01GMN-MIDASI(04)
                   MOVE  "          屯    服"  TO SPA-H01GMN-MIDASI(05)
                   MOVE  "          外    用"  TO SPA-H01GMN-MIDASI(06)
                   MOVE  "          調    剤"  TO SPA-H01GMN-MIDASI(07)
                   MOVE  "          麻    毒"  TO SPA-H01GMN-MIDASI(08)
                   MOVE  "          調    基"  TO SPA-H01GMN-MIDASI(09)
                   MOVE  "            注  射"  TO SPA-H01GMN-MIDASI(10)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(11)
                   MOVE  "            処  置"  TO SPA-H01GMN-MIDASI(12)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(13)
                   MOVE  "        手術・麻酔"  TO SPA-H01GMN-MIDASI(14)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(15)
                   MOVE  "            検  査"  TO SPA-H01GMN-MIDASI(16)
                   MOVE  "            薬  剤"  TO SPA-H01GMN-MIDASI(17)
                   MOVE  "          画像診断"  TO SPA-H01GMN-MIDASI(18)
                   MOVE  "        フィルム等"  TO SPA-H01GMN-MIDASI(19)
                   MOVE  "            その他"  TO SPA-H01GMN-MIDASI(20)
                   MOVE  "          薬    剤"  TO SPA-H01GMN-MIDASI(21)
                   MOVE  "  入院基本料・加算"  TO SPA-H01GMN-MIDASI(22)
                   MOVE  "  公害入院療養指導"  TO SPA-H01GMN-MIDASI(27)
                   MOVE  "  清浄空気室管理料"  TO SPA-H01GMN-MIDASI(30)
                   MOVE  "            その他"  TO SPA-H01GMN-MIDASI(31)
                   MOVE  "        〃        "  TO SPA-H01GMN-MIDASI(23)
                                                  SPA-H01GMN-MIDASI(24)
                                                  SPA-H01GMN-MIDASI(25)
                                                  SPA-H01GMN-MIDASI(26)
                                                  SPA-H01GMN-MIDASI(28)
                                                  SPA-H01GMN-MIDASI(29)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "        小  計" TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "（食事）小  計" TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "        合  計" TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "        実日数" TO SPA-H01GMN-TENMIDASI (4)
               WHEN    "2"             ALSO    5
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       38
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(1)
                   MOVE    "        再    診"  TO SPA-H01GMN-MIDASI(2)
                   MOVE    "    外来管理加算"  TO SPA-H01GMN-MIDASI(3)
                   MOVE    "          時間外"  TO SPA-H01GMN-MIDASI(4)
                   MOVE    "        休    日"  TO SPA-H01GMN-MIDASI(5)
                   MOVE    "        深    夜"  TO SPA-H01GMN-MIDASI(6)
                   MOVE    "        公害相談"  TO SPA-H01GMN-MIDASI(7)
                   MOVE    "公害外来療養指導"  TO SPA-H01GMN-MIDASI(8)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(9)
                   MOVE    "        往    診"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "        夜    間"  TO SPA-H01GMN-MIDASI(11)
                   MOVE    "      深夜・緊急"  TO SPA-H01GMN-MIDASI(12)
                   MOVE    "在宅患者訪問診療"  TO SPA-H01GMN-MIDASI(13)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(14)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(15)
                   MOVE    "        内服薬剤"  TO SPA-H01GMN-MIDASI(16)
                   MOVE    "        内服調剤"  TO SPA-H01GMN-MIDASI(17)
                   MOVE    "        屯    服"  TO SPA-H01GMN-MIDASI(18)
                   MOVE    "        外用薬剤"  TO SPA-H01GMN-MIDASI(19)
                   MOVE    "        外用調剤"  TO SPA-H01GMN-MIDASI(20)
                   MOVE    "        処    方"  TO SPA-H01GMN-MIDASI(21)
                   MOVE    "        麻    毒"  TO SPA-H01GMN-MIDASI(22)
                   MOVE    "        調    基"  TO SPA-H01GMN-MIDASI(23)
                   MOVE    "      皮下筋肉内"  TO SPA-H01GMN-MIDASI(24)
                   MOVE    "          静脈注"  TO SPA-H01GMN-MIDASI(25)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(26)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(27)
                   MOVE    "          処  置"  TO SPA-H01GMN-MIDASI(28)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(29)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(30)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(31)
                   MOVE    "          検  査"  TO SPA-H01GMN-MIDASI(32)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(33)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(34)
                   MOVE    "      フィルム等"  TO SPA-H01GMN-MIDASI(35)
                   MOVE    "        処方せん"  TO SPA-H01GMN-MIDASI(36)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(37)
                   MOVE    "          薬  剤"  TO SPA-H01GMN-MIDASI(38)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "小  計"         TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    SPACE            TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "合  計"         TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "実日数"         TO SPA-H01GMN-TENMIDASI (4)
               WHEN    "1"             ALSO    3
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       38
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(1)
                   MOVE    "        指    導"  TO SPA-H01GMN-MIDASI(3)
                   MOVE    "        在    宅"  TO SPA-H01GMN-MIDASI(4)
                   MOVE    "        投    薬"  TO SPA-H01GMN-MIDASI(5)
                   MOVE    "        注    射"  TO SPA-H01GMN-MIDASI(6)
                   MOVE    "        処    置"  TO SPA-H01GMN-MIDASI(7)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(8)
                   MOVE    "        検査病理"  TO SPA-H01GMN-MIDASI(9)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(11)
                   MOVE    "        入    院"  TO SPA-H01GMN-MIDASI(12)
                   MOVE    "            初診"  TO SPA-H01GMN-MIDASI(13)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(18)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "点数計"         TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "（点数欄）小計" TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "（金額欄）小計" TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "（食  事）小計" TO SPA-H01GMN-TENMIDASI (4)
                   MOVE    "合  計"         TO SPA-H01GMN-SHOMIDASI (1)
                   MOVE    "実日数"         TO SPA-H01GMN-SHOMIDASI (2)
               WHEN    "2"             ALSO    3
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       38
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(1)
                   MOVE    "        再    診"  TO SPA-H01GMN-MIDASI(2)
                   MOVE    "        指    導"  TO SPA-H01GMN-MIDASI(3)
                   MOVE    "        在    宅"  TO SPA-H01GMN-MIDASI(4)
                   MOVE    "        投    薬"  TO SPA-H01GMN-MIDASI(5)
                   MOVE    "        注    射"  TO SPA-H01GMN-MIDASI(6)
                   MOVE    "        処    置"  TO SPA-H01GMN-MIDASI(7)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(8)
                   MOVE    "        検査病理"  TO SPA-H01GMN-MIDASI(9)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(11)
                   IF      SPA-RECE10-RECESYUBETU =   "7051"
                       CONTINUE
                   ELSE
                       MOVE
                           "            初診"  TO SPA-H01GMN-MIDASI(13)
                       MOVE
                           "            再診"  TO SPA-H01GMN-MIDASI(14)
                   END-IF
                   IF      SPA-RECE10-RECESYUBETU =   "7051"   OR
                                                      "7021"
                       CONTINUE
                   ELSE
                       MOVE
                           "            指導"  TO SPA-H01GMN-MIDASI(15)
                   END-IF
                   IF      SPA-RECE10-RECESYUBETU =   "7021"
                       CONTINUE
                   ELSE
                       MOVE
                           "          その他"  TO SPA-H01GMN-MIDASI(18)
                   END-IF
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "点数計"         TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "（点数欄）小計" TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "（金額欄）小計" TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "合  計"         TO SPA-H01GMN-SHOMIDASI (1)
                   MOVE    "実日数"         TO SPA-H01GMN-SHOMIDASI (2)
               WHEN    "1"             ALSO    4
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       38
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(1)
                   MOVE    "        指    導"  TO SPA-H01GMN-MIDASI(3)
                   MOVE    "        投    薬"  TO SPA-H01GMN-MIDASI(5)
                   MOVE    "        注    射"  TO SPA-H01GMN-MIDASI(6)
                   MOVE    "        処    置"  TO SPA-H01GMN-MIDASI(7)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(8)
                   MOVE    "        検査病理"  TO SPA-H01GMN-MIDASI(9)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(11)
                   MOVE    "        入    院"  TO SPA-H01GMN-MIDASI(12)
                   IF      RECE10-RECEKBN  =   "4"
                       MOVE
                           "            初診"  TO SPA-H01GMN-MIDASI(13)
                       MOVE
                           "        救急医療"  TO SPA-H01GMN-MIDASI(16)
                   END-IF
                   MOVE    "        入院室料"  TO SPA-H01GMN-MIDASI(17)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(18)
                   MOVE    "        診断書料"  TO SPA-H01GMN-MIDASI(19)
                   MOVE    "        明細書料"  TO SPA-H01GMN-MIDASI(20)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "点数計"         TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "（点数欄）小計" TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "（金額欄）小計" TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "（食  事）小計" TO SPA-H01GMN-TENMIDASI (4)
                   MOVE    "合  計"         TO SPA-H01GMN-SHOMIDASI (1)
                   MOVE    "実日数"         TO SPA-H01GMN-SHOMIDASI (2)
               WHEN    "2"             ALSO    4
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       38
                       MOVE    "blue"  TO  SPA-H01GMN-MIDASI-STYLE (IDX)
                   END-PERFORM
                   MOVE    "        初    診"  TO SPA-H01GMN-MIDASI(1)
                   MOVE    "        再    診"  TO SPA-H01GMN-MIDASI(2)
                   MOVE    "        指    導"  TO SPA-H01GMN-MIDASI(3)
                   MOVE    "        在    宅"  TO SPA-H01GMN-MIDASI(4)
                   MOVE    "        投    薬"  TO SPA-H01GMN-MIDASI(5)
                   MOVE    "        注    射"  TO SPA-H01GMN-MIDASI(6)
                   MOVE    "        処    置"  TO SPA-H01GMN-MIDASI(7)
                   MOVE    "      手術・麻酔"  TO SPA-H01GMN-MIDASI(8)
                   MOVE    "        検査病理"  TO SPA-H01GMN-MIDASI(9)
                   MOVE    "        画像診断"  TO SPA-H01GMN-MIDASI(10)
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(11)
                   IF      RECE10-RECEKBN  =   "4"
                       MOVE
                           "            初診"  TO SPA-H01GMN-MIDASI(13)
                       MOVE
                           "            再診"  TO SPA-H01GMN-MIDASI(14)
                       MOVE
                           "            指導"  TO SPA-H01GMN-MIDASI(15)
                       MOVE
                           "        救急医療"  TO SPA-H01GMN-MIDASI(16)
                   END-IF
                   MOVE    "          その他"  TO SPA-H01GMN-MIDASI(18)
                   MOVE    "        診断書料"  TO SPA-H01GMN-MIDASI(19)
                   MOVE    "        明細書料"  TO SPA-H01GMN-MIDASI(20)
                   MOVE  WIDGET-INSENSITIVE TO SPA-H01GMN-SHO-STATE 
                   MOVE    "点数計"         TO SPA-H01GMN-TENMIDASI (1)
                   MOVE    "（点数欄）小計" TO SPA-H01GMN-TENMIDASI (2)
                   MOVE    "（金額欄）小計" TO SPA-H01GMN-TENMIDASI (3)
                   MOVE    "合  計"         TO SPA-H01GMN-SHOMIDASI (1)
                   MOVE    "実日数"         TO SPA-H01GMN-SHOMIDASI (2)
           END-EVALUATE
      *
           .
       3101-MIDASI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    対象レセプトセット処理
      *****************************************************************
       320-H99SPASET-SEC          SECTION.
      *
           IF      SPA-H99-KEY      =   SPACE
               GO  TO  320-H99SPASET-EXT
           END-IF
      *
           MOVE    SPACE           TO  SPA-UPDFLG
                                       SPA-HENFLG
      *
           INITIALIZE              RECE10-REC
           MOVE    SPA-H99-KEY     TO  RECE10-KEY
           PERFORM 900-SEIKYU-INV-SEC
           MOVE    MCPDATA-REC     TO  RECE10-REC
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  320-H99SPASET-EXT
           END-IF
      *
           PERFORM 41021-SEIKYU-HENSYU-SEC
      *
           .
       320-H99SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-HID1-SET-SEC            SECTION.
      *
           IF      SPA-HID1-FLG    =   "OK"
               EVALUATE    SPA-HIDCD
                   WHEN    "1001"
                   WHEN    "1002"
                       PERFORM 450-KOUSIN-SEC
                   WHEN    "0101"
                       MOVE    SPACE       TO  SPA-UPDFLG
                       MOVE    "JOIN"      TO  MCP-PUTTYPE
                       PERFORM 210-BACK
                   WHEN    "1003"
                       CONTINUE 
                   WHEN    "1004"
                       PERFORM 3301-HENREI-B-CLEAR-SEC
                   WHEN    "1005"
                       MOVE    "F"         TO
                                           SPA-H01GMN-HENREI-SIJI-FLG
                       MOVE    6           TO  SPA-H01GMN-CUR
                   WHEN    "1006"
                       MOVE    SPACE       TO  SPA-UPDFLG
               END-EVALUATE
           ELSE
               MOVE    "1"             TO  SPA-UPDFLG 
               EVALUATE    SPA-HIDCD
                   WHEN    "1003"
                       INITIALIZE          WRK-HENREI-B-REC
                       MOVE    "F"         TO
                                           SPA-H01GMN-HENREI-SIJI-FLG
                   WHEN    "1004"
                       MOVE    "T"     TO  SPA-H01GMN-HENREI-SIJI-FLG
                       MOVE    6           TO  SPA-H01GMN-CUR
                   WHEN    "1005"
                       MOVE    SPA-H01GMN-HENREIYM-OLD
                                       TO  SPA-H01GMN-HENREIYM
                       MOVE    SPA-H01NAI-HENREIYM-OLD
                                       TO  SPA-H01NAI-HENREIYM
                       MOVE    WIDGET-NORMAL
                                       TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
                   WHEN    "1006"
      *****************PERFORM 420-CLEAR-SEC
                       MOVE    2       TO  SPA-H01GMN-CUR
               END-EVALUATE 
           END-IF
      *
           MOVE    SPACE           TO  SPA-HID1-FLG
           MOVE    SPACE           TO  SPA-HIDCD      
           .
       330-HID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻情報解除処理
      *****************************************************************
       3301-HENREI-B-CLEAR-SEC         SECTION.
      *
      *    既に設定されていた情報をクリアする
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-HENREI-B-MAX
               IF    ( WRK-HENREI-B-HOSPNUM
                                       =   TBL-HENREI-B-HOSPNUM 
                                                             (IDXX) )
               AND   ( WRK-HENREI-B-TEISYUTUSAKI
                                       =   TBL-HENREI-B-TEISYUTUSAKI
                                                             (IDXX) )
               AND   ( WRK-HENREI-B-TEISYUTUSAKI2
                                       =   TBL-HENREI-B-TEISYUTUSAKI2
                                                             (IDXX) )
               AND   ( WRK-HENREI-B-SYORIYM
                                       =   TBL-HENREI-B-SYORIYM 
                                                             (IDXX) )
               AND   ( WRK-HENREI-B-RECEIPT-NO
                                       =   TBL-HENREI-B-RECEIPT-NO 
                                                             (IDXX) )
                   PERFORM VARYING IDZZ    FROM    1   BY  1
                           UNTIL   IDZZ    >       10
                           OR      TBL-HENREI-B-S-TEISYUTUSAKI
                                                       (IDXX IDZZ)
                                           =   SPACE
                       IF    ( SPA-RECE10-SKYYM        =
                                           TBL-HENREI-B-S-HENREIYM
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-TEISYUTUSAKI =
                                           TBL-HENREI-B-S-TEISYUTUSAKI
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-NYUGAIKBN    =
                                           TBL-HENREI-B-S-NYUGAIKBN   
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-RECEKA       =
                                           TBL-HENREI-B-S-RECEKA      
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-RECESYUBETU  =
                                           TBL-HENREI-B-S-RECESYUBETU 
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-HKNJANUM     =
                                           TBL-HENREI-B-S-HKNJANUM    
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-HOJOKBN      =
                                           TBL-HENREI-B-S-HOJOKBN-KEY 
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-KOHID-KEY    =
                                           TBL-HENREI-B-S-KOHID-KEY
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-TEKSTYMD     =
                                           TBL-HENREI-B-S-TEKSTYMD    
                                                           (IDXX IDZZ) )
                       AND   ( SPA-RECE10-RECEIPT-KBN  =
                                           TBL-HENREI-B-S-RECEIPT-KBN    
                                                           (IDXX IDZZ) )
                           INITIALIZE          TBL-HENREI-B-SEIKYU-OCC
                                                           (IDXX IDZZ)
                           PERFORM VARYING IDYY    FROM    IDZZ    BY  1
                                   UNTIL   IDYY    >       9
                               COMPUTE IDWW    =   IDYY    +   1
                               IF      TBL-HENREI-B-S-TEISYUTUSAKI
                                                       (IDXX IDWW)
                                                   =   SPACE
                                   MOVE    10              TO  IDYY
                               ELSE
                                   MOVE    TBL-HENREI-B-SEIKYU-OCC
                                                       (IDXX IDWW)
                                                           TO
                                               TBL-HENREI-B-SEIKYU-OCC
                                                           (IDXX IDYY)
                                   INITIALIZE  TBL-HENREI-B-SEIKYU-OCC
                                                       (IDXX IDWW)
                               END-IF
                           END-PERFORM
                           MOVE    10              TO  IDZZ
                       END-IF
                   END-PERFORM
                   MOVE    TBL-HENREI-B-MAX    TO  IDXX
               END-IF
           END-PERFORM
      *
           INITIALIZE                  WRK-HENREI-B-REC
      *
           MOVE    6           TO  SPA-H01GMN-CUR
           .
       3301-HENREI-B-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻情報設定更新処理
      *****************************************************************
       340-H02-SET-SEC            SECTION.
      *
           IF      SPA-H02-FLG     =   "OK"
               MOVE    SPA-H02NAI-TINDEX (SPA-H02NAI-SELNUM)
                                       TO  IDZZ
               MOVE    TBL-HENREI-B-REC (IDZZ)
                                       TO  WRK-HENREI-B-REC
           ELSE
               IF      SPA-H01-SYOKBN      =   ZERO
                   MOVE    "F"         TO  SPA-H01GMN-HENREI-SIJI-FLG
               END-IF
           END-IF
      *
           IF    ( SPA-H02-FLG     =   "OK" )
           AND   ( SPA-H01-CUR     =   302  )
               MOVE    6               TO  SPA-H01GMN-CUR
           ELSE
               MOVE    SPA-H01-CUR     TO  SPA-H01GMN-CUR
           END-IF
      *
           MOVE    SPACE           TO  SPA-HID1-FLG
                                       SPA-H02-FLG
           MOVE    SPACE           TO  SPA-HIDCD
           .
       340-H02-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻情報設定更新処理
      *****************************************************************
       350-H03-SET-SEC            SECTION.
      *
           MOVE    SPA-H01-CUR     TO  SPA-H01GMN-CUR
      *
           MOVE    SPACE           TO  SPA-HID1-FLG
                                       SPA-H02-FLG
           MOVE    SPACE           TO  SPA-HIDCD
           .
       350-H03-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE    "CHANGE"        TO  MCP-PUTTYPE
                   PERFORM 210-BACK
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLEAR-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 430-MAEKPTNUM-SEC
      *        返戻一覧
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 450-HENREI-LIST-SEC
      *        氏名検索
               WHEN    "CLICKED"       ALSO    "B09"
                   PERFORM 480-KENSAKU-SEC
      *        返戻設定確認
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 490-HENREI-SEC
      *         更新
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KOUSIN-GIDMSG-SEC
      *         返戻設定
               WHEN    "CLICKED"       ALSO    "HENREI_SIJI_FLG"
                   PERFORM 4119-HENREI-SIJI-FLG-CHK-SEC
           END-EVALUATE
      *
           .
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 4101-PTNUM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYYM"
                   PERFORM 4102-SRYYM-CHK-SEC
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 4103-SRYKA-CHK-SEC
      *        一括分
               WHEN    "ACTIVATE"       ALSO    ANY
                   PERFORM 400-ENTRY-SYORI-SEC
           END-EVALUATE
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    一括分入力処理
      *****************************************************************
       400-ENTRY-SYORI-SEC             SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-H01GMN-CUR
      *
      *    更新フラグセット
           MOVE    "1"                 TO  SPA-UPDFLG
      *
      *    入力チェック処理
           MOVE    ZERO                TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           .
       400-ENTRY-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC       SECTION.
      *
      *    入力チェック
           IF      SPA-HENFLG          =   SPACE
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-H01GMN-CUR
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
      *    基本チェック処理
           PERFORM 4102-KIHON-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    関連チェック処理
           PERFORM 4105-KANREN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
      *    基本チェック後ＳＰＡ再セット
           PERFORM 4104-GMN-SPA-HENSYU-SEC
           IF      SPA-HIDCD       NOT =   SPACE
               GO  TO  410-INPUT-CHK-EXT
           END-IF
      *
           IF      FLG-TOUROKU         =   1
      *        更新前
               PERFORM 4103-KAKUTEI-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面をＳＰＡセット処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC       SECTION.
      *
      *    入力前データ
           MOVE    SPA-H01NAI-SKYYM
                                   TO  WRK-SKYYM-OLD
           MOVE    SPA-H01NAI-HENREIYM
                                   TO  SPA-H01NAI-HENREIYM-OLD
           MOVE    SPA-H01GMN-HENREIYM
                                   TO  SPA-H01GMN-HENREIYM-OLD
      *
           MOVE    H01-SKYYM       TO  SPA-H01GMN-SKYYM
           MOVE    H01-SKYYM-KOHSKY
                                   TO  SPA-H01GMN-SKYYM-KOHSKY
           MOVE    H01-HENREIYM    TO  SPA-H01GMN-HENREIYM
           MOVE    H01-HENREIYM-KOHSKY
                                   TO  SPA-H01GMN-HENREIYM-KOHSKY
           MOVE    H01-SKYKBN      TO  SPA-H01GMN-SKYKBN-G
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       38
               MOVE    H01-NKHNTEN (IDX)   TO  SPA-H01GMN-NKHNTEN (IDX)
               MOVE    H01-NKAISU (IDX)    TO  SPA-H01GMN-NKAISU  (IDX)
               PERFORM VARYING IDY     FROM    1   BY  1
                       UNTIL   IDY     >       3
      ****         COMPUTE SPA-H01GMN-NTOTALTEN (IDY) = 
      ****                             SPA-H01GMN-NTOTALTEN (IDY)     -
      ****                             SPA-H01GMN-NTENSU    (IDY IDX)
                   MOVE    H01-NTENSU (IDY IDX)
                                       TO  SPA-H01GMN-NTENSU (IDY IDX)
               END-PERFORM
           END-PERFORM    
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
      *****    IF      IDX                 >   3
                   MOVE    H01-NTOTALTEN (IDX)
                                           TO  SPA-H01GMN-NTOTALTEN(IDX)
      *****    END-IF
               MOVE    H01-NFTNMONEY (IDX) TO  SPA-H01GMN-NFTNMONEY(IDX)
               MOVE    H01-NYKZFTN   (IDX) TO  SPA-H01GMN-NYKZFTN  (IDX)
               MOVE    H01-NJNISSU   (IDX) TO  SPA-H01GMN-NJNISSU  (IDX)
               MOVE    H01-NSHORYOYOHI
                                     (IDX) TO  SPA-H01GMN-NSHORYOYOHI
                                                                   (IDX)
               MOVE    H01-NSHOFTN   (IDX) TO  SPA-H01GMN-NSHOFTN  (IDX)
               MOVE    H01-NSHONISSU (IDX) TO  SPA-H01GMN-NSHONISSU(IDX)
           END-PERFORM
      *
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC       SECTION.
      *
           PERFORM 4104-SKYYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4105-HENREIYM-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4106-SKYKBN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           IF      SPA-RECE10-TEISYUTUSAKI
                                       =   "3" OR  "4"
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4107-SKYYM-KOHSKY-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4118-HENREIYM-KOHSKY-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM 4108-KHNTEN-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4102-KIHON-CHK-EXT
           END-IF
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       38
               PERFORM 4109-KAISU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM        
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >   3
               MOVE    ZERO            TO  SPA-H01GMN-NTOTALTEN (IDY)
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   IDX     >       38
                   PERFORM 4110-TENSU-CHK-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO  TO  4102-KIHON-CHK-EXT
                   END-IF
               END-PERFORM
           END-PERFORM        
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       5
               PERFORM 4111-FTNMONEY-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               PERFORM 4112-YKZFTN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               PERFORM 4113-JNISSU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               PERFORM 4114-SHORYOYOHI-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               PERFORM 4115-SHOFTN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
               PERFORM 4116-SHONISSU-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
           PERFORM VARYING IDX     FROM    4   BY  1
                   UNTIL   IDX     >       5
               PERFORM 4117-TOTALTEN-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO  TO  4102-KIHON-CHK-EXT
               END-IF
           END-PERFORM
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号チェック処理
      *****************************************************************
       4101-PTNUM-CHK-SEC            SECTION.
      *
           MOVE    2                   TO  SPA-H01GMN-CUR
      *
           PERFORM 420-CLEAR-SEC
      *
           MOVE    H01-PTNUM           TO  SPA-H01GMN-PTNUM
      *
           INITIALIZE                  ORCSPTNUMAREA
           MOVE    SPA-H01GMN-PTNUM    TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    00
                   MOVE    SPTNUM-PTNUM        TO  SPTID-PTNUM
                   MOVE    SPTNUM-PTID         TO  SPTID-PTID
               WHEN   10
                   MOVE    "0004"              TO  SPA-ERRCD
               WHEN    20
                   MOVE    SPACE               TO  LINKAREA
                   MOVE    SPA-KYOUTU          TO  LNK-COMMON
                   MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
                   MOVE    "H01"               TO  SPA-MOTOPG
                   MOVE    "P97"               TO  SPA-SAKIPG
      *            氏名セット
                   MOVE    SPA-H01GMN-PTNUM    TO  SPA-NAME
                   MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
                   MOVE    "CHANGE"            TO  MCP-PUTTYPE
                   MOVE    "P97"               TO  MCP-WINDOW
                   PERFORM 900-PUT-WINDOW
                   MOVE    1                   TO  FLG-END
                   GO      TO     4101-PTNUM-CHK-EXT
               WHEN    OTHER
                   MOVE    "0004"          TO  SPA-ERRCD
           END-EVALUATE
      *
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  SPA-H01GMN-TABLE
                                           SPA-H01GMN-KOUMOKU
                                           SPA-H01NAI-KOUMOKU1
               MOVE    SPTID-PTNUM     TO  SPA-H01GMN-PTNUM
               MOVE    SPTID-PTID      TO  SPA-H01NAI-PTID
      *        患者マスター存在チェック
               MOVE    SPA-HOSPNUM     TO  PTINF-HOSPNUM
               MOVE    SPA-H01NAI-PTID TO  PTINF-PTID
               PERFORM 900-PTINF-READ-SEC
               IF      FLG-PTINF   NOT =   ZERO
                   MOVE    "0001"      TO  SPA-ERRCD
               ELSE
      *
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       GO      TO     4101-PTNUM-CHK-EXT
                   ELSE
                       MOVE    PTINF-NAME  TO  SPA-H01GMN-NAME
                       MOVE    SPA-H01NAI-PTID
                                           TO  SPA-LAST-PTID
                       MOVE    SPA-H01GMN-PTNUM
                                           TO  SPA-LAST-PTNUM
                   END-IF
               END-IF
           END-IF
      *
      *    請求管理ＤＢ検索処理
      *
           IF      SPA-ERRCD               =   SPACE
               IF      SPA-SYS-1001-HOSPSBT1   =   ZERO
                   MOVE    SPACE               TO  SPA-H01GMN-SRYKA 
                   PERFORM 41021-SEIKYU-KENSAKU-SEC
               ELSE
                   MOVE    3               TO  SPA-H01GMN-CUR
               END-IF
           ELSE
               INITIALIZE                  SPA-H01GMN-TABLE    
                                           SPA-H01GMN-KOUMOKU
               INITIALIZE                  SPA-H01GMN-NAME
                                           SPA-H01NAI-KOUMOKU
                                           SPA-H01NAI-KOUMOKU1 
           END-IF
           .
       4101-PTNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療年月　入力処理
      *****************************************************************
       4102-SRYYM-CHK-SEC          SECTION.
      *
           MOVE    1               TO  SPA-H01GMN-CUR
      *
           MOVE    SPACE           TO  SPA-UPDFLG
                                       SPA-HENFLG
      *
           MOVE    H01-SRYYM       TO  SPA-H01GMN-SRYYM
           MOVE    ZERO            TO  SPA-H01NAI-SRYYM
      *
      *    日付画面チェックサブ
           INITIALIZE                      ORCSGDAYAREA
           MOVE    "1"                 TO  SGDAY-INTYPE
           MOVE    SPA-H01GMN-SRYYM    TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY(1:6)    TO  SPA-H01GMN-SRYYM
               MOVE    SGDAY-SDAY (1:6)    TO  SPA-H01NAI-SRYYM
           ELSE
               INITIALIZE                  SPA-H01GMN-TABLE
                                           SPA-H01GMN-KOUMOKU
               MOVE    "0002"          TO  SPA-ERRCD
               GO  TO  4102-SRYYM-CHK-EXT
           END-IF
      *
           IF      SPA-ERRCD               =   SPACE
               MOVE    2                       TO  SPA-H01GMN-CUR
           END-IF
           .
       4102-SRYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理ＤＢ検索処理
      *****************************************************************
       41021-SEIKYU-KENSAKU-SEC        SECTION.
      *
           INITIALIZE                  SPA-H01GMN-TABLE
                                       SPA-H01GMN-KOUMOKU
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-H01NAI-PTID     TO  RECE10-PTID
           IF      SPA-H01GMN-SRYKA    =   SPACE
               MOVE    "%%"                TO  RECE10-RECEKA
           ELSE
               MOVE    SPA-H01GMN-SRYKA    TO  RECE10-RECEKA
           END-IF
           MOVE    SPA-H01NAI-SRYYM       TO  RECE10-SRYYM
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    ZERO                TO  IDX 
           PERFORM     UNTIL       FLG-SEIKYU     =   1
                       OR          IDX            =   2
               ADD     1                   TO  IDX
               IF      IDX                 =   1 
                   MOVE    MCPDATA-REC     TO  RECE10-REC
               END-IF
      *
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *   
      *    カーソルクロース
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           EVALUATE    IDX
               WHEN    ZERO
                   MOVE    "0003"         TO  SPA-ERRCD
               WHEN    1
                   PERFORM 41021-SEIKYU-HENSYU-SEC
      *            点検用レセ０点公費のとき入力不可
                   IF      SPA-H01NAI-SKYKBN-RECEDEN   =   "1"
                       MOVE    "0009"                  TO  SPA-ERRCD
                       GO  TO  41021-SEIKYU-KENSAKU-EXT
                   END-IF
      *            主科設定時の主科未設定のチェック（特別療養費を除く）
                   IF    ( RECE10-RECEKA          =   "00"   )
                   AND   ( RECE10-RECESYUBETU NOT =   "6666" )
                       EVALUATE    RECE10-TEISYUTUSAKI
                           WHEN    "1"
                               IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"                           
                                   MOVE    "1006"         TO  SPA-HIDCD
                              END-IF
                           WHEN    "2"
                               IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                                   MOVE    "1006"         TO  SPA-HIDCD
                              END-IF
                           WHEN    "6"
                               IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                                   MOVE    "1006"         TO  SPA-HIDCD
                              END-IF
                       END-EVALUATE
                   END-IF
               WHEN    OTHER
                   PERFORM 41021-H99-HENSYU-SEC
           END-EVALUATE
           .
       41021-SEIKYU-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスタ画面編集処理
      *****************************************************************
       41021-SEIKYU-HENSYU-SEC         SECTION.
      *
           MOVE    RECE10-KEY          TO  SPA-RECE10-KEY
           IF      RECE10-TEISYUTUSAKI =   "3"
               MOVE    RECE10-KOFUNUM      TO  SPA-RECE10-KIGO
           ELSE
               MOVE    RECE10-KIGO         TO  SPA-RECE10-KIGO
           END-IF
           MOVE    RECE10-NUM          TO  SPA-RECE10-NUM
           MOVE    RECE10-SKYYM        TO  SPA-RECE10-SKYYM
           MOVE    RECE10-HKNNUM       TO  SPA-RECE10-HKNNUM
      *
      *    公費単独で生保又は中国残留邦人等のとき負担者番号、
      *    受給者番号をセット
           IF    ( RECE10-HKNNUM       =   SPACE               )
               IF    ( RECE10-KOHNUM (1)   =   "012"  OR "025"
                                                      OR "020"
                                                      OR "053"
                                                      OR "021"
                                                      OR "015"
                                                      OR "016" )
               AND   ( RECE10-KOHNUM (2)   =   SPACE           )
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                   MOVE    RECE10-PTID     TO  PTKOH-PTID
                   MOVE    RECE10-KOHID(1) TO  PTKOH-KOHID
                   MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                   PERFORM 900-PTKOHINF-READ-SEC
                   MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
                   MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                   MOVE    RECE10-KOHID(1) TO  MONTHLYNUM-KOHID
                   MOVE    RECE10-SRYYM    TO  MONTHLYNUM-SRYYM
                   MOVE    RECE10-NYUGAIKBN
                                           TO  MONTHLYNUM-NYUGAIKBN
                   MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                   PERFORM 900-MONTHLYNUM-READ-SEC
                   IF      FLG-MONTHLYNUM  =   ZERO
                       MOVE    MONTHLYNUM-JKYSNUM
                                               TO  PTKOH-JKYSNUM
                   ELSE
                       MOVE    SPA-HOSPNUM     TO  MONTHLYNUM-HOSPNUM
                       MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                       MOVE    ZERO            TO  MONTHLYNUM-KOHID
                       MOVE    RECE10-SRYYM    TO  MONTHLYNUM-SRYYM
                       MOVE    RECE10-NYUGAIKBN
                                               TO  MONTHLYNUM-NYUGAIKBN
                       MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                       PERFORM 900-MONTHLYNUM-READ-SEC
                       IF      FLG-MONTHLYNUM   =   ZERO
                           MOVE    MONTHLYNUM-JKYSNUM
                                                    TO  PTKOH-JKYSNUM
                       END-IF
                   END-IF
                   MOVE    PTKOH-FTNJANUM  TO  SPA-RECE10-FTNJANUM
                   MOVE    PTKOH-JKYSNUM   TO  SPA-RECE10-JKYSNUM
               END-IF
           END-IF
      *
      *    見出し編集
           PERFORM 3101-MIDASI-HENSYU-SEC
      *
      *    主科対応のとき主科を表示する
           IF    ( SPA-SYUKA-KBN      =   "1"  )
           AND   ( RECE10-RECEKA  NOT =   "00" )
               MOVE    RECE10-RECEKA      TO  SPA-H01GMN-SRYKA
           END-IF
      *
           IF      RECE10-NYUGAIKBN   =   "1"
               MOVE    "入  院"       TO  SPA-H01GMN-HNYUGAIKBN
           ELSE    
               MOVE    "入院外"       TO  SPA-H01GMN-HNYUGAIKBN
           END-IF
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "1"
                   MOVE    "支払基金"      TO  SPA-H01GMN-HTEISYUTUSAKI
               WHEN    "2"
                   MOVE    "国保連合会"    TO  SPA-H01GMN-HTEISYUTUSAKI
               WHEN    "3"
                   MOVE    "労災"          TO  SPA-H01GMN-HTEISYUTUSAKI
               WHEN    "4"
                   MOVE    "自賠責"        TO  SPA-H01GMN-HTEISYUTUSAKI
               WHEN    "5"
                   MOVE    "公害"          TO  SPA-H01GMN-HTEISYUTUSAKI
               WHEN    "6"
                   MOVE    "広域連合"      TO  SPA-H01GMN-HTEISYUTUSAKI
           END-EVALUATE
      *
           PERFORM 530-RECESYUBETU-HENSYU-SEC
           MOVE    WRK-RECESYUBETU     TO  SPA-H01GMN-HRECESYUBETU
      *
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "1"
               WHEN    "2"
               WHEN    "6"
                   MOVE    "保険者番号"    TO  SPA-H01GMN-TITLE-HKNJANUM
                   IF      RECE10-HKNJANUM     NOT =   "00000000"
                       MOVE    RECE10-HKNJANUM TO  SPA-H01GMN-HHKNJANUM
                   END-IF
               WHEN    "3"
                   MOVE    RECE10-KOFUNUM  TO  SPA-H01GMN-HHKNJANUM
                   EVALUATE    RECE10-RECESYUBETU
                       WHEN    "7101"
                       WHEN    "7091"
                       WHEN    "7102"
                       WHEN    "7092"
                           MOVE    "年金証書番号"      TO
                                               SPA-H01GMN-TITLE-HKNJANUM
                       WHEN    "7071"
                       WHEN    "7082"
                       WHEN    "7072"
                       WHEN    "7081"
                           MOVE    "労働保険番号"      TO
                                               SPA-H01GMN-TITLE-HKNJANUM
                       WHEN    "7021"
                           MOVE    "健康管理手帳番号"  TO
                                               SPA-H01GMN-TITLE-HKNJANUM
                   END-EVALUATE
           END-EVALUATE
      *
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "6"
                   MOVE    RECE10-NUM          TO  SPA-H01GMN-HKIGO
                   MOVE    "記号・番号"        TO
                                               SPA-H01GMN-TITLE-KIGO
               WHEN    "3"
               WHEN    "4"
                   MOVE    "傷病の部位"        TO
                                               SPA-H01GMN-TITLE-KIGO
                   MOVE    RECE10-COMMENT      TO  SPA-H01GMN-HKIGO
               WHEN    OTHER
                   MOVE    "記号・番号"        TO
                                               SPA-H01GMN-TITLE-KIGO
                   IF    ( RECE10-HKNNUM       =   SPACE               )
                       IF    ( RECE10-KOHNUM (1)   =   "012"  OR "025"
                                                              OR "020"
                                                              OR "053"
                                                              OR "021"
                                                              OR "015"
                                                              OR "016" )
                       AND   ( RECE10-KOHNUM (2)   =   SPACE           )
                           STRING  SPA-RECE10-FTNJANUM
                                                   DELIMITED   BY  SPACE
                                   "　"            DELIMITED   BY  SIZE
                                   SPA-RECE10-JKYSNUM
                                                   DELIMITED   BY  SPACE
                                               INTO    SPA-H01GMN-HKIGO
                           END-STRING
                       END-IF
                   ELSE    
                       STRING  RECE10-KIGO     DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               RECE10-NUM      DELIMITED   BY  SPACE
                                               INTO    SPA-H01GMN-HKIGO
                       END-STRING
                   END-IF
           END-EVALUATE        
      *
           EVALUATE    RECE10-HKNNUM
               WHEN    "002"
               WHEN    "031"  THRU  "034"
                   EVALUATE    RECE10-HOJOKBN-KEY
                       WHEN    "1"
                       WHEN    "A"
                       WHEN    "D"
                       WHEN    "G"
                           MOVE    "職務上"    TO  SPA-H01GMN-HHOJOKBN
                       WHEN    "2"
                       WHEN    "B"
                       WHEN    "E"
                       WHEN    "H"
                           MOVE    "下船３月"  TO  SPA-H01GMN-HHOJOKBN
                       WHEN    "3"
                       WHEN    "C"
                       WHEN    "F"
                       WHEN    "I"
                           MOVE    "通勤災害"  TO  SPA-H01GMN-HHOJOKBN
                   END-EVALUATE
           END-EVALUATE
      *
           MOVE    RECE10-CREYMD       TO  WRK-SYMD
           PERFORM 5002-HIZUKE-HEN-SEC
           STRING  WRK-HENYMDG         DELIMITED   BY  SIZE
                   " "                 DELIMITED   BY  SIZE
                   RECE10-CREHMS (1:2) DELIMITED   BY  SIZE
                   ":"                 DELIMITED   BY  SIZE
                   RECE10-CREHMS (3:2) DELIMITED   BY  SIZE
                   "作成"              DELIMITED   BY  SIZE
                                       INTO    SPA-H01GMN-HCREYMD
           END-STRING
      *
           EVALUATE    RECE10-RECEIPT-KBN (1:1)
               WHEN    "1"
                   MOVE    "老併"          TO  SPA-H01GMN-HRECEIPT-KBN
               WHEN    "2"
                   MOVE    "老健"          TO  SPA-H01GMN-HRECEIPT-KBN
               WHEN    "3"
                   MOVE    "医併"          TO  SPA-H01GMN-HRECEIPT-KBN
               WHEN    "4"
                   MOVE    "医療"          TO  SPA-H01GMN-HRECEIPT-KBN
           END-EVALUATE
      *
           MOVE    RECE10-SKYYM        TO  SPA-H01NAI-SKYYM
                                           WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-H01GMN-SKYYM
      *
           MOVE    RECE10-HENREIYM     TO  SPA-H01NAI-HENREIYM
                                           WRK-SYMD (1:6)
           MOVE    "01"                TO  WRK-SYMD (7:2) 
           PERFORM 5002-HIZUKE-HEN-SEC
           MOVE    WRK-HENYMDG(1:6)    TO  SPA-H01GMN-HENREIYM
      *
           MOVE    RECE10-SKYKBN       TO  SPA-H01GMN-SKYKBN
      * 
           PERFORM VARYING IDY FROM   1   BY  1
                   UNTIL   IDY >      3
               IF      SPA-H01GMN-SKYKBN
                                      =   SPA-H01GMN-SKYKBNL (IDY)
                   MOVE    SPA-H01GMN-SKYKBNMEIL (IDY)
                                          TO  SPA-H01GMN-SKYKBNMEI
                   MOVE    3              TO  IDY
               END-IF
           END-PERFORM
      *
           EVALUATE    RECE10-TEISEIKBN
               WHEN    "0"
                   MOVE    SPACE       TO  SPA-H01GMN-TEISEIKBNNM
               WHEN    "1"
                   MOVE    "手書き修正"
                                       TO  SPA-H01GMN-TEISEIKBNNM
               WHEN    "2"
                   MOVE    "レセプト再作成"
                                       TO  SPA-H01GMN-TEISEIKBNNM
           END-EVALUATE   
      *
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "1"
               WHEN    "2"
               WHEN    "5"
               WHEN    "6"
                   EVALUATE    RECE10-NYUGAIKBN
                       WHEN    "1"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       32
                               IF    ( RECE10-TEISYUTUSAKI =   "5" )
                               AND   ( IDX                 >   31  )
                                   CONTINUE
                               ELSE 
                                   MOVE    RECE10-KHNTEN (IDX)
                                           TO  SPA-H01GMN-KHNTEN (IDX)
                                               SPA-H01GMN-NKHNTEN(IDX)
                                   MOVE    RECE10-KAISU  (IDX)
                                           TO  SPA-H01GMN-KAISU  (IDX)
                                               SPA-H01GMN-NKAISU (IDX)
                                   PERFORM VARYING IDY FROM    1   BY  1
                                           UNTIL   IDY >       3
                                       MOVE    RECE10-TENSU  (IDX IDY)
                                           TO
                                           SPA-H01GMN-TENSU  (IDY IDX)
                                           SPA-H01GMN-NTENSU (IDY IDX)
                                  END-PERFORM
                               END-IF 
                           END-PERFORM       
                       WHEN    "2"
                           PERFORM VARYING IDX FROM    1   BY  1
                                   UNTIL   IDX >       38
                               IF    ( RECE10-TEISYUTUSAKI =
                                                           "1" OR  "2"
                                                               OR  "6" )
                               AND   ( IDX                 >   35      )
                                   CONTINUE
                               ELSE 
                                   MOVE    RECE10-KHNTEN (IDX)
                                           TO  SPA-H01GMN-KHNTEN (IDX)
                                               SPA-H01GMN-NKHNTEN(IDX)
                                   MOVE    RECE10-KAISU  (IDX)
                                           TO  SPA-H01GMN-KAISU  (IDX)
                                               SPA-H01GMN-NKAISU (IDX)
                                   PERFORM VARYING IDY FROM    1   BY  1
                                           UNTIL   IDY >       3
                                       MOVE    RECE10-TENSU  (IDX IDY)
                                           TO
                                           SPA-H01GMN-TENSU  (IDY IDX)
                                           SPA-H01GMN-NTENSU (IDY IDX)
                                   END-PERFORM
                               END-IF        
                           END-PERFORM
                   END-EVALUATE 
      *    
                   PERFORM VARYING IDY FROM    1   BY  1
                           UNTIL   IDY >       5
                       MOVE    RECE10-TOTALTEN (IDY)
                                           TO  SPA-H01GMN-TOTALTEN (IDY)
                                               SPA-H01GMN-NTOTALTEN(IDY)
                       MOVE    RECE10-JNISSU   (IDY)
                                           TO  SPA-H01GMN-JNISSU   (IDY)
                                               SPA-H01GMN-NJNISSU  (IDY)
                       IF      RECE10-TEISYUTUSAKI =   "5"
      *                    小計
                           MOVE    RECE10-FTNMONEY (IDY)
                                           TO  SPA-H01GMN-YKZFTN   (IDY)
                                               SPA-H01GMN-NYKZFTN  (IDY)
      *                    小計（食事）
                           MOVE    RECE10-YKZFTN   (IDY)
                                           TO  SPA-H01GMN-FTNMONEY (IDY)
                                               SPA-H01GMN-NFTNMONEY(IDY)
                       ELSE
                           MOVE    RECE10-FTNMONEY (IDY)
                                           TO  SPA-H01GMN-FTNMONEY (IDY)
                                               SPA-H01GMN-NFTNMONEY(IDY)
                           MOVE    RECE10-YKZFTN   (IDY)
                                           TO  SPA-H01GMN-YKZFTN   (IDY)
                                               SPA-H01GMN-NYKZFTN  (IDY)
                       END-IF
                   END-PERFORM
      *
                   IF     RECE10-NYUGAIKBN     =   "1"
                       IF      RECE10-TEISYUTUSAKI =   "5"
                           CONTINUE
                       ELSE
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       5
                               MOVE    RECE10-SHOKUJIRYOYOHI (IDY)
                                       TO  SPA-H01GMN-SHORYOYOHI (IDY)
                                           SPA-H01GMN-NSHORYOYOHI(IDY)
                               MOVE    RECE10-SHOKUJIFTN   (IDY)
                                       TO  SPA-H01GMN-SHOFTN     (IDY)
                                           SPA-H01GMN-NSHOFTN    (IDY)
                               MOVE    RECE10-SHOKUJINISSU   (IDY)
                                       TO  SPA-H01GMN-SHONISSU   (IDY)
                                           SPA-H01GMN-NSHONISSU  (IDY)
                           END-PERFORM
                       END-IF
                   END-IF
               WHEN    "3"
               WHEN    "4"
                   PERFORM VARYING IDX FROM    1   BY  1
                           UNTIL   IDX >       20
                       MOVE    RECE10-TENSU  (IDX 1)
                                       TO  SPA-H01GMN-TENSU  (1 IDX)
                                           SPA-H01GMN-NTENSU (1 IDX)
                   END-PERFORM       
      *    
                   MOVE    RECE10-TOTALTEN     (1)
                                       TO  SPA-H01GMN-TOTALTEN     (1)
                                           SPA-H01GMN-NTOTALTEN    (1)
                   MOVE    RECE10-FTNMONEY-FTN (1)
                                       TO  SPA-H01GMN-FTNMONEY     (1)
                                           SPA-H01GMN-NFTNMONEY    (1)
                   MOVE    RECE10-FTNMONEY-KOJ (1)
                                       TO  SPA-H01GMN-YKZFTN       (1)
                                           SPA-H01GMN-NYKZFTN      (1)
                   MOVE    RECE10-SHOKUJIRYOYOHI   (1)
                                       TO  SPA-H01GMN-JNISSU       (1)
                                           SPA-H01GMN-NJNISSU      (1)
                   MOVE    RECE10-FTNMONEY     (1)
                                       TO  SPA-H01GMN-SHORYOYOHI   (1)
                                           SPA-H01GMN-NSHORYOYOHI  (1)
                    MOVE    RECE10-JNISSU      (1)
                                       TO  SPA-H01GMN-SHOFTN       (1)
                                           SPA-H01GMN-NSHOFTN      (1)
           END-EVALUATE 
      *
           MOVE    RECE10-AGE      TO  SPA-H01GMN-AGE
      *
           MOVE    SPACE           TO  WRK-ZAITAKU
           MOVE    SPACE           TO  WRK-TITLE
           EVALUATE    RECE10-TEISYUTUSAKI
               WHEN    "5"
                   CONTINUE
               WHEN    "3"
                   IF      RECE10-SYOKAIFLG    =   "1"
                       INITIALIZE                  LABOR-SIO-REC
                       MOVE    RECE10-SYOCD    TO  LABOR-SIO-SYOCD
                       MOVE    RECE10-SRYYM    TO  LABOR-SIO-YUKOSTYMD
                                                           (1:6)
                       MOVE    "01"            TO  LABOR-SIO-YUKOSTYMD
                                                           (7:2)
                       MOVE    LABOR-SIO-YUKOSTYMD
                                               TO  LABOR-SIO-YUKOEDYMD
                       MOVE    LABOR-SIO-REC   TO  MCPDATA-REC
                       PERFORM 900-LABOR-SIO-READ-SEC
                       MOVE    RECE10-SYOCD    TO  SPA-H01GMN-ZAITAKU
                                                           (1:5) 
                       MOVE    LABOR-SIO-NAME (1:14)
                                               TO  SPA-H01GMN-ZAITAKU
                                                           (7:14) 
                   END-IF
               WHEN    OTHER
                   MOVE    RECE10-KYURATE  TO  SPA-H01GMN-KYURATE
      *
                   IF      RECE10-ZAITAKU  =   "1"
                       MOVE    "在医総等"      TO  WRK-ZAITAKU
                   END-IF
      *
                   EVALUATE    RECE10-ETC-YKZFTN (1)
                       WHEN    1
                       WHEN    8
                       WHEN    10
                       WHEN    11
                           MOVE    "不詳　"        TO  WRK-TITLE
                       WHEN    2
                       WHEN    7
                           MOVE    "災１　"        TO  WRK-TITLE
                       WHEN    3
                       WHEN    6
                       WHEN    9
                           MOVE    "猶予なし　"    TO  WRK-TITLE
                       WHEN    4
                           MOVE    "災２　"        TO  WRK-TITLE
                       WHEN    5
                           MOVE    "不詳災１　"    TO  WRK-TITLE
                   END-EVALUATE
                   STRING  WRK-TITLE       DELIMITED   BY  SPACE
                           WRK-ZAITAKU     DELIMITED   BY  SPACE
                                           INTO    SPA-H01GMN-ZAITAKU
                   END-STRING
                   MOVE    RECE10-ETC-YKZFTN (1)
                                       TO  SPA-RECE10-DISASTER
           END-EVALUATE
      *
           IF    ( RECE10-RECESYUBETU  =   "6666" )
           OR    ( RECE10-TEISYUTUSAKI =   "5"    )
               CONTINUE
           ELSE
             IF     RECE10-TEISYUTUSAKI =   "3" OR  "4"
               IF      RECE10-SYOKAIFLG     =   "1"
                   MOVE    "初回"          TO  SPA-H01GMN-HONKZKKBN
               END-IF
             ELSE
               EVALUATE    RECE10-RECESYUBETU (4:1)
                   WHEN    "1"
                   WHEN    "2"
                       MOVE    "本人"          TO  SPA-H01GMN-HONKZKKBN
                   WHEN    "3"
                   WHEN    "4"
                       IF      SPA-RECE10-SRYYM    >=  "200804"
                           MOVE    "未就"      TO  SPA-H01GMN-HONKZKKBN
                       ELSE
                           MOVE    "三歳"      TO  SPA-H01GMN-HONKZKKBN
                       END-IF
                   WHEN    "5"
                   WHEN    "6"
                       MOVE    "家族"          TO  SPA-H01GMN-HONKZKKBN
                   WHEN    "7"
                   WHEN    "8"
                       IF      SPA-RECE10-SRYYM    >=  "200804"
                           MOVE    "高一"      TO  SPA-H01GMN-HONKZKKBN
                       ELSE 
                           MOVE    "高９"      TO  SPA-H01GMN-HONKZKKBN
                       END-IF
                   WHEN    "9"
                   WHEN    "0"
                       IF      RECE10-SRYYM    <   WRK-CONS-SRYYM-200610
                           MOVE    "高８"      TO  SPA-H01GMN-HONKZKKBN
                       ELSE
                           EVALUATE    RECE10-KYURATE
                               WHEN    8
                                   MOVE    "高８"
                                               TO  SPA-H01GMN-HONKZKKBN
                               WHEN    7
                                   MOVE    "高７"
                                               TO  SPA-H01GMN-HONKZKKBN
                           END-EVALUATE
                       END-IF
               END-EVALUATE
             END-IF
           END-IF
      *
           IF      RECE10-HENREI-SIJI-FLG
                                       =   1
               MOVE    "T"             TO  SPA-H01GMN-HENREI-SIJI-FLG
           ELSE
               MOVE    "F"             TO  SPA-H01GMN-HENREI-SIJI-FLG
           END-IF
      *             
      *    公費請求書データ存在チェック
           MOVE    ZERO                TO  SPA-H01NAI-KOHSKY-FLG
      *
           IF    ( RECE10-RECESYUBETU  =   "6666"              )
           OR    ( RECE10-TEISYUTUSAKI =   "5" OR  "3" OR  "4" )
               CONTINUE
           ELSE
               MOVE    SPACE           TO  KOHSKY-REC   
               INITIALIZE                  KOHSKY-REC
               MOVE    SPA-RECE10-KEY  TO  KOHSKY-KEY
               PERFORM 900-KOHSKY-INV-SEC
               IF      FLG-KOHSKY      =   ZERO
                   MOVE    MCPDATA-REC     TO  KOHSKY-REC
                   MOVE    KOHSKY-SKYYM    TO  SPA-H01NAI-SKYYM-KOHSKY
                                               WRK-SYMD (1:6)
                   MOVE    "01"            TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)
                                           TO  SPA-H01GMN-SKYYM-KOHSKY
                   MOVE    KOHSKY-HENREIYM TO
                                           SPA-H01NAI-HENREIYM-KOHSKY
                                           WRK-SYMD (1:6)
                   MOVE    "01"            TO  WRK-SYMD (7:2) 
                   PERFORM 5002-HIZUKE-HEN-SEC
                   MOVE    WRK-HENYMDG(1:6)
                                           TO
                                           SPA-H01GMN-HENREIYM-KOHSKY
                   MOVE    1               TO  SPA-H01NAI-KOHSKY-FLG
               END-IF
           END-IF
      *
      *    返戻データ存在チェック
           MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
           MOVE    ZERO                TO  SPA-H01NAI-HENREI-MSG
      *    特別療養費、公害、自賠責はチェックしない
           IF    ( RECE10-RECESYUBETU  =   "6666"      )
           OR    ( RECE10-TEISYUTUSAKI =   "5" OR  "4" )
               CONTINUE
           ELSE
      *        労災でアフターケア、公務災害はチェックしない
               IF      RECE10-TEISYUTUSAKI =   "3"
                   IF    ( RECE10-HKNKBN       =   "3" OR  "4" )
                   OR    ( RECE10-SRYYM        <   201307      )
                       CONTINUE
                   ELSE
                       PERFORM 410211-HENREI-B-CHECK-SEC
                   END-IF
               ELSE
                   PERFORM 410211-HENREI-B-CHECK-SEC
               END-IF
           END-IF
      *
      *    点検レセ０点公費あり
           MOVE    RECE10-SKYKBN-RECEDEN
                                       TO  SPA-H01NAI-SKYKBN-RECEDEN
      *
      *    自賠責ＰＤＦファイル存在チェック
           MOVE    ZERO                TO  SPA-H01NAI-JIBAI-PDF-MSG
      *
           IF    ( RECE10-TEISYUTUSAKI =   "4" )
           AND   ( SPA-NAI-200501-JIBAI-PDF
                                       =   "1" )
               MOVE    SPACE               TO  JIBAI-PRT-REC
               INITIALIZE                      JIBAI-PRT-REC
               MOVE    RECE10-KEY          TO  JIBAI-PRT-KEY1
               MOVE    JIBAI-PRT-REC       TO  MCPDATA-REC
               PERFORM 900-JIBAI-PRT-READ-SEC
      *
               IF      FLG-JIBAI-PRT       =   1
                   MOVE    1               TO  SPA-H01NAI-JIBAI-PDF-MSG
               END-IF
           END-IF
      *
      *    レセ電データ存在チェック
           MOVE    ZERO                TO  SPA-H01NAI-RECEDEN-MSG
      *    特別療養費、公害、自賠責はレセ電データに記録しない
           IF    ( RECE10-RECESYUBETU  =   "6666"      )
           OR    ( RECE10-TEISYUTUSAKI =   "5" OR  "4" )
             CONTINUE
           ELSE
      *      労災でアフターケア、公務災害はレセ電データに記録しない
             IF    ( RECE10-TEISYUTUSAKI =   "3"         )
             AND   ( RECE10-HKNKBN       =   "3" OR  "4" )
               CONTINUE
             ELSE
      *        未確定分をレセ電データに記録しないとき
               IF    (  SPA-NAI-200501-RECEDEN-DISASTER
                                           =   "0"              )
               AND   ( RECE10-ETC-YKZFTN (1)
                                           =   6   OR  7  OR  8 )
                   CONTINUE
               ELSE
                   MOVE    SPACE               TO  RECEDEN-REC
                   INITIALIZE                      RECEDEN-REC
                   MOVE    RECE10-KEY          TO  RECEDEN-KEY
                   MOVE    "1"                 TO  RECEDEN-RECKBN
                   MOVE    RECEDEN-REC         TO  MCPDATA-REC
                   PERFORM 900-RECEDEN-INV-SEC
                   IF      FLG-RECEDEN         =   1
                     EVALUATE    RECE10-TEISYUTUSAKI
                                               ALSO    RECE10-NYUGAIKBN
                       WHEN    "1"             ALSO    "1"
                           IF      SPA-NAI-2005-SYARECEKBN
                                                   =   "2" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "1"             ALSO    "2"
                           IF      SPA-NAI-2005-SYARECEKBN
                                                   =   "1" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "2"             ALSO    "1"
                           IF      SPA-NAI-2005-KKHRECEKBN
                                                   =   "2" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "2"             ALSO    "2"
                           IF      SPA-NAI-2005-KKHRECEKBN
                                                   =   "1" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "6"             ALSO    "1"
                           IF      SPA-NAI-200501-KOUIKIRECEKBN
                                                   =   "2" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "6"             ALSO    "2"
                           IF      SPA-NAI-200501-KOUIKIRECEKBN
                                                   =   "1" OR  "3"
                               MOVE    1   TO  SPA-H01NAI-RECEDEN-MSG
                           END-IF
                       WHEN    "3"             ALSO    "1"
                           IF      SPA-NAI-200501-ROURECEKBN
                                                   =   "2" OR  "3"
                               PERFORM 410212-ROU-RECEDEN-CHK-SEC
                           END-IF
                       WHEN    "3"             ALSO    "2"
                           IF      SPA-NAI-200501-ROURECEKBN
                                                   =   "1" OR  "3"
                               PERFORM 410212-ROU-RECEDEN-CHK-SEC
                           END-IF
                     END-EVALUATE
                   END-IF 
               END-IF
             END-IF 
           END-IF
      *
           MOVE    "1"                 TO  SPA-HENFLG
      *     
           MOVE    4                   TO  SPA-H01GMN-CUR
           .
       41021-SEIKYU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻データ存在チェック処理
      *****************************************************************
       410211-HENREI-B-CHECK-SEC   SECTION.
      *
           INITIALIZE                      HENREI-B-REC
           MOVE    SPA-HOSPNUM         TO  HENREI-B-HOSPNUM
           MOVE    RECE10-SRYYM        TO  HENREI-B-SRYYM
           MOVE    RECE10-PTID         TO  HENREI-B-PTID
           MOVE    RECE10-NYUGAIKBN    TO  HENREI-B-NYUGAIKBN
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           PERFORM 900-HENREI-BODY-INV-SEC
           IF      FLG-HENREI-B        =   ZERO
               MOVE    1               TO  SPA-H01NAI-HENREI-MSG
      *        返戻データ退避処理
               PERFORM 410211-HENREI-B-SPA-HENSYU-SEC
           ELSE
               MOVE    ZERO            TO  SPA-H01NAI-HENREI-MSG
           END-IF
      *
           IF    ( SPA-H01NAI-HENREIYM >   ZERO )
           AND   ( SPA-H01NAI-HENREI-MSG
                                       =   1    )
               MOVE    WIDGET-NORMAL       TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
            END-IF
           .
       410211-HENREI-B-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻データ退避処理
      *****************************************************************
       410211-HENREI-B-SPA-HENSYU-SEC SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       WRK-CONS-HENREI-B-MAX    
               INITIALIZE              TBL-HENREI-B-REC (IDZZ)
                                       TBL-HENREI-B-ERR (IDZZ)
           END-PERFORM
           MOVE    ZERO            TO  TBL-HENREI-B-MAX
           MOVE    ZERO            TO  FLG-HENREI-B-CHK
      *
           INITIALIZE                  HENREI-B-REC
           MOVE    SPA-HOSPNUM     TO  HENREI-B-HOSPNUM
           MOVE    SPA-RECE10-NYUGAIKBN
                                   TO  HENREI-B-NYUGAIKBN
           MOVE    SPA-RECE10-SRYYM
                                   TO  HENREI-B-SRYYM
           EVALUATE    SPA-RECE10-TEISYUTUSAKI
               WHEN    1
                   MOVE    "1"     TO  HENREI-B-TEISYUTUSAKI
                                       HENREI-B-XXTEISYUTUSAKI
               WHEN    2
               WHEN    6
                   MOVE   "2"      TO  HENREI-B-TEISYUTUSAKI
                   MOVE   "6"      TO  HENREI-B-XXTEISYUTUSAKI
               WHEN    3
                   MOVE    "3"     TO  HENREI-B-TEISYUTUSAKI
                                       HENREI-B-XXTEISYUTUSAKI
           END-EVALUATE
           MOVE    SPA-RECE10-PTID     TO  HENREI-B-PTID
           MOVE    HENREI-B-REC    TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"
                                   TO  MCP-TABLE
           MOVE    "key2"          TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_henrei_body"
                                       TO  MCP-TABLE
               MOVE    "key2"          TO  MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HENREI-B
           END-IF
      *           
           PERFORM         UNTIL   FLG-HENREI-B    =   1
                           OR      FLG-HENREI-B-CHK
                                                   >   ZERO
      *
               ADD     1               TO  TBL-HENREI-B-MAX
               MOVE    HENREI-B-REC    TO  TBL-HENREI-B-REC
                                                  (TBL-HENREI-B-MAX)
      *        返戻設定済のデータを取得
               IF      RECE10-HENREI-SIJI-FLG
                                           =   1
                 PERFORM VARYING IDZZ    FROM    1   BY  1
                         UNTIL   IDZZ    >       10
                         OR      HENREI-B-S-TEISYUTUSAKI(IDZZ)
                                         =       SPACE
                   IF    ( HENREI-B-S-HENREIYM     (IDZZ)
                                           =   SPA-RECE10-SKYYM        )
                   AND   ( HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                           =   SPA-RECE10-TEISYUTUSAKI )
                   AND   ( HENREI-B-S-NYUGAIKBN    (IDZZ)
                                           =   SPA-RECE10-NYUGAIKBN    )
                   AND   ( HENREI-B-S-RECEKA       (IDZZ)
                                           =   SPA-RECE10-RECEKA       )
                   AND   ( HENREI-B-S-RECESYUBETU  (IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                   AND   ( HENREI-B-S-HKNJANUM     (IDZZ)
                                           =   SPA-RECE10-HKNJANUM     )
                   AND   ( HENREI-B-S-HOJOKBN-KEY  (IDZZ)
                                           =   SPA-RECE10-HOJOKBN      )
                   AND   ( HENREI-B-S-KOHID-KEY    (IDZZ)
                                           =   SPA-RECE10-KOHID-KEY    )
                   AND   ( HENREI-B-S-TEKSTYMD     (IDZZ)
                                           =   SPA-RECE10-TEKSTYMD     )
                   AND   ( HENREI-B-S-RECEIPT-KBN (IDZZ)
                                           =   SPA-RECE10-RECEIPT-KBN  )
                       IF      SPA-HENREI-B-HOSPNUM    =   ZERO
                           MOVE    HENREI-B-REC    TO  SPA-HENREI-B-REC
                                                       WRK-HENREI-B-REC
                       END-IF
      ***              MOVE    1               TO  TBL-HENREI-B-ERR
      ***                                          (TBL-HENREI-B-MAX)
                       MOVE    10              TO  IDZZ
                   END-IF  
                 END-PERFORM
               END-IF
      *
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_henrei_body"       TO  MCP-TABLE
           MOVE    "key2"                  TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
           .
       410211-HENREI-B-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電データ存在チェック
      *****************************************************************
       410212-ROU-RECEDEN-CHK-SEC  SECTION.
      *
           IF    ( RECE10-SRYYM    >=  "201307" )
           AND   ( SPA-NAI-4001-RSIIRYOKBN-FLG
                                   =   1        )
      *        労災指定医療機関でないときは対象外
               MOVE    RECE10-SRYYM    TO  WRK-SYS-SRYYMD (1:6)
               MOVE    "01"            TO  WRK-SYS-SRYYMD (7:2)
      *
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >   SPA-NAI-4001-MAX
                   IF    ( SPA-NAI-4001-STYUKYMD (IDZZ)
                                           <=  WRK-SYS-SRYYMD )
                   AND   ( SPA-NAI-4001-EDYUKYMD (IDZZ)
                                           >=  WRK-SYS-SRYYMD )
                       IF      SPA-NAI-4001-RSIIRYOKBN  (IDZZ)
                                               =   "0"
                           CONTINUE
                       ELSE
                           MOVE    1               TO
                                               SPA-H01NAI-RECEDEN-MSG
                       END-IF
                       MOVE     SPA-NAI-4001-MAX   TO  IDZZ
                   END-IF
               END-PERFORM
           END-IF
           .
       410212-ROU-RECEDEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト検索画面編集処理
      *****************************************************************
       41021-H99-HENSYU-SEC        SECTION.
      *
           INITIALIZE                      SPA-H99GMN-AREA
           MOVE    SPACE               TO  SPA-H99-AREA
      *
           MOVE    SPA-H01GMN-PTNUM    TO  SPA-H99GMN-PTNUM
           MOVE    SPA-H01GMN-NAME     TO  SPA-H99GMN-NAME
           MOVE    SPA-H01GMN-SRYYM    TO  SPA-H99GMN-SRYYM
           MOVE    SPA-H01GMN-SRYKA    TO  SPA-H99GMN-SRYKA
           MOVE    SPA-H01GMN-SRYKANAME
                                       TO  SPA-H99GMN-SRYKANAME
           MOVE    SPA-H01NAI-PTID     TO  SPA-H99NAI-PTID
           MOVE    SPA-H01NAI-SRYYM    TO  SPA-H99NAI-SRYYM
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-H01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGH99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-H01FREE
           MOVE    SPA-WORK            TO  SPA-H01KYOUTU
      *
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    "SELNUM"             TO  MCP-WIDGET
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "H99"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H99"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
           .
       41021-H99-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       4103-SRYKA-CHK-SEC             SECTION.
      *
           MOVE    3                   TO  SPA-H01GMN-CUR
      *
           MOVE    SPACE           TO  SPA-UPDFLG
                                       SPA-HENFLG
      *
           MOVE    H01-SRYKA-VALUE TO  SPA-H01GMN-SRYKA
      *
           IF      SPA-H01GMN-SRYKA    =   SPACE
               CONTINUE
           ELSE
               INITIALIZE                  SYS-1005-REC
               MOVE    "1005"              TO  SYS-1005-KANRICD
               MOVE    SPA-H01GMN-SRYKA    TO  SYS-1005-KBNCD
               MOVE    SPA-SYSYMD          TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
grpsys         MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
               MOVE    SYS-1005-REC        TO  MCPDATA-REC
               PERFORM 910-SYSKANRI-INV-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-1005-REC
                   MOVE    SYS-1005-SRYKANAME  TO  SPA-H01GMN-SRYKANAME
               ELSE
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    SPACE               TO  SPA-H01GMN-SRYKANAME
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE 
      *    請求管理ＤＢ検索処理
               PERFORM 41021-SEIKYU-KENSAKU-SEC
           END-IF
           .
       4103-SRYKA-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月　入力処理
      *****************************************************************
       4104-SKYYM-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-H01NAI-SKYYM
      *
           IF      SPA-H01GMN-SKYYM    =   SPACE
              CONTINUE
           ELSE 
      *    日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-H01GMN-SKYYM    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)
                                           TO  SPA-H01GMN-SKYYM
                   MOVE    SGDAY-SDAY (1:6)
                                           TO  SPA-H01NAI-SKYYM
      ****         IF      SPA-H01NAI-SKYYM
      ****                                 <   SPA-H01NAI-SRYYM
      ****             MOVE    "0002"          TO  SPA-ERRCD
      ****         END-IF
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    4               TO  SPA-H01GMN-CUR
           END-IF
           .
       4104-SKYYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻年月　入力処理
      *****************************************************************
       4105-HENREIYM-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-H01NAI-HENREIYM
      *
           IF      SPA-H01GMN-HENREIYM =   SPACE
               CONTINUE
           ELSE
      *    日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-H01GMN-HENREIYM TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)
                                       TO  SPA-H01GMN-HENREIYM
                   MOVE    SGDAY-SDAY (1:6)
                                       TO  SPA-H01NAI-HENREIYM
      ****         IF      SPA-H01NAI-HENREIYM
      ****                                 <   SPA-H01NAI-SRYYM
      ****             MOVE    "0002"          TO  SPA-ERRCD
      ****         END-IF
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    5               TO  SPA-H01GMN-CUR
           END-IF
           .
       4105-HENREIYM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求区分入力処理
      *****************************************************************
       4106-SKYKBN-CHK-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-H01GMN-SKYKBNMEI 
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       3
               IF      SPA-H01GMN-SKYKBN
                                       =   SPA-H01GMN-SKYKBNL (IDY)
                   MOVE    SPA-H01GMN-SKYKBNMEIL (IDY)
                                           TO  SPA-H01GMN-SKYKBNMEI
                   MOVE    3               TO  IDY
               END-IF
           END-PERFORM
      *
           IF      SPA-H01GMN-SKYKBNMEI    =   SPACE
              MOVE    "0001"       TO  SPA-ERRCD
           END-IF
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    6               TO  SPA-H01GMN-CUR
           END-IF
           .
       4106-SKYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求年月（公費請求書分）入力処理
      *****************************************************************
       4107-SKYYM-KOHSKY-CHK-SEC       SECTION.
      *
           MOVE    ZERO            TO  SPA-H01NAI-SKYYM-KOHSKY
      *
           IF      SPA-H01GMN-SKYYM-KOHSKY    =   SPACE
              CONTINUE
           ELSE 
      *    日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-H01GMN-SKYYM-KOHSKY    TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)
                                           TO  SPA-H01GMN-SKYYM-KOHSKY
                   MOVE    SGDAY-SDAY (1:6)
                                           TO  SPA-H01NAI-SKYYM-KOHSKY
                   IF      SPA-H01NAI-SKYYM-KOHSKY
                                           <   SPA-H01NAI-SRYYM
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    7               TO  SPA-H01GMN-CUR
           END-IF
           .
       4107-SKYYM-KOHSKY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻年月（公費請求書）入力処理
      *****************************************************************
       4118-HENREIYM-KOHSKY-CHK-SEC          SECTION.
      *
           MOVE    ZERO            TO  SPA-H01NAI-HENREIYM-KOHSKY
      *
           IF      SPA-H01GMN-HENREIYM-KOHSKY =   SPACE
               CONTINUE
           ELSE
      *    日付画面チェックサブ
               INITIALIZE                      ORCSGDAYAREA
               MOVE    "1"                 TO  SGDAY-INTYPE
               MOVE    SPA-H01GMN-HENREIYM-KOHSKY TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY(1:6)
                                       TO  SPA-H01GMN-HENREIYM-KOHSKY
                   MOVE    SGDAY-SDAY (1:6)
                                       TO  SPA-H01NAI-HENREIYM-KOHSKY
                   IF      SPA-H01NAI-HENREIYM-KOHSKY
                                           <   SPA-H01NAI-SRYYM
                       MOVE    "0002"          TO  SPA-ERRCD
                   END-IF
               ELSE
                   MOVE    "0002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    301             TO  SPA-H01GMN-CUR
           END-IF
           .
       4118-HENREIYM-KOHSKY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻再請求情報設定処理
      *****************************************************************
       4119-HENREI-SIJI-FLG-CHK-SEC          SECTION.
      *
      *    返戻設定時の請求年月のチェック
           EVALUATE    H01-HENREI-SIJI-FLG
               WHEN    "T"
                   IF      SPA-H01NAI-SKYYM    =   ZERO
                       MOVE    "0006"          TO  SPA-ERRCD
                       MOVE    4               TO  SPA-H01GMN-CUR
                       GO  TO  4119-HENREI-SIJI-FLG-CHK-EXT
                   ELSE
                       IF      SPA-H01NAI-HENREIYM
                                               >   SPA-H01NAI-SKYYM
                           MOVE    "0008"          TO  SPA-ERRCD
                           MOVE    5               TO  SPA-H01GMN-CUR
                           GO  TO  4119-HENREI-SIJI-FLG-CHK-EXT
                       END-IF
                   END-IF
           END-EVALUATE
      *****MOVE    "F"     TO  SPA-H01GMN-HENREI-SIJI-FLG
      *
           MOVE    H01-HENREI-SIJI-FLG TO  SPA-H01GMN-HENREI-SIJI-FLG
      *
      *    更新フラグセット
           MOVE    "1"                 TO  SPA-UPDFLG
      *
           EVALUATE    SPA-H01GMN-HENREI-SIJI-FLG
               WHEN    "T"
                   PERFORM 41191-HEIREI-B-HENSYU-SEC
               WHEN    "F"
                   PERFORM 41192-HEIREI-B-CLEAR-SEC
           END-EVALUATE
           .
       4119-HENREI-SIJI-FLG-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻再請求選択処理
      *****************************************************************
       41191-HEIREI-B-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-HENREI-B-CHK
      *           
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       TBL-HENREI-B-MAX
                   OR      FLG-HENREI-B-CHK
                                   >       ZERO
      *        該当するデータをチェック
               IF      SPA-RECE10-HKNNUM
                                       =   SPACE
                   IF    ( TBL-HENREI-B-RECESYUBETU (IDXX)
                                           =   SPA-RECE10-RECESYUBETU )
                   AND   ( TBL-HENREI-B-FTNNUM      (IDXX 1)
                                           =   SPA-RECE10-FTNJANUM    )
                   AND   ( TBL-HENREI-B-JKYNUM      (IDXX 1) 
                                           =   SPA-RECE10-JKYSNUM     )
                       EVALUATE    SPA-RECE10-TEISYUTUSAKI
                           WHEN    1
                               IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   1
                                   IF    ( TBL-HENREI-B-RECEIPT-KBN
                                                                 (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                                   AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                                       MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                                       MOVE    1   TO  FLG-HENREI-B-CHK
                                   END-IF
                               END-IF
                           WHEN    2
                           WHEN    6
                               IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   2
                                   IF    ( TBL-HENREI-B-RECEIPT-KBN
                                                                 (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                                   AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                                       MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                                       MOVE    1   TO  FLG-HENREI-B-CHK
                                   END-IF    
                               END-IF
                           WHEN    3
                               IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   3
                                   IF    ( TBL-HENREI-B-RECEIPT-KBN
                                                                 (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                                   AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                                       MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                                       MOVE    1   TO  FLG-HENREI-B-CHK
                                   END-IF
                               END-IF
                       END-EVALUATE
                   END-IF
               ELSE
                IF      SPA-RECE10-TEISYUTUSAKI    =   "3"
                   IF    ( TBL-HENREI-B-RECESYUBETU (IDXX)
                                           =   SPA-RECE10-RECESYUBETU )
                   AND   ( TBL-HENREI-B-KIGO        (IDXX)
                                           =   SPA-RECE10-KIGO        )
                       IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   3
                           IF    ( TBL-HENREI-B-RECEIPT-KBN      (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                           AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                               MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                               MOVE    1   TO  FLG-HENREI-B-CHK
                           END-IF
                       END-IF
                   END-IF
                ELSE
                   IF    ( TBL-HENREI-B-RECESYUBETU (IDXX)
                                           =   SPA-RECE10-RECESYUBETU )
                   AND   ( TBL-HENREI-B-HKNJANUM    (IDXX)
                                           =   SPA-RECE10-HKNJANUM    )
                   AND   ( TBL-HENREI-B-KIGO        (IDXX)
                                           =   SPA-RECE10-KIGO        )
                   AND   ( TBL-HENREI-B-NUM         (IDXX) 
                                           =   SPA-RECE10-NUM         )
                       EVALUATE    SPA-RECE10-TEISYUTUSAKI
                           WHEN    1
                               IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   1
                                   IF    ( TBL-HENREI-B-RECEIPT-KBN
                                                                 (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                                   AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                                       MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                                       MOVE    1   TO  FLG-HENREI-B-CHK
                                   END-IF
                               END-IF
                           WHEN    2
                           WHEN    6
                               IF      TBL-HENREI-B-TEISYUTUSAKI (IDXX)
                                               =   2
                                   IF    ( TBL-HENREI-B-RECEIPT-KBN
                                                                 (IDXX)
                                           =   SPA-RECE10-RECEIPT-KBN )
                                   AND   ( TBL-HENREI-B-MAX
                                           =   1                      )
                                       MOVE    TBL-HENREI-B-REC (IDXX)
                                                   TO  WRK-HENREI-B-REC
                                       MOVE    1   TO  FLG-HENREI-B-CHK
                                   END-IF
                               END-IF
                       END-EVALUATE
                   END-IF
                 END-IF
               END-IF
      *
      *        既に返戻設定済か
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       10
                       OR      TBL-HENREI-B-S-TEISYUTUSAKI (IDXX IDZZ)
                                       =       SPACE
                   IF    ( TBL-HENREI-B-S-TEISYUTUSAKI (IDXX IDZZ)
                                           =   SPA-RECE10-TEISYUTUSAKI )
                   AND   ( TBL-HENREI-B-S-NYUGAIKBN    (IDXX IDZZ)
                                           =   SPA-RECE10-NYUGAIKBN    )
                   AND   ( TBL-HENREI-B-S-RECEKA       (IDXX IDZZ)
                                           =   SPA-RECE10-RECEKA       )
                   AND   ( TBL-HENREI-B-S-RECESYUBETU  (IDXX IDZZ)
                                           =   SPA-RECE10-RECESYUBETU  )
                   AND   ( TBL-HENREI-B-S-HKNJANUM     (IDXX IDZZ)
                                           =   SPA-RECE10-HKNJANUM     )
                   AND   ( TBL-HENREI-B-S-HOJOKBN-KEY  (IDXX IDZZ)
                                           =   SPA-RECE10-HOJOKBN      )
                   AND   ( TBL-HENREI-B-S-KOHID-KEY    (IDXX IDZZ)
                                           =   SPA-RECE10-KOHID-KEY    )
                   AND   ( TBL-HENREI-B-S-TEKSTYMD     (IDXX IDZZ)
                                           =   SPA-RECE10-TEKSTYMD     )
                   AND   ( TBL-HENREI-B-S-RECEIPT-KBN (IDXX IDZZ)
                                           =   SPA-RECE10-RECEIPT-KBN  )
                       MOVE    1       TO  TBL-HENREI-B-ERR
                                                           (IDXX)
                       IF      FLG-HENREI-B-CHK
                                           >   ZERO
                           MOVE    2           TO  FLG-HENREI-B-CHK
                       END-IF
      *                再再請求かどうか
                       IF    ( TBL-HENREI-B-S-HENREIYM (IDXX IDZZ)
                                           <   SPA-H01NAI-SKYYM )
                       AND   ( TBL-HENREI-B-S-HENREIYM (IDXX IDZZ)
                                       NOT =   SPACE            )    
                       AND   ( TBL-HENREI-B-S-MAKE-FLG (IDXX IDZZ)
                                           =   1                )    
                           MOVE    2           TO  TBL-HENREI-B-ERR
                                                   (IDXX)
                           IF      FLG-HENREI-B-CHK
                                               >   ZERO
                               MOVE    3           TO  FLG-HENREI-B-CHK
                           END-IF
                       END-IF
      *
                       MOVE    10      TO  IDZZ
                   END-IF  
               END-PERFORM
           END-PERFORM
      *
           IF      FLG-HENREI-B-CHK    =   ZERO
      *        返戻対象一覧
               PERFORM 411911-H02-HENSYU-SEC
           ELSE
               IF      TBL-HENREI-B-SYORIYM (1)
                                       >   WRK-HENREI-B-SYORIYM
      *            返戻対象一覧
                   PERFORM 411911-H02-HENSYU-SEC
               ELSE
                   EVALUATE    FLG-HENREI-B-CHK
                       WHEN    1
                           MOVE    6               TO  SPA-H01GMN-CUR
                       WHEN    2
      *                    設定済
      *********************INITIALIZE                  WRK-HENREI-B-REC
                           MOVE    6               TO  SPA-H01GMN-CUR
                       WHEN    3
      *                    再再請求の可能性
                           MOVE    "1003"          TO  SPA-HIDCD 
                   END-EVALUATE
               END-IF
           END-IF
           .
       41191-HEIREI-B-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻対象一覧処理
      *****************************************************************
       411911-H02-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-H01-SYOKBN
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-H01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGH02"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-H01FREE
           MOVE    SPA-WORK            TO  SPA-H01KYOUTU
      *
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "H02"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
          .
       411911-H02-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻対象一覧処理
      *****************************************************************
       41192-HEIREI-B-CLEAR-SEC           SECTION.
      *
          MOVE    "1004"               TO  SPA-HIDCD
          .
       41192-HEIREI-B-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本点数チェック処理
      *****************************************************************
       4108-KHNTEN-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
           .
       4108-KHNTEN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    回数チェック処理
      *****************************************************************
       4109-KAISU-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
           .
       4109-KAISU-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数チェック処理
      *****************************************************************
       4110-TENSU-CHK-SEC          SECTION.
      *
           COMPUTE SPA-H01GMN-NTOTALTEN (IDY) = 
                                      SPA-H01GMN-NTOTALTEN (IDY)     +
                                      SPA-H01GMN-NTENSU    (IDY IDX)
      *
           .
       4110-TENSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担金チェック処理
      *****************************************************************
       4111-FTNMONEY-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
      *
           .
       4111-FTNMONEY-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金チェック処理
      *****************************************************************
       4112-YKZFTN-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
      *
           .
       4112-YKZFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     実日数チェック処理
      *****************************************************************
       4113-JNISSU-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
      *
           .
       4113-JNISSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事療養費チェック処理
      *****************************************************************
       4114-SHORYOYOHI-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                TO  SPA-UPDFLG
           .
       4114-SHORYOYOHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    食事標準負担額チェック処理
      *****************************************************************
       4115-SHOFTN-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
           .
       4115-SHOFTN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     食事日数チェック処理
      *****************************************************************
       4116-SHONISSU-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
           .
       4116-SHONISSU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *     合計点数（公費３、公費４）チェック処理
      *****************************************************************
       4117-TOTALTEN-CHK-SEC          SECTION.
      *
      *    更新フラグセット
      ***  MOVE    "1"                 TO  SPA-UPDFLG
           .
       4117-TOTALTEN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定ボタン押下時チェック処理
      *****************************************************************
       4103-KAKUTEI-CHK-SEC        SECTION.
      *
           IF      SPA-UPDFLG          =   SPACE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
      *
           .
       4103-KAKUTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック後ＳＰＡ再セット
      *****************************************************************
       4104-GMN-SPA-HENSYU-SEC         SECTION.
      *
      *    返戻年月が変更になったとき公費分が未入力ならセットする
           IF    ( SPA-H01NAI-HENREIYM NOT =   SPA-H01NAI-HENREIYM-OLD )
           AND   ( SPA-H01NAI-HENREIYM-KOHSKY
                                           =   ZERO                    )
           AND   ( SPA-H01NAI-KOHSKY-FLG   =   1                       )  
               MOVE    SPA-H01GMN-HENREIYM TO
                                           SPA-H01GMN-HENREIYM-KOHSKY
               MOVE    SPA-H01NAI-HENREIYM TO 
                                           SPA-H01NAI-HENREIYM-KOHSKY
           END-IF
      *
      *    請求年月が変更になったとき公費分が未入力ならセットする
           IF    ( SPA-H01NAI-SKYYM    NOT =   WRK-SKYYM-OLD )
           AND   ( SPA-H01NAI-SKYYM-KOHSKY =   ZERO          )
           AND   ( SPA-H01NAI-KOHSKY-FLG   =   1             )  
               MOVE    SPA-H01GMN-SKYYM    TO  SPA-H01GMN-SKYYM-KOHSKY
               MOVE    SPA-H01NAI-SKYYM    TO  SPA-H01NAI-SKYYM-KOHSKY
           END-IF
      *
      *    公費分の請求年月が未入力のとき返戻年月も未入力とする
           IF    ( SPA-H01NAI-SKYYM-KOHSKY =   ZERO          )
           AND   ( SPA-H01NAI-KOHSKY-FLG   =   1             )  
               MOVE    SPACE               TO SPA-H01GMN-HENREIYM-KOHSKY
               MOVE    ZERO                TO SPA-H01NAI-HENREIYM-KOHSKY
           END-IF
      *
      *    返戻年月入力時の返戻再請求ボタンの入力設定
      *****IF      SYS-2005-??         =   "0"
               MOVE    WIDGET-INSENSITIVE  TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
      *****ELSE
               IF    ( RECE10-RECESYUBETU  =   "6666" )
               OR    ( RECE10-TEISYUTUSAKI =   "5"    )
                   CONTINUE
               ELSE
                   IF    ( SPA-H01NAI-HENREIYM >   ZERO )
                   AND   ( SPA-H01NAI-HENREI-MSG
                                               =   1    )
                       MOVE    WIDGET-NORMAL       TO
                                       SPA-H01GMN-HENREI-SIJI-FLG-STE
                   END-IF
               END-IF
      *****END-IF
      *
      *    返戻設定時に返戻年月をクリアしたとき
           IF    ( SPA-H01NAI-HENREIYM NOT =   SPA-H01NAI-HENREIYM-OLD )
           AND   ( SPA-H01NAI-HENREIYM     =   ZERO                    )
           AND   ( SPA-H01GMN-HENREI-SIJI-FLG
                                           =   "T"                     )
               MOVE    "1005"                  TO  SPA-HIDCD
           END-IF
      *
           .
       4104-GMN-SPA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    関連チェック処理
      *****************************************************************
       4105-KANREN-CHK-SEC       SECTION.
      *
      *    診療年月とのチェック
           IF      SPA-H01NAI-SKYYM
                                   >   ZERO
               IF      SPA-H01NAI-SKYYM
                                       <   SPA-H01NAI-SRYYM
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-H01GMN-CUR
                   GO  TO  4105-KANREN-CHK-EXT
               END-IF
           END-IF
      *
           IF      SPA-H01NAI-HENREIYM
                                   >   ZERO
               IF      SPA-H01NAI-HENREIYM
                                       <   SPA-H01NAI-SRYYM
                   MOVE    "0002"          TO  SPA-ERRCD
                   MOVE    5               TO  SPA-H01GMN-CUR
                   GO  TO  4105-KANREN-CHK-EXT
               END-IF
           END-IF
      *
      *    返戻設定時の請求年月の入力
           IF      SPA-H01GMN-HENREI-SIJI-FLG
                                   =   "T"
               IF      SPA-H01NAI-SKYYM    =   ZERO
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    4               TO  SPA-H01GMN-CUR
               ELSE
                   IF      SPA-H01NAI-HENREIYM
                                           >   SPA-H01NAI-SKYYM
                       MOVE    "0008"          TO  SPA-ERRCD
                       MOVE    5               TO  SPA-H01GMN-CUR
                       GO  TO  4105-KANREN-CHK-EXT
                   END-IF
               END-IF
           END-IF
           .
       4105-KANREN-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-UPDFLG          =   "1"
               MOVE    "0101"          TO  SPA-HIDCD
               GO      TO      210-BACK-EXT
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "H01"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPACE               TO  SPA-WORK 
           INITIALIZE                      SPA-KYOTUKEY
                                           SPA-KYOTU-SET
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      ***  MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       420-CLEAR-SEC                   SECTION.
      *
           MOVE    2                   TO  SPA-H01GMN-CUR
           INITIALIZE                      SPA-H01GMN-TABLE
                                           SPA-H01GMN-KOUMOKU
                                           SPA-H01GMN-KOUMOKU1
                                           SPA-H01NAI-KOUMOKU
                                           SPA-H01NAI-KOUMOKU1
      *
           INITIALIZE                  SPA-RECE10-AREA
           INITIALIZE                  SPA-HENREI-B-REC
           INITIALIZE                  WRK-HENREI-B-REC
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       WRK-CONS-HENREI-B-MAX    
               INITIALIZE              TBL-HENREI-B-REC (IDZZ)
                                       TBL-HENREI-B-ERR (IDZZ)
           END-PERFORM
           MOVE    ZERO            TO  TBL-HENREI-B-MAX
      *
           MOVE    SPACE               TO  SPA-HENFLG
                                           SPA-UPDFLG                                     
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
           .
       420-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       430-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               GO  TO  430-MAEKPTNUM-EXT
           END-IF
      *
           MOVE    SPA-LAST-PTNUM      TO  H01-PTNUM
           PERFORM 4101-PTNUM-CHK-SEC
      *
           .
       430-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前処理
      *****************************************************************
       450-KOUSIN-GIDMSG-SEC                SECTION.
      *
      *    入力チェック
           MOVE    1               TO  FLG-TOUROKU
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO  TO  450-KOUSIN-GIDMSG-EXT
           END-IF
      *         
      *    ガイドメッセージセット
           IF    ( SPA-H01NAI-HENREI-MSG
                                       =   1    )
           AND   ( SPA-H01GMN-HENREI-SIJI-FLG
                                       =   "F"  )
           AND   ( SPA-H01NAI-HENREIYM >   ZERO )
               MOVE    "1002"              TO  SPA-HIDCD
           ELSE
               MOVE    "1001"              TO  SPA-HIDCD
           END-IF
           .
       450-KOUSIN-GIDMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       450-KOUSIN-SEC                SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    更新済
           MOVE    SPACE           TO  SPA-UPDFLG
                                       SPA-HENFLG
      *
           MOVE    SPACE           TO  RECE10-REC   
           INITIALIZE                  RECE10-REC
           MOVE    SPA-RECE10-KEY  TO  RECE10-KEY
      *
           PERFORM 900-SEIKYU-INV-SEC
           IF      FLG-SEIKYU          =   ZERO
               MOVE    MCPDATA-REC         TO  RECE10-REC
           ELSE
               MOVE    "1010"          TO  SPA-ERRCD
               GO  TO  450-KOUSIN-EXT
           END-IF
      *
           MOVE    SPA-H01NAI-SKYYM       TO  RECE10-SKYYM
           MOVE    SPA-H01NAI-HENREIYM    TO  RECE10-HENREIYM
           MOVE    SPA-H01GMN-SKYKBN      TO  RECE10-SKYKBN
           MOVE    "1"                    TO  RECE10-TEISEIKBN
      *
      *    公害、労災、自賠責は点数等の更新はしない
           IF      SPA-RECE10-TEISYUTUSAKI =   "5" OR  "3" OR  "4"
               CONTINUE
           ELSE
               PERFORM VARYING IDX FROM    1   BY  1
                       UNTIL   IDX >       5
                   MOVE    SPA-H01GMN-NJNISSU (IDX)  TO
                                           RECE10-JNISSU   (IDX)
                   MOVE    SPA-H01GMN-NTOTALTEN (IDX)    TO 
                                           RECE10-TOTALTEN (IDX)
                   MOVE    SPA-H01GMN-NFTNMONEY(IDX) TO
                                           RECE10-FTNMONEY (IDX)
                   MOVE    SPA-H01GMN-NYKZFTN  (IDX) TO
                                           RECE10-YKZFTN   (IDX)
                   IF      SPA-RECE10-NYUGAIKBN      =   "1"
                       MOVE    SPA-H01GMN-NSHONISSU (IDX)  TO
                                           RECE10-SHOKUJINISSU   (IDX)
                       MOVE    SPA-H01GMN-NSHORYOYOHI (IDX)    TO 
                                           RECE10-SHOKUJIRYOYOHI (IDX)
                       MOVE    SPA-H01GMN-NSHOFTN(IDX) TO
                                           RECE10-SHOKUJIFTN (IDX)
                   END-IF
               END-PERFORM
               EVALUATE    SPA-RECE10-NYUGAIKBN
                   WHEN    "1"
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       32
                           MOVE    SPA-H01GMN-NKHNTEN  (IDX)  TO
                                               RECE10-KHNTEN (IDX)
                           MOVE    SPA-H01GMN-NKAISU   (IDX)  TO
                                               RECE10-KAISU  (IDX)
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       3
                               MOVE    SPA-H01GMN-NTENSU (IDY IDX)  TO
                                               RECE10-TENSU (IDX IDY)
                           END-PERFORM 
                       END-PERFORM
                   WHEN    "2"
                       PERFORM VARYING IDX FROM    1   BY  1
                               UNTIL   IDX >       38
                           MOVE    SPA-H01GMN-NKHNTEN  (IDX)  TO
                                               RECE10-KHNTEN (IDX)
                           MOVE    SPA-H01GMN-NKAISU   (IDX)  TO
                                               RECE10-KAISU  (IDX)
                           PERFORM VARYING IDY FROM    1   BY  1
                                   UNTIL   IDY >       3
                               MOVE    SPA-H01GMN-NTENSU (IDY IDX)  TO
                                               RECE10-TENSU (IDX IDY)
                           END-PERFORM 
                       END-PERFORM
               END-EVALUATE
           END-IF
      *
           EVALUATE    SPA-H01GMN-HENREI-SIJI-FLG
               WHEN    "T"
                   MOVE    1               TO  RECE10-HENREI-SIJI-FLG
               WHEN    "F"
                   MOVE    ZERO            TO  RECE10-HENREI-SIJI-FLG
           END-EVALUATE
      *     
           MOVE    SMCNDATE-YMD        TO  RECE10-UPDYMD
           MOVE    SMCNDATE-HMS        TO  RECE10-UPDHMS
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-ORCDBMAIN-SEC
      *
      *    公費請求書があれば更新する
           IF      MCP-RC              =   ZERO
               MOVE    SPACE           TO  KOHSKY-REC   
               INITIALIZE                  KOHSKY-REC
               MOVE    SPA-RECE10-KEY      TO  KOHSKY-KEY
      *
               PERFORM 900-KOHSKY-INV-SEC
               IF      FLG-KOHSKY          =   ZERO
                   MOVE    MCPDATA-REC         TO  KOHSKY-REC
                   MOVE    SPA-H01NAI-SKYYM-KOHSKY
                                               TO  KOHSKY-SKYYM
                   MOVE    SPA-H01NAI-HENREIYM-KOHSKY
                                               TO  KOHSKY-HENREIYM
                   MOVE    SPA-H01GMN-SKYKBN   TO  KOHSKY-SKYKBN
                   MOVE    "1"                 TO  KOHSKY-TEISEIKBN
                   MOVE    SMCNDATE-YMD        TO  KOHSKY-UPDYMD
      *
                   MOVE    KOHSKY-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_kohsky"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             PERFORM 900-ORCDBMAIN-SEC
      *
                   IF      MCP-RC     NOT =   ZERO
                       CALL "coblog"   USING
                                       "abort tbl_kohsky update"
                       MOVE    "1002"          TO  SPA-ERRCD
                       GO  TO  450-KOUSIN-EXT
                   END-IF
               END-IF
           ELSE    
               CALL "coblog"   USING   "abort tbl_tbl_seikyu update"
               MOVE    "1001"          TO  SPA-ERRCD
               GO  TO  450-KOUSIN-EXT
           END-IF
      *     
      *    返戻データ更新処理 
           PERFORM 4501-HENREI-B-UPDATE-SEC
      *
      *    クリア処理
           PERFORM 420-CLEAR-SEC
           .
       450-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻データ更新処理
      *****************************************************************
       4501-HENREI-B-UPDATE-SEC          SECTION.
      *
           EVALUATE    SPA-H01GMN-HENREI-SIJI-FLG
               WHEN    "T"
      *            設定されていた情報と同じ返戻データのときは更新しない   
                   IF      SPA-HENREI-B-HOSPNUM    >   ZERO
                       IF    ( SPA-HENREI-B-TEISYUTUSAKI
                                       =   WRK-HENREI-B-TEISYUTUSAKI  )
                       AND   ( SPA-HENREI-B-TEISYUTUSAKI2
                                       =   WRK-HENREI-B-TEISYUTUSAKI2 )
                       AND   ( SPA-HENREI-B-SYORIYM
                                       =   WRK-HENREI-B-SYORIYM       )
                       AND   ( SPA-HENREI-B-RECEIPT-NO
                                       =   WRK-HENREI-B-RECEIPT-NO    )
                           GO  TO  4501-HENREI-B-UPDATE-EXT
                       ELSE
      *                    返戻データ請求管理情報更新処理
                           PERFORM 45011-HENREI-B-UPDATE1-SEC
                       END-IF
                   ELSE
      *                返戻データ請求管理情報更新処理
                       PERFORM 45011-HENREI-B-UPDATE1-SEC
                   END-IF
           END-EVALUATE            
      *
           EVALUATE    SPA-H01GMN-HENREI-SIJI-FLG
               WHEN    "T"
      *            設定されていた情報と違う返戻データのときはクリアする
                   IF      SPA-HENREI-B-HOSPNUM    >   ZERO
                       IF    ( SPA-HENREI-B-TEISYUTUSAKI
                                       =   WRK-HENREI-B-TEISYUTUSAKI  )
                       AND   ( SPA-HENREI-B-TEISYUTUSAKI2
                                       =   WRK-HENREI-B-TEISYUTUSAKI2 )
                       AND   ( SPA-HENREI-B-SYORIYM
                                       =   WRK-HENREI-B-SYORIYM       )
                       AND   ( SPA-HENREI-B-RECEIPT-NO
                                       =   WRK-HENREI-B-RECEIPT-NO    )
                           CONTINUE
                       ELSE
      *                    返戻データ請求管理情報クリア処理
                           PERFORM 45012-HENREI-B-UPDATE2-SEC
                       END-IF
                   END-IF
               WHEN    "F"
                   IF      SPA-HENREI-B-HOSPNUM    >   ZERO
      *                返戻データ請求管理情報クリア処理
                       PERFORM 45012-HENREI-B-UPDATE2-SEC
                   END-IF
           END-EVALUATE            
           .
       4501-HENREI-B-UPDATE-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻データ請求管理情報更新処理
      *****************************************************************
       45011-HENREI-B-UPDATE1-SEC      SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       10
               IF    ( WRK-HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                       =   SPA-RECE10-TEISYUTUSAKI )
               AND   ( WRK-HENREI-B-S-NYUGAIKBN    (IDZZ)
                                       =   SPA-RECE10-NYUGAIKBN    )
               AND   ( WRK-HENREI-B-S-RECEKA       (IDZZ)
                                       =   SPA-RECE10-RECEKA       )
               AND   ( WRK-HENREI-B-S-RECESYUBETU  (IDZZ)
                                       =   SPA-RECE10-RECESYUBETU  )
               AND   ( WRK-HENREI-B-S-HKNJANUM     (IDZZ)
                                       =   SPA-RECE10-HKNJANUM     )
               AND   ( WRK-HENREI-B-S-HOJOKBN-KEY  (IDZZ)
                                       =   SPA-RECE10-HOJOKBN      )
               AND   ( WRK-HENREI-B-S-KOHID-KEY    (IDZZ)
                                       =   SPA-RECE10-KOHID-KEY    )
               AND   ( WRK-HENREI-B-S-TEKSTYMD     (IDZZ)
                                       =   SPA-RECE10-TEKSTYMD     )
               AND   ( WRK-HENREI-B-S-RECEIPT-KBN  (IDZZ)
                                       =   SPA-RECE10-RECEIPT-KBN  )
               AND   ( WRK-HENREI-B-S-HENREIYM     (IDZZ)
                                       =   SPA-H01NAI-SKYYM    )
                   GO  TO  45011-HENREI-B-UPDATE1-EXT
               END-IF
      *                                
               IF      WRK-HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                           =   SPACE
                   MOVE    SPA-RECE10-TEISYUTUSAKI TO
                                       WRK-HENREI-B-S-TEISYUTUSAKI(IDZZ)
                   MOVE    SPA-RECE10-NYUGAIKBN    TO
                                       WRK-HENREI-B-S-NYUGAIKBN   (IDZZ)
                   MOVE    SPA-RECE10-RECEKA       TO
                                       WRK-HENREI-B-S-RECEKA      (IDZZ)
                   MOVE    SPA-RECE10-RECESYUBETU  TO 
                                       WRK-HENREI-B-S-RECESYUBETU (IDZZ)
                   MOVE    SPA-RECE10-HKNJANUM     TO
                                       WRK-HENREI-B-S-HKNJANUM    (IDZZ)
                   MOVE    SPA-RECE10-HOJOKBN      TO
                                       WRK-HENREI-B-S-HOJOKBN-KEY (IDZZ)
                   MOVE    SPA-RECE10-KOHID-KEY    TO
                                       WRK-HENREI-B-S-KOHID-KEY   (IDZZ)
                   MOVE    SPA-RECE10-TEKSTYMD     TO
                                       WRK-HENREI-B-S-TEKSTYMD    (IDZZ)
                   MOVE    SPA-RECE10-RECEIPT-KBN     TO
                                       WRK-HENREI-B-S-RECEIPT-KBN (IDZZ)
      *
      *            公費単独で生保又は中国残留邦人等のとき負担者番号、
      *            受給者番号をセット
                   IF    ( RECE10-HKNNUM       =   SPACE               )
                       IF    ( RECE10-KOHNUM (1)   =   "012"  OR "025"
                                                              OR "020"
                                                              OR "053" )
                       AND   ( RECE10-KOHNUM (2)   =   SPACE           )
                           STRING  SPA-RECE10-FTNJANUM
                                                   DELIMITED   BY  SPACE
                                   "　"            DELIMITED   BY  SIZE
                                   SPA-RECE10-JKYSNUM
                                                   DELIMITED   BY  SPACE
                                           INTO
                                           WRK-HENREI-B-S-KIGO    (IDZZ)
                           END-STRING
      *****            ELSE
      *****                INITIALIZE                  PTKOHINF-REC
      *****                MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
      *****                MOVE    RECE10-PTID     TO  PTKOH-PTID
      *****                MOVE    RECE10-KOHID(1) TO  PTKOH-KOHID
      *****                MOVE    PTKOHINF-REC    TO  MCPDATA-REC
      *****                PERFORM 900-PTKOHINF-READ-SEC
      *****                STRING  PTKOH-FTNJANUM  DELIMITED   BY  SPACE
      *****************************"　"            DELIMITED   BY  SIZE
      *****                        PTKOH-JKYSNUM   DELIMITED   BY  SPACE
      *****                                INTO
      *****                                WRK-HENREI-B-S-KIGO    (IDZZ)
      *****                END-STRING
                       END-IF
                   ELSE
                       IF      RECE10-TEISYUTUSAKI     =   "3"
                           MOVE    RECE10-KOFUNUM      TO  
                                           WRK-HENREI-B-S-KIGO    (IDZZ)
                       ELSE
                           STRING  RECE10-KIGO     DELIMITED   BY  SPACE
                                   "　"            DELIMITED   BY  SIZE
                                   RECE10-NUM      DELIMITED   BY  SPACE
                                   INTO    WRK-HENREI-B-S-KIGO    (IDZZ)
                           END-STRING
                       END-IF
                   END-IF
                   MOVE    1                   TO
                                      WRK-HENREI-B-S-HENREI-SIJI-FLG
                                                                  (IDZZ)
                   MOVE    SPA-H01NAI-SKYYM    TO
                                      WRK-HENREI-B-S-HENREIYM     (IDZZ)
                   MOVE    SPA-RECE10-DISASTER TO
                                      WRK-HENREI-B-S-DISASTER     (IDZZ)
      *
                   MOVE    SMCNDATE-YMD        TO  WRK-HENREI-B-UPYMD
                   MOVE    SMCNDATE-HMS        TO  WRK-HENREI-B-UPHMS
      *
                   MOVE    WRK-HENREI-B-REC    TO  MCPDATA-REC
                   MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       CALL "coblog"   USING
                                       "abort tbl_henrei-body update"
                       MOVE    "1003"          TO  SPA-ERRCD
                       GO  TO  45011-HENREI-B-UPDATE1-EXT
                   END-IF
      *
                   MOVE    10                  TO  IDZZ
               END-IF
           END-PERFORM
           .
       45011-HENREI-B-UPDATE1-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻データ請求管理情報クリア処理
      *****************************************************************
       45012-HENREI-B-UPDATE2-SEC      SECTION.
      *
           PERFORM VARYING IDZZ    FROM    1   BY  1
                   UNTIL   IDZZ    >       10
                   OR      SPA-HENREI-B-S-TEISYUTUSAKI (IDZZ)
                                   =   SPACE
               IF      SPA-RECE10-TEISYUTUSAKI =
                                       SPA-HENREI-B-S-TEISYUTUSAKI(IDZZ)
               AND     SPA-RECE10-NYUGAIKBN    =
                                       SPA-HENREI-B-S-NYUGAIKBN   (IDZZ)
               AND     SPA-RECE10-RECEKA       =
                                       SPA-HENREI-B-S-RECEKA      (IDZZ)
               AND     SPA-RECE10-RECESYUBETU  =
                                       SPA-HENREI-B-S-RECESYUBETU (IDZZ)
               AND     SPA-RECE10-HKNJANUM     =
                                       SPA-HENREI-B-S-HKNJANUM    (IDZZ)
               AND     SPA-RECE10-HOJOKBN      =
                                       SPA-HENREI-B-S-HOJOKBN-KEY (IDZZ)
               AND     SPA-RECE10-KOHID-KEY    =
                                       SPA-HENREI-B-S-KOHID-KEY   (IDZZ)
               AND     SPA-RECE10-TEKSTYMD     =
                                       SPA-HENREI-B-S-TEKSTYMD    (IDZZ)
               AND     SPA-RECE10-RECEIPT-KBN  =
                                       SPA-HENREI-B-S-RECEIPT-KBN (IDZZ)
                   INITIALIZE                  SPA-HENREI-B-SEIKYU-OCC
                                                                  (IDZZ)
                   PERFORM VARYING IDYY    FROM    IDZZ    BY  1
                           UNTIL   IDYY    >       9
                       COMPUTE IDWW    =   IDYY    +   1
                       IF      SPA-HENREI-B-S-TEISYUTUSAKI (IDWW)
                                               =   SPACE
                           MOVE    10              TO  IDYY
                       ELSE
                           MOVE    SPA-HENREI-B-SEIKYU-OCC (IDWW)
                                                   TO
                                       SPA-HENREI-B-SEIKYU-OCC (IDYY)
                           INITIALIZE  SPA-HENREI-B-SEIKYU-OCC (IDWW)
                       END-IF
                   END-PERFORM
                   MOVE    SMCNDATE-YMD        TO  SPA-HENREI-B-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SPA-HENREI-B-UPHMS
      *
                   MOVE    SPA-HENREI-B-REC    TO  MCPDATA-REC
                   MOVE    "tbl_henrei_body"   TO  MCP-TABLE
                   MOVE    "upd1"              TO  MCP-PATHNAME
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   PERFORM 900-ORCDBMAIN-SEC
                   IF      MCP-RC          NOT =   ZERO
                       CALL "coblog"   USING
                                       "abort tbl_henrei-body update"
                       MOVE    "1003"          TO  SPA-ERRCD
                       GO  TO  45012-HENREI-B-UPDATE2-EXT
                   END-IF
      *
                   MOVE    10                  TO  IDZZ
               END-IF
           END-PERFORM
           .
       45012-HENREI-B-UPDATE2-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻一覧処理
      *****************************************************************
       450-HENREI-LIST-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-H01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGH03"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-H01FREE
           MOVE    SPA-WORK            TO  SPA-H01KYOUTU
      *
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "H03"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H03"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END
          .
       450-HENREI-LIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       480-KENSAKU-SEC             SECTION.
      *
           IF      SPA-H01NAI-SRYYM    =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               GO  TO  480-KENSAKU-EXT
           END-IF
      *         
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-H01FREE         TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "H01"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名をクリアする
           MOVE    SPACE               TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       480-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    返戻設定確認処理
      *****************************************************************
       490-HENREI-SEC           SECTION.
      *
           MOVE    1                   TO  SPA-H01-SYOKBN
      *
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-H01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGH02"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-H01FREE
           MOVE    SPA-WORK            TO  SPA-H01KYOUTU
      *
           MOVE    SPA-H01KYOUTU       TO  SPA-WORK
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "H02"                TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "H02"                TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
          .
       490-HENREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-HIDCD       NOT =   SPACE
               PERFORM 520-RIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H01    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE              H01
      *
           MOVE    SPA-H01GMN-PTNUM       TO  H01-PTNUM
           MOVE    SPA-H01GMN-NAME        TO  H01-NAME-VALUE
           MOVE    SPA-H01GMN-SRYYM       TO  H01-SRYYM
           MOVE    SPA-H01GMN-SRYKA       TO  H01-SRYKA-VALUE
           MOVE    SPA-H01GMN-SRYKANAME   TO  H01-SRYKANAME-VALUE
           MOVE    SPA-H01GMN-SKYYM       TO  H01-SKYYM
           MOVE    SPA-H01GMN-SKYYM-KOHSKY
                                          TO  H01-SKYYM-KOHSKY
           MOVE    SPA-H01GMN-HENREIYM    TO  H01-HENREIYM
           MOVE    SPA-H01GMN-HENREIYM-KOHSKY
                                          TO  H01-HENREIYM-KOHSKY
           IF      SPA-RECE10-TEISYUTUSAKI =   "3" OR  "4"
               MOVE    SPACE                  TO  H01-LABEL-KOHSKY
           ELSE
               MOVE    "（公費請求書分）"     TO  H01-LABEL-KOHSKY
           END-IF
           IF      SPA-H01NAI-KOHSKY-FLG  =   ZERO
               MOVE    WIDGET-INSENSITIVE  TO  H01-SKYYM-KOHSKY-STATE
                                               H01-HENREIYM-KOHSKY-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  H01-SKYYM-KOHSKY-STATE           
                                               H01-HENREIYM-KOHSKY-STATE
               IF    ( SPA-H01NAI-SKYYM
                                      NOT =   SPA-H01NAI-SKYYM-KOHSKY  )
               AND   ( SPA-H01NAI-SRYYM
                                      NOT =   SPA-H01NAI-SKYYM AND ZERO)
               AND   ( SPA-H01NAI-SKYYM-KOHSKY
                                          =   SPA-H01NAI-SRYYM OR  ZERO)
               AND   ( SPA-H01GMN-SKYKBN  =   "1" OR  "2"              )
                   MOVE    "公費請求書分は月遅れ請求にはなりません"
                                              TO  H01-MSG-VALUE
               END-IF                             
           END-IF
           MOVE    SPA-H01GMN-SKYKBN-G    TO  H01-SKYKBN
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL       IDX             >   3
                   OR          SPA-H01GMN-SKYKBNL  (IDX)   =   SPACE 
               MOVE    SPA-H01GMN-SKYKBNLST (IDX)
                                          TO  H01-SKYKBNLST (IDX)
               MOVE    IDX                TO  H01-SKYKBN-COUNT
           END-PERFORM
           MOVE    SPA-H01GMN-TEISEIKBNNM TO  H01-TEISEIKBNNM-VALUE
           MOVE    SPA-H01GMN-HNYUGAIKBN  TO  H01-HNYUGAIKBN-VALUE
           MOVE    SPA-H01GMN-HTEISYUTUSAKI 
                                          TO  H01-HTEISYUTUSAKI-VALUE
           MOVE    SPA-H01GMN-HRECESYUBETU
                                          TO  H01-HRECESYUBETU-VALUE
           MOVE    SPA-H01GMN-HHKNJANUM   TO  H01-HHKNJANUM-VALUE
           MOVE    SPA-H01GMN-HHOJOKBN    TO  H01-HHOJOKBN-VALUE
           MOVE    SPA-H01GMN-HKIGO       TO  H01-HKIGO-VALUE
           MOVE    SPA-H01GMN-HCREYMD     TO  H01-HCREYMD-VALUE
           MOVE    SPA-H01GMN-HRECEIPT-KBN
                                          TO  H01-HRECEIPT-KBN-VALUE
           MOVE    SPA-H01GMN-TITLE-HKNJANUM
                                          TO  H01-TITLE-HKNJANUM
           MOVE    SPA-H01GMN-TITLE-KIGO  TO  H01-TITLE-KIGO
           IF      SPA-H01NAI-HENREI-MSG  =   1
               MOVE    "返戻データあり"       TO  H01-HENREI-MSG-VALUE
               MOVE    WIDGET-NORMAL          TO  H01-B10-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE     TO  H01-B10-STATE
           END-IF   
           IF      SPA-H01NAI-RECEDEN-MSG  =   1
               MOVE    "レセ電データがありません"
                                           TO  H01-RECEDEN-CHK-MSG-VALUE
           ELSE
               IF      SPA-H01NAI-JIBAI-PDF-MSG    =   1
                   MOVE    "ＰＤＦデータがありません"
                                           TO  H01-RECEDEN-CHK-MSG-VALUE
               END-IF
           END-IF
      *
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX     >   38
      *****        OR          SPA-H01GMN-MIDASI (IDX)    =   SPACE
               MOVE    SPA-H01GMN-MIDASI (IDX)    TO  H01-MIDASI (IDX)
               MOVE    SPA-H01GMN-KHNTEN (IDX)    TO  H01-KHNTEN (IDX)
               MOVE    SPA-H01GMN-KAISU  (IDX)    TO  H01-KAISU  (IDX)
               PERFORM VARYING IDY FROM    1       BY  1
                       UNTIL   IDY     >   3
                   MOVE    SPA-H01GMN-TENSU (IDY IDX)
                                           TO  H01-TENSU (IDY IDX)
               END-PERFORM
      *
               MOVE    SPA-H01GMN-NKHNTEN (IDX)
                                           TO  H01-NKHNTEN (IDX)
               MOVE    SPA-H01GMN-NKAISU  (IDX)
                                           TO  H01-NKAISU  (IDX)
               PERFORM VARYING IDY FROM    1       BY  1
                       UNTIL   IDY     >   3
                   MOVE    SPA-H01GMN-NTENSU (IDY IDX)
                                           TO  H01-NTENSU (IDY IDX)
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX     >   5
               MOVE    SPA-H01GMN-TOTALTEN (IDX)
                                               TO  H01-TOTALTEN (IDX)
               MOVE    SPA-H01GMN-FTNMONEY (IDX)
                                               TO  H01-FTNMONEY (IDX)
               MOVE    SPA-H01GMN-YKZFTN   (IDX)
                                               TO  H01-YKZFTN   (IDX)
               MOVE    SPA-H01GMN-JNISSU   (IDX)
                                               TO  H01-JNISSU   (IDX)
               MOVE    SPA-H01GMN-SHORYOYOHI (IDX)
                                               TO  H01-SHORYOYOHI (IDX)
               MOVE    SPA-H01GMN-SHOFTN   (IDX)
                                               TO  H01-SHOFTN   (IDX)
               MOVE    SPA-H01GMN-SHONISSU (IDX)
                                               TO  H01-SHONISSU (IDX)
      *
               MOVE    SPA-H01GMN-NTOTALTEN(IDX)
                                               TO  H01-NTOTALTEN(IDX)
               MOVE    SPA-H01GMN-NFTNMONEY(IDX)
                                               TO  H01-NFTNMONEY(IDX)
               MOVE    SPA-H01GMN-NYKZFTN  (IDX)
                                               TO  H01-NYKZFTN  (IDX)
               MOVE    SPA-H01GMN-NJNISSU  (IDX)
                                               TO  H01-NJNISSU  (IDX)
               MOVE    SPA-H01GMN-NSHORYOYOHI (IDX)
                                               TO  H01-NSHORYOYOHI (IDX)
               MOVE    SPA-H01GMN-NSHOFTN   (IDX)
                                               TO  H01-NSHOFTN   (IDX)
               MOVE    SPA-H01GMN-NSHONISSU (IDX)
                                               TO  H01-NSHONISSU (IDX)
           END-PERFORM
      *
           MOVE    SPA-H01GMN-SHOMIDASI (1)    TO  H01-SHOMIDASI (1)
                                                   H01-SHOMIDASI (4) 
                                                   H01-SHOMIDASI (7) 
                                                   H01-SHOMIDASI (10) 
           MOVE    SPA-H01GMN-SHOMIDASI (2)    TO  H01-SHOMIDASI (2)
                                                   H01-SHOMIDASI (5) 
                                                   H01-SHOMIDASI (8) 
                                                   H01-SHOMIDASI (11) 
           MOVE    SPA-H01GMN-SHOMIDASI (3)    TO  H01-SHOMIDASI (3)
                                                   H01-SHOMIDASI (6) 
                                                   H01-SHOMIDASI (9) 
                                                   H01-SHOMIDASI (12) 
      *
           MOVE    SPA-H01GMN-TENMIDASI (1)    TO  H01-TENMIDASI (1)
                                                   H01-TENMIDASI (5) 
                                                   H01-TENMIDASI (9) 
                                                   H01-TENMIDASI (13) 
           MOVE    SPA-H01GMN-TENMIDASI (2)    TO  H01-TENMIDASI (2)
                                                   H01-TENMIDASI (6) 
                                                   H01-TENMIDASI (10) 
                                                   H01-TENMIDASI (14) 
           MOVE    SPA-H01GMN-TENMIDASI (3)    TO  H01-TENMIDASI (3)
                                                   H01-TENMIDASI (7) 
                                                   H01-TENMIDASI (11) 
                                                   H01-TENMIDASI (15) 
           MOVE    SPA-H01GMN-TENMIDASI (4)    TO  H01-TENMIDASI (4)
                                                   H01-TENMIDASI (8) 
                                                   H01-TENMIDASI (12) 
                                                   H01-TENMIDASI (16) 
      *    色の設定
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX     >   38
               MOVE    SPA-H01GMN-MIDASI-STYLE (IDX)
                                       TO  H01-MIDASI-STYLE (IDX)
           END-PERFORM
           MOVE    "blue"              TO  H01-NAME-STYLE
                                           H01-SRYKANAME-STYLE
                                           H01-TEISEIKBNNM-STYLE
           MOVE    "blue"              TO  H01-HNYUGAIKBN-STYLE
                                           H01-HTEISYUTUSAKI-STYLE
                                           H01-HRECESYUBETU-STYLE
                                           H01-HHKNJANUM-STYLE
                                           H01-HKIGO-STYLE
                                           H01-HHOJOKBN-STYLE
                                           H01-HCREYMD-STYLE
           MOVE    "darkgreen"         TO  H01-MSG-STYLE
                                           H01-HRECEIPT-KBN-STYLE
           MOVE    "red"               TO  H01-HENREI-MSG-STYLE
                                           H01-RECEDEN-CHK-MSG-STYLE
      *
      *    入力の設定
           IF      SPA-SYS-1001-HOSPSBT1   =   "0"
               MOVE    WIDGET-INSENSITIVE  TO  H01-SRYKA-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  H01-SRYKA-STATE
           END-IF
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX     >   38
               MOVE    WIDGET-INSENSITIVE  TO  H01-KHNTEN-STATE  (IDX)
                                               H01-KAISU-STATE   (IDX)
               MOVE    WIDGET-INSENSITIVE  TO  H01-NKHNTEN-STATE (IDX)
                                               H01-NKAISU-STATE  (IDX)
               IF      SPA-RECE10-TEISYUTUSAKI =   "3" OR  "4"
                   CONTINUE
               ELSE
                   MOVE    WIDGET-NORMAL       TO
                                               H01-NKHNTEN-STATE (IDX)
                                               H01-NKAISU-STATE  (IDX)
               END-IF
               PERFORM VARYING IDY FROM    1       BY  1
                       UNTIL   IDY     >   3
                   MOVE    WIDGET-INSENSITIVE
                                   TO  H01-TENSU-STATE  (IDY IDX)
                   MOVE    WIDGET-INSENSITIVE
                                   TO  H01-NTENSU-STATE (IDY IDX)
                   IF      SPA-RECE10-TEISYUTUSAKI =   "3" OR  "4"
                       CONTINUE
                   ELSE
                       MOVE    WIDGET-NORMAL
                                           TO  H01-NTENSU-STATE(IDY IDX)
                   END-IF
               END-PERFORM
           END-PERFORM
           PERFORM VARYING IDX FROM    1       BY  1
                   UNTIL   IDX     >   5
               MOVE    WIDGET-INSENSITIVE
                                   TO  H01-TOTALTEN-STATE  (IDX)
                                       H01-FTNMONEY-STATE  (IDX)
                                       H01-YKZFTN-STATE    (IDX)
                                       H01-JNISSU-STATE    (IDX)
                                       H01-SHORYOYOHI-STATE(IDX)
                                       H01-SHOFTN-STATE    (IDX)
                                       H01-SHONISSU-STATE  (IDX)
               MOVE    WIDGET-INSENSITIVE
                                   TO  H01-NTOTALTEN-STATE  (IDX)
                                       H01-NFTNMONEY-STATE (IDX)
                                       H01-NYKZFTN-STATE   (IDX)
                                       H01-NJNISSU-STATE   (IDX)
               IF      SPA-RECE10-TEISYUTUSAKI =   "3" OR  "4"
                   CONTINUE
               ELSE
                   IF      IDX             <   4
                       MOVE    WIDGET-INSENSITIVE
                                           TO  H01-NTOTALTEN-STATE (IDX)
                   ELSE
                       MOVE    WIDGET-NORMAL
                                           TO  H01-NTOTALTEN-STATE (IDX)
                   END-IF
                   MOVE    WIDGET-NORMAL   TO  H01-NFTNMONEY-STATE (IDX)
                                               H01-NYKZFTN-STATE   (IDX)
                                               H01-NJNISSU-STATE   (IDX)
               END-IF
               MOVE    SPA-H01GMN-SHO-STATE
                                   TO  H01-NSHORYOYOHI-STATE(IDX)
                                       H01-NSHOFTN-STATE    (IDX)
                                       H01-NSHONISSU-STATE  (IDX)
           END-PERFORM
           MOVE    SPA-H01GMN-HENREI-SIJI-FLG-STE
                                      TO  H01-HENREI-SIJI-FLG-STATE
           MOVE    SPA-H01GMN-HENREI-SIJI-FLG
                                      TO  H01-HENREI-SIJI-FLG
           EVALUATE    SPA-H01GMN-HENREI-SIJI-FLG
               WHEN    "F"
                   MOVE    "返戻無"       TO  H01-HENREI-SIJI-FLG-LABEL
                   MOVE    "blue-bar"     TO  H01-HENREI-SIJI-FLG-STYLE
               WHEN    "T"
                   MOVE    "返戻有"       TO  H01-HENREI-SIJI-FLG-LABEL
                   MOVE    "red-bar"      TO  H01-HENREI-SIJI-FLG-STYLE
           END-EVALUATE 
      *
           MOVE    SPA-H01GMN-AGE     TO  H01-AGE
           MOVE    SPA-H01GMN-KYURATE TO  H01-KYURATE     
           MOVE    SPA-H01GMN-ZAITAKU TO  H01-ZAITAKU 
           MOVE    SPA-H01GMN-HONKZKKBN
                                      TO  H01-HONKZKKBN
      *
           IF      SPA-H01NAI-SKYKBN-RECEDEN
                                      =   "1"
               MOVE    WIDGET-INSENSITIVE     TO  H01-B12-STATE
               MOVE    "０点公費記載あり　請求できません"
                                              TO  H01-MSG-VALUE
               MOVE    "red"                  TO  H01-MSG-STYLE
           END-IF  
      *
           PERFORM 5001-MAPCUR-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF    ( SPA-ERRCD       =   SPACE )
           AND   ( SPA-H01GMN-CUR  =   ZERO  )
               PERFORM 50011-INPUT-CUR-SEC
           END-IF
      *
           EVALUATE    SPA-H01GMN-CUR
               WHEN    2
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    1
                   MOVE    "SRYYM"         TO  MCP-WIDGET
               WHEN    3
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    4
                   MOVE    "SKYYM"         TO  MCP-WIDGET
               WHEN    5
                   MOVE    "HENREIYM"      TO  MCP-WIDGET
               WHEN    6
                   MOVE    "SKYKBN"        TO  MCP-WIDGET
               WHEN    7
                   MOVE    "SKYYM_KOHSKY"  TO  MCP-WIDGET
               WHEN    301
                   MOVE    "HENREIYM_KOHSKY"
                                           TO  MCP-WIDGET
               WHEN    302
                   MOVE    "HENREI_SIJI_FLG"
                                           TO  MCP-WIDGET
               WHEN    8   THRU    182
               WHEN    215 THRU    229
                   COMPUTE IDX     =   SPA-H01GMN-CUR -   3
                   IF      SPA-H01GMN-CUR      >=   215
                       COMPUTE IDX     =   IDX     -   32
                   END-IF
                   DIVIDE  IDX         BY  5
                                       GIVING      WRK-SYO
                                       REMAINDER   WRK-AMARI
                   MOVE      WRK-SYO       TO  WRK-WIDGET 
                   EVALUATE    WRK-AMARI
                       WHEN    0
                           MOVE    "NKHNTEN"       TO  MCP-WIDGET
                           MOVE    WRK-WIDGET-X    TO  MCP-WIDGET (8:2)
                       WHEN    1
                           MOVE    "NKAISU"        TO  MCP-WIDGET
                           MOVE    WRK-WIDGET-X    TO  MCP-WIDGET (7:2)
                       WHEN    2
                           MOVE    "NTENSU"        TO  MCP-WIDGET
                           MOVE    WRK-WIDGET-X    TO  MCP-WIDGET (7:2)
                       WHEN    3
                           MOVE    "NKOHTENSU1"    TO  MCP-WIDGET
                           MOVE    WRK-WIDGET-X    TO  MCP-WIDGET (11:2)
                       WHEN    4
                           MOVE    "NKOHTENSU2"    TO  MCP-WIDGET
                           MOVE    WRK-WIDGET-X    TO  MCP-WIDGET (11:2)
                 END-EVALUATE                    
               WHEN    183
                   MOVE    "NFTNMONEY1"    TO  MCP-WIDGET
               WHEN    184
                   MOVE    "NFTNMONEY2"    TO  MCP-WIDGET
               WHEN    185
                   MOVE    "NFTNMONEY3"    TO  MCP-WIDGET
               WHEN    186
                   MOVE    "NYKZFTN1"      TO  MCP-WIDGET
               WHEN    187
                   MOVE    "NYKZFTN2"      TO  MCP-WIDGET
               WHEN    188
                   MOVE    "NYKZFTN3"      TO  MCP-WIDGET
               WHEN    189
                   MOVE    "NJNISSU1"      TO  MCP-WIDGET
               WHEN    190
                   MOVE    "NJNISSU2"      TO  MCP-WIDGET
               WHEN    191
                   MOVE    "NJNISSU3"      TO  MCP-WIDGET
               WHEN    192
                   MOVE    "NSHORYOYOHI1"  TO  MCP-WIDGET
               WHEN    193
                   MOVE    "NSHORYOYOHI2"  TO  MCP-WIDGET
               WHEN    194
                   MOVE    "NSHORYOYOHI3"  TO  MCP-WIDGET
               WHEN    195
                   MOVE    "NSHOFTN1"      TO  MCP-WIDGET
               WHEN    196
                   MOVE    "NSHOFTN2"      TO  MCP-WIDGET
               WHEN    197
                   MOVE    "NSHOFTN3"      TO  MCP-WIDGET
               WHEN    198
                   MOVE    "NSHONISSU1"    TO  MCP-WIDGET
               WHEN    199
                   MOVE    "NSHONISSU2"    TO  MCP-WIDGET
               WHEN    200
                   MOVE    "NSHONISSU3"    TO  MCP-WIDGET
               WHEN    201
                   MOVE    "NTOTALTEN4"    TO  MCP-WIDGET
               WHEN    202
                   MOVE    "NTOTALTEN5"    TO  MCP-WIDGET
               WHEN    203
                   MOVE    "NFTNMONEY4"    TO  MCP-WIDGET
               WHEN    204
                   MOVE    "NFTNMONEY5"    TO  MCP-WIDGET
               WHEN    205
                   MOVE    "NYKZFTN4"      TO  MCP-WIDGET
               WHEN    206
                   MOVE    "NYKZFTN5"      TO  MCP-WIDGET
               WHEN    207
                   MOVE    "NJNISSU4"      TO  MCP-WIDGET
               WHEN    208
                   MOVE    "NJNISSU5"      TO  MCP-WIDGET
               WHEN    209
                   MOVE    "NSHORYOYOHI4"  TO  MCP-WIDGET
               WHEN    210
                   MOVE    "NSHORYOYOHI5"  TO  MCP-WIDGET
               WHEN    211
                   MOVE    "NSHOFTN4"      TO  MCP-WIDGET
               WHEN    212
                   MOVE    "NSHOFTN5"      TO  MCP-WIDGET
               WHEN    213
                   MOVE    "NSHONISSU4"    TO  MCP-WIDGET
               WHEN    214
                   MOVE    "NSHONISSU5"    TO  MCP-WIDGET
               WHEN    300
                   MOVE  "B12"             TO  MCP-WIDGET   
           END-EVALUATE
      *
           MOVE    SPA-H01GMN-CUR      TO  SPA-H01-CUR
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       50011-INPUT-CUR-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "PTNUM"
                   IF      SPA-SYS-1001-HOSPSBT1   =   ZERO
                       MOVE    4           TO  SPA-H01GMN-CUR
                   ELSE
                       MOVE    3           TO  SPA-H01GMN-CUR
                   END-IF
               WHEN    "SRYYM"
                   MOVE    2           TO  SPA-H01GMN-CUR
               WHEN    "SRYKA"
                   MOVE    4           TO  SPA-H01GMN-CUR
               WHEN    "SKYYM"
                   MOVE    5           TO  SPA-H01GMN-CUR
               WHEN    "HENREIYM"
                   IF    ( SPA-H01NAI-HENREIYM >   ZERO )
                   AND   ( SPA-H01NAI-HENREI-MSG
                                               =   1    )
                       MOVE    302         TO  SPA-H01GMN-CUR
                   ELSE
                       MOVE    6           TO  SPA-H01GMN-CUR
                   END-IF 
               WHEN    "SKYKBN"
                   IF      SPA-RECE10-TEISYUTUSAKI =   "3" OR  "4"
                       MOVE    4               TO  SPA-H01GMN-CUR
                   ELSE 
                       IF    ( SPA-H01NAI-KOHSKY-FLG   =   1   )
                       AND   ( SPA-H01GMN-SKYKBN   NOT =   "0" )
                           MOVE    7           TO  SPA-H01GMN-CUR
                       ELSE
                           MOVE    8           TO  SPA-H01GMN-CUR
                       END-IF
                   END-IF
               WHEN    "SKYYM_KOHSKY"
                   MOVE    301         TO  SPA-H01GMN-CUR
               WHEN    "HENREIYM_KOHSKY"
                   MOVE    8           TO  SPA-H01GMN-CUR
               WHEN    "NKHNTEN01"
               WHEN    "NKHNTEN02"
               WHEN    "NKHNTEN03"
               WHEN    "NKHNTEN04"
               WHEN    "NKHNTEN05"
               WHEN    "NKHNTEN06"
               WHEN    "NKHNTEN07"
               WHEN    "NKHNTEN08"
               WHEN    "NKHNTEN09"
               WHEN    "NKHNTEN10"
               WHEN    "NKHNTEN11"
               WHEN    "NKHNTEN12"
               WHEN    "NKHNTEN13"
               WHEN    "NKHNTEN14"
               WHEN    "NKHNTEN15"
               WHEN    "NKHNTEN16"
               WHEN    "NKHNTEN17"
               WHEN    "NKHNTEN18"
               WHEN    "NKHNTEN19"
               WHEN    "NKHNTEN20"
               WHEN    "NKHNTEN21"
               WHEN    "NKHNTEN22"
               WHEN    "NKHNTEN23"
               WHEN    "NKHNTEN24"
               WHEN    "NKHNTEN25"
               WHEN    "NKHNTEN26"
               WHEN    "NKHNTEN27"
               WHEN    "NKHNTEN28"
               WHEN    "NKHNTEN29"
               WHEN    "NKHNTEN30"
               WHEN    "NKHNTEN31"
               WHEN    "NKHNTEN32"
               WHEN    "NKHNTEN33"
               WHEN    "NKHNTEN34"
               WHEN    "NKHNTEN35"
               WHEN    "NKHNTEN36"
               WHEN    "NKHNTEN37"
               WHEN    "NKHNTEN38"
                   MOVE    WRK-MCP-WIDGET(8:2) TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   4   +   IDX *  5
                   IF      IDX             >   35
                       COMPUTE SPA-H01GMN-CUR  =   SPA-H01GMN-CUR   +
                                                   32
                   END-IF
               WHEN    "NKAISU01"
               WHEN    "NKAISU02"
               WHEN    "NKAISU03"
               WHEN    "NKAISU04"
               WHEN    "NKAISU05"
               WHEN    "NKAISU06"
               WHEN    "NKAISU07"
               WHEN    "NKAISU08"
               WHEN    "NKAISU09"
               WHEN    "NKAISU10"
               WHEN    "NKAISU11"
               WHEN    "NKAISU12"
               WHEN    "NKAISU13"
               WHEN    "NKAISU14"
               WHEN    "NKAISU15"
               WHEN    "NKAISU16"
               WHEN    "NKAISU17"
               WHEN    "NKAISU18"
               WHEN    "NKAISU19"
               WHEN    "NKAISU20"
               WHEN    "NKAISU21"
               WHEN    "NKAISU22"
               WHEN    "NKAISU23"
               WHEN    "NKAISU24"
               WHEN    "NKAISU25"
               WHEN    "NKAISU26"
               WHEN    "NKAISU27"
               WHEN    "NKAISU28"
               WHEN    "NKAISU29"
               WHEN    "NKAISU30"
               WHEN    "NKAISU31"
               WHEN    "NKAISU32"
               WHEN    "NKAISU33"
               WHEN    "NKAISU34"
               WHEN    "NKAISU35"
               WHEN    "NKAISU36"
               WHEN    "NKAISU37"
               WHEN    "NKAISU38"
                   MOVE    WRK-MCP-WIDGET(7:2) TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   5   +   IDX *  5
                   IF      IDX             >   35
                       COMPUTE SPA-H01GMN-CUR  =   SPA-H01GMN-CUR   +
                                                   32
                   END-IF
               WHEN    "NTENSU01"
               WHEN    "NTENSU02"
               WHEN    "NTENSU03"
               WHEN    "NTENSU04"
               WHEN    "NTENSU05"
               WHEN    "NTENSU06"
               WHEN    "NTENSU07"
               WHEN    "NTENSU08"
               WHEN    "NTENSU09"
               WHEN    "NTENSU10"
               WHEN    "NTENSU11"
               WHEN    "NTENSU12"
               WHEN    "NTENSU13"
               WHEN    "NTENSU14"
               WHEN    "NTENSU15"
               WHEN    "NTENSU16"
               WHEN    "NTENSU17"
               WHEN    "NTENSU18"
               WHEN    "NTENSU19"
               WHEN    "NTENSU20"
               WHEN    "NTENSU21"
               WHEN    "NTENSU22"
               WHEN    "NTENSU23"
               WHEN    "NTENSU24"
               WHEN    "NTENSU25"
               WHEN    "NTENSU26"
               WHEN    "NTENSU27"
               WHEN    "NTENSU28"
               WHEN    "NTENSU29"
               WHEN    "NTENSU30"
               WHEN    "NTENSU31"
               WHEN    "NTENSU32"
               WHEN    "NTENSU33"
               WHEN    "NTENSU34"
               WHEN    "NTENSU35"
               WHEN    "NTENSU36"
               WHEN    "NTENSU37"
               WHEN    "NTENSU38"
                   MOVE    WRK-MCP-WIDGET(7:2) TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   6   +   IDX *  5
                   IF      IDX             >   35
                       COMPUTE SPA-H01GMN-CUR  =   SPA-H01GMN-CUR   +
                                                   32
                   END-IF
               WHEN    "NKOHTENSU101"
               WHEN    "NKOHTENSU102"
               WHEN    "NKOHTENSU103"
               WHEN    "NKOHTENSU104"
               WHEN    "NKOHTENSU105"
               WHEN    "NKOHTENSU106"
               WHEN    "NKOHTENSU107"
               WHEN    "NKOHTENSU108"
               WHEN    "NKOHTENSU109"
               WHEN    "NKOHTENSU110"
               WHEN    "NKOHTENSU111"
               WHEN    "NKOHTENSU112"
               WHEN    "NKOHTENSU113"
               WHEN    "NKOHTENSU114"
               WHEN    "NKOHTENSU115"
               WHEN    "NKOHTENSU116"
               WHEN    "NKOHTENSU117"
               WHEN    "NKOHTENSU118"
               WHEN    "NKOHTENSU119"
               WHEN    "NKOHTENSU120"
               WHEN    "NKOHTENSU121"
               WHEN    "NKOHTENSU122"
               WHEN    "NKOHTENSU123"
               WHEN    "NKOHTENSU124"
               WHEN    "NKOHTENSU125"
               WHEN    "NKOHTENSU126"
               WHEN    "NKOHTENSU127"
               WHEN    "NKOHTENSU128"
               WHEN    "NKOHTENSU129"
               WHEN    "NKOHTENSU130"
               WHEN    "NKOHTENSU131"
               WHEN    "NKOHTENSU132"
               WHEN    "NKOHTENSU133"
               WHEN    "NKOHTENSU134"
               WHEN    "NKOHTENSU135"
               WHEN    "NKOHTENSU136"
               WHEN    "NKOHTENSU137"
               WHEN    "NKOHTENSU138"
                   MOVE    WRK-MCP-WIDGET (11:2)
                                               TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   7   +   IDX *  5
                   IF      IDX             >   35
                       COMPUTE SPA-H01GMN-CUR  =   SPA-H01GMN-CUR   +
                                                   32
                   END-IF
               WHEN    "NKOHTENSU201"
               WHEN    "NKOHTENSU202"
               WHEN    "NKOHTENSU203"
               WHEN    "NKOHTENSU204"
               WHEN    "NKOHTENSU205"
               WHEN    "NKOHTENSU206"
               WHEN    "NKOHTENSU207"
               WHEN    "NKOHTENSU208"
               WHEN    "NKOHTENSU209"
               WHEN    "NKOHTENSU210"
               WHEN    "NKOHTENSU211"
               WHEN    "NKOHTENSU212"
               WHEN    "NKOHTENSU213"
               WHEN    "NKOHTENSU214"
               WHEN    "NKOHTENSU215"
               WHEN    "NKOHTENSU216"
               WHEN    "NKOHTENSU217"
               WHEN    "NKOHTENSU218"
               WHEN    "NKOHTENSU219"
               WHEN    "NKOHTENSU220"
               WHEN    "NKOHTENSU221"
               WHEN    "NKOHTENSU222"
               WHEN    "NKOHTENSU223"
               WHEN    "NKOHTENSU224"
               WHEN    "NKOHTENSU225"
               WHEN    "NKOHTENSU226"
               WHEN    "NKOHTENSU227"
               WHEN    "NKOHTENSU228"
               WHEN    "NKOHTENSU229"
               WHEN    "NKOHTENSU230"
               WHEN    "NKOHTENSU231"
               WHEN    "NKOHTENSU232"
               WHEN    "NKOHTENSU233"
               WHEN    "NKOHTENSU234"
                   MOVE    WRK-MCP-WIDGET (11:2)
                                               TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   8   +   IDX *  5
               WHEN    "NKOHTENSU235"
               WHEN    "NKOHTENSU236"
               WHEN    "NKOHTENSU237"
                   MOVE    WRK-MCP-WIDGET (11:2)
                                               TO  WRK-WIDGET
                   MOVE    WRK-WIDGET          TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   40  +   IDX *  5
               WHEN    "NKOHTENSU238"
                   MOVE    183             TO  SPA-H01GMN-CUR
               WHEN    "NFTNMONEY1"
               WHEN    "NFTNMONEY2"
               WHEN    "NFTNMONEY3"
                   MOVE    WRK-MCP-WIDGET (10:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   183 +   IDX
               WHEN    "NYKZFTN1"
               WHEN    "NYKZFTN2"
               WHEN    "NYKZFTN3"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   186 +   IDX
               WHEN    "NJNISSU1"
               WHEN    "NJNISSU2"
               WHEN    "NJNISSU3"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   189 +   IDX
                   IF    ( SPA-H01GMN-CUR        =   192 )
                   AND   ( SPA-RECE10-NYUGAIKBN  =   "2" )
                       MOVE    201               TO  SPA-H01GMN-CUR
                   END-IF
               WHEN    "NSHORYOYOHI1"
               WHEN    "NSHORYOYOHI2"
               WHEN    "NSHORYOYOHI3"
                   MOVE    WRK-MCP-WIDGET(12:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   192 +   IDX
               WHEN    "NSHOFTN1"
               WHEN    "NSHOFTN2"
               WHEN    "NSHOFTN3"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   195 +   IDX
               WHEN    "NSHONISSU1"
               WHEN    "NSHONISSU2"
               WHEN    "NSHONISSU3"
                   MOVE    WRK-MCP-WIDGET(10:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   198 +   IDX
               WHEN    "NTOTALTEN4"
               WHEN    "NTOTALTEN5"
                   MOVE    WRK-MCP-WIDGET (10:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   198 +   IDX
               WHEN    "NFTNMONEY4"
               WHEN    "NFTNMONEY5"
                   MOVE    WRK-MCP-WIDGET (10:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   200 +   IDX
               WHEN    "NYKZFTN4"
               WHEN    "NYKZFTN5"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   202 +   IDX
               WHEN    "NJNISSU4"
               WHEN    "NJNISSU5"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   204 +   IDX
                   IF    ( SPA-H01GMN-CUR          =   209 )
                   AND   ( SPA-RECE10-NYUGAIKBN    =   "2" )
                       MOVE    8                   TO  SPA-H01GMN-CUR
                   END-IF
               WHEN    "NSHORYOYOHI4"
               WHEN    "NSHORYOYOHI5"
                   MOVE    WRK-MCP-WIDGET(12:1)
                                               TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   206 +   IDX
               WHEN    "NSHOFTN4"
               WHEN    "NSHOFTN5"
                   MOVE    WRK-MCP-WIDGET(8:1) TO  WRK-WIDGET1
                   MOVE    WRK-WIDGET1         TO  IDX
                   COMPUTE SPA-H01GMN-CUR  =   208 +   IDX
               WHEN    "NSHONISSU4"
                   MOVE    214                 TO  SPA-H01GMN-CUR
               WHEN    "NSHONISSU5"
                   MOVE    8                   TO  SPA-H01GMN-CUR
           END-EVALUATE
           .
      *
       5011-INPUT-CUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"  TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "年月の入力エラーです"
                                             TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象のレセプトは存在しません"
                                             TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "該当の患者が存在しません"
                                               TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "診療年月が未入力です" 
                                               TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "請求年月が未入力です" 
                                               TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "返戻年月が未入力です" 
                                               TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "請求年月＞＝返戻年月で入力して下さい" 
                                               TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE
                   "０点公費記載のレセプトです、更新処理はできません" 
                                               TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "修正項目がありません" 
                                               TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "更新対象のデータを入力して下さい" 
                                               TO  SPA-ERRMSG
               WHEN    "1001"
                   MOVE    "請求管理ＤＢが更新できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1002"
                   MOVE    "公費請求書ＤＢが更新できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1003"
                   MOVE
                       "オンライン返戻明細ＤＢが更新できませんでした"
                                               TO  SPA-ERRMSG
               WHEN    "1010"
                   MOVE
                       "対象の請求データが存在しないので更新できません"
                                               TO  SPA-ERRMSG
      *
               WHEN    "9999"
                   MOVE    "他端末で使用中です。"
                                               TO  SPA-ERRMSG
                   MOVE    SLOCK-MSG           TO  SPA-ERRMSG(21:)
           END-EVALUATE
      *
           MOVE    SPACE                TO  HERR
           MOVE    SPA-ERRCD            TO  HERR-ERRCODE
           MOVE    SPA-ERRMSG           TO  HERR-ERRMSG
           MOVE    "B01"                TO  MCP-WIDGET
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "HERR"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HERR"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       510-ERRMSG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示処理
      *****************************************************************
       520-RIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-HIDMSG
      *
           EVALUATE    SPA-HIDCD
               WHEN    "1001"
                   MOVE    "更新処理をします"
                                        TO  WRK-HIDMSG
               WHEN    "1002"
                   MOVE
                   "返戻設定が行なわれていませんが更新処理をしますか"
                                        TO  WRK-HIDMSG
               WHEN    "1003"
                   MOVE
                   "既に返戻分再請求が行なわれていますが選択しますか。"
                                        TO  WRK-HIDMSG
               WHEN    "1004"
               WHEN    "1005"
                   MOVE    "返戻情報を解除しますか。"
                                        TO  WRK-HIDMSG
               WHEN    "1006"
                   MOVE    "主科未設定です。よろしいですか"
                                        TO  WRK-HIDMSG
               WHEN    "0101"
                   MOVE    "入力の途中です。終了してよろしいですか。"
                                        TO  WRK-HIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  HID1
           INITIALIZE                       HID1
           MOVE    SPA-HIDCD            TO  HID1-ID1CODE
           MOVE    WRK-HIDMSG           TO  HID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H01"                TO  SPA-MOTOPG
           MOVE    "HID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "HID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END              
      *
           .
       520-RIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    和暦西暦変換編集処理
      *****************************************************************
       5002-HIZUKE-HEN-SEC          SECTION.
      *
           IF      WRK-SYMD (1:6)          =   ZERO    OR  SPACE
               MOVE    SPACE                   TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
      *
           .
       5002-HIZUKE-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別編集処理
      *****************************************************************
       530-RECESYUBETU-HENSYU-SEC          SECTION.
      *
           MOVE   SPACE                TO  WRK-RECESYUBETU
      *
           IF      RECE10-TEISYUTUSAKI =   "5"
      ***      MOVE    "公害"              TO  WRK-RECESYUBETU
               GO  TO  530-RECESYUBETU-HENSYU-EXT
           END-IF        
      *
           IF      RECE10-RECESYUBETU  =   "6666"
               MOVE    "特別療養費"        TO  WRK-RECESYUBETU
               GO  TO  530-RECESYUBETU-HENSYU-EXT
           END-IF        
      *
           IF      RECE10-TEISYUTUSAKI =   "3" OR  "4"
               EVALUATE    RECE10-RECESYUBETU
                   WHEN    "7101"
                   WHEN    "7091"
                       MOVE    "傷病・業務"        TO  WRK-RECESYUBETU
                   WHEN    "7102"
                   WHEN    "7092"
                       MOVE    "傷病・通勤"        TO  WRK-RECESYUBETU
                   WHEN    "7071"
                   WHEN    "7081"
                       MOVE    "短期・業務"        TO  WRK-RECESYUBETU
                   WHEN    "7072"
                   WHEN    "7082"
                       MOVE    "短期・通勤"        TO  WRK-RECESYUBETU
                   WHEN    "7021"
                       MOVE    "アフターケア"      TO  WRK-RECESYUBETU
                   WHEN    "7051"
                   WHEN    "7061"
                       MOVE    "公務災害"          TO  WRK-RECESYUBETU
                   WHEN    "7011"
                   WHEN    "7001"
                       EVALUATE    RECE10-RECEKBN
                           WHEN    "4"
                              MOVE    "新様式"     TO  WRK-RECESYUBETU
                           WHEN    "5"
                               EVALUATE    RECE10-HKNKBN
                                   WHEN    "5"
                                       MOVE    "従来様式・労災"
                                                   TO  WRK-RECESYUBETU
                                   WHEN    "6"
                                       MOVE    "従来様式・健保"
                                                   TO  WRK-RECESYUBETU
                                   WHEN    OTHER
                                       MOVE    "従来様式"
                                                   TO  WRK-RECESYUBETU
                               END-EVALUATE
                           WHEN    "6"
                               MOVE    "第三者行為"
                                                   TO  WRK-RECESYUBETU
                       END-EVALUATE
               END-EVALUATE
               GO  TO  530-RECESYUBETU-HENSYU-EXT
           END-IF        
      *
           IF      RECE10-SRYYM        >   "200209"
               MOVE    RECE10-RECESYUBETU  TO  WRK-RECESYUBETU
               EVALUATE    RECE10-RECESYUBETU  (3:1)
                   WHEN    "4"
                   WHEN    "5"
                       MOVE    "3"             TO  WRK-RECESYUBETU (3:1)
               END-EVALUATE
               GO  TO  530-RECESYUBETU-HENSYU-EXT
           END-IF        
      *         
           EVALUATE  RECE10-RECESYUBETU
                                 ALSO  RECE10-TEISYUTUSAKI
           WHEN    1111      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "★"        TO  WRK-RECESYUBETU
               END-IF
           WHEN    1111      ALSO      1
           WHEN    1111      ALSO      ANY
               MOVE    "★"            TO  WRK-RECESYUBETU
      *
           WHEN    1112      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "☆☆"      TO  WRK-RECESYUBETU
               END-IF
           WHEN    1112      ALSO      1
               MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
           WHEN    1115      ALSO      ANY
               MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
           WHEN    1116      ALSO      ANY
               MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      ****     MOVE    "★★★★"      TO  WRK-RECESYUBETU
      *
           WHEN    1121      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "★"        TO  WRK-RECESYUBETU
               END-IF
           WHEN    1121      ALSO      1
               MOVE    "★"            TO  WRK-RECESYUBETU
      *
           WHEN    1122      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "☆☆"      TO  WRK-RECESYUBETU
               END-IF
           WHEN    1122      ALSO      1
               MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
           WHEN    1125      ALSO      ANY
               MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
           WHEN    1126      ALSO      ANY
               MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      *
           WHEN    1131      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "★★★"    TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "★"        TO  WRK-RECESYUBETU
               END-IF
           WHEN    1131      ALSO      1
               MOVE    "★"            TO  WRK-RECESYUBETU
      *
           WHEN    1132      ALSO      2
               IF  RECE10-HONKZKKBN    =   2
                   MOVE    "☆☆☆☆"  TO  WRK-RECESYUBETU
               ELSE
                   MOVE    "☆☆"      TO  WRK-RECESYUBETU
               END-IF
           WHEN    1132      ALSO      1
               MOVE    "☆☆"          TO  WRK-RECESYUBETU
      *
           WHEN    1135      ALSO      ANY
               MOVE    "★★★"        TO  WRK-RECESYUBETU
      *
           WHEN    1136      ALSO      ANY
               MOVE    "☆☆☆☆"      TO  WRK-RECESYUBETU
      *
           WHEN    1211      ALSO      ANY
           WHEN    1221      ALSO      ANY
           WHEN    1231      ALSO      ANY
               MOVE    "●"            TO  WRK-RECESYUBETU
      *
           WHEN    1212      ALSO      ANY
           WHEN    1222      ALSO      ANY
           WHEN    1232      ALSO      ANY
               MOVE    "○○"          TO  WRK-RECESYUBETU
      *
           WHEN    1311      ALSO      ANY
           WHEN    1321      ALSO      ANY
           WHEN    1331      ALSO      ANY
               MOVE    "▲"            TO  WRK-RECESYUBETU
      *
           WHEN    1312      ALSO      ANY
           WHEN    1322      ALSO      ANY
           WHEN    1332      ALSO      ANY
               MOVE    "△△"          TO  WRK-RECESYUBETU
      *
           WHEN    1411      ALSO      ANY
           WHEN    1421      ALSO      ANY
           WHEN    1431      ALSO      ANY
               MOVE    "◆"            TO  WRK-RECESYUBETU
      *
           WHEN    1412      ALSO      ANY
           WHEN    1422      ALSO      ANY
           WHEN    1432      ALSO      ANY
               MOVE    "◇◇"          TO  WRK-RECESYUBETU
      *
           WHEN    1415      ALSO      ANY
           WHEN    1425      ALSO      ANY
           WHEN    1435      ALSO      ANY
               MOVE    "◆◆◆"        TO  WRK-RECESYUBETU
      *
           WHEN    1416      ALSO      ANY
           WHEN    1426      ALSO      ANY
           WHEN    1436      ALSO      ANY
               MOVE    "◇◇◇◇"      TO  WRK-RECESYUBETU
           END-EVALUATE
      *
           .
       530-RECESYUBETU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       910-SYSKANRI-INV-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
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
       910-SYSKANRI-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理
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
      *    患者マスター読込（主キー）
      *****************************************************************
       900-PTINF-READ-SEC         SECTION.
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
               ELSE
                   MOVE    1                   TO  FLG-PTINF
                   INITIALIZE                      PTINF-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTINF
               INITIALIZE                      PTINF-REC
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
      *    請求マスター読込（主キー）
      *****************************************************************
       900-SEIKYU-INV-SEC         SECTION.
      *
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SEIKYU
           END-IF
      *
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
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
               PERFORM 900-KOHSKY-READ-SEC
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
      *    公費請求マスター読込
      *****************************************************************
       900-KOHSKY-READ-SEC           SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-KOHSKY
           ELSE
               MOVE    1               TO  FLG-KOHSKY
           END-IF
      *
           .
       900-KOHSKY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-INV-SEC         SECTION.
      *
           MOVE    HENREI-B-REC        TO  MCPDATA-REC
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_henrei_body"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-HENREI-BODY-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-HENREI-B
           END-IF
      *
           MOVE    "tbl_henrei_body"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-HENREI-BODY-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    オンライン返戻明細読込処理
      *****************************************************************
       900-HENREI-BODY-READ-SEC        SECTION.
      *
           PERFORM 900-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE  MCPDATA-REC           TO  HENREI-B-REC
      *********INITIALIZE                      HENREI-B-XX-AREA
               MOVE  ZERO                  TO  FLG-HENREI-B
           ELSE
               INITIALIZE                      HENREI-B-REC
               MOVE  1                     TO  FLG-HENREI-B
           END-IF
           .
       900-HENREI-BODY-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセ電ＤＢ読込
      *****************************************************************
       900-RECEDEN-INV-SEC             SECTION.
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_receden"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-RECEDEN
               ELSE
                   MOVE    1                   TO  FLG-RECEDEN
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECEDEN
           END-IF
      *
           MOVE    "tbl_receden"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-RECEDEN-INV-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者公費情報読込
      *****************************************************************
       900-PTKOHINF-READ-SEC           SECTION.
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTKOHINF-REC
                   MOVE    ZERO            TO  FLG-PTKOHINF
               ELSE
                   INITIALIZE                  PTKOHINF-REC
                   MOVE    1               TO  FLG-PTKOHINF
               END-IF
           ELSE
               INITIALIZE                  PTKOHINF-REC
               MOVE    1               TO  FLG-PTKOHINF
           END-IF
      *
           MOVE    "tbl_ptkohinf"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-PTKOHINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    月代わり受給者番号マスタ読込
      *****************************************************************
       900-MONTHLYNUM-READ-SEC         SECTION.
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-MONTHLYNUM
                   MOVE    MCPDATA-REC         TO  MONTHLYNUM-REC
               ELSE
                   MOVE    1                   TO  FLG-MONTHLYNUM
               END-IF
           ELSE
               MOVE    1                   TO  FLG-MONTHLYNUM
           END-IF
      *
           MOVE    "tbl_monthlynum"    TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-MONTHLYNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労働基準監督署名読込
      *****************************************************************
       900-LABOR-SIO-READ-SEC         SECTION.
      *
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-LABOR-SIO
                   MOVE    MCPDATA-REC         TO  LABOR-SIO-REC
               ELSE
                   MOVE    1                   TO  FLG-LABOR-SIO
               END-IF
           ELSE
               MOVE    1                   TO  FLG-LABOR-SIO
           END-IF
      *
           MOVE    "tbl_labor_sio"    TO  MCP-TABLE
           MOVE    "key"              TO  MCP-PATHNAME
           PERFORM 900-CLOSE-SEC
      *
           .
       900-LABOR-SIO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責ＰＤＦ読込
      *****************************************************************
       900-JIBAI-PRT-READ-SEC          SECTION.
      *
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
           PERFORM 900-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
               PERFORM 900-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO                TO  FLG-JIBAI-PRT
                   MOVE    MCPDATA-REC         TO  JIBAI-PRT-REC
               ELSE
                   MOVE    1                   TO  FLG-JIBAI-PRT
               END-IF
           ELSE
               MOVE    1                   TO  FLG-JIBAI-PRT
           END-IF
      *
           MOVE    "key"               TO  MCP-PATHNAME
           MOVE    "tbl_jibai_prt"     TO  MCP-TABLE
           PERFORM 900-CLOSE-SEC
      *
           .
       900-JIBAI-PRT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-H01NAI-PTID     TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    排他制御解除処理
           MOVE    2                   TO  SLOCK-MODE
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           .
       990-LOCK-OUT-EXT.
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
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"          TO  MCP-FUNC
           PERFORM 900-ORCDBMAIN-SEC
           .
      *
       900-PUT-WINDOW-EXT.
           EXIT.
