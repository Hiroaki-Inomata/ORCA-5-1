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
       PROGRAM-ID.             ORCGH99.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 請求管理
      *  コンポーネント名  : 請求管理−手書き訂正（Ｈ９９）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.01.01    MCC-藤原     01/09/21  JOINの修正      
      *  01.01.02    MCC-藤原     01/10/02  連番の追加      
      *  01.01.03    MCC-藤原     01/10/26  保険種別の記号修正
      *
      *  ２００２．１０改正対応 
      *  01.02.01    NACL-藤原    02/10/10  レセプト種別の出力の修正
      *  01.02.02    NACL-藤原    02/12/10  エラーメッセージの修正
      *  01.02.03    NACL-藤原    03/02/17  補助区分の追加
      *  01.02.04    NACL-藤原    03/05/28  補助区分の判定の修正
      *  01.02.05    NACL-藤原    04/01/26  排他制御処理追加
      *
      *  02.06.01    NACL-藤原    05/09/29  補助区分からの編集の修正
      *
      *  02.07.01    NACL-藤原    05/08/20  特別療養費対応
      *  02.07.02    NACL-藤原    05/10/31  MONFUNC対応
      *
      *  02.08.01    NACL-藤原    06/02/20  主科対応
      *
      *  03.03.01    NACL-藤原    06/09/27  ２００６年１０月改正対応
      *
      *  03.05.01    NACL-藤原    07/04/19  グループ診療対応
      *
      *  04.01.01    NACL-藤原    07/11/22  公害レセプト対応
      *
      *  04.02.01    NACL-藤原    08/03/25  平成２０年４月改正対応
      *
      *  04.03.01    NACL-藤原    08/05/07  公費４種までの併用の記録対応
      *  04.03.02    NACL-藤原    08/06/06  レセプト種別名称、作成日の追加
      *
      *  04.04.01    NACL-藤原    09/12/10  返戻対応
      *
      *  04.05.01    NACL-藤原    09/04/16  主科設定時の主科未設定のチェ
      *                                     ック追加
      *  04.05.02    NACL-藤原    11/08/25  公費単独対応
      *                                     公費を最大５０に変更
      *
      *  04.06.01    NACL-藤原    10/12/07  公費単独の負担者番号、受給者
      *                                     番号の表示に自立支援を追加
      *  04.06.02    NACL-藤原    11/02/01  主保険がない場合、児童保護(53)
      *                                     は公費単独のレセプトとする
      *  04.06.03    NACL-藤原    11/12/12  点検レセ０点公費印字対応
      *  04.06.04    NACL-藤原    12/10/26  特記事項０７（老併）及び
      *                                     ０８（老健）のレセプト対応
      *
      *  04.07.01    NACL-藤原    11/09/02  労災請求書作成対応
      *  04.07.02    NACL-藤原    13/09/20  第三者行為対応
      *
      *  04.08.01    NACL-藤原    16/08/08  災害該当分（非減免対象と減免対象）
      *                                     レセプトの分割対応
      *  04.08.02    NACL-藤原    17/04/10  患者単位での点数算定設定
      *                                     （労災準拠又は健保準拠）対応
      *  04.08.03    NACL-藤原    18/03/28  平成３０年４月改正対応
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
      *    請求管理
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
           03  FLG-MONTHLYNUM      PIC 9(01).
           03  FLG-PTKOHINF        PIC 9(01).
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
           03  IDX2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-Z9              PIC Z9.
           03  WRK-SELNO           PIC 9(02).
           03  WRK-RECESYUBETU     PIC X(44).
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
           03  WRK-SYURUINAME      PIC X(40).
      *
       01  WRK-ERRCD               PIC X(04).
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
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK2003.INC".
      *
           COPY    "CPRCF010.INC".
      *
      *    月代わり受給者番号
       01  MONTHLYNUM-REC.
           COPY    "CPMONTHLYNUM.INC".
      *
      *    患者公費情報
       01  PTKOHINF-REC.
           COPY    "CPPTKOHINF.INC".
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
           MOVE    SPA-ERRCD       TO  WRK-ERRCD
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
               IF      WRK-ERRCD           =   "0009"
                   PERFORM 210-BACK
                   GO  TO  100-INIT-EXT
               END-IF
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
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "H99    "           TO  MCP-WINDOW.
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
      *
           EVALUATE    SPA-MOTOPG
               WHEN    "HID1"
                   PERFORM 310-HID1-SET-SEC
                   GO      TO      300-SCREEN-EXT
           END-EVALUATE
      *
           MOVE    4                   TO  SPA-H99GMN-CUR
      *
           PERFORM 41021-H99-HENSYU-SEC
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面戻り処理
      *****************************************************************
       310-HID1-SET-SEC            SECTION.
      *
           IF      SPA-HID1-FLG    =   "OK"
               EVALUATE    SPA-HIDCD
                   WHEN    "1006"
                       PERFORM 210-BACK
               END-EVALUATE
           ELSE
               EVALUATE    SPA-HIDCD
                   WHEN    "1006"
      *****************MOVE    SPACE      TO  SPA-H99-AREA
      *****************INITIALIZE             SPA-H99GMN-SELNUM
                       MOVE    4          TO  SPA-H99GMN-CUR
               END-EVALUATE 
           END-IF
      *
           MOVE    SPACE           TO  SPA-HID1-FLG
           MOVE    SPACE           TO  SPA-HIDCD      
           .
       310-HID1-SET-EXT.
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
                   MOVE    SPACE      TO  SPA-H99-AREA
                   INITIALIZE             SPA-H99GMN-SELNUM
                   PERFORM 210-BACK
      *     確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM 450-KAKUTEI-SEC
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
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
                   PERFORM 4104-SELNUM-CHK-SEC
               WHEN    ANY             ALSO    "RECELST"
                   PERFORM 4105-RECESEL-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト検索画面編集処理
      *****************************************************************
       41021-H99-HENSYU-SEC         SECTION.
      *
      *    請求管理ＤＢ検索
           INITIALIZE                  RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    SPA-H99NAI-PTID     TO  RECE10-PTID
           IF      SPA-H99GMN-SRYKA    =   SPACE
               MOVE    "%%"                TO  RECE10-RECEKA
           ELSE
               MOVE    SPA-H99GMN-SRYKA    TO  RECE10-RECEKA
           END-IF
           MOVE    SPA-H99NAI-SRYYM    TO  RECE10-SRYYM
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
                       OR          IDX            =  50
               ADD     1                   TO  IDX
               MOVE    MCPDATA-REC     TO  RECE10-REC
               MOVE    RECE10-KEY      TO  SPA-H99NAI-RECE10-KEY   (IDX)
      *        点検レセ０点公費あり
               MOVE    RECE10-SKYKBN-RECEDEN
                                       TO  SPA-H99NAI-SKYKBN-RECEDEN
                                                                   (IDX)
      *  
               MOVE    IDX             TO  WRK-Z9
               MOVE    WRK-Z9          TO
                                   SPA-H99GMN-TRENNUM    (IDX)
               IF      RECE10-NYUGAIKBN    =   "1"
                   MOVE    "入  院"    TO
                                   SPA-H99GMN-TNYUGAIKBN (IDX)
               ELSE    
                   MOVE    "入院外"    TO
                                   SPA-H99GMN-TNYUGAIKBN (IDX)
               END-IF
               IF      RECE10-RECEKA   =   ZERO
                   CONTINUE
               ELSE
                   INITIALIZE              SYS-1005-REC
                   MOVE    "1005"          TO  SYS-1005-KANRICD
                   MOVE    RECE10-RECEKA   TO  SYS-1005-KBNCD
                   MOVE    SPA-SYSYMD      TO  SYS-1005-STYUKYMD
                                               SYS-1005-EDYUKYMD
grpsys             MOVE    SPA-HOSPNUM     TO  SYS-1005-HOSPNUM
                   MOVE    SYS-1005-REC    TO  MCPDATA-REC
                   PERFORM 910-SYSKANRI-INV-SEC
                   IF      FLG-SYSKANRI    =   ZERO
                       MOVE    MCPDATA-REC     TO  SYS-1005-REC
                   ELSE
                       INITIALIZE              SYS-1005-REC 
                   END-IF
                   MOVE    SYS-1005-SRYKANAME  TO
                                       SPA-H99GMN-TSRYKA(IDX)
               END-IF
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "1"
                       MOVE    "支払基金"      TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
                   WHEN    "2"
                       MOVE    "国保連合会"    TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
                   WHEN    "3"
                       MOVE    "労災"          TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
                   WHEN    "4"
                       MOVE    "自賠責"        TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
                   WHEN    "5"
                       MOVE    "公害"          TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
                   WHEN    "6"
                       MOVE    "広域連合"      TO
                                           SPA-H99GMN-TTEISYUTUSAKI(IDX)
               END-EVALUATE
      *
               PERFORM 530-RECESYUBETU-HENSYU-SEC
               MOVE    WRK-RECESYUBETU  TO
                                   SPA-H99GMN-TRECESYUBETU (IDX)
      *
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "6"
                       IF      RECE10-HKNJANUM     NOT =   "00000000"
                           MOVE    RECE10-HKNJANUM     TO
                                           SPA-H99GMN-THKNJANUM (IDX)
                       END-IF
                   WHEN    "3"
                       MOVE    RECE10-KOFUNUM      TO
                                           SPA-H99GMN-THKNJANUM (IDX)
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
                               MOVE    "職務上"    TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                           WHEN    "2"
                           WHEN    "B"
                           WHEN    "E"
                           WHEN    "H"
                               MOVE    "下船３月"  TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                           WHEN    "3"
                           WHEN    "C"
                           WHEN    "F"
                           WHEN    "I"
                               MOVE    "通勤災害"  TO
                                               SPA-H99GMN-THOJOKBN(IDX)
                       END-EVALUATE
               END-EVALUATE
               EVALUATE    RECE10-TEISYUTUSAKI
                   WHEN    "6"
                       MOVE    RECE10-NUM  TO  SPA-H99GMN-TKIGO(IDX)
                   WHEN    "3"
                   WHEN    "4"
                       MOVE    RECE10-COMMENT
                                           TO  SPA-H99GMN-TKIGO(IDX)
                   WHEN    OTHER
                       STRING  RECE10-KIGO     DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               RECE10-NUM      DELIMITED   BY  SPACE
                                       INTO    SPA-H99GMN-TKIGO(IDX)
                       END-STRING
               END-EVALUATE
               MOVE    RECE10-CREYMD       TO  WRK-SYMD
               PERFORM 5002-HIZUKE-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-H99GMN-TCREYMD (IDX)
      *
               EVALUATE    RECE10-RECEIPT-KBN (1:1)
                   WHEN    "1"
                       MOVE    "老併"          TO
                                           SPA-H99GMN-TRECEIPT-KBN (IDX)
                   WHEN    "2"
                       MOVE    "老健"          TO
                                           SPA-H99GMN-TRECEIPT-KBN (IDX)
                   WHEN    "3"
                       MOVE    "医併"          TO
                                           SPA-H99GMN-TRECEIPT-KBN (IDX)
                   WHEN    "4"
                       MOVE    "医療"          TO
                                           SPA-H99GMN-TRECEIPT-KBN (IDX)
               END-EVALUATE
               IF      RECE10-ETC-YKZFTN (1)   >   ZERO
                   MOVE    "災"            TO
                                           SPA-H99GMN-TDISASTER (IDX)
               END-IF
      *
      *       公費単独で生保又は中国残留邦人等、自立支援のとき負担者番号、
      *       受給者番号をセット
               IF    ( RECE10-HKNNUM       =   SPACE                 )
                   IF    ( RECE10-KOHNUM (1)   =   "012"   OR  "025"
                                               OR  "021"   OR  "020"
                                               OR  "015"   OR  "016"
                                               OR  "053"             )
                   AND   ( RECE10-KOHNUM (2)   =   SPACE             )
                       INITIALIZE                  PTKOHINF-REC
                       MOVE    SPA-HOSPNUM     TO  PTKOH-HOSPNUM
                       MOVE    RECE10-PTID     TO  PTKOH-PTID
                       MOVE    RECE10-KOHID(1) TO  PTKOH-KOHID
                       MOVE    PTKOHINF-REC    TO  MCPDATA-REC
                       PERFORM 900-PTKOHINF-READ-SEC
                       IF      RECE10-KOHNUM (1)   =   "012"   OR  "025"
                           MOVE    SPA-HOSPNUM     TO
                                                   MONTHLYNUM-HOSPNUM
                           MOVE    RECE10-PTID     TO  MONTHLYNUM-PTID
                           MOVE    RECE10-KOHID(1) TO  MONTHLYNUM-KOHID
                           MOVE    RECE10-SRYYM    TO  MONTHLYNUM-SRYYM
                           MOVE    RECE10-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                           MOVE    MONTHLYNUM-REC  TO  MCPDATA-REC
                           PERFORM 900-MONTHLYNUM-READ-SEC
                           IF      FLG-MONTHLYNUM  =   ZERO
                               MOVE    MONTHLYNUM-JKYSNUM
                                                   TO  PTKOH-JKYSNUM
                           ELSE
                               MOVE    SPA-HOSPNUM TO
                                                   MONTHLYNUM-HOSPNUM
                               MOVE    RECE10-PTID TO  MONTHLYNUM-PTID
                               MOVE    ZERO        TO  MONTHLYNUM-KOHID
                               MOVE    RECE10-SRYYM
                                                   TO  MONTHLYNUM-SRYYM
                               MOVE    RECE10-NYUGAIKBN
                                                   TO
                                                   MONTHLYNUM-NYUGAIKBN
                               MOVE    MONTHLYNUM-REC
                                                   TO  MCPDATA-REC
                               PERFORM 900-MONTHLYNUM-READ-SEC
                               IF      FLG-MONTHLYNUM  =   ZERO
                                   MOVE    MONTHLYNUM-JKYSNUM
                                                       TO  PTKOH-JKYSNUM
                               END-IF
                           END-IF
                       END-IF
                       STRING  PTKOH-FTNJANUM  DELIMITED   BY  SPACE
                               "　"            DELIMITED   BY  SIZE
                               PTKOH-JKYSNUM   DELIMITED   BY  SPACE
                                       INTO    SPA-H99GMN-TKIGO(IDX)
                       END-STRING
                   END-IF
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
           MOVE    IDX                 TO  SPA-H99GMN-MAX
      *
           MOVE    4                   TO  SPA-H99GMN-CUR
           .
       41021-H99-HENSYU-EXT.
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
      *
               INITIALIZE                      SYS-2003-REC
               MOVE    "2003"              TO  SYS-2003-KANRICD
               MOVE    SPA-HOSPNUM         TO  SYS-2003-HOSPNUM
               MOVE    RECE10-SRYYM        TO  SYS-2003-STYUKYMD (1:6)
               MOVE    "01"                TO  SYS-2003-STYUKYMD (7:2)
               MOVE    SYS-2003-STYUKYMD   TO  SYS-2003-EDYUKYMD
               MOVE    SYS-2003-REC        TO  MCPDATA-REC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
grpsys         PERFORM 900-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 900-SYSKANRI-READ-SEC
               ELSE
                   INITIALIZE                  SYS-2003-REC
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
      *
               PERFORM    UNTIL   FLG-SYSKANRI     =   1
                   MOVE    MCPDATA-REC   TO  SYS-2003-REC
                   IF    ( SYS-2003-TEISYUTUSAKI   =
                                           RECE10-TEISYUTUSAKI )
                   AND   ( SYS-2003-NYUGAIKBN      =
                                           RECE10-NYUGAIKBN    )
                   AND   ( SYS-2003-SYUBETU (1)    =
                                           RECE10-RECESYUBETU  )
                       MOVE    SYS-2003-SYURUINAME(1:6)
                                               TO  WRK-SYURUINAME
                       MOVE    5               TO  IDY
                       PERFORM VARYING IDZ FROM    7   BY  2
                               UNTIL   IDZ >       39
                               OR      SYS-2003-SYURUINAME (IDZ:2)
                                           =       "  "
                           IF      SYS-2003-SYURUINAME (IDZ:2)
                                                   =   "　"
                               CONTINUE
                           ELSE
                               ADD     2       TO  IDY
                               MOVE    SYS-2003-SYURUINAME(IDZ:2)
                                               TO  WRK-SYURUINAME(IDY:2)
                           END-IF
                       END-PERFORM    
                       MOVE    WRK-SYURUINAME  TO  WRK-RECESYUBETU(5:)
                       MOVE    1               TO  FLG-SYSKANRI
                   ELSE
                       MOVE    "tbl_syskanri"      TO  MCP-TABLE
                       MOVE    "key2"              TO  MCP-PATHNAME
                       PERFORM 900-SYSKANRI-READ-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-CLOSE-SEC
      *
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
      *    選択番号　入力処理
      *****************************************************************
       4104-SELNUM-CHK-SEC          SECTION.
      *
           MOVE    4               TO  SPA-H99GMN-CUR
      *
           MOVE    H99-SELNUM      TO  SPA-H99GMN-SELNUM
      *
           IF      SPA-H99GMN-SELNUM   <   1   OR
                   SPA-H99GMN-SELNUM   >   SPA-H99GMN-MAX
               MOVE    "0001"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-H99NAI-RECE10-KEY (SPA-H99GMN-SELNUM)
                                       TO  SPA-H99-KEY
      *        点検用レセ０点公費のとき入力不可
               IF      SPA-H99NAI-SKYKBN-RECEDEN  (SPA-H99GMN-SELNUM)
                                           =   "1"
                   MOVE    "0009"              TO  SPA-ERRCD
                   GO  TO  4104-SELNUM-CHK-EXT
               END-IF
      *        主科設定時の主科未設定のチェック（特別療養費を除く）
               IF    ( SPA-H99-RECEKA          =   "00"   )
               AND   ( SPA-H99-RECESYUBETU NOT =   "6666" )
                   EVALUATE    SPA-H99-TEISYUTUSAKI
                       WHEN    "1"
                           IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"                           
                               MOVE    "1006"         TO  SPA-HIDCD
                               GO  TO  4104-SELNUM-CHK-EXT
                           END-IF
                       WHEN    "2"
                           IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                               MOVE    "1006"         TO  SPA-HIDCD
                               GO  TO  4104-SELNUM-CHK-EXT
                           END-IF
                       WHEN    "6"
                           IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                               MOVE    "1006"         TO  SPA-HIDCD
                               GO  TO  4104-SELNUM-CHK-EXT
                           END-IF
                   END-EVALUATE
               END-IF
               PERFORM 210-BACK
           END-IF
           .
       4104-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    行選択処理
      *****************************************************************
       4105-RECESEL-SEC               SECTION.
      *        行選択
           MOVE    ZERO                TO  WRK-SELNO
      *     
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >       SPA-H99GMN-MAX
               IF      H99-SELECT (IDX)    =   "T"
                   MOVE    IDX             TO  WRK-SELNO
                   MOVE    SPA-H99GMN-MAX  TO  IDX
               END-IF    
           END-PERFORM
      *
           MOVE    WRK-SELNO           TO  SPA-H99GMN-SELNUM
      *
           MOVE    SPA-H99NAI-RECE10-KEY (SPA-H99GMN-SELNUM)
                                       TO  SPA-H99-KEY
      *    点検用レセ０点公費のとき入力不可
           IF      SPA-H99NAI-SKYKBN-RECEDEN  (SPA-H99GMN-SELNUM)
                                       =   "1"
               MOVE    "0009"              TO  SPA-ERRCD
               GO  TO  4105-RECESEL-EXT
           END-IF
      *    主科設定時の主科未設定のチェック（特別療養費を除く）
           IF    ( SPA-H99-RECEKA          =   "00"   )
           AND   ( SPA-H99-RECESYUBETU NOT =   "6666" )
               EVALUATE    SPA-H99-TEISYUTUSAKI
                   WHEN    "1"
                       IF     SPA-NAI-2009-SYUKA-SYAHO  =   "1"                           
                           MOVE    "1006"         TO  SPA-HIDCD
                           GO  TO  4105-RECESEL-EXT
                       END-IF
                   WHEN    "2"
                       IF      SPA-NAI-2009-SYUKA-KOKUHO =   "1"
                           MOVE    "1006"         TO  SPA-HIDCD
                           GO  TO  4105-RECESEL-EXT
                       END-IF
                   WHEN    "6"
                       IF      SPA-NAI-2009-SYUKA-KOUIKI =   "1"
                           MOVE    "1006"         TO  SPA-HIDCD
                           GO  TO  4105-RECESEL-EXT
                       END-IF
               END-EVALUATE
           END-IF
      *
           PERFORM 210-BACK
           .
       4105-RECESEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           MOVE    "H01"               TO  SPA-SAKIPG
           MOVE    "H99"               TO  SPA-MOTOPG
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
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
      *    出力処理
      *****************************************************************
       450-KAKUTEI-SEC                SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       SPA-H99GMN-MAX
               IF      H99-SELECT (IDX)    =   "T"
                   MOVE    SPA-H99NAI-RECE10-KEY (IDX)
                                           TO  SPA-H99-KEY
                   MOVE    SPA-H99GMN-MAX  TO  IDX 
               END-IF
           END-PERFORM
      * 
           MOVE    SPA-H99GMN-PTNUM    TO  SPA-H01GMN-PTNUM
           MOVE    SPA-H99NAI-PTID     TO  SPA-H01NAI-PTID
           MOVE    SPA-H99GMN-SRYYM    TO  SPA-H01GMN-SRYYM
           MOVE    SPA-H99NAI-SRYYM    TO  SPA-H01NAI-SRYYM
           MOVE    SPA-H99GMN-NAME     TO  SPA-H01GMN-NAME 
      *
           PERFORM 210-BACK
           .
       450-KAKUTEI-EXT.
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
               PERFORM 520-HIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE
           MOVE    "H99    "           TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC              SECTION.
      *
           INITIALIZE                   H99
           MOVE    SPA-H99GMN-PTNUM     TO  H99-PTNUM
           MOVE    SPA-H99GMN-NAME      TO  H99-NAME-VALUE
           MOVE    SPA-H99GMN-SRYYM     TO  H99-SRYYM
           MOVE    SPA-H99GMN-SRYKA     TO  H99-SRYKA
           MOVE    SPA-H99GMN-SRYKANAME TO
                                        H99-SRYKANAME-VALUE
      *
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SPA-H99GMN-MAX
                   OR      SPA-H99GMN-TNYUGAIKBN (IDX)
                                               =   SPACE
               MOVE    SPA-H99GMN-TRENNUM      (IDX)
                                   TO  H99-TRENNUM      (IDX)
               MOVE    SPA-H99GMN-TNYUGAIKBN   (IDX)
                                   TO  H99-TNYUGAIKBN   (IDX)
               MOVE    SPA-H99GMN-TSRYKA       (IDX)
                                   TO  H99-TSRYKA       (IDX)
               MOVE    SPA-H99GMN-TTEISYUTUSAKI(IDX)
                                   TO  H99-TTEISYUTUSAKI(IDX)
               MOVE    SPA-H99GMN-TRECESYUBETU (IDX)
                                   TO  H99-TRECESYUBETU (IDX)
               MOVE    SPA-H99GMN-THKNJANUM    (IDX)
                                   TO  H99-THKNJANUM    (IDX)
               MOVE    SPA-H99GMN-THOJOKBN     (IDX)
                                   TO  H99-THOJOKBN     (IDX)
               MOVE    SPA-H99GMN-TKIGO        (IDX)
                                   TO  H99-TKIGO        (IDX)
               MOVE    SPA-H99GMN-TCREYMD      (IDX)
                                   TO  H99-TCREYMD      (IDX)
               MOVE    SPA-H99GMN-TRECEIPT-KBN (IDX)
                                   TO  H99-TRECEIPT-KBN (IDX)
               MOVE    SPA-H99GMN-TDISASTER    (IDX)
                                   TO  H99-TDISASTER    (IDX)
               MOVE    IDX         TO  H99-COUNT
           END-PERFORM
      ***  MOVE    "T"              TO  H99-SELECT (1)
      *
           MOVE    SPA-H99GMN-SELNUM   TO  H99-SELNUM
      *            
           MOVE    "blue"              TO  H99-NAME-STYLE
                                           H99-SRYKANAME-STYLE
      *            
           IF      SPA-SYS-1001-HOSPSBT1   =   "0"
               MOVE    WIDGET-INSENSITIVE  TO  H99-SRYKA-STATE
           ELSE
               MOVE    WIDGET-NORMAL       TO  H99-SRYKA-STATE
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
         EVALUATE    SPA-H99GMN-CUR
             WHEN    4
                    MOVE  "SELNUM"       TO  MCP-WIDGET
             WHEN    12
                    MOVE  "B12"          TO  MCP-WIDGET   
         END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラーです"  TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "対象のレセプトは存在しません"
                                             TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE
                   "０点公費記載のレセプトです、更新処理はできません" 
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
           MOVE    "H99"                TO  SPA-MOTOPG
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
       520-HIDSET-SEC              SECTION.
      *
           MOVE    SPACE                TO  HID1
           INITIALIZE                       HID1
           MOVE    SPA-HIDCD            TO  HID1-ID1CODE
           EVALUATE    SPA-HIDCD
               WHEN    "1006"
                   MOVE    "主科未設定です。よろしいですか"
                                        TO  HID1-ID1MSG
           END-EVALUATE
      *
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "H99"                TO  SPA-MOTOPG
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
       520-HIDSET-EXT.
           EXIT.
      *
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
grpsys         PERFORM 900-DBFETCH-SEC
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
      *    システム管理マスタＲＥＡＤ
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
grpsys     PERFORM 900-DBFETCH-SEC
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
