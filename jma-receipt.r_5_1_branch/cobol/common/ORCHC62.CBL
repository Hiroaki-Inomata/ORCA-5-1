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
       PROGRAM-ID.             ORCHC62.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : お薬手帳印刷
      *  コンポーネント名  : お薬手帳印刷（ＨＣ６２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/03/05    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.06.00    NACL-多々納  11/02/22  用法コードコメント対応
      *  04.06.00    NACL-森脇    11/03/10  用量割合コード対応
      *  04.07.00    NACL-多々納  12/09/03  ＱＲコード対応
      *                                     商品名コード対応
      *  04.08.00    NACL-多々納  13/07/XX  電子版お薬手帳アプリ連携対応他
      *  04.08.00    NACL-多々納  14/07/30  一時ディレクトリ対応
      *  04.08.01    NACL-森脇    15/08/12  処方せん備考欄コメント表示
      *  04.08.02    NACL-森脇    15/09/14  注意事項・効能効果表示
      *  04.08.00    NACL-多々納  16/07/XX  API(GINBEE)対応
      *  04.08.00    NACL-多々納  16/11/XX  ＱＲコードver.2.1対応
      *  05.00.00    NACL-多々納  16/12/05  再印刷対応
      *  05.00.00    NACL-太田    17/06/09  API印刷対応
      *  05.01.01    NMED-太田    19/09/15  API-PDF印刷対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    印刷用データ
      **** SELECT  PRT-FILE        ASSIGN  HC01PARA
           SELECT  PRT-FILE        ASSIGN  ASGNPARA
                                   FILE    STATUS  IS  STS-PARA.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    印刷用データ
       FD  PRT-FILE.
       01  PRT-REC.
           03  PRT-PRTDATA         PIC X(50000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    印刷用データ 名称領域 
           COPY    "CPCOMMONPRT.INC".
      *v4.8
      *    一時デイレクトリ領域 
           COPY    "CPASGNPARA.INC".
      *
      *    帳票領域
           COPY    "HCM62.INC".
      *
      *    ＱＲ帳票領域
           COPY    "HCM62Q.INC".
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-TIME            PIC 9(08).
      *
      *    ステータス領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TNSP            PIC 9(01).
      *----(04.08.02)--ADD-START---
           03  FLG-YAKUJYO         PIC 9(01).
      *----(04.08.02)--ADD-END-----
      *
           03  FLG-KBN             PIC 9(01).
      *
           03  FLG-SP              PIC 9(01).
           03  FLG-SOURYO          PIC 9(01).
      *
           03  FLG-QROK            PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-PAGE            PIC 9(03).
           03  CNT-LINE            PIC 9(02).
           03  CNT-ZAI             PIC 9(03).
           03  CNT-SRY             PIC 9(02).
           03  CNT-KAISU           PIC 9(01).
      *
      *    インデックス領域
       01  IDX-AREA.
           03  IDX-STR             PIC 9(04).
           03  IDX-STR1            PIC 9(04).
      *
           03  IDX-ZAI             PIC 9(03).
           03  IDX-SRY             PIC 9(02).
           03  IDX-KBN             PIC 9(01).
      *H24.9
           03  IDX-QR1             PIC 9(04).
           03  IDX-QR2             PIC 9(04).
           03  IDX-QRB             PIC 9(04).
           03  IDX-QY              PIC 9(04).
           03  IDQ                 PIC 9(04).
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-HENSURYO        PIC X(11).
           03  WRK-SURYO           PIC 9(05)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(05).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-SURYO-X.
               05  WRK-SURYO-Z1    PIC ZZZZZ.
               05  WRK-SURYO-Z1-9  REDEFINES   WRK-SURYO-Z1
                                   PIC ZZZZ9.
               05  WRK-SURYO-Z2    PIC X(01).
               05  WRK-SURYO-Z3    PIC ZZZZZ.
      *
           03  WRK-ZZ9             PIC ZZ9.
           03  WRK-Z9              PIC Z9.
      *
           03  WRK-SRYCD           PIC X(09).
      *
           03  WRK-HEN-AREA.
               05  WRK-HENLINE     PIC X(54).
               05  WRK-HENNAIYO    PIC X(26).
      *R02.4   05  WRK-YKZNAME     PIC X(100).
               05  WRK-YKZNAME     PIC X(140).
      *R02.4   05  WRK-YOUHOU      PIC X(100).
               05  WRK-YOUHOU      PIC X(140).
               05  WRK-SURYOU      PIC X(11).
               05  WRK-TANI        PIC X(12).
               05  WRK-KAISUU      PIC X(03).
      *
      *----(04.08.02)--UPD-START---
           03  WRK-HENSYUDATA      PIC X(4000).
      *----(04.08.02)--UPD-END-----
      *
           03  WRK-SSTKIJUNCD      PIC X(05).
           03  WRK-HENKBN-G.
               05  WRK-HENKBN      PIC 9(08)V9(05)
                                               OCCURS  5.
      *
           03  WRK-YOURYO          PIC 9(05)V9(05).
           03  WRK-KAISU           PIC 9(01).
      *
      *H24.9
      *    ＱＲ編集
      *R2.403  WRK-TNS-NAME        PIC X(100).
           03  WRK-TNS-NAME        PIC X(140).
           03  WRK-YOH-NAME-G.
               05  WRK-YOH-NAME        OCCURS   5.
                   07  WRK-YOH-NAME-1  PIC X(02).
      *R02.4       07  WRK-YOH-NAME-2  PIC X(98).
                   07  WRK-YOH-NAME-2  PIC X(138).
           03  WRK-FILE-CNT        PIC 9(02).
           03  WRK-SYS-TIME        PIC 9(08).
      *    ファイル名
           03  WRK-CSV-FILE        PIC X(256).
           03  WRK-DIRECTORY       PIC X(256).
           03  WRK-DIRECTORY-E     PIC X(256).
           03  WRK-DRCD-E          PIC X(05).
      *ver.4.7
      **   03  WRK-SHO-NAME        PIC X(80).
           03  WRK-SHO-NAME        PIC X(140).
           03  WRK-SHO-TANINAME    PIC X(20).
      *
           03  WRK-QRKBN           PIC X(01).
      *H28.11
      *    QR ver2.1 対応
           03  WRK-QR-HOSPNAME     PIC X(120).
           03  WRK-QR-HOSPPOST     PIC X(08).
           03  WRK-QR-HOSPADRS     PIC X(800).
      *
      *H28.4
      *API
      *    医療機関名称編集
           03  WRK-AP-HOSPNAME-G.
               05  WRK-AP-HOSPNAME        PIC X(120).
               05  WRK-AP-HOSPNAME2       PIC X(120).
               05  WRK-AP-HOSPNAME3       PIC X(120).
           03  WRK-AP-HOSPADRS-G.
               05  WRK-AP-HOSPADRS        PIC X(200).
               05  WRK-AP-HOSPADRS2       PIC X(200).
               05  WRK-AP-HOSPADRS3       PIC X(200).
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *
      *    剤内容領域
       01  WRK-SRY-AREA.
           03  WRK-ZAIKAIKEI       PIC 9(03).
           03  WRK-SRYKBN          PIC X(02).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRY-TBL         OCCURS  50.
               05  WRK-SRYSURYO    PIC 9(05)V9(05).
               05  WRK-SRYKAISU    PIC 9(03).
      *R02.4   05  WRK-INPUTCOMENT PIC X(80).
               05  WRK-INPUTCOMENT PIC X(140).
               05  WRK-YOURYOU-TBL.
                   07  WRK-YOURYOU     PIC 9(05)V9(05)
                                       OCCURS   5.
      *
      *    印刷編集領域
       01  WRK-LINE-AREA.
           03  WRK-LINE-1.
               05  WRK-PTNAME      PIC X(54).
           03  WRK-LINE-2.
               05  FILLER          PIC X(06)   VALUE   "処方日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-SRYYMDG     PIC X(04).
               05  WRK-SRYYMDY     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "年".
               05  WRK-SRYYMDM     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "月".
               05  WRK-SRYYMDD     PIC X(02).
               05  FILLER          PIC X(02)   VALUE   "日".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  FILLER          PIC X(08)   VALUE   "保険医名".
               05  FILLER          PIC X(01)   VALUE   SPACE.
               05  WRK-DRNAME      PIC X(18).
           03  WRK-LINE-3.
               05  WRK-HOSPNAME    PIC X(54).
           03  WRK-LINE-4.
               05  WRK-HOSPTEL     PIC X(15).
           03  WRK-LINE-SEN.
               05  FILLER          PIC X(54)   VALUE
               "______________________________________________________".
           03  WRK-LINE-SEN2.
               05  FILLER          PIC X(54)   VALUE
               "   ___________________________________________________".
      *
      *
      *H24.9
      *    ＱＲ位置
       01  QR-FILE-NAME-PARA.
      *****03  FILLER              PIC X(05)   VALUE   "/tmp/".
           03  QR-FILE-HOSPNUM     PIC 9(02)   VALUE   ZERO.
           03  QR-FILE-FORM-ID     PIC X(05)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-TERMID      PIC X(32)   VALUE   SPACE.
           03  FILLER              PIC X(01)   VALUE   ".".
           03  QR-FILE-SYOHMS      PIC 9(08)   VALUE   ZERO.
           03  QR-FILE-SUFFIX.
               05  QR-FILE-TYPE    PIC X(04)   VALUE   SPACE.
               05  QR-FILE-ID      PIC 9(02)   VALUE   ZERO.
               05  QR-FILE-EXT     PIC X(24)   VALUE   SPACE.
           03  QR-FILE-SUFFIX-R    REDEFINES   QR-FILE-SUFFIX.
               05  QR-FILE-REGEX   PIC X(30).
      *v4.8
       01  QR-FILE-NAME            PIC X(1024).
      *
       01  KUHAKU-FILE-NAME        PIC X(512).
      *    空白の画像
       01  KUHAKU-NAME.
           03  FILLER          PIC X(05)   VALUE   "data/".
           03  FILLER          PIC X(15)   VALUE   "transparent.jpg".
      *　ＱＲ名称
       01  CONS-FORM-ID        PIC X(05)   VALUE   "HC62Q".
      *H24.9
      *
      *01  CONS-CSVDIR             PIC X(28)   VALUE
      *                            "/var/lib/jma-receipt/jahistc".
      *    プログラムオプションパラメタ
       01  PARA-AREA.
           03  PARA-COMMENT2           PIC X(01).
      *H24.9
           03  PARA-QRKBN              PIC X(01).
           03  PARA-SHOHIN             PIC X(01).
      *----(04.08.01)--ADD-START---
           03  PARA-BIKOU              PIC X(01).
      *----(04.08.01)--ADD-END-----
      *----(04.08.02)--ADD-START---
           03  PARA-KOUNOU             PIC X(01).
      *----(04.08.02)--ADD-END-----
      ***  03  PARA-CSVKBN             PIC X(01).
      ***  03  PARA-CSVDIR             PIC X(256).
      *H28.6
           03  PARA-API-DATA           PIC X(01).
           03  PARA-API-CUSTOM-ID      PIC X(30).
      *
           03  WRK-PARA-NAME           PIC X(25).
           03  WRK-PARA                PIC X(50).
           03  WRK-PARA2               PIC X(50).
      *
      *H28.6
      * API対応
      *    お薬手帳情報構造体
           COPY    "CPFMMNOTE.INC".
      *
           COPY    "CPORCSQRCSV62.INC" REPLACING   //ORCSQRCSV62//
                                       BY          //WQRCSV62//.
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
           COPY    "CPSYSKANRI.INC".
      *    医療機関情報−基本情報
           COPY    "CPSK1001.INC".
      *
      *    医療機関情報−所在地、連絡先
           COPY    "CPSK1002.INC".
      *
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    出力先プリンタ名割り当て情報
           COPY    "CPSK1031.INC".
      *
      *    医療機関編集情報
           COPY    "CPSK1900.INC".
      *    医療機関名称編集情報
           COPY    "CPSK1901.INC".
      *    医療機関住所編集情報
           COPY    "CPSK1902.INC".
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *
      *    点数付加情報マスタ
       01  TNSP-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *----(04.08.01)--ADD-START---
      *    薬情マスタ
           COPY    "CPYAKUJYO.INC".
      *----(04.08.01)--ADD-END-----
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
           COPY    "MCPDATA.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    印刷パラメタ
           COPY    "CPORCSMKPRT.INC".
      *
      *    プログラムオプション編集パラメタ
           COPY    "CPORCSPRGOPTION.INC".
      *
      *ＱＲ対応
      *    ＱＲデータ作成サブパラメタ
           COPY    "CPORCSQRCSV62.INC".
      *    ファイルデイレクトリ位置指定サブ
           COPY    "CPMKPASS.INC".
      *    ファイル存在チェック
           COPY    "CPORCSFILECHK.INC".
      *    ＣＳＶファイル作成パラメタ
      **** COPY    "CPORCSFILEENCODE.INC".
      *    ディレクトリ作成パラメタ
      **** COPY    "CPORCSMKDIR.INC".
      *
      *v4.8
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *H28.6
      *    API対応
           COPY    "CPORCRPMAIN.INC".
           COPY    "CPORCSBLOBIMPORT.INC".
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       COPY    "COMMON-SPA".
       COPY    "CPORCHC62.INC".
      *
      ******************************************************************
       PROCEDURE               DIVISION
                                   USING
                                   SPA-AREA
                                   ORCHC62AREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           PERFORM 100-INIT-SEC
      *
           PERFORM 200-MAIN-SEC
      *
           PERFORM 300-END-SEC
      *
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  SYS-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
      *H24.9
           INITIALIZE                  ORCSQRCSV62AREA
           MOVE    ZERO                TO  IDX-QR1
           MOVE    ZERO                TO  IDX-QRB
      *
           ACCEPT  SYS-TIME    FROM    TIME
      *    時間
           COMPUTE WRK-SYS-TIME        =   SYS-TIME
                                       +   ORCHC62-CNT
      *
      *    出力先プリンタ名割り当て情報
           PERFORM 900-1031-READ-SEC
      *
      *    医療機関情報−基本情報
           PERFORM 900-1001-READ-SEC
      *
      *    医療機関情報−所在地、連絡先
           PERFORM 900-1002-READ-SEC
      *
      *    医療機関編集情報
           PERFORM 900-1900-READ-SEC
      *
      *    職員情報
           PERFORM 900-1010-READ-SEC
      *
      *    プログラムオプション設定
           INITIALIZE                  PARA-AREA
      *    コメント２
           MOVE    "COMMENT2"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-COMMENT2
      *H24.9
      *    ＱＲ印刷
           MOVE    "QRKBN"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-QRKBN
      *    商品名記載
           MOVE    "SHOHIN"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-SHOHIN
      *----(04.08.01)--ADD-START---
      *    処方せん備考欄コメント記載
           MOVE    "BIKOU"         TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-BIKOU
      *----(04.08.01)--ADD-END-----
      *----(04.08.02)--ADD-START---
      *    効能効果・注意事項記載
           MOVE    "KOUNOU"        TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA(1:1)   TO  PARA-KOUNOU
      *----(04.08.02)--ADD-END-----
      *API
      *    ＡＰＩ返却データ（1:作成する 帳票印刷なし）
           MOVE    "API_DATA"      TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           IF    ( WRK-PARA(1:1)   =   "1" )
               MOVE    WRK-PARA(1:1)   TO  PARA-API-DATA
           END-IF
      *H28.6
           IF     (PARA-API-DATA       =   "1"       )
               CONTINUE
           ELSE
      *        API送信なし
               MOVE    "0"             TO  PARA-API-DATA
           END-IF
      *    HAORI対応
           IF     ( SPA-API-FLG    NOT =   ZERO )
               IF    ( SPA-PRTCONF     =   "2" OR "3" )
                   MOVE    ZERO        TO  PARA-API-DATA
               ELSE
                   MOVE    "1"         TO  PARA-API-DATA
               END-IF
           END-IF
      *
      *    ＡＰＩカスタムＩＤ
           MOVE    "API_CUSTOM_ID"     TO  WRK-PARA-NAME
           PERFORM 1001-OPTION-CALL-SEC
           MOVE    WRK-PARA2           TO  PARA-API-CUSTOM-ID
      *
      *****CSVファイル作成
      *    MOVE    "CSVKBN"        TO  WRK-PARA-NAME
      *    PERFORM 1001-OPTION-CALL-SEC
      *    MOVE    WRK-PARA(1:1)   TO  PARA-CSVKBN
      *    CSVファイルディレクトリ
      *    MOVE    "CSVDIR"        TO  WRK-PARA-NAME
      *    PERFORM 1001-OPTION-CALL-SEC
      *    MOVE    ORCSPRGOPTION-OT-PARA(1:)
      *                            TO  PARA-CSVDIR
      *
      *    IF     (PARA-QRKBN      =   "1")  AND
      *           (PARA-CSVKBN     =   "1")
      *        IF      PARA-CSVDIR     =   SPACE
      *            MOVE    CONS-CSVDIR     TO  WRK-DIRECTORY
      *        ELSE
      *            MOVE    PARA-CSVDIR     TO  WRK-DIRECTORY
      *            INITIALIZE                  ORCSMKDIRAREA
      *            MOVE    WRK-DIRECTORY   TO  MKDIR-DIRECTORY
      *            MOVE    "0755"          TO  MKDIR-MODE
      *            MOVE    ZERO            TO  MKDIR-RESULT
      *            CALL    "orcmkdir"   USING  ORCSMKDIRAREA
      *            IF      MKDIR-RESULT NOT =  ZERO
      *                MOVE    "0"             TO  PARA-CSVKBN
      *            END-IF
      *        END-IF
      *    ELSE
      *        MOVE    "0"             TO  PARA-CSVKBN
      *        MOVE    SPACE           TO  WRK-DIRECTORY
      **** END-IF
      *
      *ver.4.7
      *    ＱＲ印刷判定
           IF      ORCHC62-QRKBN       =   SPACE
      *        患者設定のない時、オプション設定
               MOVE    PARA-QRKBN          TO  WRK-QRKBN
           ELSE
      *        患者設定の印刷あり
               IF      ORCHC62-QRKBN       =   "1"
                   MOVE    "1"             TO  WRK-QRKBN
               END-IF
      *H28.11
      *        CSVなし
               IF      ORCHC62-QRKBN       =   "2"
                   MOVE    "2"             TO  WRK-QRKBN
               END-IF
           END-IF
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    プログラムオプションパラメタ取得処理
      *****************************************************************
       1001-OPTION-CALL-SEC        SECTION.
      *
           INITIALIZE                  ORCSPRGOPTIONAREA
           MOVE    "ORCHC62"       TO  ORCSPRGOPTION-PRGID
           MOVE    WRK-PARA-NAME   TO  ORCSPRGOPTION-IN-DATA
           CALL    "ORCSPRGOPTION"     USING
                                       SPA-AREA
                                       ORCSPRGOPTIONAREA
           MOVE    SPACE           TO  WRK-PARA
           IF      ORCSPRGOPTION-OT-TYPE   =   1
      *        半角で１文字目を値とする
               MOVE    ORCSPRGOPTION-OT-PARA(1:1)  TO  WRK-PARA(1:1)
           END-IF
      *    全桁
           MOVE    ORCSPRGOPTION-OT-PARA   TO  WRK-PARA2
      *
           .
       1001-OPTION-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    帳票ＨＥＡＤ情報編集処理
           PERFORM 210-HEAD-HENSYU-SEC
      *
      *    データがなくなるまで
           MOVE    ZERO            TO  CNT-ZAI
           PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                   UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *        帳票ＢＯＤＹ情報編集処理
               PERFORM 220-BODY-HENSYU-SEC
           END-PERFORM
      *
      *    データがなければ処理抜ける
           IF      CNT-ZAI         =   ZERO
               GO  TO  200-MAIN-EXT
           END-IF
      *
      *    帳票ＦＯＯＴ情報編集処理
           PERFORM 430-FOOT-SET-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
      *
      *----(04.08.02)--ADD-START---
      *    プログラムオプションの設定があれば
           IF      PARA-KOUNOU =   "1"
      *        効能効果・注意事項処理
               PERFORM 210-KOUNOU-MAIN-SEC
           END-IF
      *----(04.08.02)--ADD-END-----
      *
      *H24.9
      *    ＱＲ印刷
           MOVE    ORCSQRCSV62AREA     TO  WQRCSV62AREA
      *
           MOVE    ZERO                TO  FLG-QROK
           IF     (WRK-QRKBN           =   "1"
                                       OR  "2"  )
              OR  (ORCHC62-CSVKBN  NOT =   SPACE )
               PERFORM 500-QR-SYORI-SEC
           END-IF
      *H28.5
           IF     (PARA-API-DATA       =   "1"  )
            AND   (ORCHC62-PRTKBN  NOT =   "1"  )
      *        ＡＰＩ送信処理
               PERFORM 600-API-SYORI-SEC
           END-IF
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *----(04.08.02)--ADD-START---
      *****************************************************************
      *    効能効果・注意事項処理
      *****************************************************************
       210-KOUNOU-MAIN-SEC         SECTION.
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
      *    データがなくなるまで
           MOVE    ZERO            TO  CNT-ZAI
           PERFORM VARYING IDX-ZAI     FROM    1   BY  1
                   UNTIL ( IDX-ZAI     >   ORCHC62-ZAI-MAX  )
      *        帳票効能効果・注意事項情報編集処理
               PERFORM 220-KOUNOU-HENSYU-SEC
           END-PERFORM
      *
      *    帳票ＦＯＯＴ情報編集処理
           PERFORM 430-FOOT-SET-SEC
      *
      *    帳票印刷処理
           PERFORM 400-PRT-WRITE-SEC
           .
       210-KOUNOU-MAIN-EXT.
           EXIT.
      *****************************************************************
      *    帳票効能効果・注意事項情報編集処理
      *****************************************************************
       220-KOUNOU-HENSYU-SEC       SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
           IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
               GO  TO  220-KOUNOU-HENSYU-EXT
           END-IF
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
           MOVE    ZERO            TO  CNT-SRY
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-KOUNOU-NAIYO-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-KOUNOU-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
           .
       220-KOUNOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-KOUNOU-NAIYO-SEC        SECTION.
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *    薬剤のみ編集
           IF      WRK-SRYCD(1:1)  NOT =   "6"
               GO  TO  230-KOUNOU-NAIYO-EXT
           END-IF
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-KOUNOU-NAIYO-EXT
           END-IF
      *
      *    薬情マスタ検索
           PERFORM 900-YAKUJYO-READ-SEC
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
      *    １件目だけ番号表示
           IF      CNT-SRY         =   1
               MOVE    CNT-ZAI         TO  WRK-Z9
               MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
               MOVE    ")"             TO  WRK-HENLINE(3:1)
           ELSE
               MOVE    WRK-LINE-SEN2   TO  HCM62-SEN(CNT-LINE)
           END-IF
      *
      *    薬剤名編集
           MOVE    SPACE           TO  WRK-HENSYUDATA
           MOVE    TNS-NAME        TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
      *    効能編集処理
           IF      YAKUJYO-EFFECT  NOT =   SPACE
               MOVE    SPACE           TO  WRK-HENSYUDATA
               MOVE    "【効能効果】"  TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
               MOVE    SPACE           TO  WRK-HENSYUDATA
               MOVE    YAKUJYO-EFFECT  TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
      *
      *    注意事項編集処理
           IF      YAKUJYO-CAUTION NOT =   SPACE
               MOVE    SPACE           TO  WRK-HENSYUDATA
               MOVE    "【注意事項】"  TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
               MOVE    SPACE           TO  WRK-HENSYUDATA
               MOVE    YAKUJYO-CAUTION TO  WRK-HENSYUDATA
               PERFORM 250-HENSYU-SYORI-SEC
           END-IF
           .
       230-KOUNOU-NAIYO-EXT.
           EXIT.
      *----(04.08.02)--ADD-END-----
      *****************************************************************
      *    帳票ＨＥＡＤ情報編集処理
      *****************************************************************
       210-HEAD-HENSYU-SEC         SECTION.
      *
      *    診療日
           MOVE    ORCHC62-SRYYMD  TO  WRK-SYMD
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    LNK-DAY2-EDTYMD3-G  TO  WRK-SRYYMDG
           MOVE    LNK-DAY2-EDTYMD1-Y  TO  WRK-SRYYMDY
           MOVE    LNK-DAY2-EDTYMD1-M  TO  WRK-SRYYMDM
           MOVE    LNK-DAY2-EDTYMD1-D  TO  WRK-SRYYMDD
      *
      *    患者氏名
           MOVE    SPACE           TO  WRK-PTNAME
           IF      SPA-NAME        NOT =   SPACE
               STRING  SPA-NAME        DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           ELSE
               STRING  SPA-KANANAME    DELIMITED  BY  SPACE
                       "　様"          DELIMITED  BY  SIZE
                       INTO            WRK-PTNAME
               END-STRING
           END-IF
           INSPECT WRK-PTNAME          REPLACING  ALL "  "  BY  "　"
      *
      *    帳票ＨＥＡＤセット処理
           INITIALIZE                  HCM62
           MOVE    ZERO            TO  CNT-LINE
           PERFORM 410-HEAD-SET-SEC
      *
           .
       210-HEAD-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＢＯＤＹ情報編集処理
      *****************************************************************
       220-BODY-HENSYU-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-SOURYO
      *
      *----(04.08.01)--UPD-START---
      *    処方せん備考欄コメント表示しない場合
           IF      PARA-BIKOU      =   "0"
               IF      ORCHC62-SRYKBN(IDX-ZAI) =   "98"
                   GO  TO  220-BODY-HENSYU-EXT
               END-IF
           END-IF
      *----(04.08.01)--UPD-END-----
      *
           COMPUTE CNT-ZAI     =   CNT-ZAI     +   1
      *
      *    剤情報編集領域にセット
           INITIALIZE                  WRK-SRY-AREA
      *
           MOVE    ORCHC62-ZAIKAIKEI(IDX-ZAI)
                                   TO  WRK-ZAIKAIKEI
           MOVE    ORCHC62-SRYSYUKBN(IDX-ZAI)
                                   TO  WRK-SRYSYUKBN
           MOVE    ORCHC62-SRYKBN(IDX-ZAI)
                                   TO  WRK-SRYKBN
      *!
           MOVE    SPACE           TO  WRK-SSTKIJUNCD
           IF      ORCHC62-YSRYCD(IDX-ZAI) NOT =   SPACE
               MOVE    ORCHC62-YSRYCD(IDX-ZAI) TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
      *        回数カウント
               MOVE    ZERO            TO  CNT-KAISU
               PERFORM VARYING IDX-KBN     FROM    1   BY  1
                       UNTIL   IDX-KBN     >   5
                   IF      TNS-SSTKIJUNCD(IDX-KBN) =   1
                       COMPUTE CNT-KAISU   =   CNT-KAISU   +   1
                       MOVE    "1"         TO  WRK-SSTKIJUNCD(IDX-KBN:1)
                   END-IF
               END-PERFORM
           END-IF
      *
           MOVE    ZERO            TO  CNT-SRY
      *H24.9
      *    ＱＲ対応
           MOVE    ZERO                TO  IDX-QR2
      *
           PERFORM VARYING IDX-SRY     FROM    1   BY  1
                   UNTIL ( IDX-SRY     >   50  )
                   OR    ( ORCHC62-SRYCD(IDX-ZAI IDX-SRY)  =   SPACE )
      *        明細編集
               PERFORM 230-NAIYO-HENSYU-SEC
           END-PERFORM
      *
      *    剤情報があれば処理する
           IF      CNT-SRY         =   ZERO
               GO  TO  220-BODY-HENSYU-EXT
           END-IF
      *
      *    回数編集
           PERFORM 240-KAISUU-HENSYU-SEC
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *
           .
       220-BODY-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    内容編集処理
      *****************************************************************
       230-NAIYO-HENSYU-SEC        SECTION.
      *
      *ver.4.7
      *    商品名コード
           MOVE    SPACE               TO  WRK-SHO-TANINAME
           MOVE    SPACE               TO  WRK-SHO-NAME
           IF     (PARA-SHOHIN         =   "1"  )
             AND  (ORCHC62-SHO-SRYCD(IDX-ZAI IDX-SRY)
                                   NOT =   SPACE )
               MOVE    ORCHC62-SHO-SRYCD (IDX-ZAI IDX-SRY)
                                           TO  WRK-SRYCD
      *        点数検索
               PERFORM 900-TENSU-READ-SEC
               MOVE    TNS-TANINAME        TO  WRK-SHO-TANINAME
      ***      MOVE    TNS-NAME            TO  WRK-SHO-NAME
               MOVE    TNS-FORMALNAME      TO  WRK-SHO-NAME
           END-IF
      *
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYCD
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *     IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *           ( WRK-SRYCD(1:3)  NOT =   "001" )
      *         GO  TO  230-NAIYO-HENSYU-EXT
      *     END-IF
      *    減の文字は対象外
           IF      WRK-SRYCD   =   "820000047"
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
      *    点数検索
           PERFORM 900-TENSU-READ-SEC
      *
      *    点数なければ処理抜ける
           IF      FLG-TENSU       =   1
               GO  TO  230-NAIYO-HENSYU-EXT
           END-IF
      *
      *ver.4.7
      *    商品名コード
           IF      ORCHC62-SHO-SRYCD(IDX-ZAI IDX-SRY)  NOT =   SPACE
      *        名称と単位は商品名コードの内容とする
               IF      WRK-SHO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SHO-TANINAME    TO  TNS-TANINAME
               END-IF
               IF      WRK-SHO-NAME        NOT =   SPACE
                   MOVE    WRK-SHO-NAME        TO  TNS-NAME
               END-IF
           END-IF
      *!
      *
           COMPUTE CNT-SRY     =   CNT-SRY     +   1
      *
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYSURYO(CNT-SRY)
           MOVE    ORCHC62-SRYKAISU(IDX-ZAI IDX-SRY)
                                   TO  WRK-SRYKAISU(CNT-SRY)
           MOVE    ORCHC62-INPUTCOMENT(IDX-ZAI IDX-SRY)
                                   TO  WRK-INPUTCOMENT(CNT-SRY)
           MOVE    ORCHC62-YOURYOU-TBL(IDX-ZAI IDX-SRY)
                                   TO  WRK-YOURYOU-TBL(CNT-SRY)
      *
           INITIALIZE                  WRK-HEN-AREA
      *
      *    ＱＲ用
           MOVE    SPACE               TO  WRK-TNS-NAME
           MOVE    SPACE               TO  WRK-YOH-NAME-G
           MOVE    ZERO                TO  IDX-QY
      *
      *----(04.08.01)--UPD-START---
           IF      ORCHC62-SRYKBN(IDX-ZAI) NOT =   "98"
      *        １件目だけ番号表示
               IF      CNT-SRY         =   1
                   MOVE    CNT-ZAI         TO  WRK-Z9
                   MOVE    WRK-Z9          TO  WRK-HENLINE(1:2)
                   MOVE    ")"             TO  WRK-HENLINE(3:1)
               END-IF
           END-IF
      *----(04.08.01)--UPD-END-----
      *
           IF      TNS-SRYCD(1:3)  =   "001"
      *        用法編集
               PERFORM 240-YOUHOU-HENSYU-SEC
           ELSE
      *        名称編集
               PERFORM 240-YKZNAME-HENSYU-SEC
      *        数量編集
               PERFORM 240-SURYOU-HENSYU-SEC
               IF     (PARA-COMMENT2   =   "1"  )
               AND    (WRK-SRYCD(1:1)  =   "6"  )
      *            用量編集
                   PERFORM 240-YOURYOU-HENSYU-SEC
               END-IF
           END-IF
      *H24.9
      *    ＱＲ名称編集
           PERFORM 5001-QRMEISAI-HENSYU-SEC
      *
           .
       230-NAIYO-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用法編集処理
      *****************************************************************
       240-YOUHOU-HENSYU-SEC       SECTION.
      *
      *    コメント名称判定
           IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
               STRING  "【"            DELIMITED   BY  SIZE
                       TNS-NAME        DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           ELSE
               STRING  "【"            DELIMITED   BY  SIZE
                       WRK-INPUTCOMENT(CNT-SRY)
                                       DELIMITED   BY  SPACE
                       "】"            DELIMITED   BY  SIZE
                       INTO            WRK-YOUHOU
               END-STRING
           END-IF
      *    ＱＲ用
           MOVE    WRK-YOUHOU      TO  WRK-TNS-NAME
      *
           MOVE    WRK-YOUHOU      TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YOUHOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称編集処理
      *****************************************************************
       240-YKZNAME-HENSYU-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-YKZNAME
      *
      *    薬剤編集
           IF      TNS-SRYCD(1:1)  =   "6"
      *        内用薬の総量編集の判定
               IF      TNSP-SOURYOHENKBN   =   1
                   MOVE    1               TO  FLG-SOURYO
               END-IF
               MOVE    TNS-NAME            TO  WRK-YKZNAME
           ELSE
      *        コメントあればセット、なければ名称セット
               IF      WRK-INPUTCOMENT(CNT-SRY)    =   SPACE
                   MOVE    TNS-NAME        TO  WRK-YKZNAME
               ELSE
                   MOVE    WRK-INPUTCOMENT(CNT-SRY)
                                           TO  WRK-YKZNAME
               END-IF
           END-IF
      *    ＱＲ用
           MOVE    WRK-YKZNAME     TO  WRK-TNS-NAME
      *
           MOVE    WRK-YKZNAME     TO  WRK-HENSYUDATA
           PERFORM 250-HENSYU-SYORI-SEC
      *
           .
       240-YKZNAME-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       240-SURYOU-HENSYU-SEC       SECTION.
      *
      *    薬剤、その他材料、用法以外は処理抜ける
      *    IF    ( WRK-SRYCD(1:1)  NOT =   "6"   ) AND
      *          ( WRK-SRYCD(1:3)  NOT =   "059" ) AND
      *          ( WRK-SRYCD(1:3)  NOT =   "001" )
      *        GO  TO  240-SURYOU-HENSYU-EXT
      *****END-IF
      *
      *    薬剤、器材のみ
           IF    ( WRK-SRYCD(1:1)      =   "6"   )  OR
                 ( WRK-SRYCD(1:1)      =   "7"   )  OR
                 ( WRK-SRYCD(1:3)      =   "059" )
               CONTINUE
           ELSE
               GO  TO  240-SURYOU-HENSYU-EXT
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  WRK-SURYOU
           END-IF
      *
           STRING  " "             DELIMITED  BY  SIZE
                   WRK-SURYOU      DELIMITED  BY  SPACE
                   " "             DELIMITED  BY  SIZE
                   TNS-TANINAME    DELIMITED  BY  SPACE
                   INTO            WRK-HENNAIYO
           END-STRING
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-SURYOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数編集処理
      *****************************************************************
       240-KAISUU-HENSYU-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-HENNAIYO
      *
      *    内服で総量表示の薬剤のみの場合、外用と同様の表示とする
           IF    ( WRK-SRYKBN      =   "21" )  AND
                 ( FLG-SOURYO      =   1    )
               MOVE    "23"            TO  WRK-SRYKBN
           END-IF
      *
      *    内服か頓服でなければ処理抜ける
           IF    ( WRK-SRYKBN  NOT =   "21" )  AND
                 ( WRK-SRYKBN  NOT =   "22" )
               GO  TO  240-KAISUU-HENSYU-EXT
           END-IF
      *
           MOVE    WRK-ZAIKAIKEI   TO  WRK-SURYO
           IF      WRK-SURYO       >   0
               PERFORM 250-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO    TO  WRK-KAISUU
           END-IF
      *
           EVALUATE    WRK-SRYKBN
      *        内服
               WHEN    "21"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "日分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
      *        頓服
               WHEN    "22"
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-KAISUU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           "回分"          DELIMITED  BY  SPACE
                           INTO            WRK-HENNAIYO
                   END-STRING
           END-EVALUATE
      *
           PERFORM 250-MHEN-SYORI-SET-SEC
      *
           .
       240-KAISUU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    用量編集処理
      *****************************************************************
       240-YOURYOU-HENSYU-SEC       SECTION.
      *
      *    用量コード設定分セット
           MOVE    WRK-SRYSURYO(CNT-SRY)   TO  WRK-SURYO
           MOVE    CNT-KAISU       TO  WRK-KAISU
           MOVE    ZERO            TO  WRK-HENKBN-G
           MOVE    ZERO            TO  FLG-KBN
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
                   OR    ( FLG-KBN     =   1 )
      *        用量コードがあった場合
               IF      WRK-YOURYOU(CNT-SRY IDX-KBN)     >   ZERO
      *            指定してない服用時点ならエラー
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   NOT  =   "1"
                       MOVE    1               TO  FLG-KBN
                   ELSE
      *                用量の方が多い場合エラー
                       IF      WRK-SURYO      <
                                   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           MOVE    1               TO  FLG-KBN
                       ELSE
                           MOVE    WRK-YOURYOU(CNT-SRY IDX-KBN) 
                                               TO  WRK-HENKBN(IDX-KBN)
      *                    用量コード使用分は計算にいれない
                           COMPUTE WRK-SURYO  =   WRK-SURYO
                               -   WRK-YOURYOU(CNT-SRY IDX-KBN) 
                           COMPUTE WRK-KAISU   =   WRK-KAISU   -   1
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *    エラーなら処理抜ける
           IF      FLG-KBN  =   1
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
      *    入力分をセット
           COMPUTE WRK-YOURYO   =   WRK-SURYO  /   WRK-KAISU
      *    割切れない場合処理抜ける
           IF      WRK-SURYO    =   WRK-YOURYO *   WRK-KAISU
               PERFORM VARYING  IDX-KBN    FROM    1   BY  1
                       UNTIL  ( IDX-KBN    >   5 )
      *            服用時点コードで設定していない場合は割ったものをセット
                   IF      WRK-SSTKIJUNCD(IDX-KBN:1)   =   "1"
                       IF      WRK-HENKBN(IDX-KBN)     =   0
                           MOVE    WRK-YOURYO  TO  WRK-HENKBN(IDX-KBN)
                       END-IF
                   END-IF
               END-PERFORM
           ELSE
               GO  TO  240-YOURYOU-HENSYU-EXT
           END-IF
      *
           PERFORM VARYING IDX-KBN     FROM    1   BY  1
                   UNTIL ( IDX-KBN     >   5 )
      *        用量コードがあった場合
               IF      WRK-HENKBN(IDX-KBN)     >   ZERO
                   MOVE    SPACE       TO  WRK-HENSYUDATA
      *
                   EVALUATE    IDX-KBN
                       WHEN    1
                           MOVE    "　朝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    2
                           MOVE    "　昼："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    3
                           MOVE    "　夕："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    4
                           MOVE    "　寝："    TO  WRK-HENSYUDATA(1:6)
                       WHEN    5
                           MOVE    "　起："    TO  WRK-HENSYUDATA(1:6)
                   END-EVALUATE
      *
                   MOVE    WRK-HENKBN(IDX-KBN)
                                               TO  WRK-SURYO
                   PERFORM 250-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO        TO  WRK-SURYOU
      *
                   STRING  " "             DELIMITED  BY  SIZE
                           WRK-SURYOU      DELIMITED  BY  SPACE
                           " "             DELIMITED  BY  SIZE
                           TNS-TANINAME    DELIMITED  BY  SPACE
                           INTO            WRK-HENSYUDATA(7:)
                   END-STRING
      *H24.9
      *            ＱＲ　飲み方コメント
                   ADD     1                   TO  IDX-QY
                   MOVE    WRK-HENSYUDATA      TO  WRK-YOH-NAME(IDX-QY)
      *
                   PERFORM 250-HENSYU-SYORI-SEC
               END-IF
           END-PERFORM
      *
           .
       240-YOURYOU-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    頁チェック　処理
      *****************************************************************
       250-PAGE-CHK-SEC            SECTION.
      *
      *    ページ件数を超えていたら印刷
           IF      CNT-LINE    =   68
               PERFORM 400-PRT-WRITE-SEC
               INITIALIZE                  HCM62
               MOVE    ZERO            TO  CNT-LINE
               PERFORM 410-HEAD-SET-SEC
           ELSE
      *        ページ件数を超えていたら改ページ
               IF      CNT-LINE    =   34
                   PERFORM 410-HEAD-SET-SEC
               END-IF
           END-IF
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
      *
           .
       250-PAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票Ｚ編集処理
      *****************************************************************
       250-ZHEN-SURYO-SET-SEC      SECTION.
      *
      *    数量Ｚ編集
           MOVE    SPACE           TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1
      *
      *    数量小数点以下編集
           MOVE    ZERO            TO  FLG-SP
           PERFORM VARYING IDX-STR     FROM    5   BY  -1
                   UNTIL   IDX-STR     <   1
               IF      WRK-SURYO-S2(IDX-STR:1) NOT =   ZERO
                   MOVE    1           TO  FLG-SP
               END-IF
               IF      FLG-SP      =   1
                   MOVE    WRK-SURYO-S2(IDX-STR:1)
                                        TO  WRK-SURYO-Z3(IDX-STR:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3    NOT =   SPACE
               MOVE    "."             TO  WRK-SURYO-Z2
               IF  WRK-SURYO-X(1:5)    =   SPACE
                   MOVE    WRK-SURYO-S1    TO  WRK-SURYO-Z1-9
               END-IF
           END-IF
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  IDX-STR1
           PERFORM VARYING IDX-STR     FROM    1   BY  1
                   UNTIL   IDX-STR     >   11
               IF      WRK-SURYO-X(IDX-STR:1)  NOT =   SPACE
                   COMPUTE IDX-STR1    =   IDX-STR1    +   1
                   MOVE  WRK-SURYO-X(IDX-STR:1)
                                           TO  WRK-HENSURYO(IDX-STR1:1)
               END-IF
           END-PERFORM
      *
           .
       250-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-MHEN-SYORI-SET-SEC      SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    0              TO  IDX-STR1
      *
      *    最大文字を超えるか編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   23 )
                   OR     (WRK-HENNAIYO(IDX-STR:)    =   SPACE)
               ADD     1               TO  IDX-STR
               ADD     1               TO  IDX-STR1
           END-PERFORM
      *
           COMPUTE IDX-STR     =   54  -   IDX-STR1
      *
      *    文字が重なる場合は１行下に表示する
           IF    ( HCM62-NAIYO2(CNT-LINE)              NOT =   SPACE )
           OR    ( HCM62-NAIYO1(CNT-LINE)(IDX-STR:)    NOT =   SPACE )
               PERFORM 250-PAGE-CHK-SEC
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           ELSE
               MOVE    WRK-HENNAIYO    TO  HCM62-NAIYO2(CNT-LINE)
           END-IF
      *
           .
       250-MHEN-SYORI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票文字編集処理
      *****************************************************************
       250-HENSYU-SYORI-SEC        SECTION.
      *
      *    初期値セット
           MOVE    1              TO  IDX-STR
           MOVE    4              TO  IDX-STR1
      *
      *----(04.08.02)--UPD-START---
      *    編集データがなくなるまで繰り返す
           PERFORM UNTIL  (IDX-STR     >   4000 )
                   OR     (IDX-STR1    >   54 )
                   OR     (WRK-HENSYUDATA(IDX-STR:)    =   SPACE)
      *        改行
               IF      WRK-HENSYUDATA(IDX-STR:1)   =   X"0A"
                   ADD     1               TO  IDX-STR
               ELSE
                   MOVE    WRK-HENSYUDATA(IDX-STR:2)
                                       TO  WRK-HENLINE(IDX-STR1:2)
                   ADD     2               TO  IDX-STR
                   ADD     2               TO  IDX-STR1
               END-IF
      *        最大字で改行
               IF         (IDX-STR1        >=  54)
                   OR    ((IDX-STR         <=  4000)
                   AND    (WRK-HENSYUDATA(IDX-STR:) =   SPACE))
                   PERFORM 250-PAGE-CHK-SEC
                   MOVE    WRK-HENLINE     TO  HCM62-NAIYO1(CNT-LINE)
                   MOVE    SPACE           TO  WRK-HENLINE
                   MOVE    4               TO  IDX-STR1
               END-IF
           END-PERFORM
      *----(04.08.02)--UPD-END-----
      *
           .
       250-HENSYU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       400-PRT-WRITE-SEC           SECTION.
      *
      *ver.4.7
      *    ＣＳＶ出力のみは印刷なし
           IF      ORCHC62-PRTKBN      =   "1"
               GO      TO      400-PRT-WRITE-EXT
           END-IF
      *H28.6
      *    ＡＰＩ送信は印刷なし
           IF      PARA-API-DATA       =   "1"
               GO      TO      400-PRT-WRITE-EXT
           END-IF
      *
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
      *
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62           TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62.red"     TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
      *****MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(16)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"         USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       400-PRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       410-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE    =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62-NAIYO2(CNT-LINE)
           END-STRING
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-SEN    TO  HCM62-SEN(CNT-LINE)
      *
           .
       410-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票ＦＯＯＴセット処理
      *****************************************************************
       430-FOOT-SET-SEC            SECTION.
      *
      *    医療機関名称セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-3      TO  HCM62-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           PERFORM 250-PAGE-CHK-SEC
           MOVE    WRK-LINE-4      TO  HCM62-NAIYO1(CNT-LINE)
      *
           .
       430-FOOT-SET-EXT.
           EXIT.
      *
      *H24.9
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       5001-QRMEISAI-HENSYU-SEC            SECTION.
      *
           IF      ORCHC62-SRYKBN (IDX-ZAI)    =   "98"
      *        備考コメント
               PERFORM 50012-QR-BIKOU-HEN-SEC
           ELSE
      *        ＱＲ剤編集
               PERFORM 50011-QR-MEISAI-HEN-SEC
           END-IF
      *
           .
       5001-QRMEISAI-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ剤編集処理
      *****************************************************************
       50011-QR-MEISAI-HEN-SEC            SECTION.
      *
           IF      IDX-QR2             =   ZERO
               ADD     1                   TO  IDX-QR1
      *
      *        剤回数
               MOVE    ORCHC62-ZAIKAIKEI (IDX-ZAI)
                                       TO  ORCSQRCSV62-KAISU (IDX-QR1)
      *        診療区分
               MOVE    ORCHC62-SRYKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYKBN (IDX-QR1)
      *        診療種別
               MOVE    ORCHC62-SRYSYUKBN (IDX-ZAI)
                                       TO  ORCSQRCSV62-SRYSYUKBN
                                                              (IDX-QR1)
           END-IF
      *
           ADD     1                   TO  IDX-QR2
           MOVE    ORCHC62-SRYCD(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
           MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-TANINAME        TO  ORCSQRCSV62-TANINAME
                                                       (IDX-QR1 IDX-QR2)
           MOVE    ORCHC62-SRYSURYO(IDX-ZAI IDX-SRY)
                                       TO  ORCSQRCSV62-SURYOU
                                                       (IDX-QR1 IDX-QR2)
           MOVE    TNS-YKZKBN          TO  ORCSQRCSV62-YKZKBN
                                                       (IDX-QR1 IDX-QR2)
           IF      TNSP-SOURYOHENKBN   =   1
      *        内服点剤
               MOVE    "1"             TO  ORCSQRCSV62-NAIHENKBN
                                                       (IDX-QR1 IDX-QR2)
           END-IF
           IF      ORCHC62-SRYCD(IDX-ZAI IDX-SRY)(1:3) =   "001"
      *        用法コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  ORCSQRCSV62-YCOMMNTKBN
                                                  (IDX-QR1 IDX-QR2)
           END-IF
      *
      *    飲み方コメント
           PERFORM VARYING     IDX-QY  FROM    1   BY  1
                   UNTIL      (IDX-QY      >   5   )
                           OR (WRK-YOH-NAME (IDX-QY)   =   SPACE )
                           OR (IDX-QR2     >=   50   )
               ADD     1                   TO  IDX-QR2
               MOVE    "810000001"         TO  ORCSQRCSV62-SRYCD
                                                       (IDX-QR1 IDX-QR2)
               MOVE    WRK-YOH-NAME-2(IDX-QY)
                                           TO  ORCSQRCSV62-TNS-NAME
                                                       (IDX-QR1 IDX-QR2)
           END-PERFORM
      *
           .
       50011-QR-MEISAI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ備考コメント処理
      *****************************************************************
       50012-QR-BIKOU-HEN-SEC            SECTION.
      *
           ADD     1                   TO  IDX-QRB
           IF      IDX-QRB              <=  10
               MOVE    WRK-TNS-NAME        TO  ORCSQRCSV62-BIKOU
                                                       (IDX-QRB)
           END-IF
           .
       50012-QR-BIKOU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＱＲ印刷処理
      *****************************************************************
       500-QR-SYORI-SEC            SECTION.
      *
           MOVE    ORCHC62-SRYYMD      TO  ORCSQRCSV62-SRYYMD
      *    医療機関情報
           MOVE    SYS-1001-PREFNUM    TO  ORCSQRCSV62-PREFNUM
           MOVE    SYS-1001-HOSPCD     TO  ORCSQRCSV62-HOSPCD
           MOVE    SYS-1001-HOSPSBT    TO  ORCSQRCSV62-HOSPSBT
      ***  MOVE    WRK-HOSPNAME        TO  ORCSQRCSV62-HOSPNAME
           MOVE    WRK-QR-HOSPNAME     TO  ORCSQRCSV62-HOSPNAME
      *H28.11
      *    医療機関電話番号
           MOVE    WRK-QR-HOSPPOST     TO  ORCSQRCSV62-HOSPPOST
           MOVE    WRK-QR-HOSPADRS     TO  ORCSQRCSV62-HOSPADRS
           MOVE    WRK-HOSPTEL         TO  ORCSQRCSV62-HOSPTEL
      *    ドクター
           MOVE    SYS-1010-KANANAME   TO  ORCSQRCSV62-DR-KANANAME
           MOVE    SYS-1010-NAME       TO  ORCSQRCSV62-DR-NAME
           MOVE    ORCHC62-DRCD        TO  ORCSQRCSV62-DRCD
      *    患者情報
           MOVE    SPA-NAME            TO  ORCSQRCSV62-NAME
           MOVE    SPA-KANANAME        TO  ORCSQRCSV62-KANANAME
           MOVE    SPA-SEX             TO  ORCSQRCSV62-SEX
           MOVE    SPA-BIRTHDAY        TO  ORCSQRCSV62-BIRTHDAY
      *    ファイル名
           MOVE    CONS-FORM-ID        TO  ORCSQRCSV62-FORM-ID
      *    ファイル名組み立て項目
           MOVE    WRK-SYS-TIME        TO  ORCSQRCSV62-SYSTIME
      *
           IF      ORCHC62-CSVKBN  NOT =   SPACE
      *H28.11      CSVなし
              AND (WRK-QRKBN       NOT =   "2"   )
      *        ＣＳＶ保存ファイル名 
               PERFORM 50034-QRCSVFILE-SEC
               MOVE    WRK-CSV-FILE        TO  ORCSQRCSV62-CSV-FILENAME
           END-IF
      *
           MOVE    ORCHC62-CSVKBN      TO  ORCSQRCSV62-CSV-KBN
           MOVE    WRK-QRKBN           TO  ORCSQRCSV62-QR-KBN
      *H28.11
           IF      WRK-QRKBN           =   "2"
               MOVE    "1"             TO  ORCSQRCSV62-QR-KBN
           END-IF
           MOVE    ORCHC62-CNT         TO  ORCSQRCSV62-CNT
           MOVE    ORCHC62-PGKBN       TO  ORCSQRCSV62-PGKBN
      *
           CALL    "ORCSQRCSV62"       USING
                                       SPA-AREA
                                       ORCSQRCSV62AREA
      *
      *    ＱＲイメージ印刷
           IF     (ORCSQRCSV62-RET-COUNT   >   ZERO)
             AND  (ORCSQRCSV62-RET-COUNT   <=  12  )
             AND  (WRK-QRKBN               =   "1"
                                           OR  "2" )
             AND  (ORCHC62-PRTKBN          =   SPACE)
               PERFORM 5003-QRIMAGE-HEN-SEC
           END-IF
           .
       500-QR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *        ＱＲイメージ印刷処理
      *****************************************************************
       5003-QRIMAGE-HEN-SEC      SECTION.
      *
      *    ＱＲ空白画像位置
           INITIALIZE                  ORCSMKPASSAREA
           MOVE    KUHAKU-NAME         TO  MKPASS-IN-01
           CALL    "ORCSMKPASS"        USING
                                       ORCSMKPASSAREA
           MOVE    MKPASS-OT-01        TO  KUHAKU-FILE-NAME
      *
           MOVE    SPACE               TO  HCM62Q
           MOVE    ZERO                TO  CNT-LINE
      *
           MOVE    ZERO                TO  WRK-FILE-CNT
           PERFORM VARYING     IDQ     FROM    1  BY  1
                   UNTIL       IDQ     >   12
               MOVE    KUHAKU-FILE-NAME    TO  HCM62Q-IMAGE-1 (IDQ)
               ADD     1                   TO  WRK-FILE-CNT
               IF      WRK-FILE-CNT        <=  ORCSQRCSV62-RET-COUNT 
                   MOVE    SPA-HOSPNUM         TO  QR-FILE-HOSPNUM
                   MOVE    CONS-FORM-ID        TO  QR-FILE-FORM-ID
                   MOVE    SPA-TERMID          TO  QR-FILE-TERMID
                   MOVE    WRK-SYS-TIME        TO  QR-FILE-SYOHMS
                   MOVE    ".qr_"              TO  QR-FILE-TYPE
                   MOVE    WRK-FILE-CNT        TO  QR-FILE-ID
                   MOVE    ".png"              TO  QR-FILE-EXT
      *
      *v4.8
      *            一時ファイル名取得
                   INITIALIZE                      SGETTEMP-AREA
                   MOVE    QR-FILE-NAME-PARA   TO  SGETTEMP-BASENAME
                   CALL    "ORCSGETTEMP"       USING
                                                   SGETTEMP-AREA
                   MOVE    SGETTEMP-FULLNAME   TO  QR-FILE-NAME
      *
      *            ファイル存在チェック
                   INITIALIZE                      ORCSFILECHKAREA
                   MOVE    QR-FILE-NAME        TO  FILECHK-NAME
                   CALL    "ORCSFILECHK"       USING
                                               ORCSFILECHKAREA
                   IF      FILECHK-RC          =   ZERO OR 2
                       MOVE   QR-FILE-NAME     TO  HCM62Q-IMAGE-1 (IDQ)
                       MOVE   1                TO  FLG-QROK
                   END-IF
      *
                   IF      WRK-FILE-CNT        =   1
      *                帳票ＨＥＡＤセット処理
                       PERFORM 50010-HEAD-SET-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      PARA-API-DATA       =   "1"
               CONTINUE
           ELSE
               IF  FLG-QROK            =   1
      *            帳票出力処理
                   PERFORM 50031-QRPRT-WRITE-SEC
               END-IF
           END-IF
           .
       5003-QRIMAGE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    帳票ＨＥＡＤセット処理
      *****************************************************************
       50010-HEAD-SET-SEC            SECTION.
      *
           COMPUTE CNT-PAGE        =   CNT-PAGE    +   1
           MOVE    CNT-PAGE        TO  WRK-ZZ9
      *
           COMPUTE CNT-LINE        =   CNT-LINE    +   1
           MOVE    WRK-LINE-1      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    SPACE           TO  HCM62Q-NAIYO2(CNT-LINE)
           STRING  WRK-ZZ9         DELIMITED  BY  SIZE
                   " 頁"           DELIMITED  BY  SIZE
                   INTO            HCM62Q-NAIYO2(CNT-LINE)
           END-STRING
      *
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-2      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *
           MOVE    4               TO   CNT-LINE
           MOVE    WRK-LINE-SEN    TO  HCM62Q-SEN(CNT-LINE)
      *    医療機関名称セット
           MOVE    WRK-LINE-3      TO  HCM62Q-NAIYO1(CNT-LINE)
      *
      *    医療機関電話番号セット
           COMPUTE CNT-LINE    =   CNT-LINE    +   1
           MOVE    WRK-LINE-4      TO  HCM62Q-NAIYO1(CNT-LINE)
           .
       50010-HEAD-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    帳票出力処理
      *****************************************************************
       50031-QRPRT-WRITE-SEC           SECTION.
      *
      *    印刷用ファイル作成
           MOVE    SPA-HOSPNUM     TO  HC01PARA-HOSPNUM
           MOVE    "HC62"          TO  HC01PARA-FORM-ID
           MOVE    SPA-TERMID      TO  HC01PARA-TERMID
           MOVE    SYS-TIME        TO  HC01PARA-SYOHMS
           MOVE    ORCHC62-CNT     TO  HC01PARA-CNT(1:3)
           MOVE    CNT-PAGE        TO  HC01PARA-CNT(4:3)
      *v4.8
      *    一時ファイル名取得
           INITIALIZE                      SGETTEMP-AREA
           MOVE    HC01PARA            TO  SGETTEMP-BASENAME
           CALL    "ORCSGETTEMP"       USING
                                           SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAME   TO  ASGNPARA
      *
           OPEN    OUTPUT              PRT-FILE
           MOVE    SPACE           TO  PRT-REC
      *
      *    印刷用データ出力
           MOVE    HCM62Q          TO  PRT-PRTDATA
           WRITE                       PRT-REC
      *
           CLOSE                       PRT-FILE
      *
      *    帳票印刷処理
           INITIALIZE                  ORCSMKPRTAREA
           MOVE    "HC01.sh"       TO  MKPRT-ID
           MOVE    "HCM62Q.red"    TO  MKPRT-DIA
           MOVE    SPACE           TO  MKPRT-DEF
           MOVE   "pdf"            TO  MKPRT-TYPE
     ***** MOVE    HC01PARA        TO  MKPRT-DAT
           MOVE    ASGNPARA        TO  MKPRT-DAT
           MOVE    SYS-1031-PRTNM(16)
                                   TO  MKPRT-PRTNM
           MOVE   "お薬手帳"       TO  MKPRT-INFO
           CALL   "ORCSMKPRT1"          USING
                                       ORCSMKPRTAREA
                                       SPA-AREA
      *
           .
       50031-QRPRT-WRITE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＣＳＶデータの変換　処理
      *****************************************************************
       50034-QRCSVFILE-SEC             SECTION.
      *
           IF      ORCHC62-DRCD        =   SPACE
               MOVE    "XXXXX"             TO  WRK-DRCD-E
           ELSE
               MOVE    ORCHC62-DRCD        TO  WRK-DRCD-E
           END-IF
      *
      *    ファイル名作成
           MOVE    SPACE               TO  WRK-CSV-FILE
           STRING  "okusuri_techo_"
                   ORCHC62-SRYYMD      DELIMITED   BY  SIZE
                   "_"
                   WRK-DRCD-E          DELIMITED   BY  SIZE
                   "_"
                   SPA-SYSYMD          DELIMITED   BY  SIZE
                   "_"
                   WRK-SYS-TIME        DELIMITED   BY  SIZE
                   ".csv"
                                       INTO    WRK-CSV-FILE
           END-STRING
      *
      *    ディレクトリ作成（医療機関識別番号）
      *    INITIALIZE                      ORCSMKDIRAREA
      *    MOVE    SPACE               TO  WRK-DIRECTORY-E
      *    STRING  WRK-DIRECTORY       DELIMITED   BY  SPACE
      *            "/"
      *            SPA-HOSPNUM         DELIMITED   BY  SIZE
      *                                INTO    WRK-DIRECTORY-E
      *    END-STRING
      *    MOVE    WRK-DIRECTORY-E     TO  MKDIR-DIRECTORY
      *    MOVE    "0755"              TO  MKDIR-MODE
      *    MOVE    ZERO                TO  MKDIR-RESULT
      *    CALL    "orcmkdir"          USING   ORCSMKDIRAREA
      *    IF      MKDIR-RESULT     NOT =  ZERO
      *        MOVE    "0"                 TO  PARA-CSVKBN
      *        GO  TO  50034-QRCSVFILE-EXT
      *    END-IF
      *
      *    ディレクトリ作成（診療日）
      *    INITIALIZE                      ORCSMKDIRAREA
      *    STRING  WRK-DIRECTORY-E     DELIMITED   BY  SPACE
      *            "/"
      *            SPA-SRYYMD          DELIMITED   BY  SIZE
      *                                INTO    MKDIR-DIRECTORY
      *    END-STRING
      *
      *    MOVE    MKDIR-DIRECTORY     TO  WRK-DIRECTORY-E
      *
      *    MOVE    "0755"              TO  MKDIR-MODE
      *    MOVE    ZERO                TO  MKDIR-RESULT
      *    CALL    "orcmkdir"          USING   ORCSMKDIRAREA
      *    IF      MKDIR-RESULT     NOT =  ZERO
      *        MOVE    "0"                 TO  PARA-CSVKBN
      *        GO  TO  50034-QRCSVFILE-EXT
      *    END-IF
      *    ファイル名作成
      *    MOVE    SPACE               TO  WRK-CSV-FILE
      *    STRING  WRK-DIRECTORY-E     DELIMITED   BY  SPACE
      *            "/JAHISTC"
      *            SPA-HOSPNUM         DELIMITED   BY  SIZE
      *            SPA-PTNUM           DELIMITED   BY  SPACE
      *            "-"
      *            "2"
      *            SPA-SRYYMD          DELIMITED   BY  SIZE
      *            WRK-DRCD-E          DELIMITED   BY  SIZE
      *            SPA-SYSYMD          DELIMITED   BY  SIZE
      *            WRK-SYS-TIME        DELIMITED   BY  SIZE
      *            ".csv"
      *                                INTO    WRK-CSV-FILE
      **** END-STRING
      *
           .
       50034-QRCSVFILE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信処理
      *****************************************************************
       600-API-SYORI-SEC                 SECTION.
      *
      *    項目編集
           PERFORM 6001-API-HENSYU-SEC
      *    カスタムＩＤ
           MOVE    PARA-API-CUSTOM-ID  TO  FMMNOTE-CUSTOM-ID
      *
           INITIALIZE                      RPMAIN-AREA
           MOVE    "okusuri_techo"     TO  RPMAIN-ID
           MOVE    PARA-API-CUSTOM-ID  TO  RPMAIN-CUSTOM-ID
           MOVE    "お薬手帳"          TO  RPMAIN-TITLE
           MOVE    "fm_medicine_notebook"
                                       TO  MCP-TABLE
           MOVE    FMMNOTE-DATA        TO  RPMAIN-DATA
           CALL    "ORCRPMAIN"         USING
                                       MCPAREA
                                       RPMAIN-AREA
                                       SPA-AREA
      *
           .
       600-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＡＰＩ送信編集処理
      *****************************************************************
       6001-API-HENSYU-SEC                 SECTION.
      *
           INITIALIZE                  FMMNOTE-REC
      *
           MOVE    "okusuri_techo"     TO  FMMNOTE-FORM-ID
           MOVE    SYS-1031-PRTNM(16)  TO  FMMNOTE-PRINTER
      *
      *    診療日 YYYY-MM-DD
           MOVE    ORCHC62-SRYYMD      TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMMNOTE-SRYYMD
      *    伝票番号
           MOVE    ORCHC62-DENPNUM     TO  FMMNOTE-DENPNUM
      *    患者情報
      *    患者番号
           MOVE    SPA-PTNUM           TO  FMMNOTE-PTNUM
      *    氏名
           MOVE    SPA-NAME            TO  FMMNOTE-NAME
           MOVE    SPA-KANANAME        TO  FMMNOTE-KANANAME
      *    性別区分
           EVALUATE    SPA-SEX
           WHEN    "男"
               MOVE    "1"             TO  FMMNOTE-SEX
           WHEN    "女"
               MOVE    "2"             TO  FMMNOTE-SEX
           END-EVALUATE
      *    誕生日
           MOVE    SPA-BIRTHDAY        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  FMMNOTE-BIRTHDAY
      *    医療機関情報
      *    県番
           MOVE    SYS-1001-PREFNUM    TO  FMMNOTE-PREFNUM
      *    名称
           MOVE    WRK-AP-HOSPNAME     TO  FMMNOTE-HOSP-NAME(1)
           MOVE    WRK-AP-HOSPNAME2    TO  FMMNOTE-HOSP-NAME(2)
           MOVE    WRK-AP-HOSPNAME3    TO  FMMNOTE-HOSP-NAME(3)
      *    住所
           MOVE    WRK-AP-HOSPADRS     TO  FMMNOTE-HOSP-ADRS(1)
           MOVE    WRK-AP-HOSPADRS2    TO  FMMNOTE-HOSP-ADRS(2)
           MOVE    WRK-AP-HOSPADRS3    TO  FMMNOTE-HOSP-ADRS(3)
      *    電話番号
           MOVE    WRK-HOSPTEL         TO  FMMNOTE-HOSP-TEL
      *    ドクター
           MOVE    SYS-1010-NAME       TO  FMMNOTE-DR-NAME
           MOVE    ORCHC62-DRCD        TO  FMMNOTE-DR-CODE
      *
      *    明細
           MOVE    WQRCSV62AREA        TO  ORCSQRCSV62AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   50   )
                           OR (ORCSQRCSV62-SRYKBN (IDX)  =   SPACE )
      *        診療区分
               MOVE    ORCSQRCSV62-SRYKBN (IDX)
                                       TO  FMMNOTE-SRYKBN (IDX)
      *        剤回数
               MOVE    ORCSQRCSV62-KAISU (IDX)
                                       TO  WRK-SURYO
               IF      WRK-SURYO       >   0
                   PERFORM 250-ZHEN-SURYO-SET-SEC
                   MOVE    WRK-HENSURYO    TO  WRK-KAISUU
                   MOVE    WRK-KAISUU      TO  FMMNOTE-ZAIKAISU
                                                           (IDX)
               END-IF
      *        剤単位名（回数／回分）
               EVALUATE    ORCSQRCSV62-SRYKBN (IDX)
                   WHEN    "21"
                       MOVE    "日分"          TO  FMMNOTE-ZAITANINAME
                                                           (IDX)
                   WHEN    "22"
                       MOVE    "回分"          TO  FMMNOTE-ZAITANINAME
                                                           (IDX)
               END-EVALUATE
      *        診療明細
               PERFORM VARYING     IDY     FROM    1   BY    1
                       UNTIL     ( IDY     >   50   )
                           OR    (ORCSQRCSV62-TNS-NAME (IDX IDY)
                                                   =   SPACE )
      *            薬剤名
                   MOVE    ORCSQRCSV62-TNS-NAME (IDX IDY)
                                               TO  FMMNOTE-YKZNAME
                                                           (IDX IDY)
      *            数量(ZZZZ9.99999)
                   IF     (ORCSQRCSV62-SRYCD (IDX IDY)(1:1)
                                               =   "6"
                                               OR  "7"   )
                      OR  (ORCSQRCSV62-SRYCD (IDX IDY)(1:3)
                                               =   "059" )
                       MOVE    ORCSQRCSV62-SURYOU (IDX IDY)
                                               TO  WRK-SURYO
                       PERFORM 250-ZHEN-SURYO-SET-SEC
                       MOVE    WRK-HENSURYO    TO  FMMNOTE-SURYO
                                                           (IDX IDY)
                   END-IF
      *            単位名
                   MOVE    ORCSQRCSV62-TANINAME (IDX IDY)
                                               TO  FMMNOTE-TANINAME
                                                           (IDX IDY)
      *            診療コード
                   MOVE    ORCSQRCSV62-SRYCD (IDX IDY)
                                               TO  FMMNOTE-SRYCD
                                                           (IDX IDY)
      *            内服点剤
                   IF      ORCSQRCSV62-NAIHENKBN (IDX IDY)
                                               =   "1"
                       MOVE    "1"             TO  FMMNOTE-NAIHENKBN
                                                           (IDX)
                   END-IF
               END-PERFORM
      *        内服点剤ありは、単位名なし
               IF      FMMNOTE-NAIHENKBN (IDX) =   "1"
                   MOVE    SPACE               TO  FMMNOTE-ZAITANINAME
                                                           (IDX)
               END-IF
           END-PERFORM
      *    備考
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   10   )
                           OR (ORCSQRCSV62-BIKOU (IDX)  =   SPACE )
               MOVE    ORCSQRCSV62-BIKOU (IDX)
                                       TO  FMMNOTE-MEMO (IDX)
           END-PERFORM
      *
      *    QR
           IF    ( FLG-QROK            =   1 )
                INITIALIZE                      SBI-AREA
                MOVE    SPA-HOSPNUM         TO  QR-FILE-HOSPNUM
                MOVE    CONS-FORM-ID        TO  QR-FILE-FORM-ID
                MOVE    SPA-TERMID          TO  QR-FILE-TERMID
                MOVE    WRK-SYS-TIME        TO  QR-FILE-SYOHMS
                MOVE    "\.qr(.*[0-9][0-9]\.png|\.dat)$"
                                            TO  QR-FILE-REGEX
                MOVE    QR-FILE-NAME-PARA   TO  SBI-AREA
                CALL    "ORCSBLOBIMPORT"    USING
                                            SBI-AREA
                                            SPA-AREA
                MOVE    SBI-BLOB-ID         TO  FMMNOTE-QR-ID
           END-IF
      *
           .
       600-API-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦日本語変換処理
      *****************************************************************
       31012-SEIWA-HEN-SEC         SECTION.
      *
           INITIALIZE                  STS-AREA-DAY
           INITIALIZE                  LNK-DAY2-AREA
           MOVE    "21"            TO  LNK-DAY2-IRAI
           MOVE    WRK-SYMD        TO  LNK-DAY2-YMD
           CALL    "ORCSDAY"           USING
                                       STS-AREA-DAY
                                       LNK-DAY2-AREA
      *     MOVE    LNK-DAY2-EDTYMD3
      *                             TO  WRK-HENYMDG
      *     INSPECT WRK-HENYMDG         REPLACING  ALL "  "  BY  "　"
      *
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           IF      WRK-SYMD            =   SPACE
               MOVE    SPACE           TO  WRK-HEN-DATE
           ELSE
               INITIALIZE                  WRK-HEN-DATE
               MOVE    WRK-SYY             TO  WRK-HEN-YY
               MOVE    WRK-SMM             TO  WRK-HEN-MM
               MOVE    WRK-SDD             TO  WRK-HEN-DD
               MOVE    "-"                 TO  WRK-HEN-H1
               MOVE    "-"                 TO  WRK-HEN-H2
               IF      WRK-SYMD            =   "99999999"
                   MOVE    "12"                TO  WRK-HEN-MM
                   MOVE    "31"                TO  WRK-HEN-DD
               END-IF
           END-IF
      *
           .
       801-DAYHEN01-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ読み込み
      *****************************************************************
       900-TENSU-READ-SEC          SECTION.
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    WRK-SRYCD       TO  TNS-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC   TO  MCPDATA-REC
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensu"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TENSU
                   MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               ELSE
                   MOVE    1               TO  FLG-TENSU
               END-IF
           ELSE
               MOVE    1               TO  FLG-TENSU
           END-IF
      *
           MOVE    "tbl_tensu"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
      *    点数附加情報検索
           IF    ( TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )  AND
                 ( FLG-TENSU           =   ZERO )
               PERFORM 900-TNSP-READ-SEC
           ELSE
               INITIALIZE                      TNSP-REC
           END-IF
      *
           .
       900-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数付加情報読み込み
      *****************************************************************
       900-TNSP-READ-SEC           SECTION.
      *
           INITIALIZE                  TNSP-REC
           MOVE    WRK-SRYCD       TO  TNSP-SRYCD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOSTYMD
           MOVE    ORCHC62-SRYYMD  TO  TNSP-YUKOEDYMD
           MOVE    SPA-HOSPNUM     TO  TNSP-HOSPNUM
           MOVE    TNSP-REC        TO  MCPDATA-REC
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_tensuplus" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-TNSP
                   MOVE    MCPDATA-REC     TO  TNSP-REC
               ELSE
                   MOVE    1               TO  FLG-TNSP
               END-IF
           ELSE
               MOVE    1               TO  FLG-TNSP
           END-IF
      *
           MOVE    "tbl_tensuplus" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
              AND (FLG-TNSP            =   ZERO  )
              AND (TNSP-USERTANICD NOT =   SPACE )
                MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
           END-IF
           .
       900-TNSP-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み
      *****************************************************************
       900-SYSKANRI-READ-SEC       SECTION.
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key10"         TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    MCPDATA-REC     TO  SYSKANRI-REC
                   MOVE    ZERO            TO  FLG-SYSKANRI
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key10"         TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−基本情報読み込み
      *****************************************************************
       900-1001-READ-SEC           SECTION.
      *
      *    医療機関情報−基本情報読み込み
           MOVE    SPACE           TO  SYS-1001-REC
           INITIALIZE                  SYS-1001-REC
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SYS-1001-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1001-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1001-REC
           ELSE
               INITIALIZE                  SYS-1001-REC
           END-IF
      *
           MOVE    SYS-1001-HOSPNAME   TO  WRK-HOSPNAME
      *API
           MOVE    SYS-1001-HOSPNAME   TO  WRK-AP-HOSPNAME
      *QR
           MOVE    SYS-1001-HOSPNAME   TO  WRK-QR-HOSPNAME
      *
           .
       900-1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関情報−所在地、連絡先読み込み
      *****************************************************************
       900-1002-READ-SEC           SECTION.
      *
      *    医療機関情報−所在地、連絡先読み込み
           MOVE    SPACE           TO  SYS-1002-REC
           INITIALIZE                  SYS-1002-REC
           MOVE    "1002"          TO  SYS-1002-KANRICD
           MOVE    "*"             TO  SYS-1002-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1002-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1002-HOSPNUM
           MOVE    SYS-1002-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1002-REC
           ELSE
               INITIALIZE                  SYS-1002-REC
           END-IF
      *
           MOVE    SYS-1002-TEL        TO  WRK-HOSPTEL
      *API
           MOVE    SYS-1002-ADRS       TO  WRK-AP-HOSPADRS
      *QR
           MOVE    SYS-1002-POST       TO  WRK-QR-HOSPPOST
           MOVE    SYS-1002-ADRS       TO  WRK-QR-HOSPADRS
           .
       900-1002-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    職員情報読み込み
      *****************************************************************
       900-1010-READ-SEC           SECTION.
      *
      *    職員情報読み込み
           MOVE    SPACE           TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"          TO  SYS-1010-KANRICD
           MOVE    ORCHC62-DRCD    TO  SYS-1010-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1010-REC
           ELSE
               INITIALIZE                  SYS-1010-REC
           END-IF
      *
           MOVE    SYS-1010-NAME   TO  WRK-DRNAME
      *
           .
       900-1010-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    出力先プリンタ名割り当て情報読み込み
      *****************************************************************
       900-1031-READ-SEC           SECTION.
      *
      *    出力先プリンタ名割り当て情報編集
           INITIALIZE                  SYS-1031-REC
           INITIALIZE                  ORCSPRTNMAREA
           MOVE    "2"             TO  ORCSPRTNM-KBN
           MOVE    ORCHC62-SRYYMD  TO  ORCSPRTNM-SRYYMD
           MOVE    "2"             TO  ORCSPRTNM-NYUGAIKBN
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1031-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               INITIALIZE                  SYS-1031-REC
           END-IF
      *
           .
       900-1031-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関編集情報読み込み
      *****************************************************************
       900-1900-READ-SEC           SECTION.
      *
      *    医療機関編集情報読み込み
           MOVE    SPACE           TO  SYS-1900-REC
           INITIALIZE                  SYS-1900-REC
           MOVE    "1900"          TO  SYS-1900-KANRICD
           MOVE    "*"             TO  SYS-1900-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1900-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1900-HOSPNUM
           MOVE    SYS-1900-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1900-REC
               IF      SYS-1900-PRTKBN(18) NOT =   SPACE
      *            医療機関名称編集情報
                   PERFORM 900-1901-READ-SEC
      *            医療機関住所編集情報
                   PERFORM 900-1902-READ-SEC
               END-IF
           ELSE
               INITIALIZE                  SYS-1900-REC
           END-IF
      *
           .
       900-1900-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関名称編集情報読み込み
      *****************************************************************
       900-1901-READ-SEC           SECTION.
      *
      *    医療機関名称編集情報読み込み
           MOVE    SPACE           TO  SYS-1901-REC
           INITIALIZE                  SYS-1901-REC
           MOVE    "1901"          TO  SYS-1901-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1901-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1901-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1901-HOSPNUM
           MOVE    SYS-1901-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1901-REC
               MOVE    SYS-1901-HOSPNAME1
                                       TO  WRK-HOSPNAME
      *API
               MOVE    SPACE           TO  WRK-AP-HOSPNAME-G
               MOVE    SYS-1901-HOSPNAME1  TO  WRK-AP-HOSPNAME
               MOVE    SYS-1901-HOSPNAME2  TO  WRK-AP-HOSPNAME2
               MOVE    SYS-1901-HOSPNAME3  TO  WRK-AP-HOSPNAME3
      *QR
               MOVE    SPACE           TO  WRK-QR-HOSPADRS
               STRING  SYS-1901-HOSPNAME1
                       SYS-1901-HOSPNAME2
                       SYS-1901-HOSPNAME3
                                       DELIMITED  BY  SPACE
                                       INTO   WRK-QR-HOSPNAME
               END-STRING
           ELSE
               INITIALIZE                  SYS-1901-REC
           END-IF
      *
           .
       900-1901-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関住所編集情報読み込み
      *****************************************************************
       900-1902-READ-SEC           SECTION.
      *
      *    医療機関住所編集情報読み込み
           MOVE    SPACE           TO  SYS-1902-REC
           INITIALIZE                  SYS-1902-REC
           MOVE    "1902"          TO  SYS-1902-KANRICD
           MOVE    SYS-1900-PRTKBN(18)
                                   TO  SYS-1902-KBNCD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-STYUKYMD
           MOVE    ORCHC62-SRYYMD  TO  SYS-1902-EDYUKYMD
           MOVE    SPA-HOSPNUM     TO  SYS-1902-HOSPNUM
           MOVE    SYS-1902-REC    TO  MCPDATA-REC
           PERFORM 900-SYSKANRI-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC    TO  SYS-1902-REC
               MOVE    SYS-1902-TEL    TO  WRK-HOSPTEL
      *API
               MOVE    SPACE           TO  WRK-AP-HOSPADRS-G
               MOVE    SYS-1902-ADRS1  TO  WRK-AP-HOSPADRS
               MOVE    SYS-1902-ADRS2  TO  WRK-AP-HOSPADRS2
               MOVE    SYS-1902-ADRS3  TO  WRK-AP-HOSPADRS3
      *QR
               IF      SYS-1902-POST   NOT =   SPACE
                   MOVE    SYS-1902-POST   TO  WRK-QR-HOSPPOST
               END-IF
               IF    ( SYS-1902-ADRS1      =   SPACE )
                 AND ( SYS-1902-ADRS2      =   SPACE )
                 AND ( SYS-1902-ADRS3      =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  WRK-QR-HOSPADRS
                   STRING  SYS-1902-ADRS1
                           SYS-1902-ADRS2
                           SYS-1902-ADRS3
                                       DELIMITED  BY  SPACE
                                       INTO   WRK-QR-HOSPADRS
                   END-STRING
               END-IF
           ELSE
               INITIALIZE                  SYS-1902-REC
           END-IF
      *
           .
       900-1902-READ-EXT.
           EXIT.
      *----(04.08.02)--ADD-START---
      *****************************************************************
      *    薬情マスタ読み込み
      *****************************************************************
       900-YAKUJYO-READ-SEC        SECTION.
      *
           INITIALIZE                  YAKUJYO-REC
           MOVE    SPA-HOSPNUM     TO  YAKUJYO-HOSPNUM
           MOVE    TNS-SRYCD       TO  YAKUJYO-SRYCD
           MOVE    1               TO  YAKUJYO-TEKNUM
           MOVE    YAKUJYO-REC     TO  MCPDATA-REC
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC          =   ZERO
               MOVE    "tbl_yakujyo"   TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-YAKUJYO
                   MOVE    MCPDATA-REC     TO  YAKUJYO-REC
               ELSE
                   MOVE    1               TO  FLG-YAKUJYO
               END-IF
           ELSE
               MOVE    1               TO  FLG-YAKUJYO
           END-IF
      *
           MOVE    "tbl_yakujyo"   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           PERFORM 930-DBCLOSE-SEC
      *
           .
       900-YAKUJYO-READ-EXT.
           EXIT.
      *----(04.08.02)--ADD-END-----
      *****************************************************************
      *    ＤＢ読込開始処理
      *****************************************************************
       910-DBSELECT-SEC            SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込検索処理
      *****************************************************************
       920-DBFETCH-SEC             SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ読込終了処理
      *****************************************************************
       930-DBCLOSE-SEC             SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       930-DBCLOSE-EXT.
           EXIT.
      *
