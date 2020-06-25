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
       IDENTIFICATION                            DIVISION.
       PROGRAM-ID.                               ORCGZ04.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ医薬品設定画面（Ｚ０４）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.00.02    MCC-多々納   01/07/18  正式名称等追加
      *  01.01.03    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.04    MCC-藤原     01/10/31  項目をコンボボックスに変更
      *                                     単位名称を削除      
      *  01.01.05    MCC-多々納   02/03/17  後発医薬品追加
      *  01.01.06    MCC-多々納   02/07/18  服用方法画面追加
      *  01.01.07    NACL-多々納  02/10/09  半角空白→全角空白追加
      *  01.01.08    NACL-太田    03/08/05  分類区分追加
      *  01.01.09    NACL-多々納  04/12/22  一括入力へ
      *                                     入力ＣＤのみ変更可能へ
      *  01.01.10    NACL-多々納  05/12/12  登録コードチェック追加
      *                                     MONFUNC対応 他
      *  01.01.11    NACL-多々納  06/03/07  経過措置追加
      *  03.04.00    NACL-多々納  06/12/08  長期投与・投与量設定
      *                                     一般名等追加
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  04.05.00    NACL-太田    10/01/15  月上限回数、月総投与量設定追加
      *  04.06.00    NACL-多々納  12/08/16  期間変更警告追加
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  05.00.00    NACL-多々納  17/01/XX  ユーザー設定他GINBEE対応
      *  05.00.00    NACL-多々納  19/03/XX  点数識別区分＝５対応
      *  05.01.00    NACL-多々納  20/02/XX  ユーザー単位表示対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
       DATA                                      DIVISION.
       FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    ユーザー設定ＳＰＡエリア
           COPY    "Z010SCR-SPA".
      *
       01  SPA-Z04.
      *
      *    第四画面ＳＰＡエリア
           03  SPA-GMN-AREA.
               05  SPA-GMN-SRYCDH              PIC X(09).
               05  SPA-GMN-NAMEH               PIC X(100).
               05  SPA-GMN-YUKOSTYMDH          PIC X(09).
               05  SPA-GMN-YUKOEDYMDH          PIC X(09).
      * 
               05  SPA-GMN-TENSIKIBETULST      OCCURS  9.
                   07  SPA-GMN-TENSIKIBETUL    PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-TENSIKIBETUMEIL PIC X(40).
               05  SPA-TENSKB-MAX              PIC 9(02).
               05  SPA-GMN-OLDTENSKBLST        OCCURS  9.
                   07  SPA-GMN-OLDTENSKBL      PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-OLDTENSKBMEIL   PIC X(40).
               05  SPA-GMN-TANICDLST           OCCURS  200.
                   07  SPA-GMN-TANICDL         PIC X(03).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-TANICDMEIL      PIC X(24).
               05  SPA-GMN-TANICD-MAX          PIC 9(04).
               05  SPA-GMN-MADOKUKBNLST        OCCURS  5.
                   07  SPA-GMN-MADOKUKBNL      PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-MADOKUKBNMEIL   PIC X(14).
               05  SPA-GMN-SINKEIHAKAIKBNLST   OCCURS  2.
                   07  SPA-GMN-SINKEIHAKAIKBNL PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-SINKEIHAKAIKBNMEIL
                                               PIC X(16).
               05  SPA-GMN-SEIBUTUGAKUKBNLST   OCCURS  2.
                   07  SPA-GMN-SEIBUTUGAKUKBNL PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-SEIBUTUGAKUKBNMEIL
                                               PIC X(30).
               05  SPA-GMN-ZOEIZAIKBNLST       OCCURS  3.
                   07  SPA-GMN-ZOEIZAIKBNL     PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-ZOEIZAIKBNMEIL  PIC X(24).
               05  SPA-GMN-YKZKBNLST           OCCURS  6.
                   07  SPA-GMN-YKZKBNL         PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-YKZKBNMEIL      PIC X(14).
               05  SPA-GMN-ZAIGATAKBNLST       OCCURS  4.
                   07  SPA-GMN-ZAIGATAKBNL     PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-ZAIGATAKBNMEIL  PIC X(34).
               05  SPA-GMN-BUNRUIKBNLST        OCCURS  20.
                   07  SPA-GMN-BUNRUIKBNL      PIC X(02).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-BUNRUIKBNMEIL   PIC X(40).
               05  SPA-GMN-BUNRUIKBN-MAX       PIC 9(04).
      *
               05  SPA-GMN-ALLSURYOKBNLST      OCCURS  2.
                   07  SPA-GMN-ALLSURYOKBNL    PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-ALLSURYOKBNMEIL PIC X(12).
               05  SPA-GMN-ALLSURYOKBN-MAX     PIC 9(05).
      *
      *        後発医薬品
               05  SPA-GMN-KOUHATUKBNLST       OCCURS  9.
                   07  SPA-GMN-KOUHATUKBNL     PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-KOUHATUKBNMEIL  PIC X(32).
               05  SPA-GMN-KOUHATUKBN-G.
                   07  SPA-GMN-KOUHATUKBN      PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-KOUHATUKBNMEI   PIC X(32).
               05  SPA-GMN-KOUHATUKBN-MAX      PIC 9(02).
      *
               05  SPA-GMN-YUKOSTYMD           PIC X(10).
               05  SPA-GMN-YUKOEDYMD           PIC X(10).
               05  SPA-GMN-KANANAME            PIC X(100).
               05  SPA-GMN-NAME                PIC X(100).
               05  SPA-GMN-FORMALNAME          PIC X(200).
               05  SPA-GMN-SHONAME             PIC X(200).
               05  SPA-GMN-TEN                 PIC 9(8)V9(2).
               05  SPA-GMN-TENSIKIBETU-G.
                   07  SPA-GMN-TENSIKIBETU     PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-TENSIKIBETUMEI  PIC X(40).
               05  SPA-GMN-OLDTEN              PIC 9(8)V9(2).
               05  SPA-GMN-OLDTENSKB-G.
                   07  SPA-GMN-OLDTENSKB       PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-OLDTENSKBMEI    PIC X(40).
               05  SPA-GMN-TANICD-G.
                   07  SPA-GMN-TANICD          PIC X(03).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-TANICDMEI       PIC X(24).
               05  SPA-GMN-MADOKUKBN-G.
                   07  SPA-GMN-MADOKUKBN       PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-MADOKUKBNMEI    PIC X(14).
               05  SPA-GMN-SINKEIHAKAIKBN-G.
                   07  SPA-GMN-SINKEIHAKAIKBN  PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-SINKEIHAKAIKBNMEI
                                               PIC X(16).
               05  SPA-GMN-SEIBUTUGAKUKBN-G.
                   07  SPA-GMN-SEIBUTUGAKUKBN  PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-SEIBUTUGAKUKBNMEI
                                               PIC X(30).
               05  SPA-GMN-ZOEIZAIKBN-G.
                   07  SPA-GMN-ZOEIZAIKBN      PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-ZOEIZAIKBNMEI   PIC X(24).
               05  SPA-GMN-CSYYOURYO           PIC 9(5).
               05  SPA-GMN-YKZKBN-G.
                   07  SPA-GMN-YKZKBN          PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-YKZKBNMEI       PIC X(14).
               05  SPA-GMN-ZAIGATAKBN-G.
                   07  SPA-GMN-ZAIGATAKBN      PIC X(01).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-ZAIGATAKBNMEI   PIC X(34).
               05  SPA-GMN-LONGTOYOKBN         PIC 9(1).
               05  SPA-GMN-KOUHYOJYUNNUM       PIC 9(09).
               05  SPA-GMN-YAKKAKJNCD          PIC X(12).
               05  SPA-GMN-SYUSAISKB           PIC 9(1).
               05  SPA-GMN-SYOMEIKANREN        PIC X(09).
               05  SPA-GMN-KEIKASOCHIYMD       PIC X(09).
               05  SPA-GMN-INPCHKKBN           PIC 9(01).
               05  SPA-GMN-BUNRUIKBN-G.
                   07  SPA-GMN-BUNRUIKBN       PIC X(02).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-BUNRUIKBNMEI    PIC X(40).
      *
               05  SPA-GMN-ALLSURYOKBN-G.
                   07  SPA-GMN-ALLSURYOKBN     PIC X(01).
                   07  SPA-GMN-F1L             PIC X(01).
                   07  SPA-GMN-ALLSURYOKBNMEI  PIC X(12).
               05  SPA-GMN-KANZANTANICD-G.
                   07  SPA-GMN-KANZANTANICD    PIC X(03).
                   07  SPA-GMN-F1              PIC X(01).
                   07  SPA-GMN-KANZANTANICDMEI PIC X(24).
               05  SPA-GMN-KANZANCHI           PIC 9(5)V9(5).
      *
               05  SPA-GMN-CHNGCD              PIC X(09).
               05  SPA-GMN-CHNGCDNAME          PIC X(100).
               05  SPA-GMN-CHNGCDU             PIC X(09).
               05  SPA-GMN-CHNGCDNAMEU         PIC X(100).
      *H28.4
      *        規格単位数
               05  SPA-GMN-KIKAKUTANI          PIC 9(05).
      *        湿布薬区分
               05  SPA-GMN-SIPPKBN             PIC 9(01).
      *        湿布薬　膏体量１
               05  SPA-GMN-SIPRYOU1            PIC 9(03).
      *        湿布薬　包装入数１
               05  SPA-GMN-SIPIRISU1           PIC 9(03).
      *        湿布薬　膏体量２
               05  SPA-GMN-SIPRYOU2            PIC 9(03).
      *        湿布薬　包装入数２
               05  SPA-GMN-SIPIRISU2           PIC 9(03).
      *
      *        薬剤付加情報
      *        総量編集
               05  SPA-GMN-SOURYOKBN-G.
                   07  SPA-GMN-SOURYOKBN    PIC X(01).
                   07  SPA-GMN-SOURYOKBNF   PIC X(01).
                   07  SPA-GMN-SOURYOKBNMEI PIC X(18).
               05  SPA-GMN-SOURYOKBN-LSTG.
                   07  SPA-GMN-SOURYOKBN-LST    PIC X(20)
                                               OCCURS   5.
                   07  SPA-GMN-SOURYO-MAX      PIC 9(03).
      *        内服算定区分
               05  SPA-GMN-NAIFUKUKBN-G.
                   07  SPA-GMN-NAIFUKUKBN      PIC X(01).
                   07  SPA-GMN-NAIFUKUKBNF     PIC X(01).
                   07  SPA-GMN-NAIFUKUKBNMEI   PIC X(28).
               05  SPA-GMN-NAIFUKUKBN-LSTG.
                   07  SPA-GMN-NAIFUKUKBN-LST  PIC X(30)
                                               OCCURS   5.
                   07  SPA-GMN-NAIFUKU-MAX      PIC 9(03).
      *        一般名記載区分
               05  SPA-GMN-IPN-KISAIKBN-G.
                   07  SPA-GMN-IPN-KISAIKBN     PIC X(01).
                   07  SPA-GMN-IPN-KISAIKBNF    PIC X(01).
                   07  SPA-GMN-IPN-KISAIKBNMEI  PIC X(26).
               05  SPA-GMN-IPN-KISAIKBN-LSTG.
                   07  SPA-GMN-IPN-KISAIKBN-LST  PIC X(28)
                                               OCCURS   5.
                   07  SPA-GMN-IPN-KISAI-MAX      PIC 9(03).
      *        一般名記載単位
               05  SPA-GMN-IPN-TANICD-G.
                   07  SPA-GMN-IPN-TANICD       PIC X(03).
                   07  SPA-GMN-IPN-F3           PIC X(01).
                   07  SPA-GMN-IPN-TANICDMEI    PIC X(24).
      *        一般名変換値
               05  SPA-GMN-IPN-KANZANCHI        PIC 9(5)V9(5).
      *
      *        長期投与日数
               05  SPA-GMN-CHOKI-AREA.
                   07  SPA-GMN-CHOKINISSU      PIC 9(03).
                   07  SPA-GMN-CHOKIKIGEN      PIC X(10).
                   07  SPA-GMN-CHOKIERR        PIC 9(01).
      *        投与量
               05  SPA-GMN-TOUYO-G.
                   07  SPA-GMN-TOUYO-T         OCCURS   10.
                       09  SPA-GMN-TOUYOKGNAGE     PIC X(02).
                       09  SPA-GMN-TOUYOJGNAGE     PIC X(02).
                       09  SPA-GMN-TOUYORYO1       PIC 9(05)V9(03).
                       09  SPA-GMN-TOUYORYO2       PIC 9(05)V9(03).
                   07  SPA-GMN-TOUYOERR        PIC 9(01).
      *        一般名
               05  SPA-GMN-GENERICNAME         PIC X(200).
               05  SPA-GMN-GENERICHEN          PIC X(80).
      *        月上限
               05  SPA-GMN-TUKIJGN-G.
                   07  SPA-GMN-TUKIJGNKAISU    PIC 9(02).
                   07  SPA-GMN-TUKIJGNTOUYORYO PIC 9(05)V9(03).
      *R02.2
      *        ユーザー単位
               05  SPA-GMN-USER-TANICD-G.
                   07  SPA-GMN-USER-TANICD     PIC X(03).
                   07  SPA-GMN-USER-F1         PIC X(01).
                   07  SPA-GMN-USER-TANICDMEI  PIC X(24).
      *
               05  SPA-GMN-CUR                 PIC 9(03).
      *
      *    第四画面内容
           03  SPA-NAI-AREA.
               05  SPA-NAI-YUKOSTYMD           PIC X(08).
               05  SPA-NAI-YUKOEDYMD           PIC X(08).
               05  SPA-NAI-KEIKASOCHIYMD       PIC X(08).
               05  SPA-NAI-CHOKIKIGEN          PIC X(08).
      *
               05  SPA-NAI-KEIFLG1             PIC 9(01).
               05  SPA-NAI-KEIFLG2             PIC 9(01).
      *H24.8
               05  SPA-NAI-KEIFLG3             PIC 9(01).
      *
               05  SPA-NAI-CHNGCD              PIC X(09).
               05  SPA-MAE-CHNGCD              PIC X(09).
               05  SPA-NAI-CHNGCDU             PIC X(09).
               05  SPA-MAE-CHNGCDU             PIC X(09).
      *
               05  SPA-NAI-SIPKBN              PIC X(01).
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-SYSKANRI                      PIC 9(01).
           03  FLG-TENSU                         PIC 9(01).
           03  FLG-TENSUPLUS                     PIC 9(01).
           03  FLG-TENSUPLUS2                    PIC 9(01).
           03  FLG-OK                            PIC 9(01).
           03  FLG-ERR                           PIC 9(01).
           03  FLG-TOUROKU                       PIC 9(01).
           03  FLG-SRYCDCHG                      PIC 9(01).
           03  FLG-CHNGE                         PIC 9(01).
           03  FLG-GENERICNAME                   PIC 9(01).
           03  FLG-TENSUPLUS2-CHK                PIC 9(01).
           03  FLG-NENREI-CD                     PIC 9(01).
           03  FLG-TENSU-OK                      PIC 9(01).
           03  FLG-SRYCDCHGU                     PIC 9(01).
           03  FLG-CHNGEU                        PIC 9(01).
      *H31.4
           03  FLG-ERR-NO                        PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDXX                              PIC 9(02).
           03  IDXY                              PIC 9(02).
           03  IDXZ                              PIC 9(02).
           03  IDX                               PIC 9(04).
           03  IDX1                              PIC 9(04).
           03  IDX-T                             PIC 9(04).
           03  IDX-T2                            PIC 9(04).
           03  ID1                               PIC 9(02).
      *
      *    ワーク領域
       01  WRK-AREA.
           03  WRK-GMN-CUR                       PIC X(30).
           03  WRK-ZIDMSG                        PIC X(70).
           03  WRK-SNUM-INX                      PIC X(26).
           03  WRK-STRINGTBL.
               05  WRK-STRING                    PIC X(01)  OCCURS 200.
           03  WRK-LNKYMD                        PIC X(10).
           03  WRK-WYMD.
               05  WRK-WGO                       PIC X(01).
               05  WRK-WYY                       PIC 9(02).
               05  WRK-WMM                       PIC 9(02).
               05  WRK-WDD                       PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *    改正前日
           03  WRK-YUKOEDYMD       PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-OUT-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
           03  WRK-FLGSAVE         PIC 9(01).
      *
           03  WRK-TANINAME        PIC X(24).
           03  WRK-KANZANCHI       PIC ZZZZ9.9(5).
      *    年令チェック
           03  WRK-NENCD               PIC X(02).
           03  WRK-NENREI.
               05  WRK-NEN-MM          PIC 9(02).
               05  WRK-NEN-DD          PIC 9(02).
           03  WRK-KGN-NENREI.
               05  WRK-KGN-YY          PIC 9(02).
               05  WRK-KGN-MM          PIC 9(02).
               05  WRK-KGN-DD          PIC 9(02).
           03  WRK-JGN-NENREI.
               05  WRK-JGN-YY          PIC 9(02).
               05  WRK-JGN-MM          PIC 9(02).
               05  WRK-JGN-DD          PIC 9(02).
      *
      *    更新テーブル名称
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME    PIC X(64).
      *
           03  WRK-TABLE           PIC X(64).
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
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *    年令単位テーブル
           COPY    "CMNENREICD.INC".
      *
      *    ワーク点数マスタ
       01  WRK-TNS-TENSU-REC           PIC X(1290).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数マスタ
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    点数マスタ薬剤付加コード
        01 TENSUPLUS2-REC.
           COPY    "CPTENSUPLUS2.INC".
      *
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *    高額請求資料用薬剤分類情報
           COPY  "CPSK1036.INC".
      *
      *    老人一般コード変換
       01  SRYCDCHG-REC.
           COPY    "CPSRYCDCHG.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *    数字変換領域
             COPY    "CPORCSNUM.INC".
      *----(01.00.01) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    点数附加情報更新内容
      **** COPY    "CPORCGZS02.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
           COPY    "LINKAREA".
      *
       01  SCRAREA.
           COPY    "ORCA102SCRAREA.INC".
      *
       PROCEDURE                                 DIVISION    USING
                                                 MCPAREA
                                                 SPAAREA
                                                 LINKAREA
                                                 SCRAREA.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                              SECTION.
      *
           INITIALIZE                            FLG-AREA
           INITIALIZE                            CNT-AREA
           INITIALIZE                            WRK-AREA
           INITIALIZE                            IDX-AREA
      *
           MOVE    SPA-COMMON                    TO  SPA-AREA.
           MOVE    SPA-WORK                      TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE                      TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z04
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
           EVALUATE    MCP-STATUS       ALSO    MCP-EVENT
               WHEN    "LINK"           ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN    "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI-SEC
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY-SEC
           END-EVALUATE.
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END                       =   ZERO
               PERFORM 500-SET-SCREEN-SEC
           END-IF.
      *
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z04                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z010                      TO  SPA-Z010FREE
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           .
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                            FLG-AREA.
           INITIALIZE                            CNT-AREA.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "ZERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END         =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z04"               TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
               MOVE    1                             TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *    入力ＣＤより
               WHEN    "Z99"
                  MOVE  1                       TO  SPA-GMN-CUR
      *           ユーザー点数識別（2：ユーザーマスタ）
                  IF     (SPA-MASTER-CLASS    =   2 )
                      OR (SPA-MENTE-OK        =   1 )
                       CONTINUE
                   ELSE
      *                内服算定へ
                       MOVE  36                      TO  SPA-GMN-CUR
                   END-IF
      *H29.01
      *        ユーザ設定画面
               WHEN    "Z010"
                   PERFORM 310-SPASET-SEC
      ***             MOVE    1                   TO  SPA-GMN-CUR
               WHEN    OTHER
                   PERFORM 310-SPASET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    スパ初期編集処理
      *****************************************************************
       310-SPASET-SEC              SECTION.
      *    点数マスタ該当データ読み出し
           INITIALIZE                  SPA-Z04
      *
           MOVE    SPA-Z01-SRYCD       TO  SPA-GMN-SRYCDH
      *
           MOVE    SPA-Z01-YUKOSTYMD   TO  SPA-NAI-YUKOSTYMD
           IF      SPA-Z01-SYORIKBN    =   2
               MOVE  SPA-Z01-YUKOSTYMDH    TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE    SPA-Z01-YUKOSTYMDH  TO  SPA-GMN-YUKOSTYMD
      *
           MOVE    SPA-Z01-YUKOEDYMD   TO  SPA-NAI-YUKOEDYMD
           IF      SPA-Z01-SYORIKBN        =   2
               MOVE  SPA-Z01-YUKOEDYMDH    TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE  SPA-Z01-YUKOEDYMDH    TO  SPA-GMN-YUKOEDYMD
      *
           MOVE    "0"                 TO  SPA-GMN-TENSIKIBETUL   (1)
           MOVE    SPACE               TO  SPA-GMN-TENSIKIBETUMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-TENSIKIBETUL   (2)
           MOVE    "金額"              TO  SPA-GMN-TENSIKIBETUMEIL(2)
           MOVE    "2"                 TO  SPA-GMN-TENSIKIBETUL   (3)
           MOVE    "都道府県購入価格"  TO  SPA-GMN-TENSIKIBETUMEIL(3)
           MOVE    "4"                 TO  SPA-GMN-TENSIKIBETUL   (4)
           MOVE    "除算金額（金額を１０で除す。）" 
                                       TO  SPA-GMN-TENSIKIBETUMEIL(4)
      *H31.3
           MOVE    "5"                 TO  SPA-GMN-TENSIKIBETUL   (5)
           MOVE    "乗算金額（金額を１０で乗ずる。）" 
                                       TO  SPA-GMN-TENSIKIBETUMEIL(5)
           MOVE    "7"                 TO  SPA-GMN-TENSIKIBETUL   (6)
           MOVE    "減点"              TO  SPA-GMN-TENSIKIBETUMEIL(6)
           MOVE    6                   TO  SPA-TENSKB-MAX
      *
           MOVE    "0"                 TO  SPA-GMN-OLDTENSKBL     (1)
           MOVE    SPACE               TO  SPA-GMN-OLDTENSKBMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-OLDTENSKBL     (2)
           MOVE    "金額"              TO  SPA-GMN-OLDTENSKBMEIL  (2)
           MOVE    "2"                 TO  SPA-GMN-OLDTENSKBL     (3)
           MOVE    "都道府県購入価格"  TO  SPA-GMN-OLDTENSKBMEIL  (3)
           MOVE    "4"                 TO  SPA-GMN-OLDTENSKBL     (4)
           MOVE    "除算金額（金額を１０で除す。）" 
                                       TO  SPA-GMN-OLDTENSKBMEIL  (4)
      *H31.3
           MOVE    "5"                 TO  SPA-GMN-OLDTENSKBL     (5)
           MOVE    "乗算金額（金額を１０で乗ずる。）" 
                                       TO  SPA-GMN-OLDTENSKBMEIL  (5)
           MOVE    "7"                 TO  SPA-GMN-OLDTENSKBL     (6)
           MOVE    "減点"              TO  SPA-GMN-OLDTENSKBMEIL  (6)
      *
           MOVE    "0"                 TO  SPA-GMN-MADOKUKBNL     (1)
           MOVE    "なし"              TO  SPA-GMN-MADOKUKBNMEIL  (1)
           MOVE    "1"                 TO  SPA-GMN-MADOKUKBNL     (2)
           MOVE    "麻薬"              TO  SPA-GMN-MADOKUKBNMEIL  (2)
           MOVE    "2"                 TO  SPA-GMN-MADOKUKBNL     (3)
           MOVE    "毒薬"              TO  SPA-GMN-MADOKUKBNMEIL  (3)
           MOVE    "3"                 TO  SPA-GMN-MADOKUKBNL     (4)
           MOVE    "覚せい剤原料"      TO  SPA-GMN-MADOKUKBNMEIL  (4)
           MOVE    "5"                 TO  SPA-GMN-MADOKUKBNL     (5)
           MOVE    "向精神薬"          TO  SPA-GMN-MADOKUKBNMEIL  (5)
      *
           MOVE    "0"                 TO  SPA-GMN-SINKEIHAKAIKBNL   (1)
           MOVE    "なし"              TO  SPA-GMN-SINKEIHAKAIKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-SINKEIHAKAIKBNL   (2)
           MOVE    "神経破壊剤"        TO  SPA-GMN-SINKEIHAKAIKBNMEIL(2)
      *
           MOVE    "0"                 TO  SPA-GMN-SEIBUTUGAKUKBNL   (1)
           MOVE    "なし"              TO  SPA-GMN-SEIBUTUGAKUKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-SEIBUTUGAKUKBNL   (2)
           MOVE    "生物学的製剤加算対象品目"
                                       TO  SPA-GMN-SEIBUTUGAKUKBNMEIL(2)
      *
           MOVE    "0"                 TO  SPA-GMN-ZOEIZAIKBNL    (1)
           MOVE    "なし"              TO  SPA-GMN-ZOEIZAIKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-ZOEIZAIKBNL    (2)
           MOVE    "造影剤"            TO  SPA-GMN-ZOEIZAIKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-ZOEIZAIKBNL    (3)
           MOVE    "造影補助剤"        TO  SPA-GMN-ZOEIZAIKBNMEIL (3)
      *
           MOVE    "1"                 TO  SPA-GMN-YKZKBNL        (1)
           MOVE    "内用薬"            TO  SPA-GMN-YKZKBNMEIL     (1)
           MOVE    "3"                 TO  SPA-GMN-YKZKBNL        (2)
           MOVE    "その他"            TO  SPA-GMN-YKZKBNMEIL     (2)
           MOVE    "4"                 TO  SPA-GMN-YKZKBNL        (3)
           MOVE    "注射薬"            TO  SPA-GMN-YKZKBNMEIL     (3)
           MOVE    "6"                 TO  SPA-GMN-YKZKBNL        (4)
           MOVE    "外用薬"            TO  SPA-GMN-YKZKBNMEIL     (4)
           MOVE    "8"                 TO  SPA-GMN-YKZKBNL        (5)
           MOVE    "歯科用薬剤"        TO  SPA-GMN-YKZKBNMEIL     (5)
           MOVE    "9"                 TO  SPA-GMN-YKZKBNL        (6)
           MOVE    "歯科特定薬剤"      TO  SPA-GMN-YKZKBNMEIL     (6)
      *
           MOVE    "0"                 TO  SPA-GMN-ZAIGATAKBNL    (1)
           MOVE    "散剤、顆粒剤（細粒剤）、液剤以外"
                                       TO  SPA-GMN-ZAIGATAKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-ZAIGATAKBNL    (2)
           MOVE    "散剤"              TO  SPA-GMN-ZAIGATAKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-ZAIGATAKBNL    (3)
           MOVE    "顆粒剤（細粒剤）"  TO  SPA-GMN-ZAIGATAKBNMEIL (3)
           MOVE    "3"                 TO  SPA-GMN-ZAIGATAKBNL    (4)
           MOVE    "液剤"              TO  SPA-GMN-ZAIGATAKBNMEIL (4)
      *
           MOVE    "0"                 TO  SPA-GMN-KOUHATUKBNL    (1)
           MOVE    "後発医薬品でない"  TO  SPA-GMN-KOUHATUKBNMEIL (1)
           MOVE    "1"                 TO  SPA-GMN-KOUHATUKBNL    (2)
           MOVE    "先発医薬品がある後発医薬品である"
                                       TO  SPA-GMN-KOUHATUKBNMEIL (2)
           MOVE    "2"                 TO  SPA-GMN-KOUHATUKBNL    (3)
           MOVE    "後発医薬品がある先発医薬品である"
                                       TO  SPA-GMN-KOUHATUKBNMEIL (3)
           MOVE    "7"                 TO  SPA-GMN-KOUHATUKBNL    (4)
           MOVE    "先発医薬品のない後発医薬品である"
                                       TO  SPA-GMN-KOUHATUKBNMEIL (4)
           MOVE    4                   TO  SPA-GMN-KOUHATUKBN-MAX
      *H29.3
           IF     (SPA-Z01-YUKOSTYMD   >=  "20170317" )
             OR   (SPA-Z01-YUKOEDYMD   >=  "20170317" )
               MOVE    "3"                 TO  SPA-GMN-KOUHATUKBNL (4)
               MOVE    "先発なし後発医薬品（加算対象）"
                                           TO  SPA-GMN-KOUHATUKBNMEIL(4)
               MOVE    "4"                 TO  SPA-GMN-KOUHATUKBNL (5)
               MOVE    "後発医薬品でない（加算対象）"
                                           TO  SPA-GMN-KOUHATUKBNMEIL(5)
               MOVE    "7"                 TO  SPA-GMN-KOUHATUKBNL (6)
               MOVE    "先発なし後発医薬品（加算非対象）"
                                           TO  SPA-GMN-KOUHATUKBNMEIL(6)
               MOVE    6                   TO  SPA-GMN-KOUHATUKBN-MAX
           END-IF
      *
           MOVE    "0"                 TO  SPA-GMN-ALLSURYOKBNL   (1)
           MOVE    "記載しない"        TO  SPA-GMN-ALLSURYOKBNMEIL(1)
           MOVE    "1"                 TO  SPA-GMN-ALLSURYOKBNL   (2)
           MOVE    "記載する"          TO  SPA-GMN-ALLSURYOKBNMEIL(2)
      *
      *    総量編集
           MOVE    "0 なし"             TO  SPA-GMN-SOURYOKBN-LST(1)
           MOVE    "1 内服滴剤"
                                       TO  SPA-GMN-SOURYOKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-SOURYO-MAX
      *    内服算定
           MOVE    "0 なし"            TO  SPA-GMN-NAIFUKUKBN-LST(1)
           MOVE    "1 内服として算定可"
                                       TO  SPA-GMN-NAIFUKUKBN-LST(2)
           MOVE    2                   TO  SPA-GMN-NAIFUKU-MAX
      *    一般名記載区分
           MOVE    "0 記載しない"      TO  SPA-GMN-IPN-KISAIKBN-LST(1)
           MOVE    "1 一般名を記載する"
                                       TO  SPA-GMN-IPN-KISAIKBN-LST(2)
           MOVE    "2 処方名称を記載する"
                                       TO  SPA-GMN-IPN-KISAIKBN-LST(3)
           MOVE    "3 処方名称（一般名）を記載"
                                       TO  SPA-GMN-IPN-KISAIKBN-LST(4)
           MOVE    4                   TO  SPA-GMN-IPN-KISAI-MAX
      *
           MOVE    ZERO                TO  SPA-GMN-TANICD-MAX
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       MTANI-MAX
               MOVE    MTANI-TANICD  (IDXX)
                                       TO  SPA-GMN-TANICDL   (IDXX)
               MOVE    MTANI-TANINAME(IDXX)
                                       TO  SPA-GMN-TANICDMEIL(IDXX)
               MOVE    IDXX            TO  SPA-GMN-TANICD-MAX
           END-PERFORM
      *
      *    高額請求資料用薬剤分類情報
           MOVE    ZERO                TO  SPA-GMN-BUNRUIKBN-MAX
           MOVE    ZERO                TO  FLG-SYSKANRI
           MOVE    SPACE               TO  SYS-1036-REC
           INITIALIZE                      SYS-1036-REC
           MOVE    "1036"              TO  SYS-1036-KANRICD
           MOVE    SPA-NAI-YUKOSTYMD   TO  SYS-1036-STYUKYMD
           MOVE    SPA-NAI-YUKOSTYMD   TO  SYS-1036-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1036-HOSPNUM
           MOVE    SYS-1036-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    ZERO                TO  IDX1
           PERFORM UNTIL      (FLG-SYSKANRI   =   1 )
                         OR   (IDX1           >=  20)
      *
               COMPUTE SPA-GMN-BUNRUIKBN-MAX
                   =   SPA-GMN-BUNRUIKBN-MAX   +   1
               MOVE    SPA-GMN-BUNRUIKBN-MAX
                                           TO  IDX1
      *
               MOVE    MCPDATA-REC         TO  SYS-1036-REC
               MOVE    SYS-1036-KBNCD(1:2) TO  SPA-GMN-BUNRUIKBNL
                                                               (IDX1)
               MOVE    SYS-1036-BUNRUIMEI  TO  SPA-GMN-BUNRUIKBNMEIL
                                                               (IDX1)
      *
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
      *
      *    カーソルクロース
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPA-GMN-TENSIKIBETULST (2)
                                      TO  SPA-GMN-TENSIKIBETU-G
           MOVE    SPA-GMN-OLDTENSKBLST (1)
                                      TO  SPA-GMN-OLDTENSKB-G
           MOVE    SPA-GMN-TANICDLST      (1)
                                      TO  SPA-GMN-TANICD-G
           MOVE    SPA-GMN-MADOKUKBNLST   (1)
                                      TO  SPA-GMN-MADOKUKBN-G
           MOVE    SPA-GMN-SINKEIHAKAIKBNLST(1)
                                      TO  SPA-GMN-SINKEIHAKAIKBN-G
           MOVE    SPA-GMN-SEIBUTUGAKUKBNLST(1)
                                      TO  SPA-GMN-SEIBUTUGAKUKBN-G
           MOVE    SPA-GMN-ZOEIZAIKBNLST  (1)
                                      TO  SPA-GMN-ZOEIZAIKBN-G
           MOVE    SPA-GMN-YKZKBNLST (1)
                                      TO  SPA-GMN-YKZKBN-G
           MOVE    SPA-GMN-ZAIGATAKBNLST  (1)
                                      TO  SPA-GMN-ZAIGATAKBN-G
      *
           MOVE    SPA-GMN-KOUHATUKBNLST  (1)
                                       TO  SPA-GMN-KOUHATUKBN-G
      *    総量編集
           MOVE    SPA-GMN-SOURYOKBN-LST(1)
                                       TO  SPA-GMN-SOURYOKBN-G
           MOVE    SPA-GMN-NAIFUKUKBN-LST(1)
                                       TO  SPA-GMN-NAIFUKUKBN-G
           MOVE    SPA-GMN-IPN-KISAIKBN-LST(1)
                                       TO  SPA-GMN-IPN-KISAIKBN-G
      *
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           IF      SPA-Z01-SELNUM      =   ZERO
               MOVE    ALL "9"             TO  TNS-YUKOSTYMD
               MOVE    ALL "9"             TO  TNS-YUKOEDYMD
           ELSE
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNS-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
      *
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    1                   TO  FLG-TENSU-OK
               MOVE    TNS-TENSU-REC       TO  WRK-TNS-TENSU-REC
      *        点数マスタ編集
               PERFORM 3001-SPASET-SEC
           ELSE
               MOVE    ZERO                TO  FLG-TENSU-OK
               MOVE    SPACE               TO  WRK-TNS-TENSU-REC
           END-IF
      *
      *    点数マスタ付加情報検索
           IF      FLG-TENSU           =   ZERO
      *H29.1
      *        期間内の最終内容
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    TNS-SRYCD           TO  TNSP-SRYCD
      *        MOVE    TNS-YUKOSTYMD       TO  TNSP-YUKOSTYMD
      *        MOVE    TNS-YUKOEDYMD       TO  TNSP-YUKOEDYMD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                   MOVE    "key4"              TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS-READ-SEC
               END-IF
               PERFORM UNTIL       FLG-TENSUPLUS   =   1
                   IF     (TNSP-YUKOSTYMD      >   TNS-YUKOEDYMD )
      *R01.10
                     OR   (TNSP-YUKOEDYMD      <   TNS-YUKOSTYMD )
                       CONTINUE
                   ELSE
                       PERFORM  3002-SPASET-TENSUPLUS-SEC
                       MOVE    1               TO  FLG-OK
                       MOVE    1               TO  FLG-TENSUPLUS
                   END-IF
                   IF      FLG-TENSUPLUS       =   ZERO
                       MOVE    "tbl_tensuplus"     TO  MCP-TABLE
                       MOVE    "key4"              TO  MCP-PATHNAME
                       PERFORM 990-TENSUPLUS-READ-SEC
                   END-IF
               END-PERFORM
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        経過措置コード
               PERFORM 3101-SRYCDCHG-HEN-SEC
           END-IF
      *    点数マスタ薬剤付加情報
           PERFORM 3102-TENSUPLUS2-HEN-SEC
      *    一般名マスタ編集
           PERFORM 3103-GENERICNAME-HEN-SEC
      *
           MOVE  1                       TO  SPA-GMN-CUR
      *H31.1
      *    ユーザー点数識別（2：ユーザーマスタ）
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               MOVE  1                       TO  SPA-GMN-CUR
           ELSE
      *        内服算定へ
               MOVE  36                      TO  SPA-GMN-CUR
           END-IF
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット
      *****************************************************************
       3001-SPASET-SEC                     SECTION.
      *
           MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
           MOVE    TNS-NAME            TO  SPA-GMN-NAMEH
      *
           MOVE    TNS-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    TNS-NAME            TO  SPA-GMN-NAME
           MOVE    TNS-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    TNS-TEN             TO  SPA-GMN-TEN
           MOVE    TNS-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-TENSKB-MAX
               IF      SPA-GMN-TENSIKIBETU 
                                       =   SPA-GMN-TENSIKIBETUL (IDXX)
                   MOVE    SPA-GMN-TENSIKIBETULST (IDXX)
                                           TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    SPA-TENSKB-MAX  TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    TNS-OLDTENSKB       TO  SPA-GMN-OLDTENSKB
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-TENSKB-MAX
               IF      SPA-GMN-OLDTENSKB 
                                       =   SPA-GMN-OLDTENSKBL (IDXX)
                   MOVE    SPA-GMN-OLDTENSKBLST (IDXX)
                                           TO  SPA-GMN-OLDTENSKB-G
                   MOVE    SPA-TENSKB-MAX  TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-TANICD          TO  SPA-GMN-TANICD
           MOVE    TNS-TANINAME        TO  SPA-GMN-TANICDMEI
           MOVE    TNS-MADOKUKBN       TO  SPA-GMN-MADOKUKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               IF      SPA-GMN-MADOKUKBN 
                                       =   SPA-GMN-MADOKUKBNL (IDXX)
                   MOVE    SPA-GMN-MADOKUKBNLST (IDXX)
                                           TO  SPA-GMN-MADOKUKBN-G
                   MOVE    5               TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-SINKEIHAKAIKBN  TO  SPA-GMN-SINKEIHAKAIKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               IF      SPA-GMN-SINKEIHAKAIKBN  
                                       =   SPA-GMN-SINKEIHAKAIKBNL
                                                             (IDXX)
                     MOVE    SPA-GMN-SINKEIHAKAIKBNLST (IDXX)
                                            TO
                                            SPA-GMN-SINKEIHAKAIKBN-G
                    MOVE    2               TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-SEIBUTUGAKUKBN  TO  SPA-GMN-SEIBUTUGAKUKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               IF      SPA-GMN-SEIBUTUGAKUKBN  
                                       =   SPA-GMN-SEIBUTUGAKUKBNL
                                                            (IDXX)
                   MOVE    SPA-GMN-SEIBUTUGAKUKBNLST (IDXX)
                                           TO  SPA-GMN-SEIBUTUGAKUKBN-G
                   MOVE    2               TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-ZOEIZAIKBN      TO  SPA-GMN-ZOEIZAIKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       3
               IF      SPA-GMN-ZOEIZAIKBN  
                                       =   SPA-GMN-ZOEIZAIKBNL(IDXX)
                   MOVE    SPA-GMN-ZOEIZAIKBNLST (IDXX)
                                           TO  SPA-GMN-ZOEIZAIKBN-G
                   MOVE    3               TO  IDXX
               END-IF
           END-PERFORM
           MOVE    TNS-CSYYOURYO       TO  SPA-GMN-CSYYOURYO
           MOVE    TNS-YKZKBN          TO  SPA-GMN-YKZKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
               IF      SPA-GMN-YKZKBN      =   SPA-GMN-YKZKBNL (IDXX)
                   MOVE    SPA-GMN-YKZKBNLST (IDXX)
                                           TO  SPA-GMN-YKZKBN-G
                   MOVE    6               TO  IDXX
              END-IF
           END-PERFORM
           MOVE    TNS-ZAIGATAKBN      TO  SPA-GMN-ZAIGATAKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       4
               IF      SPA-GMN-ZAIGATAKBN 
                                           =   SPA-GMN-ZAIGATAKBNL(IDXX)
                   MOVE    SPA-GMN-ZAIGATAKBNLST (IDXX)
                                               TO  SPA-GMN-ZAIGATAKBN-G
                   MOVE    4               TO  IDXX
                END-IF
           END-PERFORM
      *    後発医薬品
           MOVE    TNS-KOUHATUKBN      TO  SPA-GMN-KOUHATUKBN
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-KOUHATUKBN-MAX
               IF      SPA-GMN-KOUHATUKBN 
                                       =   SPA-GMN-KOUHATUKBNL(IDXX)
                   MOVE    SPA-GMN-KOUHATUKBNLST (IDXX)
                                           TO  SPA-GMN-KOUHATUKBN-G
                   MOVE    SPA-GMN-KOUHATUKBN-MAX  TO  IDXX
               END-IF
           END-PERFORM
      *
           MOVE    TNS-LONGTOYOKBN     TO  SPA-GMN-LONGTOYOKBN
           MOVE    TNS-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
           MOVE    TNS-YAKKAKJNCD      TO  SPA-GMN-YAKKAKJNCD
           MOVE    TNS-SYUSAISKB       TO  SPA-GMN-SYUSAISKB
           MOVE    TNS-SYOMEIKANREN    TO  SPA-GMN-SYOMEIKANREN
           MOVE    TNS-KEIKASOCHIYMD   TO  SPA-GMN-KEIKASOCHIYMD
      *
           MOVE    TNS-KEIKASOCHIYMD   TO  WRK-LNKYMD
           MOVE    TNS-KEIKASOCHIYMD   TO  SPA-NAI-KEIKASOCHIYMD
           PERFORM  832-YMDCONV21-SEC
           MOVE  WRK-HENYMDG           TO  SPA-GMN-KEIKASOCHIYMD
      *
      *H28.4
      *    規格単位数
           MOVE    TNS-SSTKIJUNCD(1)   TO  SPA-GMN-KIKAKUTANI
      *    湿布薬区分
           MOVE    TNS-MEIUSESKB       TO  SPA-GMN-SIPPKBN
      *    湿布薬　膏体量１
           MOVE    TNS-SSTKIJUNCD(2)   TO  SPA-GMN-SIPRYOU1
      *    湿布薬　包装入数１
           MOVE    TNS-SSTKIJUNCD(3)   TO  SPA-GMN-SIPIRISU1
      *    湿布薬　膏体量２
           MOVE    TNS-SSTKIJUNCD(4)   TO  SPA-GMN-SIPRYOU2
      *    湿布薬　包装入数２
           MOVE    TNS-SSTKIJUNCD(5)   TO  SPA-GMN-SIPIRISU2
           .
       3001-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット（点数マスタ付加情報）
      *****************************************************************
       3002-SPASET-TENSUPLUS-SEC       SECTION.
      *
      *    入力チェック区分
           MOVE    TNSP-INPCHKKBN      TO  SPA-GMN-INPCHKKBN
      *    処方名
           MOVE    TNSP-SHONAME        TO  SPA-GMN-SHONAME
      *    分類区分
           MOVE    TNSP-TENPUYAKKBN    TO  SPA-GMN-BUNRUIKBN
           MOVE    TNSP-ALLSURYOKBN    TO  SPA-GMN-ALLSURYOKBN
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-BUNRUIKBN-MAX )
      *
               IF    ( TNSP-TENPUYAKKBN
                                  =    SPA-GMN-BUNRUIKBNL (IDX1))
                   MOVE    SPA-GMN-BUNRUIKBNLST (IDX1)
                                       TO   SPA-GMN-BUNRUIKBN-G
                   MOVE    SPA-GMN-BUNRUIKBN-MAX
                                       TO   IDX1
               END-IF
           END-PERFORM
      *
      *    全数量記載区分
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               IF      TNSP-ALLSURYOKBN 
                                       =   SPA-GMN-ALLSURYOKBNL(IDXX)
                   MOVE    SPA-GMN-ALLSURYOKBNLST (IDXX)
                                           TO  SPA-GMN-ALLSURYOKBN-G
                   MOVE    2               TO  IDXX
               END-IF
           END-PERFORM
      *
      *    数量換算単位コード
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-TANICD-MAX
               IF      TNSP-KANZANTANICD 
                                       =   SPA-GMN-TANICDL(IDXX)
                   MOVE    SPA-GMN-TANICDLST (IDXX)
                                           TO  SPA-GMN-KANZANTANICD-G
                   MOVE    SPA-GMN-TANICD-MAX
                                           TO  IDXX
               END-IF
           END-PERFORM
      *    数量換算値
           MOVE    TNSP-KANZANCHI      TO  SPA-GMN-KANZANCHI
      *
      *    総量編集
           MOVE    TNSP-SOURYOHENKBN   TO  SPA-GMN-SOURYOKBN
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-GMN-SOURYO-MAX
               IF      TNSP-SOURYOHENKBN
                                       =   SPA-GMN-SOURYOKBN-LST
                                                       (IDX-T)(1:1)
                   MOVE    SPA-GMN-SOURYOKBN-LST (IDX-T)
                                       TO  SPA-GMN-SOURYOKBN-G
                   MOVE    SPA-GMN-SOURYO-MAX  TO  IDX-T
               END-IF
           END-PERFORM
      *    一般名記載区分
           MOVE    TNSP-IPN-KISAIKBN    TO  SPA-GMN-IPN-KISAIKBN
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL  (IDX1    >   SPA-GMN-IPN-KISAI-MAX)
               IF      TNSP-IPN-KISAIKBN
                                       =    SPA-GMN-IPN-KISAIKBN-LST
                                                         (IDX1)(1:1)
                   MOVE    SPA-GMN-IPN-KISAIKBN-LST (IDX1)
                                       TO   SPA-GMN-IPN-KISAIKBN-G
                   MOVE    SPA-GMN-IPN-KISAI-MAX
                                       TO   IDX1
               END-IF
           END-PERFORM
      *    一般名記載単位
           MOVE    TNSP-IPN-KANZANTANICD   TO  SPA-GMN-IPN-TANICD
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1    >   SPA-GMN-TANICD-MAX
               IF      TNSP-IPN-KANZANTANICD
                                           =    SPA-GMN-TANICDL (IDX1)
                   MOVE    SPA-GMN-TANICDLST (IDX1)
                                           TO   SPA-GMN-IPN-TANICD-G
                   MOVE    SPA-GMN-TANICD-MAX
                                           TO   IDX1
               END-IF
           END-PERFORM
      *    一般名記載換算値
           MOVE    TNSP-IPN-KANZANCHI  TO  SPA-GMN-IPN-KANZANCHI
      *
      *    湿布薬関係  付加情報あり
           IF     (TNSP-SIPPURYO1      =   ZERO )
              AND (TNSP-SIPPURYO2      =   ZERO )
               MOVE    "0"             TO  SPA-NAI-SIPKBN
           ELSE
               MOVE    "1"             TO  SPA-NAI-SIPKBN
           END-IF
      *
      *R02.2
      *    ユーザー単位
           MOVE    SPACE               TO  SPA-GMN-USER-TANICD-G
           MOVE    TNSP-USERTANICD     TO  SPA-GMN-USER-TANICD
           MOVE    TNSP-USERTANINAME   TO  SPA-GMN-USER-TANICDMEI
           .
       3002-SPASET-TENSUPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *    経過措置コード処理
      *****************************************************************
       3101-SRYCDCHG-HEN-SEC           SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    "tbl_srycdchg_master"
                                       TO  WRK-TABLE
           PERFORM 31011-SRYCDCHG-HEN-SEC
           IF     (FLG-SRYCDCHG        =   ZERO)   AND
                  (FLG-TENSU           =   ZERO)
               MOVE    CHG-RJNSRYCD        TO  SPA-GMN-CHNGCD
               MOVE    TNS-NAME            TO  SPA-GMN-CHNGCDNAME
           END-IF
           MOVE    SPA-GMN-CHNGCD      TO  SPA-NAI-CHNGCD
           MOVE    SPA-GMN-CHNGCD      TO  SPA-MAE-CHNGCD
           MOVE    FLG-SRYCDCHG        TO  WRK-FLGSAVE
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    "tbl_srycdchg_user" TO  WRK-TABLE
           PERFORM 31011-SRYCDCHG-HEN-SEC
           IF     (FLG-SRYCDCHG        =   ZERO)   AND
                  (FLG-TENSU           =   ZERO)
               MOVE    CHG-RJNSRYCD        TO  SPA-GMN-CHNGCDU
               MOVE    TNS-NAME            TO  SPA-GMN-CHNGCDNAMEU
           END-IF
           MOVE    SPA-GMN-CHNGCDU     TO  SPA-NAI-CHNGCDU
           MOVE    SPA-GMN-CHNGCDU     TO  SPA-MAE-CHNGCDU
           MOVE    FLG-SRYCDCHG        TO  FLG-SRYCDCHGU
           MOVE    WRK-FLGSAVE         TO  FLG-SRYCDCHG
           .
      *
       3101-SRYCDCHG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    経過措置コード処理
      *****************************************************************
       31011-SRYCDCHG-HEN-SEC          SECTION.
      *
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
             IF      CHG-RJNSRYCD        =   "999999999"
               MOVE    SPACE               TO  TNS-NAME
               MOVE    ZERO                TO  FLG-TENSU
             ELSE
      *        点数マスタ編集
               MOVE    CHG-RJNSRYCD        TO  TNS-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 900-TENSU-READ-SEC
               ELSE
                   MOVE    1                   TO  FLG-TENSU
                   INITIALIZE                      TNS-TENSU-REC
               END-IF
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
             END-IF
           END-IF
           .
      *
       3101-SRYCDCHG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ薬剤付加情報処理
      *****************************************************************
       3102-TENSUPLUS2-HEN-SEC            SECTION.
      *
           IF      FLG-TENSU-OK        =   1
               MOVE    WRK-TNS-TENSU-REC   TO  TNS-TENSU-REC
               INITIALIZE                  TENSUPLUS2-REC 
               MOVE    SPACE               TO  TENSUPLUS2-REC
               MOVE    TNS-SRYCD           TO  TNSP2-SRYCD
      *
               MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
               MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC               =   ZERO
                   MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-TENSUPLUS2-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-TENSUPLUS2
               END-IF
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS2
           END-IF
      *
           IF      FLG-TENSUPLUS2      =   ZERO
      *        長期投与
               MOVE    TNSP2-CHOKI-NISSU   TO  SPA-GMN-CHOKINISSU
               MOVE    TNSP2-CHOKI-KIGEN   TO  SPA-NAI-CHOKIKIGEN
               IF      TNSP2-CHOKI-KIGEN   NOT =   SPACE
                   MOVE    TNSP2-CHOKI-KIGEN   TO  WRK-LNKYMD
                   PERFORM 832-YMDCONV21-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-CHOKIKIGEN
               END-IF
               MOVE    TNSP2-CHOKIERR      TO  SPA-GMN-CHOKIERR
      *        投与量
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL       IDX-T   >   10
                   MOVE    TNSP2-TOUYOKGNAGE(IDX-T)
                                           TO  SPA-GMN-TOUYOKGNAGE
                                                                (IDX-T)
                   MOVE    TNSP2-TOUYOJGNAGE(IDX-T)
                                           TO  SPA-GMN-TOUYOJGNAGE
                                                                (IDX-T)
                   MOVE    TNSP2-TOUYORYO1(IDX-T)
                                           TO  SPA-GMN-TOUYORYO1(IDX-T)
                   MOVE    TNSP2-TOUYORYO2(IDX-T)
                                           TO  SPA-GMN-TOUYORYO2(IDX-T)
                   IF      TNSP2-TOUYOKGNAGE(IDX-T) =   SPACE
                       MOVE    ZERO            TO  SPA-GMN-TOUYOKGNAGE
                                                                (IDX-T)
                       MOVE    ZERO            TO  SPA-GMN-TOUYOJGNAGE
                                                                (IDX-T)
                   END-IF
               END-PERFORM
               MOVE    TNSP2-TOUYOERR      TO  SPA-GMN-TOUYOERR
      *        内服算定
               MOVE    TNSP2-NAIFUKUKBN    TO  SPA-GMN-NAIFUKUKBN
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL       IDX-T   >   SPA-GMN-NAIFUKU-MAX
                   IF      TNSP2-NAIFUKUKBN
                                           =   SPA-GMN-NAIFUKUKBN-LST
                                                           (IDX-T)(1:1)
                       MOVE    SPA-GMN-NAIFUKUKBN-LST (IDX-T)
                                           TO  SPA-GMN-NAIFUKUKBN-G
                       MOVE    SPA-GMN-NAIFUKU-MAX
                                           TO  IDX-T
                   END-IF
               END-PERFORM
      *        月上限
               MOVE    TNSP2-TUKIJGNKAISU  TO  SPA-GMN-TUKIJGNKAISU
               MOVE    TNSP2-TUKIJGNTOUYORYO
                                           TO  SPA-GMN-TUKIJGNTOUYORYO
           ELSE
      *        下限・上限年齢はゼロを初期設定
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL       IDX-T   >   10
                   MOVE    ZERO            TO  SPA-GMN-TOUYOKGNAGE
                                                                (IDX-T)
                   MOVE    ZERO            TO  SPA-GMN-TOUYOJGNAGE
                                                                (IDX-T)
               END-PERFORM
           END-IF
           .
      *
       3102-TENSUPLUS2-HEN-EXT.
           EXIT.
      *****************************************************************
      *    一般名マスタ編集処理
      *****************************************************************
       3103-GENERICNAME-HEN-SEC            SECTION.
      *
           IF      FLG-TENSU-OK        =   1
               MOVE    WRK-TNS-TENSU-REC   TO  TNS-TENSU-REC
               INITIALIZE                  GENERICNAME-REC 
               MOVE    SPACE               TO  GENERICNAME-REC
               MOVE    TNS-YAKKAKJNCD
                                           TO  GENERIC-YAKKAKJNCD
      *
grpsys*        MOVE    SPA-HOSPNUM         TO  -HOSPNUM
               MOVE    GENERICNAME-REC     TO  MCPDATA-REC
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_genericname"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 920-DBFETCH-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    MCPDATA-REC     TO  GENERICNAME-REC
                       MOVE    ZERO            TO  FLG-GENERICNAME
                   ELSE
                       INITIALIZE                  GENERICNAME-REC
                       MOVE    1               TO  FLG-GENERICNAME
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-GENERICNAME
               END-IF
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           ELSE
               MOVE    1               TO  FLG-GENERICNAME
           END-IF
      *
           IF      FLG-GENERICNAME     =   ZERO
               MOVE    GENERIC-GENERICNAME TO  SPA-GMN-GENERICNAME
      *        単位
      ******   MOVE    SPACE               TO  WRK-TANINAME
      *        PERFORM VARYING     IDX1    FROM    1   BY  1
      *                UNTIL       IDX1    >   SPA-GMN-TANICD-MAX
      *            IF      GENERIC-TANICD  =    SPA-GMN-TANICDL (IDX1)
      *                MOVE    SPA-GMN-TANICDMEIL(IDX1)
      *                                    TO   WRK-TANINAME
      *                MOVE    SPA-GMN-TANICD-MAX
      *                                    TO   IDX1
      *            END-IF
      *****    END-PERFORM
               MOVE    GENERIC-TANINAME    TO  WRK-TANINAME
               MOVE    SPACE               TO  SPA-GMN-GENERICHEN 
      *        換算値
               MOVE    GENERIC-KANZANCHI   TO  WRK-KANZANCHI
               STRING  "単位名："
                       WRK-TANINAME        DELIMITED  BY  SPACE
                       "　換算値："        DELIMITED  BY  SIZE
                       WRK-KANZANCHI       DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-GENERICHEN
               END-STRING
           END-IF
           .
       3103-GENERICNAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＣＩＤ１）ＯＫ処理
      *****************************************************************
       330-ZID1-SET-SEC            SECTION.
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
      *            削除
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4031-DEL-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG1
                   END-IF
               WHEN    "0002"
               WHEN    "0004"
      *            登録
                   IF      SPA-ZID1-FLG    =   "OK"
                       PERFORM  4121-TOUROKU-SEC
                   ELSE
                       MOVE    ZERO            TO  SPA-NAI-KEIFLG2
                   END-IF
           END-EVALUATE
      *
           IF      SPA-ERRCD       NOT =  SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD      
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI-SEC                           SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   MOVE  "CHANGE"      TO  MCP-PUTTYPE
                   PERFORM  401-BACK-SEC
      *        削除
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM  403-DEL-SEC
      *        入力コード設定
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM  405-INPUTCD-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *ver.5.0
      *H29.01
      *        ユーザー項目登録
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  407-B10-SYORI-SEC
           END-EVALUATE
           .
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       410-INPUT-CHK-SEC          SECTION.
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *    基本チェックと別画面処理
           PERFORM 4102-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
             AND  (FLG-ERR-NO          =   ZERO  )
      *        登録時、内容変更の無い場合、エラーとしない
               IF     (FLG-TOUROKU         =   1  )   AND
                      (SPA-Z01-SYORIKBN    =   2  )
      *            更新処理の時のみ
                   PERFORM 4103-HENKOU-CHK-SEC
               END-IF
           END-IF
      *
      *    登録時、必須入力チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-TOUROKU         =   1     )
               PERFORM 4109-TOUROKU-CHK-SEC
           END-IF
      *
           .
      *
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
      *H29.1
      *    ユーザー点数識別（2：ユーザーマスタ）のみ反映
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               CONTINUE
           ELSE
      *        付加情報２のみ反映
               PERFORM 41011-GMN-SPA-SET02-SEC
               GO      TO      4101-GMN-SPA-SET-EXT
           END-IF
      *
           MOVE    Z04-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z04-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
           MOVE    Z04-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z04-NAME            TO  SPA-GMN-NAME
           MOVE    Z04-FORMALNAME      TO  SPA-GMN-FORMALNAME
           MOVE    Z04-TEN             TO  SPA-GMN-TEN
           MOVE    Z04-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU-G
           MOVE    Z04-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    Z04-OLDTENSKB       TO  SPA-GMN-OLDTENSKB-G
           MOVE    Z04-TANICD          TO  SPA-GMN-TANICD-G
           MOVE    Z04-MADOKUKBN       TO  SPA-GMN-MADOKUKBN-G
           MOVE    Z04-SINKEIHAKAIKBN  TO  SPA-GMN-SINKEIHAKAIKBN-G
           MOVE    Z04-SEIBUTUGAKUKBN  TO  SPA-GMN-SEIBUTUGAKUKBN-G
           MOVE    Z04-ZOEIZAIKBN      TO  SPA-GMN-ZOEIZAIKBN-G
           MOVE    Z04-CSYYOURYO       TO  SPA-GMN-CSYYOURYO
           MOVE    Z04-YKZKBN          TO  SPA-GMN-YKZKBN-G
           MOVE    Z04-ZAIGATAKBN      TO  SPA-GMN-ZAIGATAKBN-G
           MOVE    Z04-LONGTOYOKBN     TO  SPA-GMN-LONGTOYOKBN
           MOVE    Z04-KOUHATUKBN      TO  SPA-GMN-KOUHATUKBN-G
           MOVE    Z04-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
           MOVE    Z04-YAKKAKJNCD      TO  SPA-GMN-YAKKAKJNCD
           MOVE    Z04-SYUSAISKB       TO  SPA-GMN-SYUSAISKB
           MOVE    Z04-SYOMEIKANREN    TO  SPA-GMN-SYOMEIKANREN
           MOVE    Z04-KEIKASOCHIYMD   TO  SPA-GMN-KEIKASOCHIYMD
      *
      *    ユーザー指定は反映なし
      ***  MOVE    Z04-SHONAME         TO  SPA-GMN-SHONAME
      *    MOVE    Z04-INPCHKKBN       TO  SPA-GMN-INPCHKKBN
      *    MOVE    Z04-BUNRUIKBN       TO  SPA-GMN-BUNRUIKBN-G
      *    MOVE    Z04-ALLSURYOKBN     TO  SPA-GMN-ALLSURYOKBN-G
      *    MOVE    Z04-KANZANTANICD    TO  SPA-GMN-KANZANTANICD-G
      *    MOVE    Z04-KANZANCHI       TO  SPA-GMN-KANZANCHI
      *    MOVE    Z04-SOURYOKBN       TO  SPA-GMN-SOURYOKBN-G
      *
      *    MOVE    Z04-IPNKISAIKBN     TO  SPA-GMN-IPN-KISAIKBN-G
      *    MOVE    Z04-IPNTANICD       TO  SPA-GMN-IPN-TANICD-G
      *    MOVE    Z04-IPNKANZANCHI    TO  SPA-GMN-IPN-KANZANCHI
      *!!!
      *    付加情報２編集反映
           PERFORM 41011-GMN-SPA-SET02-SEC
      *
      *    MOVE    ZERO                TO  FLG-CHNGE
      *    IF      Z04-CHNGCD      NOT =   SPA-NAI-CHNGCD
      *        MOVE    1               TO  FLG-CHNGE
      *    END-IF
      *    MOVE    Z04-CHNGCD          TO  SPA-GMN-CHNGCD
      *H28.4
      *    規格単位数
           MOVE    Z04-KIKAKUTANI      TO  SPA-GMN-KIKAKUTANI
      *    湿布薬区分
           MOVE    Z04-SIPPKBN         TO  SPA-GMN-SIPPKBN
      *    湿布薬
           MOVE    Z04-SIPRYOU1        TO  SPA-GMN-SIPRYOU1
           MOVE    Z04-SIPIRISU1       TO  SPA-GMN-SIPIRISU1
           MOVE    Z04-SIPRYOU2        TO  SPA-GMN-SIPRYOU2
           MOVE    Z04-SIPIRISU2       TO  SPA-GMN-SIPIRISU2
           .
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    付加情報２編集反映処理
      *****************************************************************
       41011-GMN-SPA-SET02-SEC          SECTION.
      *
           MOVE    Z04-NAIFUKUKBN      TO  SPA-GMN-NAIFUKUKBN-G
           MOVE    Z04-CHOKINISSU      TO  SPA-GMN-CHOKINISSU
           MOVE    Z04-CHOKIKIGEN      TO  SPA-GMN-CHOKIKIGEN
           MOVE    Z04-CHOKIERR        TO  SPA-GMN-CHOKIERR
           MOVE    Z04-TOUYOERR        TO  SPA-GMN-TOUYOERR
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   10
               MOVE    Z04-KGNAGE(IDX-T)   TO  SPA-GMN-TOUYOKGNAGE
                                                                (IDX-T)
               MOVE    Z04-JGNAGE(IDX-T)   TO  SPA-GMN-TOUYOJGNAGE
                                                                (IDX-T)
               MOVE    Z04-TOUYORYO1(IDX-T)
                                           TO  SPA-GMN-TOUYORYO1(IDX-T)
               MOVE    Z04-TOUYORYO2(IDX-T)
                                           TO  SPA-GMN-TOUYORYO2(IDX-T)
           END-PERFORM
      *
           MOVE    Z04-TUKIJGNKAISU        TO  SPA-GMN-TUKIJGNKAISU
           MOVE    Z04-TUKIJGNTOUYORYO     TO  SPA-GMN-TUKIJGNTOUYORYO
      *
           MOVE    ZERO                TO  FLG-CHNGE
      *H31.4  標準　経過措置品目移行コードはユーザー不可
           IF     (SPA-MENTE-OK        =   1     )
            AND   (SPA-MASTER-CLASS    NOT =   2 )
               IF      Z04-CHNGCD      NOT =   SPA-NAI-CHNGCD
                   MOVE    1               TO  FLG-CHNGE
               END-IF
               MOVE    Z04-CHNGCD          TO  SPA-GMN-CHNGCD
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHNGEU
           IF      Z04-CHNGCDU     NOT =   SPA-NAI-CHNGCDU
               MOVE    1               TO  FLG-CHNGEU
           END-IF
           MOVE    Z04-CHNGCDU         TO  SPA-GMN-CHNGCDU
           .
       41011-GMN-SPA-SET02-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *H31.4
           MOVE    ZERO               TO  FLG-ERR-NO
      *
      *    有効開始年月日チェック
           PERFORM  501-YUKOSTYMDCHK-SEC
      *    有効終了年月日チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM  502-YUKOEDYMDCHK-SEC
           END-IF
      *    カナ名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-KANANAME-CHK-SEC
           END-IF
      *    名称チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-NAME-CHK-SEC
           END-IF
      *    正式名称
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-FORMALNAME-CHK-SEC
           END-IF
      *    処方名称
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-SHONAME-CHK-SEC
           END-IF
      *    金額種別
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-TENSIKIBETU-CHK-SEC
           END-IF
      *    旧金額種別
           IF      SPA-ERRCD           =   SPACE
               PERFORM  503-OLDTENSKB-CHK-SEC
           END-IF
      *    単位コード
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5012-TANICD-CHK-SEC
           END-IF
      *    麻毒
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-MADOKUKBN-CHK-SEC
           END-IF
      *    神経破壊剤
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-SINKEIHAKAIKBN-CHK-SEC
           END-IF
      *    生物学的区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-SEIBUTUGAKUKBN-CHK-SEC
           END-IF
      *    造影剤
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-ZOEIZAIKBN-CHK-SEC
           END-IF
      *    薬剤
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-YKZKBN-CHK-SEC
           END-IF
      *    剤型
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-ZAIGATAKBN-CHK-SEC
           END-IF
      *    後発医薬品
           IF      SPA-ERRCD           =   SPACE
               PERFORM  5013-KOUHATUKBN-CHK-SEC
           END-IF
      *    経過措置年月日
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-KEIKASOCHIYMDCHK-SEC
           END-IF
      *H28.4
      *    湿布薬関係
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-SIPPKBN-CHK-SEC
           END-IF
      *H31.4
      *    点数マスタ項目エラーなし
           MOVE    1                  TO  FLG-ERR-NO
      *
      *    内服算定
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-NAIFUKUKBN-SEC
           END-IF
      *    総量編集
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-SOURYOHENKBN-SEC
           END-IF
      *    一般名記載
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-IPN-KISAIKBN-SEC
           END-IF
      *    一般名単位
           IF      SPA-ERRCD           =   SPACE
               PERFORM  523-IPN-TANICD-SEC
           END-IF
      *    入力チェック区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM  524-INPCHKKBNCHK-SEC
           END-IF
      *    分類区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-BUNRUIKBNCHK-SEC
           END-IF
      *    全数量記載区分
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-ALLSURYOKBNCHK-SEC
           END-IF
      *    数量換算単位コード
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-KANZANTANICDCHK-SEC
           END-IF
      *    換算値
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-KANZANCHICHK-SEC
           END-IF
      *    長期投与期限
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-CHOKIKIGEN-SEC
           END-IF
      *    長期投与エラー
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-CHOKIERR-SEC
           END-IF
      *    投与量エラー
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-TOUYOERR-SEC
           END-IF
      *    投与量
           IF      SPA-ERRCD           =   SPACE
               PERFORM 524-TOUYO-CHK-SEC
           END-IF
      *    経過措置（標準）
           IF      FLG-CHNGE           =   1
               PERFORM 524-CHNGCD-CHK-SEC
           END-IF
      *    経過措置（自院）
           IF      FLG-CHNGEU          =   1
               PERFORM 524-CHNGCDU-CHK-SEC
           END-IF
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE  "Z01"                           TO  SPA-SAKIPG.
      *
           MOVE  "Z04"                           TO  SPA-MOTOPG.
      *
      ***  MOVE  "CHANGE"                        TO  MCP-PUTTYPE.
           MOVE  SPA-SAKIPG                      TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       401-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除確認画面表示処理
      *****************************************************************
       403-DEL-SEC                               SECTION.
      *
           IF      SPA-NAI-KEIFLG1     =   ZERO
               IF      SPA-Z01-SRYCD(1:1)  =   "6"
                   MOVE    "K003"          TO  SPA-ERRCD
               END-IF
               MOVE    1               TO  SPA-NAI-KEIFLG1
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-ZIDCD
           END-IF
      *
           .    
       403-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4031-DEL-SEC                     SECTION.
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設定分
               PERFORM 40311-TENSU-USER-DEL-SEC
           ELSE
      *        点数マスタ削除
               PERFORM 40312-TENSU-MAST-DEL-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               PERFORM 401-BACK-SEC
           END-IF
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数設設削除処理
      *****************************************************************
       40311-TENSU-USER-DEL-SEC       SECTION.
      *
      *    点数マスタ削除
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z04 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
           .
       40311-TENSU-USER-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    マスタ点数削除処理
      *****************************************************************
       40312-TENSU-MAST-DEL-SEC       SECTION.
      *
      *    点数削除
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z04 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
               MOVE    SPA-Z01-SRYCD       TO  ICD-SRYCD
               MOVE    INPUTCD-REC         TO  MCPDATA-REC
      *
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "del22"             TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
      *        点数付加情報削除
               MOVE    SPA-Z01-SRYCD       TO  TNSP-SRYCD
               MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
               MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           END-IF
      *
      *    置換えマスタ削除処理
           IF      SPA-MAE-CHNGCD      NOT =   SPACE
               INITIALIZE                  SRYCDCHG-REC 
               MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
               MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
               MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
               MOVE    "tbl_srycdchg_master"
                                           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_srycdchg_master"
                                               TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 990-SRYCDCHG-READ-SEC
               ELSE
                   INITIALIZE                  SRYCDCHG-REC
                   MOVE    1               TO  FLG-SRYCDCHG
               END-IF
               MOVE    "tbl_srycdchg_master"
                                           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SRYCDCHG        =   ZERO
                   MOVE    "tbl_srycdchg_master"
                                               TO  WRK-TABLE
                   PERFORM 41291-SRYCDCHG-DEL-SEC
               END-IF
           END-IF
           IF      SPA-MAE-CHNGCDU     NOT =   SPACE
               INITIALIZE                  SRYCDCHG-REC 
               MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
               MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
               MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
               MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
                   MOVE    "key2"              TO  MCP-PATHNAME
                   PERFORM 990-SRYCDCHG-READ-SEC
               ELSE
                   INITIALIZE                  SRYCDCHG-REC
                   MOVE    1               TO  FLG-SRYCDCHG
               END-IF
               MOVE    "tbl_srycdchg_user" TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SRYCDCHG        =   ZERO
                   MOVE    "tbl_srycdchg_user" TO  WRK-TABLE
                   PERFORM 41291-SRYCDCHG-DEL-SEC
               END-IF
           END-IF
      *
           INITIALIZE                  TENSUPLUS2-REC 
           MOVE    SPACE               TO  TENSUPLUS2-REC
           MOVE    TNS-SRYCD           TO  TNSP2-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS2-READ-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS2
           END-IF
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-TENSUPLUS2      =   ZERO
               PERFORM 412913-TENSUPLUS2-DEL-SEC
           END-IF
           .
       40312-TENSU-MAST-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                              SECTION.
      *
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z04             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ99"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z04
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z04"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
       405-INPUTCD-EXT.
           EXIT.
      *ver.5.0
      *****************************************************************
      *    ユーザー項目登録処理
      *****************************************************************
       407-B10-SYORI-SEC                SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *    変更はエラーでも遷移する
           IF      SPA-Z01-SYORIKBN    =   2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-Z01-SRYCD(1:1)  NOT =   "6"
               MOVE    "0050"              TO  SPA-ERRCD
           END-IF
      *    変更以外はエラー
           IF      SPA-Z01-SYORIKBN    NOT =   2
               MOVE    "0050"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      407-B10-SYORI-EXT
           END-IF
      *
           MOVE    "Z010"              TO  SPA-SAKIPG
           MOVE    "Z04"               TO  SPA-MOTOPG
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z04             TO  SPA-Z01SONOTA
           MOVE    SPA-Z01FREE         TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGZ010"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-Z01KYOUTU
           MOVE    SPA-FREE            TO  SPA-Z01FREE
           MOVE    SPA-Z01SONOTA       TO  SPA-Z04
           MOVE    SPA-Z010FREE        TO  SPA-Z010
      *    画面情報編集
           MOVE    SPA-GMN-YKZKBN      TO  SPA-Z010-YKZKBN
           MOVE    SPA-GMN-GENERICNAME TO  SPA-Z010-GENERICNAME
           MOVE    SPA-GMN-TANICD      TO  SPA-Z010-TANICD 
           MOVE    SPA-GMN-SIPPKBN     TO  SPA-Z010-SIPPKBN
      *
      *    初期カーソル
           IF      SPA-Z010-RRKMAX     =   ZERO
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    "Z010"              TO  SPA-SAKIPG
           MOVE    "Z04"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .
      *
       407-B10-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SYORIKBN    =   1
      *            新規処理
                   IF      SPA-NAI-KEIFLG2     =   ZERO
      *!!
                     AND  (SPA-MASTER-CLASS    NOT =   2 )
                       MOVE    "K001"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG2
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0002"              TO  SPA-ZIDCD
      *H29.01
      *    ユーザー点数識別（2 ：ユーザーマスタ）のみ反映
               IF     (SPA-MASTER-CLASS    =   2 )
                   OR (SPA-MENTE-OK        =   1 )
                   CONTINUE
               ELSE
                   MOVE    "0004"          TO  SPA-ZIDCD
               END-IF
           END-IF
      *
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、内容変更チェック処理
      ****************************************************************
       4103-HENKOU-CHK-SEC      SECTION.
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設登録
               MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           ELSE
               MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           END-IF
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TENSU-REC(1:1290)   TO  WRK-TNS-TENSU-REC
      *        更新項目編集
               PERFORM 4121-KOUMOKUISO-SEC
               IF      TNS-TENSU-REC(1:1290)   =   WRK-TNS-TENSU-REC
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4103-HENKOU-CHK-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、必須入力チェック処理
      ****************************************************************
       4109-TOUROKU-CHK-SEC      SECTION.
      *
      *    登録時、必須入力チェック
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
           IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
               MOVE    "1010"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      4109-TOUROKU-CHK-EXT
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-Z01-SYORIKBN    =   2
      *        前チェック
               IF      SPA-Z01-SELNUM      >   1
                   MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
                   IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
      *        後チェック
               IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
                   MOVE    SPA-NAI-YUKOEDYMD
                                           TO  WRK-YMD
      *            対象日＋１日
                   PERFORM 800-DAYSET-SEC
                   IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                                   <   WRK-OTYMD
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    改正チェック
           IF      SPA-Z01-SYORIKBN    =   3
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
                   MOVE    "1012"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
               IF  SPA-NAI-YUKOEDYMD    NOT    =  ALL "9"
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
      *
      *H24.8
               IF      SPA-NAI-KEIFLG3     =   ZERO
      *!!
                 AND  (SPA-MASTER-CLASS    NOT =   2 )
                   MOVE    "K005"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-NAI-KEIFLG3
                   MOVE    1               TO  SPA-GMN-CUR
                   GO      TO      4109-TOUROKU-CHK-EXT
               END-IF
           END-IF
      *
      *H24.8
      *    期間変更チェック
           IF      SPA-Z01-SELNUM      >=  1
               IF      (SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOSTYMD)
               OR      (SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOEDYMD)
                   IF      SPA-NAI-KEIFLG3     =   ZERO
      *!!
                     AND  (SPA-MASTER-CLASS    NOT =   2 )
                       MOVE    "K004"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLG3
                       MOVE    1               TO  SPA-GMN-CUR
                       GO      TO      4109-TOUROKU-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-NAME        =   SPACE
               MOVE    "1014"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN-CUR
           END-IF
      *    薬剤付加情報
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41091-TOUROKU-PLUSCHK-SEC
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤付加情報登録チェック処理
      *****************************************************************
       41091-TOUROKU-PLUSCHK-SEC        SECTION.
      *
      *    単位と換算値
           IF     ((SPA-GMN-IPN-TANICD         =   SPACE) AND
                   (SPA-GMN-IPN-KANZANCHI  NOT =   ZERO ))  OR
                  ((SPA-GMN-IPN-TANICD     NOT =   SPACE) AND
                   (SPA-GMN-IPN-KANZANCHI      =   ZERO ))
               MOVE    "0034"              TO  SPA-ERRCD
               MOVE    39                  TO  SPA-GMN-CUR
           END-IF
      *
      *    換算値単位の判定
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-KANZANTANICD        =   SPACE
                                                   OR  "0000"
                   IF      SPA-GMN-KANZANCHI   NOT =   ZERO
                       MOVE    "0037"              TO  SPA-ERRCD
                       MOVE    29                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   IF      SPA-GMN-KANZANCHI       =   ZERO
                       MOVE    "0038"              TO  SPA-ERRCD
                       MOVE    30                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      * 
      *    投与量
           PERFORM VARYING IDX1        FROM    1   BY  1
                   UNTIL  (IDX1        >   10   )  OR
                          (SPA-ERRCD   NOT =   SPACE )
               IF      (SPA-GMN-TOUYOKGNAGE (IDX1) NOT =  ZERO )  OR
                       (SPA-GMN-TOUYOJGNAGE (IDX1) NOT =  ZERO )
                   IF      (SPA-GMN-TOUYORYO1 (IDX1)   =   ZERO) AND
                           (SPA-GMN-TOUYORYO2 (IDX1)   =   ZERO)
                       MOVE    "0023"              TO  SPA-ERRCD
                   END-IF
               END-IF
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      IDX1            =   10
                       MOVE    110             TO  SPA-GMN-CUR
                   ELSE
                       COMPUTE SPA-GMN-CUR     =   110     +   IDX1
                   END-IF
               END-IF
           END-PERFORM
           .
      *
       41091-TOUROKU-PLUSCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-TOUROKU-SEC                          SECTION.
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設登録
               PERFORM 41211-TENSU-USER-UPD-SEC
           ELSE
      *        点数マスタ登録
               PERFORM 41212-TENSU-MAST-UPD-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *    前画面に戻る
               MOVE    "JOIN"          TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数登録処理
      *****************************************************************
       41211-TENSU-USER-UPD-SEC        SECTION.
      *
           MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *            新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *        更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *        新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *    点数マスタ投与付加情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41291-TENSUPLUS2-KOUSIN-SEC
           END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *    置換えマスタ更新
      *    ユーザーマスタなので標準の更新は要らない
      *    IF      SPA-ERRCD           =   SPACE
      *        IF      SPA-GMN-CHNGCD  NOT =   SPA-MAE-CHNGCD
      *            MOVE    "tbl_srycdchg_master"
      *                                        TO  WRK-TABLE
      *            PERFORM 4129-SRYCDCHG-UPD-SEC
      *        END-IF
      *    END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-CHNGCDU NOT =   SPA-MAE-CHNGCDU
                   MOVE    "tbl_srycdchg_user" TO  WRK-TABLE
                   PERFORM 4129-SRYCDCHG-UPD-SEC
               END-IF
           END-IF
           .
       41211-TENSU-USER-UPD-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数マスタ登録処理
      *****************************************************************
       41212-TENSU-MAST-UPD-SEC        SECTION.
      *
           IF     (SPA-MENTE-OK        =   1 )
      *    メンテナンスモードのみ、マスタ更新
           MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
      *
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *            新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *        更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *        新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
           END-IF
      *
      *    点数マスタ付加情報更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 4129-TENSUPLUS-UPD-SEC
      *    END-IF
      *    点数マスタ投与付加情報更新
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41291-TENSUPLUS2-KOUSIN-SEC
           END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *    置換えマスタ更新
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-CHNGCD  NOT =   SPA-MAE-CHNGCD
                  AND (SPA-MENTE-OK        =   1 )
                   MOVE    "tbl_srycdchg_master"
                                               TO  WRK-TABLE
                   PERFORM 4129-SRYCDCHG-UPD-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-CHNGCDU NOT =   SPA-MAE-CHNGCDU
                   MOVE    "tbl_srycdchg_user" TO  WRK-TABLE
                   PERFORM 4129-SRYCDCHG-UPD-SEC
               END-IF
           END-IF
           .
       41212-TENSU-MAST-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正時処理
      *****************************************************************
       41211-KAISEI-SYORI-SEC          SECTION.
      *
      *    有効終了日を決定する
      *    今回の有効開始日の前日とする
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-NAI-YUKOSTYMD   TO  LNK-DAY6-KIJUN
           MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    -1                  TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           IF      STS-DAY-RC1         =   ZERO
               MOVE    LNK-DAY6-KEISAN     TO  WRK-YUKOEDYMD
           ELSE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      41211-KAISEI-SYORI-EXT
           END-IF
      *
      *    有効年月日を更新する
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
     ****      MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新
               MOVE    WRK-YUKOEDYMD       TO  TNS-YUKOEDYMD
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z04 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41211-KAISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41212-UPDATE-SYORI-SEC                       SECTION.
      *
      *
           INITIALIZE                      TNS-TENSU-REC
      *
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-TENSU
               INITIALIZE                      TNS-TENSU-REC
           END-IF
      *
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               PERFORM 4121-KOUMOKUISO-SEC
      *
               MOVE    SPA-Z01-SRYCD       TO  TNSUP-SRYCD
               MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOSTYMD
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                           TO  TNSUP-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNSUP-HOSPNUM
      *
               MOVE    SPA-OPID            TO  TNS-OPID
               MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
               MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
               MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
      ****     MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z04 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNS-KEY
                   MOVE    1                     TO  SPA-GMN-CUR
                   MOVE    "9002"                TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       41212-UPDATE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       41213-INSERT-SYORI-SEC          SECTION.
      *
      *    データＩＮＳＥＲＴ
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
           MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
           PERFORM 4121-KOUMOKUISO-SEC
      *
           MOVE    SPA-OPID            TO  TNS-OPID
           MOVE    SMCNDATE-YMD        TO  TNS-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNS-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNS-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 TENSU INS ERR:" MCP-RC
                       ",KEY:" TNS-KEY
           END-IF
      *
           .
       41213-INSERT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       4121-KOUMOKUISO-SEC                       SECTION.
      *
           MOVE  SPA-GMN-KANANAME              TO  TNS-KANANAME
                                                   WRK-STRINGTBL
           MOVE  100                           TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-KANAYUKOKETA
           MOVE  SPA-GMN-NAME                  TO  TNS-NAME
                                                   WRK-STRINGTBL
           MOVE  100                           TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-YUKOKETA
      *
           MOVE  SPA-GMN-FORMALNAME            TO  TNS-FORMALNAME
                                                   WRK-STRINGTBL
           MOVE  200                           TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE  CNT-STRING                    TO  TNS-FORMALYUKOKETA
      *
           MOVE  SPA-GMN-TEN                   TO  TNS-TEN
      *
           MOVE  SPA-GMN-TENSIKIBETU           TO  TNS-TENSIKIBETU
           MOVE  SPA-GMN-OLDTEN                TO  TNS-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB             TO  TNS-OLDTENSKB
           MOVE  SPA-GMN-TANICD                TO  TNS-TANICD
      ***  MOVE  SPA-GMN-TANINAME              TO  TNS-TANINAME
           MOVE  SPA-GMN-TANICDMEI             TO  TNS-TANINAME
           MOVE  SPA-GMN-MADOKUKBN             TO  TNS-MADOKUKBN
           MOVE  SPA-GMN-SINKEIHAKAIKBN        TO  TNS-SINKEIHAKAIKBN
           MOVE  SPA-GMN-SEIBUTUGAKUKBN        TO  TNS-SEIBUTUGAKUKBN
           MOVE  SPA-GMN-ZOEIZAIKBN            TO  TNS-ZOEIZAIKBN
           MOVE  SPA-GMN-CSYYOURYO             TO  TNS-CSYYOURYO
           MOVE  SPA-GMN-YKZKBN                TO  TNS-YKZKBN
           MOVE  SPA-GMN-ZAIGATAKBN            TO  TNS-ZAIGATAKBN
           MOVE  SPA-GMN-KOUHATUKBN            TO  TNS-KOUHATUKBN
           MOVE  SPA-GMN-LONGTOYOKBN           TO  TNS-LONGTOYOKBN
           MOVE  SPA-GMN-KOUHYOJYUNNUM         TO  TNS-KOUHYOJYUNNUM
           MOVE  SPA-GMN-YAKKAKJNCD            TO  TNS-YAKKAKJNCD
           MOVE  SPA-GMN-SYUSAISKB             TO  TNS-SYUSAISKB
           MOVE  SPA-GMN-SYOMEIKANREN          TO  TNS-SYOMEIKANREN
           MOVE  SPA-NAI-KEIKASOCHIYMD         TO  TNS-KEIKASOCHIYMD
      *H28.4
      *    規格単位数
           MOVE    SPA-GMN-KIKAKUTANI          TO  TNS-SSTKIJUNCD(1)
      *    湿布薬区分
           MOVE    SPA-GMN-SIPPKBN             TO  TNS-MEIUSESKB
      *    湿布薬　膏体量１
           MOVE    SPA-GMN-SIPRYOU1            TO  TNS-SSTKIJUNCD(2)
      *    湿布薬　包装入数１
           MOVE    SPA-GMN-SIPIRISU1           TO  TNS-SSTKIJUNCD(3)
      *    湿布薬　膏体量２
           MOVE    SPA-GMN-SIPRYOU2            TO  TNS-SSTKIJUNCD(4)
      *    湿布薬　包装入数２
           MOVE    SPA-GMN-SIPIRISU2           TO  TNS-SSTKIJUNCD(5)
      *
           .
      *
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
      *4129-TENSUPLUS-UPD-SEC          SECTION.
      *
      *    INITIALIZE                  ORCGZS02AREA
      *    MOVE    SPA-NAI-YUKOSTYMD   TO  ZS02-NAI-YUKOSTYMD
      *    MOVE    SPA-NAI-YUKOEDYMD   TO  ZS02-NAI-YUKOEDYMD
      *    MOVE    WRK-YUKOEDYMD       TO  ZS02-YUKOEDYMD
      *
      *    INITIALIZE                  TENSUPLUS-REC
      *    MOVE    SPA-GMN-SHONAME     TO  TNSP-SHONAME
      *                                    WRK-STRINGTBL
      *    MOVE    200                 TO  CNT-STRING
      *    PERFORM 891-STRCOUNT-SEC
      *    MOVE    CNT-STRING          TO  TNSP-SHOYUKOKETA
      *    MOVE    SPA-GMN-INPCHKKBN   TO  TNSP-INPCHKKBN
      *    MOVE    SPA-GMN-BUNRUIKBN   TO  TNSP-TENPUYAKKBN
      *    MOVE    SPA-GMN-ALLSURYOKBN TO  TNSP-ALLSURYOKBN
      *    MOVE    SPA-GMN-KANZANTANICD    TO  TNSP-KANZANTANICD
      *    MOVE    SPA-GMN-KANZANTANICDMEI TO  TNSP-KANZANTANINAME
      *    MOVE    SPA-GMN-KANZANCHI       TO  TNSP-KANZANCHI
      *    総量編集
      *    MOVE    SPA-GMN-SOURYOKBN       TO  TNSP-SOURYOHENKBN
      *    一般名記載区分
      *    MOVE    SPA-GMN-IPN-KISAIKBN    TO  TNSP-IPN-KISAIKBN 
      *    MOVE    SPA-GMN-IPN-TANICD      TO  TNSP-IPN-KANZANTANICD 
      *    MOVE    SPA-GMN-IPN-TANICDMEI   TO  TNSP-IPN-KANZANTANINAME 
      *    MOVE    SPA-GMN-IPN-KANZANCHI   TO  TNSP-IPN-KANZANCHI
      *
      *    入力コード更新
      *    CALL    "ORCGZS02"          USING
      *                                MCPAREA
      *                                SPAAREA
      *                                ORCGZS02AREA
      *                                TENSUPLUS-REC
      *
      *    .
      *4129-TENSUPLUS-UPD-EXT.
      ***  EXIT.
      *
      *****************************************************************
      *     点数マスタ投与付加情報更新処理
      *****************************************************************
       41291-TENSUPLUS2-KOUSIN-SEC     SECTION.
      *
           INITIALIZE                  TENSUPLUS2-REC 
           MOVE    SPACE               TO  TENSUPLUS2-REC
           MOVE    TNS-SRYCD           TO  TNSP2-SRYCD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC               =   ZERO
               MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-TENSUPLUS2-READ-SEC
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS2
           END-IF
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    内容判定
           IF     (SPA-GMN-CHOKINISSU  =   ZERO )  AND
                  (SPA-NAI-CHOKIKIGEN  =   SPACE)  AND
                  (SPA-GMN-CHOKIERR    =   ZERO )  AND
                  (SPA-GMN-TOUYOERR    =   ZERO )  AND
                  (SPA-GMN-NAIFUKUKBN  =   ZERO )
               MOVE    ZERO                TO  FLG-TENSUPLUS2-CHK
               PERFORM VARYING     IDX-T   FROM    1   BY  1
                       UNTIL      (IDX-T   >   10  )   OR
                                  (FLG-TENSUPLUS2-CHK  =   1 )
                   IF    ( SPA-GMN-TOUYOKGNAGE (IDX-T) =   ZERO )
                     AND ( SPA-GMN-TOUYOJGNAGE (IDX-T) =   ZERO )
                     AND ( SPA-GMN-TOUYORYO1   (IDX-T) =   ZERO )
                     AND ( SPA-GMN-TOUYORYO2   (IDX-T) =   ZERO )
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-TENSUPLUS2-CHK
                   END-IF
               END-PERFORM
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS2-CHK
           END-IF
      *
           IF    ( SPA-GMN-TUKIJGNKAISU    =   ZERO )
            AND  ( SPA-GMN-TUKIJGNTOUYORYO =   ZERO )
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-TENSUPLUS2-CHK
           END-IF
      *
           IF      FLG-TENSUPLUS2-CHK  =   ZERO
      *        削除
               IF      FLG-TENSUPLUS2      =   ZERO
                   PERFORM 412913-TENSUPLUS2-DEL-SEC
               END-IF
           ELSE
               IF      FLG-TENSUPLUS2      =   1
      *            追加
                   PERFORM 412911-TENSUPLUS2-INS-SEC
               ELSE
      *            更新
                   PERFORM 412912-TENSUPLUS2-UPD-SEC
               END-IF
           END-IF
      *
           .
       41291-TENSUPLUS2-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *   点数マスタ投与付加追加処理
      *****************************************************************
       412911-TENSUPLUS2-INS-SEC        SECTION.
      *
           INITIALIZE                  TENSUPLUS2-REC 
           MOVE    SPACE               TO  TENSUPLUS2-REC
           MOVE    SPA-Z01-SRYCD       TO  TNSP2-SRYCD
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           PERFORM 41291-TENSUPLUS2-HEN-SEC
      *
           MOVE    SPA-OPID            TO  TNSP2-OPID
           MOVE    SMCNDATE-YMD        TO  TNSP2-CREYMD
           MOVE    SMCNDATE-YMD        TO  TNSP2-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP2-UPHMS
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 TENSUPLUS2 INS ERR:" MCP-RC
                       ",KEY:" TNSP2-KEY
           END-IF
      *
           .
       412911-TENSUPLUS2-INS-EXT.
           EXIT.
      *****************************************************************
      *   点数マスタ投与付加追加処理
      *****************************************************************
       412912-TENSUPLUS2-UPD-SEC        SECTION.
      *
           MOVE    SPA-HOSPNUM         TO  TNSP2-HOSPNUM
           PERFORM 41291-TENSUPLUS2-HEN-SEC
      *
           MOVE    SPA-OPID            TO  TNSP2-OPID
           MOVE    SMCNDATE-YMD        TO  TNSP2-UPYMD
           MOVE    SMCNDATE-HMS        TO  TNSP2-UPHMS
      *
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 TENSUPLUS2 UPD ERR:" MCP-RC
                       ",KEY:" TNSP2-KEY
           END-IF
      *
           .
       412912-TENSUPLUS2-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ投与付加編集処理
      *****************************************************************
       41291-TENSUPLUS2-HEN-SEC             SECTION.
      *
           MOVE    SPA-GMN-NAIFUKUKBN  TO  TNSP2-NAIFUKUKBN
      *
           MOVE    SPA-GMN-CHOKINISSU  TO  TNSP2-CHOKI-NISSU
           MOVE    SPA-NAI-CHOKIKIGEN  TO  TNSP2-CHOKI-KIGEN
           MOVE    SPA-GMN-CHOKIERR    TO  TNSP2-CHOKIERR
           MOVE    SPA-GMN-TOUYOERR    TO  TNSP2-TOUYOERR
      *    投与量
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   10
               MOVE    ZERO                TO  TNSP2-TOUYOKGNAGE(IDX-T)
               MOVE    ZERO                TO  TNSP2-TOUYOJGNAGE(IDX-T)
               MOVE    ZERO                TO  TNSP2-TOUYORYO1  (IDX-T)
               MOVE    ZERO                TO  TNSP2-TOUYORYO2  (IDX-T)
           END-PERFORM
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   10
               IF     (SPA-GMN-TOUYOKGNAGE (IDX-T) =   ZERO )  AND
                      (SPA-GMN-TOUYOJGNAGE (IDX-T) =   ZERO )  AND
                      (SPA-GMN-TOUYORYO1   (IDX-T) =   ZERO )  AND
                      (SPA-GMN-TOUYORYO2   (IDX-T) =   ZERO )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX-T2
                   MOVE    SPA-GMN-TOUYOKGNAGE (IDX-T)
                                           TO  TNSP2-TOUYOKGNAGE
                                                                (IDX-T2)
                   MOVE    SPA-GMN-TOUYOJGNAGE (IDX-T)
                                           TO  TNSP2-TOUYOJGNAGE
                                                                (IDX-T2)
                   MOVE    SPA-GMN-TOUYORYO1 (IDX-T)
                                           TO  TNSP2-TOUYORYO1  (IDX-T2)
                   MOVE    SPA-GMN-TOUYORYO2 (IDX-T)
                                           TO  TNSP2-TOUYORYO2  (IDX-T2)
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-TUKIJGNKAISU    TO  TNSP2-TUKIJGNKAISU
           MOVE    SPA-GMN-TUKIJGNTOUYORYO TO  TNSP2-TUKIJGNTOUYORYO
      *
           .
       41291-TENSUPLUS2-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ投与付加削除処理
      *****************************************************************
       412913-TENSUPLUS2-DEL-SEC             SECTION.
      *
           MOVE    TENSUPLUS2-REC      TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensuplus2"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 TENSUPLUS2 DEL ERR:" MCP-RC
                       ",KEY:" TNSP2-KEY
           END-IF
      *
           .
       412913-TENSUPLUS2-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    置換えマスタ更新処理
      *****************************************************************
       4129-SRYCDCHG-UPD-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    WRK-TABLE           TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
               IF    ((WRK-TABLE           =
                                       "tbl_srycdchg_master")  AND
                      (SPA-GMN-CHNGCD      =   SPACE))   OR
                     ((WRK-TABLE           =
                                       "tbl_srycdchg_user")  AND
                      (SPA-GMN-CHNGCDU     =   SPACE))
      *            削除
                   PERFORM 41291-SRYCDCHG-DEL-SEC
               ELSE
      *            更新
                   PERFORM 41292-SRYCDCHG-UPDATE-SEC
               END-IF
           ELSE
      *        追加
               PERFORM 41292-SRYCDCHG-INSERT-SEC
           END-IF
      *
           .
       4129-SRYCDCHG-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    置換えマスタ追加処理
      *****************************************************************
       41292-SRYCDCHG-INSERT-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC
           MOVE    SPA-Z01-SRYCD       TO  CHG-IPNSRYCD
           IF      WRK-TABLE           =   "tbl_srycdchg_user"
               MOVE    SPA-GMN-CHNGCDU     TO  CHG-RJNSRYCD
           ELSE
               MOVE    SPA-GMN-CHNGCD      TO  CHG-RJNSRYCD
           END-IF
           MOVE    SMCNDATE-YMD        TO  CHG-CREYMD
           MOVE    SMCNDATE-YMD        TO  CHG-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHG-UPHMS
           MOVE    SPA-OPID            TO  CHG-OPID
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 SRYCDCHG INS ERR:" MCP-RC
                       ",KEY:" CHG-KEY
           END-IF
      *
           .
       41292-SRYCDCHG-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    置換えマスタ更新処理
      *****************************************************************
       41292-SRYCDCHG-UPDATE-SEC             SECTION.
      *
           MOVE    CHG-KEY             TO  CHG-UPKEY
           IF      WRK-TABLE           =   "tbl_srycdchg_user"
               MOVE    SPA-GMN-CHNGCDU     TO  CHG-RJNSRYCD
           ELSE
               MOVE    SPA-GMN-CHNGCD      TO  CHG-RJNSRYCD
           END-IF
      *
           MOVE    SPA-OPID            TO  CHG-OPID
           MOVE    SMCNDATE-YMD        TO  CHG-UPYMD
           MOVE    SMCNDATE-HMS        TO  CHG-UPHMS
      *
      **   MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "upd1"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 SRYCDCHG UPD ERR:" MCP-RC
                       ",KEY:" CHG-KEY
           END-IF
      *
           .
       41292-SRYCDCHG-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    置換えマスタ削除処理
      *****************************************************************
       41291-SRYCDCHG-DEL-SEC             SECTION.
      *
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    WRK-TABLE           TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    "9003"              TO  SPA-ERRCD
               DISPLAY "Z04 SRYCDCHG DEL ERR:" MCP-RC
                       ",KEY:" CHG-KEY
           END-IF
      *
           .
       41291-SRYCDCHG-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角チェック処理
      *****************************************************************
       400-ZENCHK-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
      *        全角半角混在エラー
               MOVE    KANACHK-MAE-INPUT   TO  WRK-OUT-INPUT 
               MOVE    1                   TO  FLG-ERR
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT 
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC                      SECTION.
      *
           IF  SPA-GMN-YUKOSTYMD                 =  SPACE
               MOVE    "0000000"                 TO  SPA-GMN-YUKOSTYMD
           END-IF.
      *
           MOVE  SPA-GMN-YUKOSTYMD               TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       501-YUKOSTYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効終了年月日チェック
      *****************************************************************
       502-YUKOEDYMDCHK-SEC                      SECTION.
      *
           IF  SPA-GMN-YUKOEDYMD                 =  SPACE
               MOVE   "9999999"                  TO  SPA-GMN-YUKOEDYMD
           END-IF.
      *
           MOVE  SPA-GMN-YUKOEDYMD               TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   カナ名称チェック
      *****************************************************************
       503-KANANAME-CHK-SEC                  SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   1
               MOVE    "0003"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称チェック
      *****************************************************************
       503-NAME-CHK-SEC                  SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   1
               MOVE    "0004"              TO  SPA-ERRCD
               MOVE    4                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-NAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    正式名称チェック
      *****************************************************************
       503-FORMALNAME-CHK-SEC                  SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   1
               MOVE    "0005"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-FORMALNAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    処方名称チェック
      *****************************************************************
       503-SHONAME-CHK-SEC                  SECTION.
      *
           MOVE    SPA-GMN-SHONAME     TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SHONAME
           IF      FLG-ERR             =   1
               MOVE    "0006"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-SHONAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額種別チェック
      *****************************************************************
       503-TENSIKIBETU-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >   SPA-TENSKB-MAX
               IF      SPA-GMN-TENSIKIBETU
                                       =   SPA-GMN-TENSIKIBETUL (IDXX)
                   MOVE    SPA-GMN-TENSIKIBETULST (IDXX)
                                           TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-TENSKB-MAX  TO  IDXX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-TENSIKIBETU-CHK-EXT.
           EXIT.
      *****************************************************************
      *    金額種別チェック
      *****************************************************************
       503-OLDTENSKB-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >   SPA-TENSKB-MAX
               IF      SPA-GMN-OLDTENSKB
                                       =   SPA-GMN-OLDTENSKBL (IDXX)
                   MOVE    SPA-GMN-OLDTENSKBLST (IDXX)
                                           TO  SPA-GMN-OLDTENSKB-G
                   MOVE    1               TO  FLG-OK
                   MOVE    SPA-TENSKB-MAX  TO  IDXX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    10                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       503-OLDTENSKB-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　単位コードチェック
      *****************************************************************
       5012-TANICD-CHK-SEC                     SECTION.
      *
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-TANICD
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   SPA-GMN-TANICD-MAX ) OR
                              (FLG-OK  =   1              )
               IF      SPA-GMN-TANICD      =   SPA-GMN-TANICDL (IDXX)
                   MOVE    SPA-GMN-TANICDLST (IDXX)
                                           TO  SPA-GMN-TANICD-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0008"              TO  SPA-ERRCD
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       5012-TANICD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    麻毒チェック
      *****************************************************************
       5013-MADOKUKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL       IDXX    >   5
               IF      SPA-GMN-MADOKUKBN
                                       =   SPA-GMN-MADOKUKBNL (IDXX)
                   MOVE    SPA-GMN-MADOKUKBNLST (IDXX)
                                           TO  SPA-GMN-MADOKUKBN-G
                   MOVE    1               TO  FLG-OK
                   MOVE    5               TO  IDXX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0009"              TO  SPA-ERRCD
               MOVE    12                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-MADOKUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    神経破壊剤チェック
      *****************************************************************
       5013-SINKEIHAKAIKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   2   )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SINKEIHAKAIKBN
                                       =   SPA-GMN-SINKEIHAKAIKBNL(IDXX)
                   MOVE    SPA-GMN-SINKEIHAKAIKBNLST (IDXX)
                                           TO  SPA-GMN-SINKEIHAKAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-SINKEIHAKAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    生物学的製剤チェック
      *****************************************************************
       5013-SEIBUTUGAKUKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   2   )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SEIBUTUGAKUKBN
                                       =   SPA-GMN-SEIBUTUGAKUKBNL(IDXX)
                   MOVE    SPA-GMN-SEIBUTUGAKUKBNLST (IDXX)
                                           TO  SPA-GMN-SEIBUTUGAKUKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0011"              TO  SPA-ERRCD
               MOVE    14                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-SEIBUTUGAKUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    造影剤チェック
      *****************************************************************
       5013-ZOEIZAIKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   3   )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-ZOEIZAIKBN
                                       =   SPA-GMN-ZOEIZAIKBNL(IDXX)
                   MOVE    SPA-GMN-ZOEIZAIKBNLST (IDXX)
                                           TO  SPA-GMN-ZOEIZAIKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0012"              TO  SPA-ERRCD
               MOVE    15                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-ZOEIZAIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    薬剤チェック
      *****************************************************************
       5013-YKZKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   6   )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-YKZKBN  =   SPA-GMN-YKZKBNL(IDXX)
                   MOVE    SPA-GMN-YKZKBNLST (IDXX)
                                           TO  SPA-GMN-YKZKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0013"              TO  SPA-ERRCD
               MOVE    17                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-YKZKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    剤型チェック
      *****************************************************************
       5013-ZAIGATAKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   4   )   OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-ZAIGATAKBN  =   SPA-GMN-ZAIGATAKBNL(IDXX)
                   MOVE    SPA-GMN-ZAIGATAKBNLST (IDXX)
                                           TO  SPA-GMN-ZAIGATAKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0014"              TO  SPA-ERRCD
               MOVE    18                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-ZAIGATAKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    後発医薬品チェック
      *****************************************************************
       5013-KOUHATUKBN-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDXX    FROM    1   BY  1
                   UNTIL      (IDXX    >   SPA-GMN-KOUHATUKBN-MAX) OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-KOUHATUKBN  =   SPA-GMN-KOUHATUKBNL(IDXX)
                   MOVE    SPA-GMN-KOUHATUKBNLST (IDXX)
                                           TO  SPA-GMN-KOUHATUKBN-G
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0015"              TO  SPA-ERRCD
               MOVE    20                  TO  SPA-GMN-CUR
           END-IF
           .
      *
       5013-KOUHATUKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　経過措置年月日チェック
      *****************************************************************
       523-KEIKASOCHIYMDCHK-SEC                  SECTION.
      *
           IF    SPA-GMN-KEIKASOCHIYMD   =  SPACE
               MOVE  ZERO            TO  SPA-GMN-KEIKASOCHIYMD(1:7)
               MOVE  ZERO            TO  SPA-NAI-KEIKASOCHIYMD
           ELSE
               MOVE  SPA-GMN-KEIKASOCHIYMD     TO  WRK-LNKYMD
               PERFORM  831-YMDCONV12-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD        TO  SPA-NAI-KEIKASOCHIYMD
                   MOVE    WRK-LNKYMD      TO  SPA-GMN-KEIKASOCHIYMD
               ELSE
                   MOVE    "0016"              TO  SPA-ERRCD
                   MOVE    25                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       523-KEIKASOCHIYMDCHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *    入力チェック区分チェック
      *****************************************************************
       524-INPCHKKBNCHK-SEC        SECTION.
      *
           .
       524-INPCHKKBNCHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *    分類区分チェック
      *****************************************************************
       524-BUNRUIKBNCHK-SEC                    SECTION.
      *
           IF      SPA-GMN-BUNRUIKBN   =   SPACE
               GO  TO  524-BUNRUIKBNCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-BUNRUIKBN-MAX )
      *
               IF    ( SPA-GMN-BUNRUIKBN
                                  =    SPA-GMN-BUNRUIKBNL (IDX1))
                   MOVE    SPA-GMN-BUNRUIKBNLST (IDX1)
                                       TO   SPA-GMN-BUNRUIKBN-G
                   MOVE    1           TO   FLG-OK
                   MOVE    SPA-GMN-BUNRUIKBN-MAX
                                       TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (SPA-GMN-BUNRUIKBN-MAX   >   ZERO  )  AND
                  (FLG-OK                  =   ZERO )
               MOVE    "0017"              TO  SPA-ERRCD
               MOVE    27                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       524-BUNRUIKBNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    全数量記載区分チェック
      *****************************************************************
       524-ALLSURYOKBNCHK-SEC      SECTION.
      *
           IF      SPA-GMN-ALLSURYOKBN   =   SPACE
               GO  TO  524-ALLSURYOKBNCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   2 )
      *
               IF    ( SPA-GMN-ALLSURYOKBN
                                  =    SPA-GMN-ALLSURYOKBNL (IDX1))
                   MOVE    SPA-GMN-ALLSURYOKBNLST (IDX1)
                                       TO   SPA-GMN-ALLSURYOKBN-G
                   MOVE    1           TO   FLG-OK
                   MOVE    2           TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (FLG-OK                  =   ZERO )
               MOVE    "0018"              TO  SPA-ERRCD
               MOVE    28                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       524-ALLSURYOKBNCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量換算単位コードチェック
      *****************************************************************
       524-KANZANTANICDCHK-SEC     SECTION.
      *
           IF      SPA-GMN-KANZANTANICD   =   SPACE
               GO  TO  524-KANZANTANICDCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
      *
      *    分類区分
           PERFORM VARYING IDX1    FROM    1   BY  1
                   UNTIL ( IDX1    >   SPA-GMN-TANICD-MAX )
      *
               IF    ( SPA-GMN-KANZANTANICD
                                  =    SPA-GMN-TANICDL (IDX1))
                   MOVE    SPA-GMN-TANICDLST (IDX1)
                                       TO   SPA-GMN-KANZANTANICD-G
                   MOVE    1           TO   FLG-OK
                   MOVE    SPA-GMN-TANICD-MAX
                                       TO   IDX1
               END-IF
      *
           END-PERFORM
      *
           IF     (SPA-GMN-TANICD-MAX      >   ZERO  )  AND
                  (FLG-OK                  =   ZERO )
               MOVE    "0019"              TO  SPA-ERRCD
               MOVE    29                  TO  SPA-GMN-CUR
           END-IF
      *
      *    単位コードと一致しないこと
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-KANZANTANICD    =   SPA-GMN-TANICD
                   MOVE    "0035"              TO  SPA-ERRCD
                   MOVE    29                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    単位コード 000 はエラー
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-KANZANTANICD    =   "000"
                   MOVE    "0036"              TO  SPA-ERRCD
                   MOVE    29                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       524-KANZANTANICDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    換算値チェック
      *****************************************************************
       524-KANZANCHICHK-SEC        SECTION.
      *
           .
       524-KANZANCHICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    湿布薬関係チェック
      *****************************************************************
       523-SIPPKBN-CHK-SEC        SECTION.
      *
           .
       523-SIPPKBN-CHK-EXT.
           EXIT.
      *    
      *****************************************************************
      *    総量編集チェック
      *****************************************************************
       523-SOURYOHENKBN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-GMN-SOURYO-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-GMN-SOURYOKBN
                                       =   SPA-GMN-SOURYOKBN-LST
                                                        (IDX1)(1:1)
                   MOVE    SPA-GMN-SOURYOKBN-LST(IDX1)
                                       TO  SPA-GMN-SOURYOKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0031"              TO  SPA-ERRCD
               MOVE    37                  TO  SPA-GMN-CUR
           ELSE
               IF     (SPA-GMN-SOURYOKBN    NOT =   ZERO )
                   IF     (SPA-GMN-YKZKBN      =   "1"
                                               OR  "3"      )
                       CONTINUE
                   ELSE
                       MOVE    "0026"              TO  SPA-ERRCD
                       MOVE    37                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       523-SOURYOHENKBN-EXT.
           EXIT.
      *****************************************************************
      *    内服算定チェック
      *****************************************************************
       523-NAIFUKUKBN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-GMN-NAIFUKU-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-GMN-NAIFUKUKBN
                                       =   SPA-GMN-NAIFUKUKBN-LST
                                                        (IDX1)(1:1)
                   MOVE    SPA-GMN-NAIFUKUKBN-LST(IDX1)
                                       TO  SPA-GMN-NAIFUKUKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0027"              TO  SPA-ERRCD
               MOVE    36                  TO  SPA-GMN-CUR
           ELSE
               IF     (SPA-GMN-NAIFUKUKBN  NOT =   ZERO )
                   IF     (SPA-GMN-YKZKBN      =   "1"
                                               OR  "3"      )
                       MOVE    "0028"              TO  SPA-ERRCD
                       MOVE    36                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       523-NAIFUKUKBN-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載チェック
      *****************************************************************
       523-IPN-KISAIKBN-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-GMN-IPN-KISAI-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-GMN-IPN-KISAIKBN
                                       =   SPA-GMN-IPN-KISAIKBN-LST
                                                        (IDX1)(1:1)
                   MOVE    SPA-GMN-IPN-KISAIKBN-LST(IDX1)
                                       TO  SPA-GMN-IPN-KISAIKBN-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0029"              TO  SPA-ERRCD
               MOVE    38                  TO  SPA-GMN-CUR
           END-IF
      *    一般名記載
           IF      SPA-GMN-IPN-KISAIKBN    =   "1"
               IF      SPA-GMN-GENERICNAME     =   SPACE
                   MOVE    "0032"              TO  SPA-ERRCD
                   MOVE    38                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-GMN-IPN-KISAIKBN    =   "2"
               IF      SPA-GMN-SHONAME         =   SPACE
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    38                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           IF      SPA-GMN-IPN-KISAIKBN    =   "3"
               IF      SPA-GMN-SHONAME         =   SPACE
                   MOVE    "0033"              TO  SPA-ERRCD
                   MOVE    38                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       523-IPN-KISAIKBN-EXT.
           EXIT.
      *****************************************************************
      *    一般名記載単位チェック
      *****************************************************************
       523-IPN-TANICD-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL      (IDX1    >   SPA-GMN-TANICD-MAX )
                           OR (FLG-OK  =   1           )
               IF      SPA-GMN-IPN-TANICD
                                       =   SPA-GMN-TANICDL(IDX1)
                   MOVE    SPA-GMN-TANICDLST (IDX1)
                                       TO  SPA-GMN-IPN-TANICD-G
                   MOVE    1           TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-GMN-IPN-TANICD  =   SPACE
               CONTINUE
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0030"              TO  SPA-ERRCD
                   MOVE    39                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       523-IPN-TANICD-EXT.
           EXIT.
      *
      *****************************************************************
      *    長期投与期限チェック
      *****************************************************************
       524-CHOKIKIGEN-SEC        SECTION.
      *
           IF      SPA-GMN-CHOKIKIGEN  =   SPACE
               MOVE    SPACE               TO  SPA-NAI-CHOKIKIGEN
           ELSE
               MOVE    SPA-GMN-CHOKIKIGEN  TO  WRK-LNKYMD
               PERFORM 831-YMDCONV12-SEC
               IF      FLG-ERR             =   ZERO
                   MOVE    WRK-SYMD            TO  SPA-NAI-CHOKIKIGEN
                   MOVE    WRK-LNKYMD          TO  SPA-GMN-CHOKIKIGEN
      *            日数があること
                   IF      SPA-GMN-CHOKINISSU  =   ZERO
                       MOVE    "0025"              TO  SPA-ERRCD
                       MOVE    32                  TO  SPA-GMN-CUR
                   END-IF
               ELSE
                   MOVE    "0020"              TO  SPA-ERRCD
                   MOVE    33                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       524-CHOKIKIGEN-EXT.
           EXIT.
      *****************************************************************
      *    長期投与エラーチェック
      *****************************************************************
       524-CHOKIERR-SEC        SECTION.
      *
           IF      SPA-GMN-CHOKIERR    =   0  OR  9
               CONTINUE
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    34                  TO  SPA-GMN-CUR
           END-IF
           .
       524-CHOKIERR-EXT.
           EXIT.
      *****************************************************************
      *    投与量エラー処理チェック
      *****************************************************************
       524-TOUYOERR-SEC        SECTION.
      *
           IF      SPA-GMN-TOUYOERR    =   0  OR  9
               CONTINUE
           ELSE
               MOVE    "0021"              TO  SPA-ERRCD
               MOVE    35                  TO  SPA-GMN-CUR
           END-IF
           .
       524-TOUYOERR-EXT.
           EXIT.
      * 
      *****************************************************************
      *    投与量チェック
      *****************************************************************
       524-TOUYO-CHK-SEC        SECTION.
      *
           PERFORM VARYING IDX1        FROM    1   BY  1
                   UNTIL  (IDX1        >   10  )  OR
                          (SPA-ERRCD   NOT =   SPACE )
               PERFORM 5241-KGNAGE-CHK-SEC
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 5241-JGNAGE-CHK-SEC
               END-IF
           END-PERFORM
           .
       524-TOUYO-CHK-EXT.
           EXIT.
      * 
      *****************************************************************
      *    年齢コードチェック処理
      *****************************************************************
       5300-NENREICD-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-NENREI-CD
           MOVE    ZERO                TO  WRK-NENREI
      *
           SET     NENREI-IDX          TO  1
           SEARCH      NENREI-TBL-OCC VARYING   NENREI-IDX
               AT  END
                   MOVE    ZERO            TO  FLG-NENREI-CD
               WHEN    NENREI-NENCD(NENREI-IDX) 
                                           =   WRK-NENCD
                   MOVE    1               TO  FLG-NENREI-CD
                   MOVE    NENREI-DD (NENREI-IDX) 
                                           TO  WRK-NEN-DD
                   MOVE    NENREI-MM (NENREI-IDX) 
                                           TO  WRK-NEN-MM
           END-SEARCH
           .
       5300-NENREICD-CHK-EXT.
           EXIT.
      * 
      *****************************************************************
      *    下限年齢チェック処理
      *****************************************************************
       5241-KGNAGE-CHK-SEC             SECTION.
      *
           IF     (SPA-GMN-TOUYOKGNAGE (IDX1)  =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-TOUYOKGNAGE (IDX1)
           END-IF
           MOVE    ZERO                TO  WRK-KGN-NENREI
           MOVE    SPA-GMN-TOUYOKGNAGE (IDX1)  TO  WRK-NENCD
           PERFORM 5300-NENREICD-CHK-SEC
           IF      FLG-NENREI-CD       =   1
               MOVE    WRK-NEN-DD          TO  WRK-KGN-DD
               MOVE    WRK-NEN-MM          TO  WRK-KGN-MM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-TOUYOKGNAGE (IDX1)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0022"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-TOUYOKGNAGE
                                                                 (IDX1)
                   MOVE    SPA-GMN-TOUYOKGNAGE (IDX1)
                                               TO  WRK-KGN-YY
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               IF      IDX1            =   10
                   MOVE    110             TO  SPA-GMN-CUR
               ELSE
                   COMPUTE SPA-GMN-CUR     =   110     +   IDX1
               END-IF
           END-IF
           .
       5241-KGNAGE-CHK-EXT.
           EXIT.
      *****************************************************************
      *    上限年齢チェック処理
      *****************************************************************
       5241-JGNAGE-CHK-SEC             SECTION.
      *
           IF     (SPA-GMN-TOUYOJGNAGE (IDX1)  =   SPACE )
               MOVE    ZERO            TO  SPA-GMN-TOUYOJGNAGE (IDX1)
           END-IF
           MOVE    ZERO                TO  WRK-JGN-NENREI
           MOVE    SPA-GMN-TOUYOJGNAGE (IDX1)  TO  WRK-NENCD
           PERFORM 5300-NENREICD-CHK-SEC
           IF      FLG-NENREI-CD       =   1
               MOVE    WRK-NEN-DD          TO  WRK-JGN-DD
               MOVE    WRK-NEN-MM          TO  WRK-JGN-MM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-TOUYOJGNAGE (IDX1)  TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0022"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)   TO  SPA-GMN-TOUYOJGNAGE
                                                                 (IDX1)
                   MOVE    SPA-GMN-TOUYOJGNAGE (IDX1)
                                               TO  WRK-JGN-YY
               END-IF
           END-IF
           IF     (SPA-GMN-TOUYOJGNAGE (IDX1)  =   ZERO )  OR
                  (SPA-GMN-TOUYOKGNAGE (IDX1)  =   ZERO )
               CONTINUE
           ELSE
               IF     SPA-GMN-TOUYOKGNAGE (IDX1)
                                       =   SPA-GMN-TOUYOJGNAGE (IDX1)
                   MOVE    "0024"              TO  SPA-ERRCD
               ELSE
      *            下限＞上限年齢はエラー
                   IF     WRK-KGN-NENREI   >   WRK-JGN-NENREI
                       MOVE    "0024"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               IF      IDX1            =   10
                   MOVE    120             TO  SPA-GMN-CUR
               ELSE
                   COMPUTE SPA-GMN-CUR     =   120     +   IDX1
               END-IF
           END-IF
           .
       5241-JGNAGE-CHK-EXT.
           EXIT.
      * 
      *****************************************************************
      *    経過措置コードチェック処理
      *****************************************************************
       524-CHNGCD-CHK-SEC             SECTION.
      *
           IF      SPA-GMN-CHNGCD      =   SPACE
               MOVE    SPACE               TO  SPA-GMN-CHNGCDNAME
           ELSE
               IF     (SPA-NAI-YUKOEDYMD   =   "99999999" )  OR
                      (SPA-NAI-KEIKASOCHIYMD
                                      NOT  =   SPA-NAI-YUKOEDYMD)
                   MOVE    "8002"              TO  SPA-ERRCD
                   MOVE    31                  TO  SPA-GMN-CUR
               ELSE
      *            点数マスタ編集
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    SPA-GMN-CHNGCD      TO  TNS-SRYCD
                   MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
                   MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_tensu"         TO  MCP-TABLE
                       MOVE    "key24"             TO  MCP-PATHNAME
                       PERFORM 900-TENSU-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-TENSU
                       INITIALIZE                      TNS-TENSU-REC
                   END-IF
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME        TO  SPA-GMN-CHNGCDNAME
                       IF      TNS-YUKOEDYMD   <=  SPA-NAI-YUKOEDYMD
                           MOVE    "8001"          TO  SPA-ERRCD
                           MOVE    31              TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    "8000"          TO  SPA-ERRCD
                       MOVE    31              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-Z01-SRYCD (1:1) NOT =   "6"
                   PERFORM 5241-SRYCDCHG-CHK-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-CHNGCD      TO  SPA-NAI-CHNGCD
           END-IF
      *
           .
       524-CHNGCD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    経過措置コードチェック処理
      *****************************************************************
       524-CHNGCDU-CHK-SEC            SECTION.
      *
           IF     (SPA-GMN-CHNGCDU     =   SPACE)  OR
                 ((SPA-GMN-CHNGCDU     =   "999999999")    AND
                  (SPA-GMN-CHNGCD  NOT =   SPACE))
               MOVE    SPACE               TO  SPA-GMN-CHNGCDNAMEU
           ELSE
               IF     (SPA-NAI-YUKOEDYMD   =   "99999999" )  OR
                      (SPA-NAI-KEIKASOCHIYMD
                                      NOT  =   SPA-NAI-YUKOEDYMD)
                   MOVE    "8002"              TO  SPA-ERRCD
                   MOVE    48                  TO  SPA-GMN-CUR
               ELSE
      *            点数マスタ編集
                   INITIALIZE                      TNS-TENSU-REC
                   MOVE    SPA-GMN-CHNGCDU     TO  TNS-SRYCD
                   MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
                   MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_tensu"         TO  MCP-TABLE
                       MOVE    "key24"             TO  MCP-PATHNAME
                       PERFORM 900-TENSU-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-TENSU
                       INITIALIZE                      TNS-TENSU-REC
                   END-IF
                   MOVE    "tbl_tensu"         TO  MCP-TABLE
                   MOVE    "key24"             TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
                   IF      FLG-TENSU           =   ZERO
                       MOVE    TNS-NAME        TO  SPA-GMN-CHNGCDNAMEU
                       IF      TNS-YUKOEDYMD   <=  SPA-NAI-YUKOEDYMD
                           MOVE    "8001"          TO  SPA-ERRCD
                           MOVE    48              TO  SPA-GMN-CUR
                       END-IF
                   ELSE
                       MOVE    "8000"          TO  SPA-ERRCD
                       MOVE    48              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 5241-SRYCDCHG-CHK-SEC
      *    END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-CHNGCDU     TO  SPA-NAI-CHNGCDU
           END-IF
      *
           .
       524-CHNGCDU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    置換えコードチェック処理
      *****************************************************************
       5241-SRYCDCHG-CHK-SEC             SECTION.
      *
           INITIALIZE                  SRYCDCHG-REC 
           MOVE    SPA-GMN-CHNGCD      TO  CHG-RJNSRYCD
      *
           MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
           MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srycdchg"      TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 990-SRYCDCHG-READ-SEC
           ELSE
               INITIALIZE                  SRYCDCHG-REC
               MOVE    1               TO  FLG-SRYCDCHG
           END-IF
           MOVE    "tbl_srycdchg"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYCDCHG        =   ZERO
               IF      CHG-IPNSRYCD    NOT =   SPA-Z01-SRYCD
                   MOVE    "8003"          TO  SPA-ERRCD
                   MOVE    31              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       5241-SRYCDCHG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN-SEC             SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-ZIDCD       NOT =   SPACE
               PERFORM 520-ZIDSET-SEC
               GO  TO  500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z04    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面セット処理
      *****************************************************************
       500-GMNHEN-SEC                            SECTION.
      *     画面ワークエリアにセット
           MOVE    SPACE               TO  Z04
           INITIALIZE                  Z04
      *
           MOVE  SPA-GMN-SRYCDH        TO  Z04-SRYCDH
           MOVE  SPA-GMN-NAMEH         TO  Z04-NAMEH
           MOVE  SPA-GMN-YUKOSTYMDH    TO  Z04-YUKOSTYMDH
           MOVE  SPA-GMN-YUKOEDYMDH    TO  Z04-YUKOEDYMDH
      *                                         
           MOVE  SPA-GMN-YUKOSTYMD     TO  Z04-YUKOSTYMD
           MOVE  SPA-GMN-YUKOEDYMD     TO  Z04-YUKOEDYMD
           MOVE  SPA-GMN-KANANAME      TO  Z04-KANANAME
           MOVE  SPA-GMN-NAME          TO  Z04-NAME
           MOVE  SPA-GMN-FORMALNAME    TO  Z04-FORMALNAME
           MOVE  SPA-GMN-SHONAME       TO  Z04-SHONAME
           MOVE  SPA-GMN-TEN           TO  Z04-TEN
           MOVE  SPA-GMN-TENSIKIBETU-G TO  Z04-TENSIKIBETU
           MOVE  SPA-GMN-OLDTEN        TO  Z04-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB-G   TO  Z04-OLDTENSKB
           MOVE  SPA-GMN-TANICD-G      TO  Z04-TANICD
      **** MOVE  SPA-GMN-TANINAME      TO  Z04-TANINAME
           MOVE  SPA-GMN-MADOKUKBN-G   TO  Z04-MADOKUKBN
           MOVE  SPA-GMN-SINKEIHAKAIKBN-G
                                       TO  Z04-SINKEIHAKAIKBN
           MOVE  SPA-GMN-SEIBUTUGAKUKBN-G
                                       TO  Z04-SEIBUTUGAKUKBN
           MOVE  SPA-GMN-ZOEIZAIKBN-G  TO  Z04-ZOEIZAIKBN
           MOVE  SPA-GMN-CSYYOURYO     TO  Z04-CSYYOURYO
           MOVE  SPA-GMN-YKZKBN-G      TO  Z04-YKZKBN
           MOVE  SPA-GMN-ZAIGATAKBN-G  TO  Z04-ZAIGATAKBN
      *    後発医薬品
           MOVE  SPA-GMN-KOUHATUKBN-G  TO  Z04-KOUHATUKBN
           MOVE  SPA-GMN-LONGTOYOKBN   TO  Z04-LONGTOYOKBN
           MOVE  SPA-GMN-KOUHYOJYUNNUM TO  Z04-KOUHYOJYUNNUM
           MOVE  SPA-GMN-YAKKAKJNCD    TO  Z04-YAKKAKJNCD
           MOVE  SPA-GMN-SYUSAISKB     TO  Z04-SYUSAISKB
           MOVE  SPA-GMN-SYOMEIKANREN  TO  Z04-SYOMEIKANREN
           MOVE  SPA-GMN-KEIKASOCHIYMD TO  Z04-KEIKASOCHIYMD
      *     
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-TENSKB-MAX
               MOVE    SPA-GMN-TENSIKIBETULST (IDXX)
                                       TO  Z04-TENSIKIBETULST   (IDXX)
           END-PERFORM
           MOVE    SPA-TENSKB-MAX      TO  Z04-TENSIKIBETU-COUNT
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-TENSKB-MAX
               MOVE    SPA-GMN-OLDTENSKBLST(IDXX)
                                       TO  Z04-OLDTENSKBLST (IDXX)
           END-PERFORM    
           MOVE    SPA-TENSKB-MAX      TO  Z04-OLDTENSKB-COUNT
      *
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDXX    FROM    1   BY  1
      ************ UNTIL   IDXX    >       59
                   UNTIL   IDXX    >       SPA-GMN-TANICD-MAX
               IF      SPA-GMN-TANICDLST(IDXX)  NOT =   SPACE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-TANICDLST(IDXX)
                                           TO  Z04-TANICDLST   (IDX)
                   MOVE    IDX             TO  Z04-TANICD-COUNT
               END-IF
           END-PERFORM
      *
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       5
               MOVE    SPA-GMN-MADOKUKBNLST (IDXX)
                                       TO  Z04-MADOKUKBNLST   (IDXX)
           END-PERFORM
           MOVE    5                   TO  Z04-MADOKUKBN-COUNT 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               MOVE    SPA-GMN-SINKEIHAKAIKBNLST(IDXX)
                                       TO  Z04-SINKEIHAKAIKBNLST (IDXX)
           END-PERFORM    
           MOVE    2                   TO  Z04-SINKEIHAKAIKBN-COUNT
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               MOVE    SPA-GMN-SEIBUTUGAKUKBNLST(IDXX)
                                       TO  Z04-SEIBUTUGAKUKBNLST(IDXX)
           END-PERFORM    
           MOVE    2                   TO  Z04-SEIBUTUGAKUKBN-COUNT
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       3
               MOVE    SPA-GMN-ZOEIZAIKBNLST (IDXX)
                                       TO  Z04-ZOEIZAIKBNLST   (IDXX)
           END-PERFORM
           MOVE    3                   TO  Z04-ZOEIZAIKBN-COUNT 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       6
               MOVE    SPA-GMN-YKZKBNLST(IDXX)
                                       TO  Z04-YKZKBNLST (IDXX)
           END-PERFORM    
           MOVE    6                   TO  Z04-YKZKBN-COUNT 
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       4
               MOVE    SPA-GMN-ZAIGATAKBNLST(IDXX)
                                       TO  Z04-ZAIGATAKBNLST   (IDXX)
           END-PERFORM    
           MOVE    4                   TO  Z04-ZAIGATAKBN-COUNT
      *    後発医薬品
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-KOUHATUKBN-MAX
               MOVE    SPA-GMN-KOUHATUKBNLST(IDXX)
                                       TO  Z04-KOUHATUKBN-ITEM  (IDXX)
           END-PERFORM
           MOVE    SPA-GMN-KOUHATUKBN-MAX  TO  Z04-KOUHATUKBN-COUNT
      *
           MOVE  SPA-GMN-INPCHKKBN     TO  Z04-INPCHKKBN
      *
      *    分類区分
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-BUNRUIKBN-MAX
               MOVE    SPA-GMN-BUNRUIKBNLST(IDXX)
                                       TO  Z04-BUNRUIKBNLST  (IDXX)
           END-PERFORM
           MOVE    SPA-GMN-BUNRUIKBN-MAX
                                       TO  Z04-BUNRUIKBN-COUNT
           MOVE    SPA-GMN-BUNRUIKBN-G TO  Z04-BUNRUIKBN
      *
           MOVE    SPA-GMN-CHNGCD      TO  Z04-CHNGCD
           MOVE    SPA-GMN-CHNGCDNAME  TO  Z04-CHNGENAME
           MOVE    SPA-GMN-CHNGCDU     TO  Z04-CHNGCDU
           MOVE    SPA-GMN-CHNGCDNAMEU TO  Z04-CHNGENAMEU
      *
      *    全数量記載区分
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       2
               MOVE    SPA-GMN-ALLSURYOKBNLST(IDXX)
                                       TO  Z04-ALLSURYOKBNLST  (IDXX)
           END-PERFORM
           MOVE    2                   TO  Z04-ALLSURYOKBN-COUNT
           MOVE    SPA-GMN-ALLSURYOKBN-G   TO  Z04-ALLSURYOKBN
      *
      *    数量換算単位コード
           MOVE    ZERO                TO  IDX
           PERFORM VARYING IDXX    FROM    1   BY  1
                   UNTIL   IDXX    >       SPA-GMN-TANICD-MAX
               IF      SPA-GMN-TANICDLST(IDXX)  NOT =   SPACE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-TANICDLST(IDXX)
                                           TO  Z04-KANZANTANICDLST(IDX)
                   MOVE    IDX             TO  Z04-KANZANTANICD-COUNT
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-KANZANTANICD-G  TO  Z04-KANZANTANICD
      *
      *    数量換算値
           MOVE  SPA-GMN-KANZANCHI     TO  Z04-KANZANCHI
      *    総量編集
           MOVE    SPA-GMN-SOURYOKBN-G  TO  Z04-SOURYOKBN
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-GMN-SOURYO-MAX
               MOVE    SPA-GMN-SOURYOKBN-LST (IDX-T)
                                       TO  Z04-SOURYOKBN-ITEM (IDX-T)
           END-PERFORM
           MOVE    SPA-GMN-SOURYO-MAX  TO  Z04-SOURYOKBN-COUNT
      *
           MOVE    SPA-GMN-NAIFUKUKBN-G    TO  Z04-NAIFUKUKBN
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-GMN-NAIFUKU-MAX
               MOVE    SPA-GMN-NAIFUKUKBN-LST (IDX-T)
                                       TO  Z04-NAIFUKUKBN-ITEM (IDX-T)
           END-PERFORM
           MOVE    SPA-GMN-NAIFUKU-MAX TO  Z04-NAIFUKUKBN-COUNT
      *
           MOVE    SPA-GMN-IPN-KISAIKBN-G  TO  Z04-IPNKISAIKBN
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-GMN-IPN-KISAI-MAX
               MOVE    SPA-GMN-IPN-KISAIKBN-LST (IDX-T)
                                       TO  Z04-IPNKISAIKBN-ITEM (IDX-T)
           END-PERFORM
           MOVE    SPA-GMN-IPN-KISAI-MAX   TO  Z04-IPNKISAIKBN-COUNT
      *
           MOVE    SPA-GMN-IPN-TANICD-G    TO  Z04-IPNTANICD 
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL       IDX-T   >   SPA-GMN-TANICD-MAX
               MOVE    SPA-GMN-TANICDLST (IDX-T)
                                       TO  Z04-IPNTANICD-ITEM (IDX-T)
           END-PERFORM
           MOVE    SPA-GMN-TANICD-MAX  TO  Z04-IPNTANICD-COUNT
      *
           MOVE    SPA-GMN-IPN-KANZANCHI   TO  Z04-IPNKANZANCHI
      *
      *    長期投与
           MOVE    SPA-GMN-CHOKINISSU  TO  Z04-CHOKINISSU
           MOVE    SPA-GMN-CHOKIKIGEN  TO  Z04-CHOKIKIGEN
           MOVE    SPA-GMN-CHOKIERR    TO  Z04-CHOKIERR
           MOVE    SPA-GMN-TOUYOERR    TO  Z04-TOUYOERR
      *    投与量
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       10
               MOVE    SPA-GMN-TOUYOKGNAGE (IDX)   TO  Z04-KGNAGE (IDX)
               MOVE    SPA-GMN-TOUYOJGNAGE (IDX)   TO  Z04-JGNAGE (IDX)
               MOVE    SPA-GMN-TOUYORYO1   (IDX)   TO  Z04-TOUYORYO1
                                                                 (IDX)
               MOVE    SPA-GMN-TOUYORYO2   (IDX)   TO  Z04-TOUYORYO2
                                                                 (IDX)
           END-PERFORM
      *
           MOVE    SPA-GMN-TUKIJGNKAISU    TO  Z04-TUKIJGNKAISU
           MOVE    SPA-GMN-TUKIJGNTOUYORYO TO  Z04-TUKIJGNTOUYORYO
      *
           MOVE    SPA-GMN-GENERICNAME TO  Z04-GENERICNAME
           MOVE    SPA-GMN-GENERICHEN  TO  Z04-GENERICHEN
      *H28.4
      *    規格単位数
           MOVE    SPA-GMN-KIKAKUTANI  TO  Z04-KIKAKUTANI
      *    湿布薬区分
           MOVE    SPA-GMN-SIPPKBN     TO  Z04-SIPPKBN
      *    湿布薬
           MOVE    SPA-GMN-SIPRYOU1    TO  Z04-SIPRYOU1
           MOVE    SPA-GMN-SIPIRISU1   TO  Z04-SIPIRISU1
           MOVE    SPA-GMN-SIPRYOU2    TO  Z04-SIPRYOU2
           MOVE    SPA-GMN-SIPIRISU2   TO  Z04-SIPIRISU2
      *R02.2
      *    ユーザー単位
           MOVE    SPA-GMN-USER-TANICD-G   TO  Z04-USER-TANICD
           MOVE    "black"             TO  Z04-USER-TANICD-STYLE
           IF      SPA-GMN-USER-TANICD     =   SPACE
                                           OR  ZERO
               CONTINUE
           ELSE
               IF      SPA-GMN-TANICD      =   SPACE
                                           OR  ZERO
                   CONTINUE
               ELSE
                   MOVE    "red"       TO  Z04-USER-TANICD-STYLE
               END-IF
           END-IF
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z04-MIDASHI-DATA
               MOVE    "red"               TO  Z04-MIDASHI-STYLE
           END-IF
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               MOVE    WIDGET-NORMAL           TO  Z04-B03-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE      TO  Z04-B03-STATE
           END-IF
      *H30.9
      *    新規・改正処理はなし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE      TO  Z04-B03-STATE
           END-IF
      *
      *H30.12
           IF      SPA-NAI-SIPKBN      =   "1"
               MOVE    "湿布薬関係　※"    TO  Z04-LABEL2-DATA
               MOVE    "black"             TO  Z04-LABEL2-STYLE
           ELSE
               MOVE    "湿布薬関係"        TO  Z04-LABEL2-DATA
               MOVE    "black"             TO  Z04-LABEL2-STYLE
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
             IF      SPA-GMN-CUR       =   ZERO
                 PERFORM 50011-CUR-SET-SEC
             END-IF
      *
             EVALUATE    SPA-GMN-CUR
                  WHEN   1
                      MOVE  "YUKOSTYMD"        TO  MCP-WIDGET
                  WHEN   2
                      MOVE  "YUKOEDYMD"        TO  MCP-WIDGET
                  WHEN   3
                      MOVE  "KANANAME"         TO  MCP-WIDGET
                  WHEN   4
                      MOVE  "NAME"             TO  MCP-WIDGET
                  WHEN   5
                      MOVE  "SHONAME"          TO  MCP-WIDGET
                  WHEN   6
                      MOVE  "FORMALNAME"       TO  MCP-WIDGET
                  WHEN   7
                      MOVE  "TEN"              TO  MCP-WIDGET
                  WHEN   8
                      MOVE  "TENSIKIBETU"      TO  MCP-WIDGET
                  WHEN   9
                      MOVE  "OLDTEN"           TO  MCP-WIDGET
                  WHEN   10
                      MOVE  "OLDTENSKB"        TO  MCP-WIDGET
                  WHEN   11
                      MOVE  "TANICD"           TO  MCP-WIDGET
                  WHEN   12
                      MOVE  "MADOKUKBN"        TO  MCP-WIDGET
                  WHEN   13
                      MOVE  "SINKEIHAKAIKBN"   TO  MCP-WIDGET
                  WHEN   14
                      MOVE  "SEIBUTUGAKUKBN"   TO  MCP-WIDGET
                  WHEN   15
                      MOVE  "ZOEIZAIKBN"       TO  MCP-WIDGET
                  WHEN   16
                      MOVE  "CSYYOURYO"        TO  MCP-WIDGET
                  WHEN   17
                      MOVE  "YKZKBN"           TO  MCP-WIDGET
                  WHEN   18
                      MOVE  "ZAIGATAKBN"       TO  MCP-WIDGET
                  WHEN   19
                      MOVE  "LONGTOYOKBN"      TO  MCP-WIDGET
                  WHEN   20
                      MOVE  "KOUHATUKBN"       TO  MCP-WIDGET
                  WHEN   21
                      MOVE  "KOUHYOJYUNNUM"    TO  MCP-WIDGET
                  WHEN   22
                      MOVE  "YAKKAKJNCD"       TO  MCP-WIDGET
                  WHEN   23
                      MOVE  "SYUSAISKB"        TO  MCP-WIDGET
                  WHEN   24
                      MOVE  "SYOMEIKANREN"     TO  MCP-WIDGET
                  WHEN   25
                      MOVE  "KEIKASOCHIYMD"    TO  MCP-WIDGET
                  WHEN   26
                      MOVE  "INPCHKKBN"        TO  MCP-WIDGET
                  WHEN   27
                      MOVE  "BUNRUIKBN"        TO  MCP-WIDGET
                  WHEN   28
                      MOVE  "ALLSURYOKBN"      TO  MCP-WIDGET
                  WHEN   29
                      MOVE  "KANZANTANICD"     TO  MCP-WIDGET
                  WHEN   30
                      MOVE  "KANZANCHI"        TO  MCP-WIDGET
                  WHEN   31
                      MOVE  "CHNGCD"           TO  MCP-WIDGET
                  WHEN   32
                      MOVE  "CHOKINISSU"       TO  MCP-WIDGET
                  WHEN   33
                      MOVE  "CHOKIKIGEN"       TO  MCP-WIDGET
                  WHEN   34
                      MOVE  "CHOKIERR"         TO  MCP-WIDGET
                  WHEN   35
                      MOVE  "TOUYOERR"         TO  MCP-WIDGET
      *
                  WHEN   36
                      MOVE  "NAIFUKUKBN"       TO  MCP-WIDGET
                  WHEN   37
                      MOVE  "SOURYOKBN"        TO  MCP-WIDGET
                  WHEN   38
                      MOVE  "IPNKISAIKBN"      TO  MCP-WIDGET
                  WHEN   39
                      MOVE  "IPNTANICD"        TO  MCP-WIDGET
                  WHEN   40
                      MOVE  "IPNKANZANCHI"     TO  MCP-WIDGET
      *H28.4
                  WHEN   42
                      MOVE  "KIKAKUTANI"       TO  MCP-WIDGET
                  WHEN   43
                      MOVE  "SIPPKBN"          TO  MCP-WIDGET
                  WHEN   44
                      MOVE  "SIPRYOU1"         TO  MCP-WIDGET
                  WHEN   45
                      MOVE  "SIPIRISU1"        TO  MCP-WIDGET
                  WHEN   46
                      MOVE  "SIPRYOU2"         TO  MCP-WIDGET
                  WHEN   47
                      MOVE  "SIPIRISU2"        TO  MCP-WIDGET
                  WHEN   48
                      MOVE  "CHNGCDU"          TO  MCP-WIDGET
      *
                   WHEN   110  THRU   119
                       COMPUTE ID1             =   SPA-GMN-CUR  -  110
                       IF      ID1             =   ZERO
                           MOVE    10              TO  ID1
                       END-IF
                       MOVE    "KGNAGE"            TO  MCP-WIDGET
                       MOVE    ID1                 TO  MCP-WIDGET(7:2)
                   WHEN   120  THRU   129
                       COMPUTE ID1             =   SPA-GMN-CUR  -  120
                       IF      ID1             =   ZERO
                           MOVE    10              TO  ID1
                       END-IF
                       MOVE    "JGNAGE"            TO  MCP-WIDGET
                       MOVE    ID1                 TO  MCP-WIDGET(7:2)
                   WHEN   130  THRU   139
                       COMPUTE ID1             =   SPA-GMN-CUR  -  130
                       IF      ID1             =   ZERO
                           MOVE    10              TO  ID1
                       END-IF
                       MOVE    "TOUYORYO1"         TO  MCP-WIDGET
                       MOVE    ID1                 TO  MCP-WIDGET(10:2)
                   WHEN   140  THRU   149
                       COMPUTE ID1             =   SPA-GMN-CUR  -  140
                       IF      ID1             =   ZERO
                           MOVE    10              TO  ID1
                       END-IF
                       MOVE    "TOUYORYO2"         TO  MCP-WIDGET
                       MOVE    ID1                 TO  MCP-WIDGET(10:2)
      *
                  WHEN 150
                       MOVE    "TUKIJGNKAISU"      TO  MCP-WIDGET
                  WHEN 151
                       MOVE    "TUKIJGNTOUYORYO"   TO  MCP-WIDGET
      *
                  WHEN   99
                      MOVE  "B12"              TO  MCP-WIDGET
             END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC         SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "YUKOSTYMD"
                   MOVE    2           TO  SPA-GMN-CUR
               WHEN    "YUKOEDYMD"
                   MOVE    3           TO  SPA-GMN-CUR
               WHEN    "KANANAME"
                   MOVE    4           TO  SPA-GMN-CUR
               WHEN    "NAME"
      *****        MOVE    5           TO  SPA-GMN-CUR
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "SHONAME"
                   MOVE    6           TO  SPA-GMN-CUR
               WHEN    "FORMALNAME"
                   MOVE    7           TO  SPA-GMN-CUR
               WHEN    "TEN"
                   MOVE    8           TO  SPA-GMN-CUR
               WHEN    "TENSIKIBETU"
                   MOVE    9           TO  SPA-GMN-CUR
               WHEN    "OLDTEN"
                   MOVE    10          TO  SPA-GMN-CUR
               WHEN    "OLDTENSKB"
                   MOVE    11          TO  SPA-GMN-CUR
               WHEN    "TANICD"
                   MOVE    12          TO  SPA-GMN-CUR
               WHEN    "MADOKUKBN"
                   MOVE    13          TO  SPA-GMN-CUR
               WHEN    "SINKEIHAKAIKBN"
                   MOVE    14          TO  SPA-GMN-CUR
               WHEN    "SEIBUTUGAKUKBN"
                   MOVE    15          TO  SPA-GMN-CUR
               WHEN    "ZOEIZAIKBN"
                   MOVE    16          TO  SPA-GMN-CUR
               WHEN    "CSYYOURYO"
      *H28         MOVE    17          TO  SPA-GMN-CUR
      *            規格単位へ
                   MOVE    42          TO  SPA-GMN-CUR
               WHEN    "KIKAKUTANI"
                   MOVE    17          TO  SPA-GMN-CUR
      *
               WHEN    "YKZKBN"
                   MOVE    18          TO  SPA-GMN-CUR
               WHEN    "ZAIGATAKBN"
                   MOVE    19          TO  SPA-GMN-CUR
               WHEN    "LONGTOYOKBN"
                   MOVE    20          TO  SPA-GMN-CUR
               WHEN    "KOUHATUKBN"
      *D           MOVE    21          TO  SPA-GMN-CUR
                   MOVE    24          TO  SPA-GMN-CUR
      *
               WHEN    "KOUHYOJYUNNUM"
                   MOVE    22          TO  SPA-GMN-CUR
               WHEN    "YAKKAKJNCD"
                   MOVE    23          TO  SPA-GMN-CUR
               WHEN    "SYUSAISKB"
      *D           MOVE    24          TO  SPA-GMN-CUR
      *H28         MOVE    36          TO  SPA-GMN-CUR
      *            経過措置へ
                   MOVE    25          TO  SPA-GMN-CUR
      *
               WHEN    "SYOMEIKANREN"
      *H28         MOVE    25          TO  SPA-GMN-CUR
      *            湿布薬関係
                   MOVE    43          TO  SPA-GMN-CUR
      *        湿布薬
               WHEN    "SIPPKBN"
                   MOVE    44          TO  SPA-GMN-CUR
               WHEN    "SIPRYOU1"
                   MOVE    45          TO  SPA-GMN-CUR
               WHEN    "SIPIRISU1"
                   MOVE    46          TO  SPA-GMN-CUR
               WHEN    "SIPRYOU2"
                   MOVE    47          TO  SPA-GMN-CUR
               WHEN    "SIPIRISU2"
                   MOVE    21          TO  SPA-GMN-CUR
      *経過措置年月日
               WHEN    "KEIKASOCHIYMD"
      ******       MOVE    26          TO  SPA-GMN-CUR
      *H28.4       MOVE    21          TO  SPA-GMN-CUR
                   MOVE    36          TO  SPA-GMN-CUR
      *
               WHEN    "INPCHKKBN"
      **********   MOVE    27          TO  SPA-GMN-CUR
                   MOVE    32          TO  SPA-GMN-CUR
               WHEN    "BUNRUIKBN"
                   MOVE    28          TO  SPA-GMN-CUR
               WHEN    "ALLSURYOKBN"
                   MOVE    29          TO  SPA-GMN-CUR
               WHEN    "KANZANTANICD"
                   MOVE    30          TO  SPA-GMN-CUR
               WHEN    "KANZANCHI"
      **********   MOVE    31          TO  SPA-GMN-CUR
      *H29.1       MOVE    26          TO  SPA-GMN-CUR
                   MOVE    32          TO  SPA-GMN-CUR
      *
               WHEN    "NAIFUKUKBN"
      **           MOVE    37          TO  SPA-GMN-CUR
      *H29.1 長期投与へ
                   MOVE    32          TO  SPA-GMN-CUR
               WHEN    "SOURYOKBN"
                   MOVE    38          TO  SPA-GMN-CUR
               WHEN    "IPNKISAIKBN"
                   MOVE    39          TO  SPA-GMN-CUR
               WHEN    "IPNTANICD"
                   MOVE    40          TO  SPA-GMN-CUR
               WHEN    "IPNKANZANCHI"
                   MOVE    27          TO  SPA-GMN-CUR
      *
               WHEN    "CHOKINISSU"
                   MOVE    33          TO  SPA-GMN-CUR
               WHEN    "CHOKIKIGEN"
                   MOVE    34          TO  SPA-GMN-CUR
               WHEN    "CHOKIERR"  
                   MOVE    35          TO  SPA-GMN-CUR
               WHEN    "TOUYOERR"
                   MOVE    111         TO  SPA-GMN-CUR
      *
               WHEN    "CHNGCD"
                   MOVE    48          TO  SPA-GMN-CUR
               WHEN    "CHNGCDU"
                   MOVE    99          TO  SPA-GMN-CUR
               WHEN    "TUKIJGNKAISU"
                   MOVE    151             TO  SPA-GMN-CUR
               WHEN    "TUKIJGNTOUYORYO"
                   MOVE    31              TO  SPA-GMN-CUR
      *H31.1
      *    ユーザー点数識別（2：ユーザーマスタ）
                   IF     (SPA-MASTER-CLASS    =   2 )
                       OR (SPA-MENTE-OK        =   1 )
                       CONTINUE
                   ELSE
                       MOVE  48                      TO  SPA-GMN-CUR
                   END-IF
               WHEN    OTHER
                   IF      WRK-MCP-WIDGET(1:6) =   "KGNAGE"
                       MOVE    WRK-MCP-WIDGET(7:2)  TO  ID1
                       IF      ID1             =   10
                           MOVE    120             TO   SPA-GMN-CUR
                       ELSE
                           COMPUTE SPA-GMN-CUR     =   120     +   ID1
                       END-IF
                   END-IF
                   IF      WRK-MCP-WIDGET(1:6) =   "JGNAGE"
                       MOVE    WRK-MCP-WIDGET(7:2)  TO  ID1
                       IF      ID1             =   10
                           MOVE    130             TO   SPA-GMN-CUR
                       ELSE
                           COMPUTE SPA-GMN-CUR     =   130     +   ID1
                       END-IF
                   END-IF
                   IF      WRK-MCP-WIDGET(1:9) =   "TOUYORYO1"
                       MOVE    WRK-MCP-WIDGET(10:2) TO  ID1
                       IF      ID1             =   10
                           MOVE    140             TO   SPA-GMN-CUR
                       ELSE
                           COMPUTE SPA-GMN-CUR     =   140     +   ID1
                       END-IF
                   END-IF
                   IF      WRK-MCP-WIDGET(1:9) =   "TOUYORYO2"
                       MOVE    WRK-MCP-WIDGET(10:2) TO  ID1
                       IF      ID1             =   10
                           MOVE    150             TO  SPA-GMN-CUR
                       ELSE
                           IF      ID1             =   9
                               MOVE    110             TO   SPA-GMN-CUR
                           ELSE
                               ADD     1               TO  ID1
                               COMPUTE SPA-GMN-CUR     =   110
                                                       +   ID1
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
           MOVE    SPACE                         TO  SPA-ERRMSG.
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "有効開始年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "有効終了年月日が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "カナ名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "漢字名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "正式名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "処方名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "金額種別がありません。"
                                                 TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "単位がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0009"
                   MOVE    "麻毒がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0010"
                   MOVE    "神経破壊剤がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "生物学的製剤がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "造影剤がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "薬剤がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "剤型がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "後発医薬品がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   MOVE    "経過措置年月日がエラーです"
                                                 TO  SPA-ERRMSG
               WHEN    "0017"
                   MOVE    "分類区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0018"
                   MOVE    "記載区分がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0019"
                   MOVE    "換算単位がありません"
                                                 TO  SPA-ERRMSG
      *ver.4.5
               WHEN    "0035"
                   STRING  "単位コードと数量換算単位が同じです。"
                           "数量換算単位を変更して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0036"
                   MOVE    "数量換算単位は 000 の選択はできません"
                                                 TO  SPA-ERRMSG
               WHEN    "0037"
                   MOVE    "数量換算単位を設定して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0038"
                   MOVE    "数量換算値を入力して下さい"
                                                 TO  SPA-ERRMSG
      *
               WHEN    "0020"
                   MOVE    "長期投与期限入力エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0025"
                   MOVE    "長期投与日数を入力して下さい。"
                                                 TO  SPA-ERRMSG
               WHEN    "0021"
                   STRING  "エラー処理は ０（エラー）または ９（警告）"
                           "を入力して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0022"
                   STRING  "下限・上限年齢は、数字・AA（28日）・"
                           "M1（1ヵ月）〜MB（11ヵ月）"
                           "で入力して下さい"      DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0023"
                   STRING  "投与量が設定してありません。"
                           "設定して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0024"
                   STRING  "下限年齢＜上限年齢で入力して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0031"
                   MOVE    "総量編集入力エラー"
                                                   TO    SPA-ERRMSG
               WHEN    "0026"
                   MOVE    "総量編集は内用薬剤のみ設定できます。"
                                                   TO    SPA-ERRMSG
               WHEN    "0027"
                   MOVE    "内服算定入力エラー"
                                                   TO    SPA-ERRMSG
               WHEN    "0028"
                   MOVE    "内用薬剤です。内服算定は設定できません。"
                                                   TO    SPA-ERRMSG
               WHEN    "0029"
                   MOVE    "一般名記載区分入力エラー"
                                                   TO    SPA-ERRMSG
               WHEN    "0030"
                   MOVE    "一般名記載単位コード入力エラー"
                                                   TO    SPA-ERRMSG
               WHEN    "0032"
                   STRING  "一般名がありません。"
                           "一般名記載はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0033"
                   STRING  "処方名称がありません。"
                           "処方名称記載はできません。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0034"
                   STRING  "一般名記載単位コードと換算値は一緒に"
                           "設定して下さい。"
                                                   DELIMITED  BY  SIZE
                                                   INTO  SPA-ERRMSG
                   END-STRING
      *
      *H29.1
               WHEN    "0050"
                   STRING   "ユーザー設定はできません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *        以下共通
               WHEN    "1010"
                   MOVE
                   "有効開始年月日≧有効終了年月日で入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "1011"
                   MOVE    "有効年月日の範囲が違います"
                                                 TO  SPA-ERRMSG
               WHEN    "1012"
                   MOVE    "有効年月日が重複しています"
                                                 TO  SPA-ERRMSG
               WHEN    "1013"
                   MOVE    "改正の終了年月日はＡＬＬ９のみ入力できます"
                                                 TO  SPA-ERRMSG
               WHEN    "1014"
                   MOVE    "漢字名称を入力してください"
                                                 TO  SPA-ERRMSG
              WHEN    "1001"
                   MOVE    "入力コード更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "9001"
                   MOVE    "削除処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "更新処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "追加処理が失敗しました"
                                                 TO  SPA-ERRMSG
               WHEN    "9004"
                   MOVE    "点数マスタ付加情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    "9005"
                   MOVE    "老人一般置換情報が更新できません"
                                                 TO  SPA-ERRMSG
               WHEN    "8000"
                   STRING  "経過措置品目移行コードが"
                                                 DELIMITED  BY  SIZE
                           "点数マスタにありません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "8001"
                   STRING  "経過措置品目移行コードの有効終了日≦"
                                                 DELIMITED  BY  SIZE
                           "有効終了日です。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "8002"
                   STRING  "経過措置品目移行コードは、"
                                                 DELIMITED  BY  SIZE
                           "有効終了日＝経過措年月日の時に登録できます"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "8003"
                   STRING  "既に別のコードに登録されています。"
                           "経過措置品目移行コードを変更して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K001"
                   STRING    "警告！厚生労働省から提供される"
                                                   DELIMITED BY  SIZE
                             "コードの範囲です。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                                                   DELIMITED BY  SIZE
                             "ではありません。"
                                                   DELIMITED BY  SIZE
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING   "警告！厚生労働省から提供されたデータです。"
                                                   DELIMITED BY  SIZE
                            "削除しないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H24.8
               WHEN    "K004"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K005"
                   STRING    "警告！"
                             "厚生労働省から提供される範囲のコードです"
                             "。改正処理はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
           END-EVALUATE.
      *
           MOVE    SPACE                         TO  ZERR
           MOVE    SPA-ERRCD                     TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG                TO  ZERR-ERRMSG
      *
           MOVE    "Z04"                         TO  SPA-MOTOPG
           MOVE    "ZERR"                        TO  SPA-SAKIPG
      *
           MOVE    "NEW"                         TO  MCP-PUTTYPE
           MOVE    "ZERR"                        TO  MCP-WINDOW
           MOVE    "B01"                         TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END
           .    
      *
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-ZIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-ZIDMSG
      *
           EVALUATE    SPA-ZIDCD
               WHEN    "0001"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0002"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0003"
                   MOVE    "削除します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
               WHEN    "0004"
                   MOVE    "更新します。よろしいですか？"
                                                       TO  WRK-ZIDMSG
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z04"                TO  SPA-MOTOPG
           MOVE    "ZID1"               TO  SPA-SAKIPG
      *
           MOVE    "NEW"                TO  MCP-PUTTYPE
           MOVE    "ZID1"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                    TO  FLG-END 
      *
           .
       520-ZIDSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    日付変換　処理
      *****************************************************************
      *****************************************************************
      *     対象日＋１日計算処理
      *****************************************************************
       800-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                      STS-AREA-DAY
           EVALUATE    WRK-YMD
               WHEN    ZERO
                   MOVE    ZERO            TO  WRK-OTYMD
               WHEN    "99999999"
                   MOVE    ALL "9"         TO  WRK-OTYMD
               WHEN    OTHER
                   INITIALIZE                  STS-AREA-DAY
                   INITIALIZE                  LNK-DAY6-AREA
                   MOVE    "61"                TO  LNK-DAY6-IRAI
                   MOVE    WRK-YMD             TO  LNK-DAY6-KIJUN
                   MOVE    "1"                 TO  LNK-DAY6-ZOGENPTN
                   MOVE    1                   TO  LNK-DAY6-ZOGEN
                   CALL  "ORCSDAY"             USING   STS-AREA-DAY
                                                       LNK-DAY6-AREA
                  MOVE    LNK-DAY6-KEISAN      TO  WRK-OTYMD
           END-EVALUATE
      *
           .
       800-DAYSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   和暦年月日→西暦年月日（機能１２）
      *****************************************************************
       831-YMDCONV12-SEC                         SECTION.
      *
           MOVE    ZERO                TO  FLG-ERR
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:7)
               WHEN  ZERO
                   MOVE  ZERO                    TO  WRK-SYMD
               WHEN  "9999999"
                   MOVE  ALL "9"                 TO  WRK-SYMD
               WHEN  OTHER
      *----(01.00.01) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    SGDAY-OTDAY         TO  WRK-LNKYMD
                       MOVE    SGDAY-SDAY          TO  WRK-SYMD
                   ELSE
                       MOVE    1                   TO  FLG-ERR
                   END-IF
      *----(01.00.01) LINE ADD END   --------------------------------
           END-EVALUATE
           .
      *
       831-YMDCONV12-EXT.
           EXIT.
      *
      *****************************************************************
      *   西暦→和暦（機能２１）
      *****************************************************************
       832-YMDCONV21-SEC                          SECTION.
      *
      *
           INITIALIZE                            STS-AREA-DAY
           EVALUATE  WRK-LNKYMD(1:8)
               WHEN  SPACE
               WHEN  ZERO 
               WHEN  "99999999"
                   MOVE    WRK-LNKYMD            TO  WRK-HENYMDG
               WHEN  OTHER
                  INITIALIZE                     STS-AREA-DAY
                  INITIALIZE                     LNK-DAY2-AREA
                  MOVE  "21"                     TO  LNK-DAY2-IRAI
                  MOVE  WRK-LNKYMD(1:8)          TO  LNK-DAY2-YMD
                  CALL  "ORCSDAY"                USING   STS-AREA-DAY
                                                 LNK-DAY2-AREA
                  MOVE    LNK-DAY2-EDTYMD1       TO  WRK-HENYMDG
           END-EVALUATE
           .
      *
       832-YMDCONV21-EXT.
           EXIT.
      *
      *****************************************************************
      *   文字数カウント処理（全角のみ）
      *****************************************************************
       891-STRCOUNT-SEC                         SECTION.
      *
           PERFORM  UNTIL  WRK-STRING(CNT-STRING)  NOT  =  SPACE
                    OR  CNT-STRING  =  1
               COMPUTE  CNT-STRING  =  CNT-STRING  -  1
           END-PERFORM
      *
           IF  CNT-STRING                        =  1
               MOVE  ZERO                        TO  CNT-STRING
           ELSE
               COMPUTE  CNT-STRING               =  CNT-STRING  /  2
           END-IF
           .
       891-STRCOUNT-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読み込み処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-SYSKANRI
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           .
       900-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ読込処理
      *****************************************************************
       900-TENSU-READ-SEC              SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               MOVE    ZERO            TO  FLG-TENSU
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1               TO  FLG-TENSU
           END-IF
           .
      *
       900-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報読込
      *****************************************************************
       990-TENSUPLUS-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS
           ELSE
               INITIALIZE                  TENSUPLUS-REC
               MOVE    1               TO  FLG-TENSUPLUS
           END-IF
           .
      *
       990-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ投与付加情報読込
      *****************************************************************
       990-TENSUPLUS2-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  TENSUPLUS2-REC
               MOVE    ZERO            TO  FLG-TENSUPLUS2
           ELSE
               INITIALIZE                  TENSUPLUS2-REC
               MOVE    1               TO  FLG-TENSUPLUS2
           END-IF
           .
      *
       990-TENSUPLUS2-READ-EXT.
           EXIT.
      *****************************************************************
      *    老人一般コード変換読込
      *****************************************************************
       990-SRYCDCHG-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYCDCHG-REC
               MOVE    ZERO                TO  FLG-SRYCDCHG
           ELSE
               INITIALIZE                      SRYCDCHG-REC
               MOVE    1                   TO  FLG-SRYCDCHG
           END-IF
      *
           .
       990-SRYCDCHG-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       910-DBSELECT-EXT.
           EXIT.
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *****************************************************************
      *   ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW                            SECTION.
      *
           MOVE   "PUTWINDOW"                    TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
