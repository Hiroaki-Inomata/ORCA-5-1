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
       PROGRAM-ID.                               ORCGZ03.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 点数マスタ設定
      *  コンポーネント名  : 点数マスタ診療行為設定画面（Ｚ０３）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−藤原　   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   01/07/03  日付入力チェックサブ追加
      *  01.00.02    MCC-多々納   01/07/04  老人一般コード振替追加
      *                                         （Ｚ９０）
      *  01.01.01    MCC-藤原     01/09/26  JOINの修正      
      *  01.01.02    MCC-藤原     01/10/29  採血料区分の修正      
      *  01.01.03    MCC-多々納   02/03/17  施設基準不適合の追加  
      *  01.01.04    MCC-多々納   02/07/18  服用方法画面追加
      *  01.01.05    NACL-多々納  02/10/09  診療種別の最大数を１００へ
      *                                     半角空白→全角空白追加
      *  01.01.06    NACL-多々納  02/12/02  食事追加
      *  01.01.07    NACL-多々納  02/12/20  点数識別８ 追加
      *  01.01.08    NACL-多々納  03/01/24  入院追加
      *  01.01.09    NACL-多々納  03/03/17  指導管理料明細追加
      *  02.00.00    NACL-多々納  03/06/20  一括入力へ変更
      *  02.00.01    NACL-多々納  03/12/19  入力ＣＤのみ変更可能へ
      *  02.00.02    NACL-多々納  05/12/09  登録コードチェック追加
      *                                     MONFUNC対応 他
      *  02.00.03    NACL-多々納  06/03/23  Ｈ１８／４改正対応
      *  03.04.00    NACL-多々納  06/12/05  ユーザ設定 上限回数追加
      *  03.05.00    NACL-多々納  07/05/07  グループ診療対応
      *  03.05.01    NACL-多々納  07/07/XX  ユーザ設定 他月情報追加
      *  04.02.00    NACL-多々納  08/03/XX  平成２０年４月改正対応
      *  04.04.00    NACL-多々納  09/01/05  前回検査日区分追加
      *  04.05.00    NACL-多々納  10/03/XX  平成２２年４月改正対応
      *  04.05.00    NACL-多々納  10/04/XX  帳票名称追加対応
      *  04.06.00    NACL-多々納  10/10/19  傷病関連区分追加対応、施設基準４桁対応他
      *  04.07.00    NACL-多々納  14/03/XX  平成２６年４月改正対応
      *                                    （DPC区分、副鼻腔等加算等）
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  05.01.00    NACL-多々納  17/02/XX  ユーザー設定他GINBEE対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ３０年４月改正対応
      *  05.01.00    NACL-多々納  19/10/30  老人一般コード振替中止対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT                               DIVISION.
       CONFIGURATION                             SECTION.
       INPUT-OUTPUT                              SECTION.
       FILE-CONTROL.
      *
      *
       DATA                                      DIVISION.
      *FILE                                      SECTION.
      *
       WORKING-STORAGE                           SECTION.
      *
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    点数マスタ設定用共通パラメタ
           COPY    "Z01COMMON-SPA".
      *
      *
      *    第一画面ＳＰＡエリア
           COPY    "Z01SCR-SPA".
      *
      *    ＳＰＡエリア
           COPY    "Z03SCR-SPA".
      *
      *    ユーザー設定ＳＰＡエリア
           COPY    "Z010SCR-SPA".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    フラグエリア
       01  FLG-AREA.
           03  FLG-END                           PIC 9(01).
           03  FLG-SRYCDCHG        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOUROKU         PIC 9(01).
      *R01.10
           03  FLG-USERIN          PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-ERR                           PIC 9(06).
           03  CNT-PAGE                          PIC 9(02).
           03  CNT-ERRCHK                        PIC 9(03).
           03  CNT-STRING                        PIC 9(03).
      *
           03  CNT-CHK                           PIC 9(04).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                               PIC 9(04).
           03  IDX-1                             PIC 9(04).
           03  IDX-2                             PIC 9(04).
           03  IDXX                              PIC 9(04).
      *
           03  IDX-T                             PIC 9(04).
      *
           03  IDX1                              PIC 9(04).
           03  IDX2                              PIC 9(04).
           03  IDX-H                             PIC 9(04).
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
           03  WRK-SRYKBN              PIC X(03).
      *
           03  WRK-AGEKSNKGN           PIC X(02).
           03  WRK-AGEKSNJGN           PIC X(02).
      *
           03  WRK-PAGE            PIC 9(01).
      *
           03  WRK-YMD             PIC X(08).
           03  WRK-OTYMD           PIC X(08).
      *
           03  WRK-MAE-INPUT       PIC X(400).
           03  WRK-OUT-INPUT       PIC X(400).
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-YUKOEDYMD       PIC X(08).
      *
           03  WRK-MCP-WIDGET      PIC X(64).
      *    更新テーブル名称
           03  WRK-MCP-TABLE       PIC X(64).
           03  WRK-MCP-PATHNAME       PIC X(64).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
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
      *  画面の項目順テーブル
       01  TBL-GMN-JUN.
      *    有効開始年月日
           03  FILLER          PIC X(32)  VALUE   "0 YUKOSTYMD".
      *    有効終了年月日
           03  FILLER          PIC X(32)  VALUE   "0 YUKOEDYMD".
      *    診療区分
           03  FILLER          PIC X(32)  VALUE   "0 SRYKBN".
      *    診療種別
           03  FILLER          PIC X(32)  VALUE   "0 SRYSYUKBN".
      *    データ区分
           03  FILLER          PIC X(32)  VALUE   "0 DATAKBN".
      *    カナ氏名チェック
           03  FILLER          PIC X(32)  VALUE   "0 KANANAME".
      *    氏名チェック
           03  FILLER          PIC X(32)  VALUE   "0 NAME".
      *    正式名称
           03  FILLER          PIC X(32)  VALUE   "0 FORMALNAME".
      *    処方名称
           03  FILLER          PIC X(32)  VALUE   "0 SHONAME".
      *    点数
           03  FILLER          PIC X(32)  VALUE   "0 TEN".
           03  FILLER          PIC X(32)  VALUE   "0 TENSIKIBETU".
           03  FILLER          PIC X(32)  VALUE   "0 OLDTEN".
           03  FILLER          PIC X(32)  VALUE   "0 OLDTENSKB".
           03  FILLER          PIC X(32)  VALUE   "0 TANICD".
     ******03  FILLER          PIC X(32)  VALUE   "0 TANINAME".
           03  FILLER          PIC X(32)  VALUE   "0 HKNTEKKBN".
           03  FILLER          PIC X(32)  VALUE   "0 BYOKANRENKBN".
           03  FILLER          PIC X(32)  VALUE   "0 SDOKNRYO".
           03  FILLER          PIC X(32)  VALUE   "0 YKZKNRKBN".
           03  FILLER          PIC X(32)  VALUE   "0 NYUGAITEKKBN".
           03  FILLER          PIC X(32)  VALUE   "0 ROUTEKKBN".
           03  FILLER          PIC X(32)  VALUE   "0 HOSPSRYKBN".
      *    医療観察法
           03  FILLER          PIC X(32)  VALUE   "0 IRYOKAN".
      *    DPC区分
           03  FILLER          PIC X(32)  VALUE   "0 DPCKBN".
           03  FILLER          PIC X(32)  VALUE   "0 OSINKBN".
           03  FILLER          PIC X(32)  VALUE   "0 BYOSYOKBN".
           03  FILLER          PIC X(32)  VALUE   "0 SANTEIRRKKBN".
           03  FILLER          PIC X(32)  VALUE   "0 TIMEKSNKBN".
           03  FILLER          PIC X(32)  VALUE   "0 GAIKANRIKBN".
           03  FILLER          PIC X(32)  VALUE   "0 TSUSOKUAGE".
           03  FILLER          PIC X(32)  VALUE   "0 KGNAGE".
           03  FILLER          PIC X(32)  VALUE   "0 JGNAGE".
      *    労災算定不可
           03  FILLER          PIC X(32)  VALUE   "0 ROSAIKBN".
      *    採血料区分
           03  FILLER          PIC X(32)  VALUE   "0 SAIKETUKBN".
      *    前回検査日区分
           03  FILLER          PIC X(32)  VALUE   "0 ZENKNSKBN".
      *    入力チェック区分
      **** 03  FILLER          PIC X(32)  VALUE   "0 INPCHKKBN".
      *    回数、年齢
           03  FILLER          PIC X(32)  VALUE   "1 JITUDAY".
           03  FILLER          PIC X(32)  VALUE   "1 DAYCNT".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNT".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNT1D".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNT1W".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNTETCM".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNTETC".
           03  FILLER          PIC X(32)  VALUE   "1 JGNCNTERR".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_SANTEIRRK".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNT".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNT1D".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNT1W".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNTETCM".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNTETC".
           03  FILLER          PIC X(32)  VALUE   "1 PLUS_JGNCNTERR".
           03  FILLER          PIC X(32)  VALUE   "1 KZMCOMPSIKIBETU".
           03  FILLER          PIC X(32)  VALUE   "1 KZMKGN".
           03  FILLER          PIC X(32)  VALUE   "1 KZMJGN".
           03  FILLER          PIC X(32)  VALUE   "1 KZM".
           03  FILLER          PIC X(32)  VALUE   "1 KZMTEN".
           03  FILLER          PIC X(32)  VALUE   "1 KZMERR".
           03  FILLER          PIC X(32)  VALUE   "1 CHUKSNCD".
           03  FILLER          PIC X(32)  VALUE   "1 CHUKSNTSUBAN".
      *        年齢加算
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNKGN1".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNJGN1".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNSRYCD1".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNKGN2".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNJGN2".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNSRYCD2".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNKGN3".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNJGN3".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNSRYCD3".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNKGN4".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNJGN4".
           03  FILLER          PIC X(32)  VALUE   "1 AGEKSNSRYCD4".
      *手術、検査、入院
      *    
           03  FILLER          PIC X(32)  VALUE   "2 TUSOKUGAIKBN".
           03  FILLER          PIC X(32)  VALUE   "2 LASERKSN".
           03  FILLER          PIC X(32)  VALUE   "2 CHPMESUKSN".
           03  FILLER          PIC X(32)  VALUE   "2 DONORKBN".
           03  FILLER          PIC X(32)  VALUE   "2 SEKIZUISOKUTEI".
           03  FILLER          PIC X(32)  VALUE   "2 KEIBUJYUTU".
           03  FILLER          PIC X(32)  VALUE   "2 AUTOHOUGO".
           03  FILLER          PIC X(32)  VALUE   "2 AUTOFUNGO".
           03  FILLER          PIC X(32)  VALUE   "2 CHPGYOKOSOTIKSN".
           03  FILLER          PIC X(32)  VALUE   "2 SHORTSTAYOPE".
      *    画像等手術支援加算
           03  FILLER          PIC X(32)  VALUE   "2 GAZOOPESUP".
      *    麻酔識別区分
           03  FILLER          PIC X(32)  VALUE   "2 MASUISKBKBN".
      *    施設基準不適合区分
           03  FILLER          PIC X(32)  VALUE   "2 KIJUNTEIGENKBN".
      *    基準不適合逓減対象施設基準コード
           03  FILLER          PIC X(32)  VALUE   "2 KIJUNTEIGENCD".
      *    副鼻腔内視鏡加算
           03  FILLER          PIC X(32)  VALUE   "2 FUKUBIKUNAIKBN".
      *    副鼻腔切除機器加算
           03  FILLER          PIC X(32)  VALUE   "2 FUKUBIKUKOTUKBN".
      *    長時間麻酔管理加算
           03  FILLER          PIC X(32)  VALUE   "2 TIMEMASUIKBN".
      *
           03  FILLER          PIC X(32)  VALUE   "2 KNSJISKBN".
           03  FILLER          PIC X(32)  VALUE   "2 KNSJISGRPKBN".
           03  FILLER          PIC X(32)  VALUE   "2 KENTAICOMMENT".
           03  FILLER          PIC X(32)  VALUE   "2 TEIGENKBN".
           03  FILLER          PIC X(32)  VALUE   "2 HOKATUTEIGENKBN".
           03  FILLER          PIC X(32)  VALUE   "2 HOUKNSKBN".
           03  FILLER          PIC X(32)  VALUE   "2 CHPNAISIKSN".
      *    非侵襲的血行動態モニタリング加算
           03  FILLER          PIC X(32)  VALUE   "2 HISINSYUMONKSN".
      *    凍結保存同種組織加算
           03  FILLER          PIC X(32)  VALUE   "2 TOUKETUHOZONKSN".
      *    四肢加算（労災）
           03  FILLER          PIC X(32)  VALUE   "2 SISIKSN".
      *H30.4
      *    悪性腫瘍病理組織標本加算
           03  FILLER          PIC X(32)  VALUE   "2 AKUSEIBYORIKSN".
      *R02.4
      *    創外固定器加算
           03  FILLER          PIC X(32)  VALUE   "2 SOGAIKOTEIKSN".
      *    超音波切削器加算
           03  FILLER          PIC X(32)  VALUE   "2 CHPSESSAKUKSN".
      *    左心耳閉鎖術併施区分
           03  FILLER          PIC X(32)  VALUE   "2 SASINJIHEISIKBN".
      *
           03  FILLER          PIC X(32)  VALUE   "2 NYUKHNKBN".
           03  FILLER          PIC X(32)  VALUE   "2 NYUKHNKSNKBN".
           03  FILLER          PIC X(32)  VALUE   "2 KANGOKSN".
           03  FILLER          PIC X(32)  VALUE   "2 CHIIKIKSN".
           03  FILLER          PIC X(32)  VALUE   "2 MICMESUKSN".
      *各番号等
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD1".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD2".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD3".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD4".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD5".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD6".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD7".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD8".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD9".
           03  FILLER          PIC X(32)  VALUE   "3 SSTKIJUNCD10".
           03  FILLER          PIC X(32)  VALUE   "3 GAITENTTLKBN".
           03  FILLER          PIC X(32)  VALUE   "3 NYUTENTTLSKB".
           03  FILLER          PIC X(32)  VALUE   "3 KOUHYOJYUNNUM".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_KBN".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_SYO".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_BU".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_KBNNUM".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_KBNNUM_EDA".
           03  FILLER          PIC X(32)  VALUE   "3 CDKBN_KOUBAN".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUCHI_KBN".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUCHI_SYO".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUCHI_BU".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUCHI_KBNNUM".
           03  FILLER          PIC X(32)  VALUE "3 KOKUCHI_KBNNUM_EDA".
      *
           03  FILLER          PIC X(32)  VALUE   "3 KOKUCHI_KOUBAN".
      *    点数表区分番号
           03  FILLER          PIC X(32)  VALUE   "3 KUBUNBANGOU".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUJISKBKBN1".
           03  FILLER          PIC X(32)  VALUE   "3 KOKUJISKBKBN2".
      *
       01  TBL-GMN-JUN-R       REDEFINES   TBL-GMN-JUN.
      *****03  TBL-GMN-OCC                 OCCURS  107
      **** 03  TBL-GMN-OCC                 OCCURS  109
      **** 03  TBL-GMN-OCC                 OCCURS  115
      **** 03  TBL-GMN-OCC                 OCCURS  118
      **** 03  TBL-GMN-OCC                 OCCURS  119
      ***  03  TBL-GMN-OCC                 OCCURS  121
      **** 03  TBL-GMN-OCC                 OCCURS  130
      **** 03  TBL-GMN-OCC                 OCCURS  129
      **** 03  TBL-GMN-OCC                 OCCURS  130
           03  TBL-GMN-OCC                 OCCURS  133
                                           INDEXED  IDX-GMN.
               05  TBL-PAGE                PIC 9(01).
               05  TBL-YOBI1               PIC X(01).
               05  TBL-WIDGET              PIC X(30).
      *
       01  TBL-GMN-MAX                     PIC 9(04)   VALUE   133.
      *
      *    単位の項目順テーブル
           COPY    "CMTANICD.INC".
      *
      *    指導管理コードテーブル
       01  TBL-SDOKNRYO-AREA.
           03  FILLER              PIC X(02)   VALUE   "00".
           03  FILLER              PIC X(50)   VALUE
               "指導管理加算以外".
           03  FILLER              PIC X(02)   VALUE   "01".
           03  FILLER              PIC X(50)   VALUE
               "特定疾患療養指導料".
           03  FILLER              PIC X(02)   VALUE   "02".
           03  FILLER              PIC X(50)   VALUE
               "ウイルス疾患指導料".
           03  FILLER              PIC X(02)   VALUE   "03".
           03  FILLER              PIC X(50)   VALUE
               "在宅自己注射指導管理料".
           03  FILLER              PIC X(02)   VALUE   "04".
           03  FILLER              PIC X(50)   VALUE
               "在宅自己腹膜灌流指導管理料".
           03  FILLER              PIC X(02)   VALUE   "05".
           03  FILLER              PIC X(50)   VALUE
               "特定薬剤治療管理料".
           03  FILLER              PIC X(02)   VALUE   "06".
           03  FILLER              PIC X(50)   VALUE
               "小児特定疾患カウンセリング料".
           03  FILLER              PIC X(02)   VALUE   "07".
           03  FILLER              PIC X(50)   VALUE
               "放射線治療管理料".
           03  FILLER              PIC X(02)   VALUE   "08".
           03  FILLER              PIC X(50)   VALUE
               "老人慢性疾患生活指導料".
           03  FILLER              PIC X(02)   VALUE   "09".
           03  FILLER              PIC X(50)   VALUE
               "退院時指導料".
           03  FILLER              PIC X(02)   VALUE   "10".
           03  FILLER              PIC X(50)   VALUE
               "寝たきり老人訪問看護・指導料".
           03  FILLER              PIC X(02)   VALUE   "11".
           03  FILLER              PIC X(50)   VALUE
               "投薬特別指導料".
           03  FILLER              PIC X(02)   VALUE   "12".
           03  FILLER              PIC X(50)   VALUE
               "在宅酸素療法指導管理料".
           03  FILLER              PIC X(02)   VALUE   "13".
           03  FILLER              PIC X(50)   VALUE
               "在宅中心静脈栄養法指導管理料".
           03  FILLER              PIC X(02)   VALUE   "14".
           03  FILLER              PIC X(50)   VALUE
               "皮膚科特定疾患指導管理料（１）".
           03  FILLER              PIC X(02)   VALUE   "15".
           03  FILLER              PIC X(50)   VALUE
               "歯科口腟衛生指導料".
           03  FILLER              PIC X(02)   VALUE   "16".
           03  FILLER              PIC X(50)   VALUE
               "有床義歯指導料（一般）".
           03  FILLER              PIC X(02)   VALUE   "17".
           03  FILLER              PIC X(50)   VALUE
               "歯槽膿漏指導管理料".
           03  FILLER              PIC X(02)   VALUE   "18".
           03  FILLER              PIC X(50)   VALUE
               "歯科口腟疾患指導料".
           03  FILLER              PIC X(02)   VALUE   "19".
           03  FILLER              PIC X(50)   VALUE
               "在宅自己腹膜灌流指導管理（集中的指導）".
           03  FILLER              PIC X(02)   VALUE   "20".
           03  FILLER              PIC X(50)   VALUE
               "在宅酸素療法指導管理（チアノーゼ型）".
           03  FILLER              PIC X(02)   VALUE   "21".
           03  FILLER              PIC X(50)   VALUE
               "寝たきり老人訪問指導管理料".
           03  FILLER              PIC X(02)   VALUE   "22".
           03  FILLER              PIC X(50)   VALUE
               "歯科矯正管理料".
           03  FILLER              PIC X(02)   VALUE   "23".
           03  FILLER              PIC X(50)   VALUE
               "有床義歯指導料（老人）".
           03  FILLER              PIC X(02)   VALUE   "24".
           03  FILLER              PIC X(50)   VALUE
               "在宅成分栄養管理栄養法指導管理料".
           03  FILLER              PIC X(02)   VALUE   "25".
           03  FILLER              PIC X(50)   VALUE
               "在宅自己導尿指導管理料".
           03  FILLER              PIC X(02)   VALUE   "26".
           03  FILLER              PIC X(50)   VALUE
               "痴呆患者在宅療養指導料".
           03  FILLER              PIC X(02)   VALUE   "27".
           03  FILLER              PIC X(50)   VALUE
               "寝たきり老人訪問リハビリテーション指導管理料".
           03  FILLER              PIC X(02)   VALUE   "28".
           03  FILLER              PIC X(50)   VALUE
               "悪性腫瘍特異物治療管理料".
           03  FILLER              PIC X(02)   VALUE   "29".
           03  FILLER              PIC X(50)   VALUE
               "心臓ペースメーカー指導管理料".
           03  FILLER              PIC X(02)   VALUE   "30".
           03  FILLER              PIC X(50)   VALUE
               "在宅悪性腫瘍患者指導管理料".
           03  FILLER              PIC X(02)   VALUE   "31".
           03  FILLER              PIC X(50)   VALUE
               "在宅人工呼吸指導管理料".
           03  FILLER              PIC X(02)   VALUE   "32".
           03  FILLER              PIC X(50)   VALUE
               "在宅寝たきり患者処置指導管理料".
           03  FILLER              PIC X(02)   VALUE   "33".
           03  FILLER              PIC X(50)   VALUE
               "寝たきり老人患者処置指導管理料".
           03  FILLER              PIC X(02)   VALUE   "34".
           03  FILLER              PIC X(50)   VALUE
               "小児科療養指導料".
           03  FILLER              PIC X(02)   VALUE   "35".
           03  FILLER              PIC X(50)   VALUE
               "皮膚科特定疾患指導管理料（２）".
           03  FILLER              PIC X(02)   VALUE   "36".
           03  FILLER              PIC X(50)   VALUE
               "てんかん指導料".
           03  FILLER              PIC X(02)   VALUE   "37".
           03  FILLER              PIC X(50)   VALUE
               "在宅自己疼痛管理指導管理料".
           03  FILLER              PIC X(02)   VALUE   "38".
           03  FILLER              PIC X(50)   VALUE
               "難病外来指導管理料".
           03  FILLER              PIC X(02)   VALUE   "39".
           03  FILLER              PIC X(50)   VALUE
               "在宅時医学総合管理料".
           03  FILLER              PIC X(02)   VALUE   "40".
           03  FILLER              PIC X(50)   VALUE
               "老人慢性疾患外来総合診療料".
           03  FILLER              PIC X(02)   VALUE   "41".
           03  FILLER              PIC X(50)   VALUE
               "皮膚科特定疾患指導管理料（３）".
           03  FILLER              PIC X(02)   VALUE   "42".
           03  FILLER              PIC X(50)   VALUE
               "在宅血液透析指導管理料".
           03  FILLER              PIC X(02)   VALUE   "43".
           03  FILLER              PIC X(50)   VALUE
               "在宅持続陽圧式呼吸療法指導管理料".
           03  FILLER              PIC X(02)   VALUE   "44".
           03  FILLER              PIC X(50)   VALUE
               "慢性疾患外来共同指導料".
      *
           03  FILLER              PIC X(02)   VALUE   "45".
           03  FILLER              PIC X(50)   VALUE
               "生活習慣病指導管理料（糖尿病を主病）".
           03  FILLER              PIC X(02)   VALUE   "46".
           03  FILLER              PIC X(50)   VALUE
               "慢性疼痛疾患管理料".
           03  FILLER              PIC X(02)   VALUE   "47".
           03  FILLER              PIC X(50)   VALUE
               "小児悪性腫瘍患者指導管理料".
           03  FILLER              PIC X(02)   VALUE   "48".
           03  FILLER              PIC X(50)   VALUE
               "在宅気管切開患者指導管理料".
           03  FILLER              PIC X(02)   VALUE   "49".
           03  FILLER              PIC X(50)   VALUE
               "在宅肺高血圧症患者指導管理料".
           03  FILLER              PIC X(02)   VALUE   "50".
           03  FILLER              PIC X(50)   VALUE
               "開放型病院共同指導料（１）".
           03  FILLER              PIC X(02)   VALUE   "51".
           03  FILLER              PIC X(50)   VALUE
               "開放型病院共同指導料（２）".
           03  FILLER              PIC X(02)   VALUE   "52".
           03  FILLER              PIC X(50)   VALUE
               "地域連帯退院時共同指導料１".
           03  FILLER              PIC X(02)   VALUE   "53".
           03  FILLER              PIC X(50)   VALUE
               "地域連帯退院時共同指導料２".
           03  FILLER              PIC X(02)   VALUE   "54".
           03  FILLER              PIC X(50)   VALUE
               "地域連携診療計画管理料".
           03  FILLER              PIC X(02)   VALUE   "55".
           03  FILLER              PIC X(50)   VALUE
               "地域連携診療計画退院時指導料（１）".
           03  FILLER              PIC X(02)   VALUE   "56".
           03  FILLER              PIC X(50)   VALUE
               "薬剤管理指導料".
           03  FILLER              PIC X(02)   VALUE   "57".
           03  FILLER              PIC X(50)   VALUE
               "後期高齢者退院時薬剤情報提供料".
           03  FILLER              PIC X(02)   VALUE   "58".
           03  FILLER              PIC X(50)   VALUE
               "特定施設入居時等医学総合管理料".
           03  FILLER              PIC X(02)   VALUE   "59".
           03  FILLER              PIC X(50)   VALUE
               "在宅患者連携指導料".
           03  FILLER              PIC X(02)   VALUE   "60".
           03  FILLER              PIC X(50)   VALUE
               "耳鼻咽喉科特定疾患指導管理料".
      *H22.4から
           03  FILLER              PIC X(02)   VALUE   "61".
           03  FILLER              PIC X(50)   VALUE
               "地域連携診療計画退院時指導料（２）".
           03  FILLER              PIC X(02)   VALUE   "62".
           03  FILLER              PIC X(50)   VALUE
               "がん治療連携計画策定料".
           03  FILLER              PIC X(02)   VALUE   "63".
           03  FILLER              PIC X(50)   VALUE
               "がん治療連携指導料".
           03  FILLER              PIC X(02)   VALUE   "64".
           03  FILLER              PIC X(50)   VALUE
               "認知症専門診断管理料".
           03  FILLER              PIC X(02)   VALUE   "65".
           03  FILLER              PIC X(50)   VALUE
               "肝炎インターフェロン治療計画料".
           03  FILLER              PIC X(02)   VALUE   "66".
           03  FILLER              PIC X(50)   VALUE
               "在宅難治性皮膚疾患処置指導管理料".
           03  FILLER              PIC X(02)   VALUE   "67".
           03  FILLER              PIC X(50)   VALUE
               "在宅小児低血糖症患者指導管理料".
           03  FILLER              PIC X(02)   VALUE   "80".
           03  FILLER              PIC X(50)   VALUE
               "小児科外来診療料等".
           03  FILLER              PIC X(02)   VALUE   "81".
           03  FILLER              PIC X(50)   VALUE
               "生活習慣病管理料".
           03  FILLER              PIC X(02)   VALUE   "82".
           03  FILLER              PIC X(50)   VALUE
               "在宅末期医療総合診察料".
           03  FILLER              PIC X(02)   VALUE   "97".
           03  FILLER              PIC X(50)   VALUE
               "処方せん料".
           03  FILLER              PIC X(02)   VALUE   "98".
           03  FILLER              PIC X(50)   VALUE
               "処方料".
      *
       01  TBL-SDOKNRYO-AREA-R     REDEFINES   TBL-SDOKNRYO-AREA.
      *    03  TBL-SDOKNRYO-OCC        OCCURS  45      INDEXED  SDO-IDX.
      **** 03  TBL-SDOKNRYO-OCC        OCCURS  58      INDEXED  SDO-IDX.
           03  TBL-SDOKNRYO-OCC        OCCURS  73      INDEXED  SDO-IDX.
               05  TBL-SDOKNRYO        PIC X(02).
               05  TBL-SDOKNRYOMEI     PIC X(50).
      *01  SDO-MAX                     PIC 9(04)   VALUE   45.
      *01  SDO-MAX                     PIC 9(04)   VALUE   58.
       01  SDO-MAX                     PIC 9(04)   VALUE   73.
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
      *    入力コード
        01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *    老人一般コード振替
        01  SRYCDCHG-REC.
            COPY    "CPSRYCDCHG.INC".
      *
      *    点数マスタ器材金額
        01 TENSU-PRICE-REC.
           COPY    "CPTENSU-PRICE.INC".
      *
      *    ワーク点数マスタ
       01  WRK-TNS-TENSU-REC           PIC X(1290).
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
      *----(01.02.03) LINE ADD START ----------------------------------
      *   画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *----(01.02.03) LINE ADD END   ----------------------------------
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
      *****COPY    "CPORCMCP.INC".
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
           MOVE    SPA-Z01SONOTA                 TO  SPA-Z03
           MOVE    SPA-Z010FREE                  TO  SPA-Z010
      *
           MOVE    SPACE                         TO  SPA-ERRCD.
           MOVE    ZERO                          TO  FLG-END.
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
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
      *    
           MOVE    SPA-Z01KYOUTU                 TO  SPA-WORK
           MOVE    SPA-AREA                      TO  SPA-COMMON
           MOVE    SPA-Z03                       TO  SPA-Z01SONOTA 
           MOVE    SPA-Z010                      TO  SPA-Z010FREE
           MOVE    SPA-Z01FREE                   TO  SPA-FREE
      *
           EXIT    PROGRAM.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                              SECTION.
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
               IF      FLG-END             =   ZERO
                    PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE    SPACE               TO  MCP-PUTTYPE
               MOVE    "Z03"               TO  MCP-WINDOW
               PERFORM 900-PUT-WINDOW
      *
              MOVE    1                    TO  FLG-END
           END-IF
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      * 
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
     *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC              SECTION.
      *
           EVALUATE    SPA-MOTOPG
      *    更新処理
               WHEN    "ZID1"
                   PERFORM 330-ZID1-SET-SEC
      *           入力ＣＤより
               WHEN    "Z99"
               WHEN    "Z90"
                   MOVE    1                   TO  SPA-GMN-CUR
      *H29.01
      *        ユーザ設定画面
               WHEN    "Z010"
                   PERFORM 310-SPASET-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
      *
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
      *
           INITIALIZE                      SPA-GMN-AREA
           INITIALIZE                      SPA-NAI-AREA
           MOVE  0                               TO  SPA-GMN-PAGENO
           MOVE  SPA-Z01-SRYCD                   TO  SPA-GMN-SRYCDH
      *
      *    点数マスタ該当データ読み出し
           MOVE  SPA-Z01-YUKOSTYMD               TO  SPA-NAI-YUKOSTYMD
           IF  SPA-Z01-SYORIKBN                  =  2
               MOVE  SPA-Z01-YUKOSTYMDH          TO  SPA-GMN-YUKOSTYMDH
           END-IF
           MOVE  SPA-Z01-YUKOSTYMDH              TO  SPA-GMN-YUKOSTYMD
      *
           MOVE  SPA-Z01-YUKOEDYMD               TO  SPA-NAI-YUKOEDYMD
           IF  SPA-Z01-SYORIKBN                  =  2
                MOVE  SPA-Z01-YUKOEDYMDH         TO  SPA-GMN-YUKOEDYMDH
           END-IF
           MOVE  SPA-Z01-YUKOEDYMDH              TO  SPA-GMN-YUKOEDYMD
      *
      *
      *    点数マスタ読込み
           MOVE    SPACE               TO  TNS-TENSU-REC
           INITIALIZE                      TNS-TENSU-REC
      *
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
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        点数マスタ編集
               PERFORM 3001-SPASET-SEC
           END-IF
      *    各リストボックス編集
           PERFORM 30012-COB-HEN-SEC
      *
      *H29.1
           PERFORM  3003-TENSUPLUS-COMB-SEC
           MOVE    ZERO                TO  FLG-OK
      *
      *    降順に検索　最終履歴を表示
           IF      FLG-TENSU           =   ZERO
      *
               MOVE    SPACE               TO  TENSUPLUS-REC
               MOVE    TNS-SRYCD           TO  TNSP-SRYCD
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
               ELSE
                   INITIALIZE                 TENSUPLUS-REC
                   MOVE    1                  TO  FLG-TENSUPLUS
               END-IF
               PERFORM UNTIL       FLG-TENSUPLUS  =   1
                   IF     (TNSP-YUKOSTYMD      >   TNS-YUKOEDYMD )
      *R01.10
                     OR   (TNSP-YUKOEDYMD      <   TNS-YUKOSTYMD )
                       CONTINUE
                   ELSE
                       PERFORM 3002-SPASET-TENSUPLUS-SEC
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
               PERFORM 990-DBLOSEC-SEC
           END-IF
      *
           IF      FLG-OK              =   ZERO
      *        初期表示
               INITIALIZE                 TENSUPLUS-REC
               PERFORM  3002-SPASET-TENSUPLUS-SEC
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
           .
       310-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット
      *****************************************************************
       3001-SPASET-SEC                     SECTION.
      *
           MOVE  MCPDATA-REC               TO  TNS-TENSU-REC
           MOVE  TNS-NAME                  TO  SPA-GMN-NAMEH
      *
           MOVE  TNS-SRYKBN                TO  SPA-GMN-SRYKBN   
           MOVE  TNS-SRYSYUKBN             TO  SPA-GMN-SRYSYUKBN
           MOVE  TNS-DATAKBN               TO  SPA-GMN-DATAKBN  
           MOVE  TNS-KANANAME              TO  SPA-GMN-KANANAME
           MOVE  TNS-NAME                  TO  SPA-GMN-NAME
           MOVE  TNS-TEN                   TO  SPA-GMN-TEN
           MOVE  TNS-TENSIKIBETU           TO  SPA-GMN-TENSIKIBETU
           MOVE  TNS-OLDTEN                TO  SPA-GMN-OLDTEN
           MOVE  TNS-OLDTENSKB             TO  SPA-GMN-OLDTENSKB
           MOVE  TNS-TANICD                TO  SPA-GMN-TANICD
           MOVE  TNS-TANINAME              TO  SPA-GMN-TANINAME
           MOVE  TNS-HKNTEKKBN             TO  SPA-GMN-HKNTEKKBN   
           MOVE  TNS-BYOKANRENKBN          TO  SPA-GMN-BYOKANRENKBN
           MOVE  TNS-SDOKNRYO              TO  SPA-GMN-SDOKNRYO    
           MOVE  TNS-YKZKNRKBN             TO  SPA-GMN-YKZKNRKBN   
           MOVE  TNS-NYUGAITEKKBN          TO  SPA-GMN-NYUGAITEKKBN
           MOVE  TNS-ROUTEKKBN             TO  SPA-GMN-ROUTEKKBN   
           MOVE  TNS-HOSPSRYKBN            TO  SPA-GMN-HOSPSRYKBN  
           MOVE  TNS-OSINKBN               TO  SPA-GMN-OSINKBN     
           MOVE  TNS-BYOSYOKBN             TO  SPA-GMN-BYOSYOKBN   
           MOVE  TNS-SANTEIRRKKBN          TO  SPA-GMN-SANTEIRRKKBN
           MOVE  TNS-TIMEKSNKBN            TO  SPA-GMN-TIMEKSNKBN  
           MOVE  TNS-GAIKANRIKBN           TO  SPA-GMN-GAIKANRIKBN 
           MOVE  TNS-TSUSOKUAGE            TO  SPA-GMN-TSUSOKUAGE  
           MOVE  TNS-KGNAGE                TO  SPA-GMN-KGNAGE      
           MOVE  TNS-JGNAGE                TO  SPA-GMN-JGNAGE      
      *                                         
           MOVE  TNS-JITUDAY               TO  SPA-GMN-JITUDAY        
           MOVE  TNS-DAYCNT                TO  SPA-GMN-DAYCNT         
           MOVE  TNS-JGNCNT                TO  SPA-GMN-JGNCNT         
           MOVE  TNS-JGNCNT1D              TO  SPA-GMN-JGNCNT1D       
           MOVE  TNS-JGNCNT1W              TO  SPA-GMN-JGNCNT1W       
           MOVE  TNS-JGNCNTETCM            TO  SPA-GMN-JGNCNTETCM     
           MOVE  TNS-JGNCNTETC             TO  SPA-GMN-JGNCNTETC      
           MOVE  TNS-JGNCNTERR             TO  SPA-GMN-JGNCNTERR      
           MOVE  TNS-KZMCOMPSIKIBETU       TO  SPA-GMN-KZMCOMPSIKIBETU
           MOVE  TNS-KZMKGN                TO  SPA-GMN-KZMKGN         
           MOVE  TNS-KZMJGN                TO  SPA-GMN-KZMJGN         
           MOVE  TNS-KZM                   TO  SPA-GMN-KZM            
           MOVE  TNS-KZMTEN                TO  SPA-GMN-KZMTEN         
           MOVE  TNS-KZMERR                TO  SPA-GMN-KZMERR         
           MOVE  TNS-CHUKSNCD              TO  SPA-GMN-CHUKSNCD       
           MOVE  TNS-CHUKSNTUBAN           TO  SPA-GMN-CHUKSNTSUBAN   
           PERFORM  VARYING  IDXX
                    FROM  1  BY  1  UNTIL  IDXX  >  4
               MOVE  TNS-AGEKSNKGN(IDXX)   TO  SPA-GMN-AGEKSNKGN(IDXX)
               MOVE  TNS-AGEKSNJGN(IDXX)   TO  SPA-GMN-AGEKSNJGN(IDXX) 
               MOVE  TNS-AGEKSNSRYCD(IDXX) TO  SPA-GMN-AGEKSNSRYCD
                                                                (IDXX) 
           END-PERFORM
      *                                          
           MOVE  TNS-TUSOKUGAIKBN          TO  SPA-GMN-TUSOKUGAIKBN    
           MOVE  TNS-LASERKBN              TO  SPA-GMN-LASERKSN        
           MOVE  TNS-CHPMESUKSN            TO  SPA-GMN-CHPMESUKSN      
           MOVE  TNS-DONORKBN              TO  SPA-GMN-DONORKBN      
           MOVE  TNS-MICMESUKSN            TO  SPA-GMN-MICMESUKSN      
           MOVE  TNS-SEKIZUISOKUTEI        TO  SPA-GMN-SEKIZUISOKUTEI  
           MOVE  TNS-KEIBUJYUTU            TO  SPA-GMN-KEIBUJYUTU      
           MOVE  TNS-AUTOHOUGO             TO  SPA-GMN-AUTOHOUGO       
           MOVE  TNS-AUTOFUNGO             TO  SPA-GMN-AUTOFUNGO       
           MOVE  TNS-CHPGYOKOSOTIKSN       TO  SPA-GMN-CHPGYOKOSOTIKSN 
           MOVE  TNS-SHORTSTAYOPE          TO  SPA-GMN-SHORTSTAYOPE    
      *    施設基準不適合区分
           MOVE  TNS-KIJUNTEIGENKBN        TO  SPA-GMN-KIJUNTEIGENKBN
      *    基準不適合逓減対象施設基準コード
           MOVE  TNS-KIJUNTEIGENCD         TO  SPA-GMN-KIJUNTEIGENCD
      *    副鼻腔内視鏡加算
           MOVE  TNS-FUKUBIKUNAIKBN        TO  SPA-GMN-FUKUBIKUNAIKBN
           MOVE  TNS-FUKUBIKUKOTUKBN       TO  SPA-GMN-FUKUBIKUKOTUKBN
      *    長時間麻酔管理加算
           MOVE  TNS-TIMEMASUIKBN          TO  SPA-GMN-TIMEMASUIKBN
      *
           MOVE  TNS-KNSJISKBN             TO  SPA-GMN-KNSJISKBN       
           MOVE  TNS-KNSJISGRPKBN          TO  SPA-GMN-KNSJISGRPKBN    
           MOVE  TNS-KENTAICOMMENT         TO  SPA-GMN-KENTAICOMMENT   
           MOVE  TNS-TEIGENKBN             TO  SPA-GMN-TEIGENKBN       
           MOVE  TNS-HOUKATUTEIGENKBN      TO  SPA-GMN-HOKATUTEIGENKBN 
           MOVE  TNS-HOUKNSKBN             TO  SPA-GMN-HOUKNSKBN       
           MOVE  TNS-CHPNAISIKSN           TO  SPA-GMN-CHPNAISIKSN     
           MOVE  TNS-NYUKHNKBN             TO  SPA-GMN-NYUKHNKBN
           MOVE  TNS-NYUKHNKSNKBN          TO  SPA-GMN-NYUKHNKSNKBN
           MOVE  TNS-KANGOKSN              TO  SPA-GMN-KANGOKSN 
           MOVE  TNS-CHIIKIKSN             TO  SPA-GMN-CHIIKIKSN
      *H28.4
      *    非侵襲的血行動態モニタリング加算
           MOVE  TNS-HISINSYUMONITORKSN    TO  SPA-GMN-HISINSYUMONKSN
           MOVE  TNS-TOUKETUHOZONKSN       TO  SPA-GMN-TOUKETUHOZONKSN
      *    点数表区分番号
           MOVE  TNS-KUBUNBANGOU           TO  SPA-GMN-KUBUNBANGOU
      *    労災算定不可
           MOVE  TNS-ROSAIKBN              TO  SPA-GMN-ROSAIKBN
      *    四肢加算（労災）
           MOVE  TNS-SISIKSN               TO  SPA-GMN-SISIKSN
      *H30.4
      *    悪性腫瘍病理組織標本加算
           MOVE  TNS-AKUSEIBYORIKSN        TO  SPA-GMN-AKUSEIBYORIKSN
      *R02.4
      *    創外固定器加算
           MOVE  TNS-SOGAIKOTEIKSN         TO  SPA-GMN-SOGAIKOTEIKSN
      *    超音波切削器加算
           MOVE  TNS-CHPSESSAKUKSN         TO  SPA-GMN-CHPSESSAKUKSN
      *    左心耳閉鎖術併施区分
           MOVE  TNS-SASINJIHEISIKBN       TO  SPA-GMN-SASINJIHEISIKBN
      *                                          
           PERFORM  VARYING  IDXX
                    FROM  1  BY  1  UNTIL  IDXX  >  10
               MOVE  TNS-SSTKIJUNCD(IDXX)  TO  SPA-GMN-SSTKIJUNCD(IDXX)
           END-PERFORM
           MOVE  TNS-GAITENTTLKBN          TO  SPA-GMN-GAITENTTLKBN    
           MOVE  TNS-NYUTENTTLKBN          TO  SPA-GMN-NYUTENTTLSKB   
           MOVE  TNS-KOUHYOJYUNNUM         TO  SPA-GMN-KOUHYOJYUNNUM   
           MOVE  TNS-CDKBN-KBN             TO  SPA-GMN-CDKBN-KBN       
           MOVE  TNS-CDKBN-SYO             TO  SPA-GMN-CDKBN-SYO       
           MOVE  TNS-CDKBN-BU              TO  SPA-GMN-CDKBN-BU        
           MOVE  TNS-CDKBN-KBNNUM          TO  SPA-GMN-CDKBN-KBNNUM    
           MOVE  TNS-CDKBN-KBNNUM-EDA      TO  SPA-GMN-CDKBN-KBNNUM-EDA
           MOVE  TNS-CDKBN-KOUBAN          TO  SPA-GMN-CDKBN-KOUBAN    
           MOVE  TNS-KOKUCHI-KBN           TO  SPA-GMN-KOKUCHI-KBN     
           MOVE  TNS-KOKUCHI-SYO           TO  SPA-GMN-KOKUCHI-SYO     
           MOVE  TNS-KOKUCHI-BU            TO  SPA-GMN-KOKUCHI-BU      
           MOVE  TNS-KOKUCHI-KBNNUM        TO  SPA-GMN-KOKUCHI-KBNNUM  
           MOVE  TNS-KOKUCHI-KBNNUM-EDA    TO
                                           SPA-GMN-KOKUCHI-KBNNUM-EDA
           MOVE  TNS-KOKUCHI-KOUBAN        TO  SPA-GMN-KOKUCHI-KOUBAN  
           MOVE  TNS-KOKUJISKBKBN1         TO  SPA-GMN-KOKUJISKBKBN1   
           MOVE  TNS-KOKUJISKBKBN2         TO  SPA-GMN-KOKUJISKBKBN2   
      *    正式名称
           MOVE  TNS-FORMALNAME            TO  SPA-GMN-FORMALNAME
      *    医療観察法
           MOVE  TNS-IRYOKANSATUKBN        TO  SPA-GMN-IRYOKAN
      *    ＤＰＣ区分
           MOVE  TNS-DPCKBN                TO  SPA-GMN-DPCKBN
           MOVE  TNS-GAZOOPESUP            TO  SPA-GMN-GAZOOPESUP
           MOVE  TNS-MASUISKBKBN           TO  SPA-GMN-MASUISKBKBN
      *    各リストボックス編集
      *****PERFORM 30012-COB-HEN-SEC
           .
      *
       3001-SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット（点数マスタ付加情報）
      *****************************************************************
       3003-TENSUPLUS-COMB-SEC       SECTION.
      *
      *    採血料区分一覧
           MOVE    "0:　　　　"        TO  SPA-GMN-SAIKETUCOMB (01)
           MOVE    "1:末梢採血"        TO  SPA-GMN-SAIKETUCOMB (02)
           MOVE    "2:静脈採血"        TO  SPA-GMN-SAIKETUCOMB (03)
           MOVE    "3:動脈採血"        TO  SPA-GMN-SAIKETUCOMB (04)
           MOVE    4                   TO  SPA-SAIKETUKBN-MAX
      *    前回検査日区分
           MOVE    "0 表示しない"      TO  SPA-GMN-ZENKNSKBN-COMB(1)
           MOVE    "1 表示する（初回コメント）"
                                       TO  SPA-GMN-ZENKNSKBN-COMB(2)
           MOVE    "2 表示する（初回実施日）"
                                       TO  SPA-GMN-ZENKNSKBN-COMB(3)
           MOVE    3                   TO  SPA-ZENKNSKBN-MAX
      *    上限回数
           MOVE    "0 無効"            TO  SPA-GMN-PLUS-SANTEICOMB(1)
           MOVE    "1 有効（算定履歴作成）"
                                       TO  SPA-GMN-PLUS-SANTEICOMB(2)
           MOVE    2                   TO  SPA-PLUS-SANTEI-MAX
           .
       3003-TENSUPLUS-COMB-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ情報をＳＰＡにセット（点数マスタ付加情報）
      *****************************************************************
       3002-SPASET-TENSUPLUS-SEC       SECTION.
      *
      *    入力チェック区分
      **** MOVE    TNSP-INPCHKKBN      TO  SPA-GMN-INPCHKKBN
      *    処方名
           MOVE    TNSP-SHONAME        TO  SPA-GMN-SHONAME
      *    採血区分
           MOVE    TNSP-SAIKETUKBN     TO  SPA-GMN-SAIKETUKBN
      *    採血料区分一覧
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SAIKETUKBN-MAX
               IF      SPA-GMN-SAIKETUKBN  =   SPA-GMN-SAIKETUCOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-GMN-SAIKETUCOMB (IDX)
                                           TO  SPA-GMN-SAIKETUKBN-G
                   MOVE    SPA-SAIKETUKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    前回検査日区分
           MOVE    TNSP-ZENKNSKBN      TO  SPA-GMN-ZENKNSKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ZENKNSKBN-MAX
               IF      SPA-GMN-ZENKNSKBN   =   SPA-GMN-ZENKNSKBN-COMB
                                                            (IDX)(1:1)
                   MOVE    SPA-GMN-ZENKNSKBN-COMB (IDX)
                                           TO  SPA-GMN-ZENKNSKBN-G
                   MOVE    SPA-ZENKNSKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    上限回数
           MOVE    TNSP-SANTEIRRKKBN   TO  SPA-GMN-PLUS-SANTEIRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PLUS-SANTEI-MAX
               IF      SPA-GMN-PLUS-SANTEIRRK
                                       =   SPA-GMN-PLUS-SANTEICOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-GMN-PLUS-SANTEICOMB (IDX)
                                           TO  SPA-GMN-PLUS-SANTEIRRK-G
                   MOVE    SPA-PLUS-SANTEI-MAX  TO  IDX
               END-IF
           END-PERFORM
      *    月上限回数
           MOVE    TNSP-JGNCNT         TO  SPA-GMN-PLUS-JGNCNT
      *    月上限回数
           MOVE    TNSP-JGNCNT         TO  SPA-GMN-PLUS-JGNCNT
      *    日上限回数
           MOVE    TNSP-JGNCNT1D       TO  SPA-GMN-PLUS-JGNCNT1D
      *    週上限回数
           MOVE    TNSP-JGNCNT1W       TO  SPA-GMN-PLUS-JGNCNT1W
      *    他月数
           MOVE    TNSP-JGNCNTETCM     TO  SPA-GMN-PLUS-JGNCNTETCM
      *    他回数
           MOVE    TNSP-JGNCNTETC      TO  SPA-GMN-PLUS-JGNCNTETC
      *    エラー処理
           MOVE    TNSP-JGNCNTERR      TO  SPA-GMN-PLUS-JGNCNTERR
           .
      *
       3002-SPASET-TENSUPLUS-EXT.
           EXIT.
      *
      *****************************************************************
      *   各リストボックス編集処理
      *****************************************************************
       30012-COB-HEN-SEC            SECTION.
      *
      *    診療区分・診療種別
           MOVE    ZERO                TO  IDX-1
           MOVE    ZERO                TO  IDX-2
      *    MOVE    1                   TO  IDX-1
      *    MOVE    1                   TO  IDX-2
      **** MOVE    SPACE               TO  SPA-GMN-SRYKBNCOB(IDX-1)
      *    MOVE    SPACE               TO  SPA-GMN-SRYSYUKBNCOB
      *                                                      (IDX-2)
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   200  )   OR
                              (MSYU-SRYKBN (IDX)   =   SPACE )
      *        加算は対象外
             IF      MSYU-SRYSYUKBN (IDX)      =   "114"
                                               OR  "124"
                                               OR  "133"
               CONTINUE
             ELSE
      *
      *        診療区分
               IF      (IDX-1          =   ZERO )  OR
                       (SPA-GMN-SRYKBN-L(IDX-1)(1:2)
                                   NOT =   MSYU-SRYKBN (IDX))
                   ADD     1               TO  IDX-1
                   MOVE    MSYU-SRYKBN (IDX)
                                           TO  SPA-GMN-SRYKBN-L(IDX-1)
                   EVALUATE    MSYU-SRYKBN (IDX)
                       WHEN    "11"
                       WHEN    "12"
                           MOVE    "診察"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "13"
                           MOVE    "指導"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "14"
                           MOVE    "在宅"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "21"
                       WHEN    "22"
                       WHEN    "23"
                       WHEN    "24"
                           MOVE    "薬剤"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "25"
                           MOVE    "処方"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "26"
                           MOVE    "麻毒"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "27"
                           MOVE    "調基"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
                           MOVE    "注射"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "40"
                           MOVE    "処置"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "50"
                           MOVE    "手術"  TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "54"
                           MOVE    "麻酔"  TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "60"
                           MOVE    "検査"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "64"
                           MOVE    "病理診断"  TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "70"
                           MOVE    "画像診断"  TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "80"
                           MOVE    "その他"    TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "93"
                           MOVE    "老人負担"  TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "95"
                           MOVE    "保険外"    TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "96"
                           MOVE    "保険外"    TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "97"
                           MOVE    "食事"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "90"
                           MOVE    "入院"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    "92"
                           MOVE    "入院"      TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                       WHEN    OTHER
                           MOVE    "その他"    TO  SPA-GMN-SRYKBNME-L
                                                               (IDX-1)
                   END-EVALUATE
               END-IF
      *        診療種別
               ADD     1                   TO  IDX-2
               MOVE    MSYU-SRYSYUKBN (IDX)    TO  SPA-GMN-SRYSYUKBNCOB
                                                           (IDX-2)(1:3)
               MOVE    MSYU-SRYMEI    (IDX)    TO  SPA-GMN-SRYSYUKBNCOB
                                                           (IDX-2)(5:)
      *
             END-IF
           END-PERFORM
      *
           MOVE    IDX-1               TO  SPA-SRYKBN-MAX
           MOVE    IDX-2               TO  SPA-SRYSYUKBN-MAX
      *
      **** ADD     1                   TO  IDX-1
      *    MOVE    "90"                 TO  SPA-GMN-SRYKBN-L(IDX-1)
      *    MOVE    "入院"              TO  SPA-GMN-SRYKBNME-L (IDX-1)
      *    ADD     1                   TO  IDX-2
      **** MOVE    "900 入院"          TO  SPA-GMN-SRYSYUKBNCOB(IDX-2)
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKBN-MAX
               IF      SPA-GMN-SRYKBN      =   SPA-GMN-SRYKBN-L(IDX)
                   MOVE    SPA-GMN-SRYKBNCOB (IDX) TO  SPA-GMN-SRYKBN-G
                   MOVE    SPA-SRYKBN-MAX          TO  IDX
               END-IF
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYSYUKBN-MAX
               IF      SPA-GMN-SRYSYUKBN   =   SPA-GMN-SRYSYUKBNCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-SRYSYUKBNCOB (IDX)
                                               TO  SPA-GMN-SRYSYUKBN-G
                   MOVE    SPA-SRYSYUKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    データ区分
           MOVE    "0 なし"            TO  SPA-GMN-DATAKBNCOB (1)
           MOVE    "1 手技"            TO  SPA-GMN-DATAKBNCOB (2)
           MOVE    "2 加算"            TO  SPA-GMN-DATAKBNCOB (3)
           MOVE    3                   TO  SPA-DATAKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN     =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                               TO  SPA-GMN-DATAKBN-G
                   MOVE    SPA-DATAKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    データ規格
           MOVE    ZERO            TO  IDX-1
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       MTANI-MAX
               ADD     1               TO  IDX-1
               MOVE    MTANI-TANICD  (IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(1:3)
               MOVE    MTANI-TANINAME(IDX)
                                       TO  SPA-GMN-TANICDCOB(IDX-1)(5:)
               MOVE   IDX-1            TO  SPA-TANICD-MAX
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               IF      SPA-GMN-TANICD     =   SPA-GMN-TANICDCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-TANICDCOB (IDX)
                                               TO  SPA-GMN-TANICD-G
                   MOVE    SPA-TANICD-MAX      TO  IDX
               END-IF
           END-PERFORM
      *
      *    点数識別
           MOVE    "0     "            TO  SPA-GMN-TENSIKIBETUCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-TENSIKIBETUCOB (2)
           MOVE    "3 点数"            TO  SPA-GMN-TENSIKIBETUCOB (3)
           MOVE    "4 都道府県購入価格"
                                       TO  SPA-GMN-TENSIKIBETUCOB (4)
           MOVE    "5 ％加算"          TO  SPA-GMN-TENSIKIBETUCOB (5)
           MOVE    "6 ％減算"          TO  SPA-GMN-TENSIKIBETUCOB (6)
           MOVE    "7 減点診療行為"    TO  SPA-GMN-TENSIKIBETUCOB (7)
           MOVE    "8 点数（マイナス）"
                                       TO  SPA-GMN-TENSIKIBETUCOB (8)
           MOVE    8                   TO  SPA-TENSIKI-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                               TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    旧点数識別
           MOVE    "0     "            TO  SPA-GMN-OLDTENSKBCOB (1)
           MOVE    "1 金額"            TO  SPA-GMN-OLDTENSKBCOB (2)
           MOVE    "3 点数"            TO  SPA-GMN-OLDTENSKBCOB (3)
           MOVE    "4 都道府県購入価格"
                                       TO  SPA-GMN-OLDTENSKBCOB (4)
           MOVE    "5 ％加算"          TO  SPA-GMN-OLDTENSKBCOB (5)
           MOVE    "6 ％減算"          TO  SPA-GMN-OLDTENSKBCOB (6)
           MOVE    "7 減点診療行為"    TO  SPA-GMN-OLDTENSKBCOB (7)
           MOVE    "8 点数（マイナス）"
                                       TO  SPA-GMN-OLDTENSKBCOB (8)
           MOVE    8                   TO  SPA-OLDTENSKB-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSKB-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                               TO  SPA-GMN-OLDTENSKB-G
                   MOVE    SPA-OLDTENSKB-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    保険適用
           MOVE    "0 保険適用なし"    TO  SPA-GMN-HKNTEKKBNCOB (1)
           MOVE    "1 保険適用あり"    TO  SPA-GMN-HKNTEKKBNCOB (2)
           MOVE    2                   TO  SPA-HKNTEKKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNTEKKBN-MAX
               IF      SPA-GMN-HKNTEKKBN   =   SPA-GMN-HKNTEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HKNTEKKBNCOB (IDX)
                                           TO  SPA-GMN-HKNTEKKBN-G
                   MOVE    SPA-HKNTEKKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    入外区分
           MOVE    "0 入院・入院外"    TO  SPA-GMN-NYUGAITEKKBNCOB (1)
           MOVE    "1 入院"            TO  SPA-GMN-NYUGAITEKKBNCOB (2)
           MOVE    "2 入院外"          TO  SPA-GMN-NYUGAITEKKBNCOB (3)
           MOVE    3                   TO  SPA-NYUGAITEKKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAITEKKBN-MAX
               IF      SPA-GMN-NYUGAITEKKBN
                                           =   SPA-GMN-NYUGAITEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAITEKKBNCOB (IDX)
                                           TO  SPA-GMN-NYUGAITEKKBN-G
                   MOVE    SPA-NYUGAITEKKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *    老人適応
           MOVE    "0 社会保険・老人保健"
                                       TO  SPA-GMN-ROUTEKKBNCOB (1)
           MOVE    "1 社会保険のみ"
                                       TO  SPA-GMN-ROUTEKKBNCOB (2)
           MOVE    "2 老人保健のみ"
                                       TO  SPA-GMN-ROUTEKKBNCOB (3)
           MOVE    3                   TO  SPA-ROUTEKKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ROUTEKKBN-MAX
               IF      SPA-GMN-ROUTEKKBN   =   SPA-GMN-ROUTEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-ROUTEKKBNCOB (IDX)
                                           TO  SPA-GMN-ROUTEKKBN-G
                   MOVE    SPA-ROUTEKKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    病院適応
           MOVE    "0 病院・診療所"
                                       TO  SPA-GMN-HOSPSRYKBNCOB (1)
           MOVE    "1 病院のみ"
                                       TO  SPA-GMN-HOSPSRYKBNCOB (2)
           MOVE    "2 診療所のみ"
                                       TO  SPA-GMN-HOSPSRYKBNCOB (3)
      *H30.4
           MOVE    "3 短期滞在手術等基本料２"
                                       TO  SPA-GMN-HOSPSRYKBNCOB (4)
           MOVE    "4 短期滞在手術等基本料３"
                                       TO  SPA-GMN-HOSPSRYKBNCOB (5)
           MOVE    5                   TO  SPA-HOSPSRYKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HOSPSRYKBN-MAX
               IF      SPA-GMN-HOSPSRYKBN  =   SPA-GMN-HOSPSRYKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HOSPSRYKBNCOB (IDX)
                                           TO  SPA-GMN-HOSPSRYKBN-G
                   MOVE    SPA-HOSPSRYKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    医療観察法
           MOVE    "0 適用外"
                                       TO  SPA-GMN-IRYOKANCOB (1)
           MOVE    "1 入院出来高"
                                       TO  SPA-GMN-IRYOKANCOB (2)
           MOVE    "2 通院出来高"
                                       TO  SPA-GMN-IRYOKANCOB (3)
           MOVE    "3 入院通院出来高"
                                       TO  SPA-GMN-IRYOKANCOB (4)
           MOVE    "4 医療観察法専用"
                                       TO  SPA-GMN-IRYOKANCOB (5)
           MOVE    5                   TO  SPA-IRYOKAN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-IRYOKAN-MAX
               IF      SPA-GMN-IRYOKAN     =   SPA-GMN-IRYOKANCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-IRYOKANCOB (IDX)
                                           TO  SPA-GMN-IRYOKAN-G
                   MOVE    SPA-IRYOKAN-MAX TO  IDX
               END-IF
           END-PERFORM
      *
      *    往診区分
           MOVE    "0 往診対象外"
                                       TO  SPA-GMN-OSINKBNCOB (1)
           MOVE    "1 往診料、訪問診療"
                                       TO  SPA-GMN-OSINKBNCOB (2)
           MOVE    "2 診療時間加算"
                                       TO  SPA-GMN-OSINKBNCOB (3)
           MOVE    "4 夜間加算・深夜加算"
                                       TO  SPA-GMN-OSINKBNCOB (4)
           MOVE    "5 特別往診加算"
                                       TO  SPA-GMN-OSINKBNCOB (5)
           MOVE    "6 特別往診料"
                                       TO  SPA-GMN-OSINKBNCOB (6)
           MOVE    "7 死亡診診加算"
                                       TO  SPA-GMN-OSINKBNCOB (7)
           MOVE    "8 緊急加算"
                                       TO  SPA-GMN-OSINKBNCOB (8)
           MOVE    8                   TO  SPA-OSINKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OSINKBN-MAX
               IF      SPA-GMN-OSINKBN  =   SPA-GMN-OSINKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OSINKBNCOB (IDX)
                                               TO  SPA-GMN-OSINKBN-G
                   MOVE    SPA-OSINKBN-MAX     TO  IDX
               END-IF
           END-PERFORM
      *
      *    算定履歴
           MOVE    "0 算定履歴なし"
                                       TO  SPA-GMN-SANTEIRRKKBNCOB (1)
           MOVE    "1 診療科、入外区分の指定なし"
                                       TO  SPA-GMN-SANTEIRRKKBNCOB (2)
           MOVE    "2 入外区分の指定"
                                       TO  SPA-GMN-SANTEIRRKKBNCOB (3)
           MOVE    "3 診療科の指定"
                                       TO  SPA-GMN-SANTEIRRKKBNCOB (4)
           MOVE    "4 診療科、入外区分の指定"
                                       TO  SPA-GMN-SANTEIRRKKBNCOB (5)
           MOVE    5                   TO  SPA-SANTEIRRKKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIRRKKBN-MAX
               IF      SPA-GMN-SANTEIRRKKBN  =   SPA-GMN-SANTEIRRKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIRRKKBNCOB (IDX)
                                           TO  SPA-GMN-SANTEIRRKKBN-G
                   MOVE    SPA-SANTEIRRKKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *    時間外
           MOVE    ZERO                  TO  SPA-TIMEKSNKBN-MAX
           IF      SPA-GMN-DATAKBN       =   1  OR  ZERO
               MOVE    "0 時間外算定不可"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (1)
               MOVE    "1 時間外算定可能"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (2)
               MOVE    "3 初診料の休日に係る"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (3)
               MOVE    3                 TO  SPA-TIMEKSNKBN-MAX
           END-IF
           IF    ( SPA-GMN-DATAKBN        =   2  )
      *H28.9
      *        手技料で時間区分が時間外区分である時
           OR    ((SPA-GMN-DATAKBN        =   1   )  AND
                  (TNS-KOKUJISKBKBN2      =   "7" )  AND
                  (TNS-TIMEKSNKBN         <>  "0" )  AND
                  (TNS-SRYKBN             =   "11"
                                          OR  "12"
                                          OR  "13"
                                          OR  "14" ))
               MOVE    "0 下記以外の診療行為"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (1)
               MOVE    "1 時間外加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (2)
               MOVE    "2 休日加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (3)
               MOVE    "3 初診料の休日加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (4)
               MOVE    "4 深夜加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (5)
               MOVE    "5 時間外特定加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (6)
               MOVE    "6 夜間・早朝加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (7)
               MOVE    "7 夜間加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (8)
               MOVE    "8 時間外等加算（100点以上）"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (9)
               MOVE    "9 休日加算（100点以上）"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (10)
               MOVE    10                TO  SPA-TIMEKSNKBN-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TIMEKSNKBN-MAX
               IF      SPA-GMN-TIMEKSNKBN  =   SPA-GMN-TIMEKSNKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TIMEKSNKBNCOB (IDX)
                                           TO  SPA-GMN-TIMEKSNKBN-G
                   MOVE    SPA-TIMEKSNKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
      *
      *    外来管理加算
           MOVE    "0 外来管理加算が算定できる"
                                       TO  SPA-GMN-GAIKANRIKBNCOB (1)
           MOVE    "1 外来管理加算が算定できない"
                                       TO  SPA-GMN-GAIKANRIKBNCOB (2)
           MOVE    "2 外来管理加算"
                                       TO  SPA-GMN-GAIKANRIKBNCOB (3)
           MOVE    3                   TO  SPA-GAIKANRIKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    SPA-GAIKANRIKBN-MAX
               IF      SPA-GMN-GAIKANRIKBN  =   SPA-GMN-GAIKANRIKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-GAIKANRIKBNCOB (IDX)
                                           TO  SPA-GMN-GAIKANRIKBN-G
                   MOVE    SPA-GAIKANRIKBN-MAX TO  IDX
               END-IF
           END-PERFORM
      *
      *    傷病名関連
           MOVE    "00 "
                                       TO  SPA-GMN-BYOKANRENKBNCOB (1)
           MOVE    "03 皮膚科特定疾病指導管理料（１）"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (2)
           MOVE    "04 皮膚科特定疾病指導管理料（２）"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (3)
           MOVE    "05 特定疾患療養指導料及び老人慢性疾患生活指導"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (4)
           MOVE    "06 皮膚科特定疾病指導管理料（３）"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (5)
           MOVE    "07 てんかん指導料"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (6)
           MOVE    "09 難病外来指導管理料"
                                       TO  SPA-GMN-BYOKANRENKBNCOB (7)
           MOVE    7                   TO  SPA-BYOKANRENKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-BYOKANRENKBN-MAX
               IF      SPA-GMN-BYOKANRENKBN  =   SPA-GMN-BYOKANRENKBNCOB
                                                         (IDX)(1:2)
                   MOVE    SPA-GMN-BYOKANRENKBNCOB (IDX)
                                           TO  SPA-GMN-BYOKANRENKBN-G
                   MOVE    SPA-BYOKANRENKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *    医薬品関連
           MOVE    "0"
                                           TO  SPA-GMN-YKZKNRKBNCOB (1)
           MOVE    "1 麻毒、覚せい剤、向精神薬加算"
                                           TO  SPA-GMN-YKZKNRKBNCOB (2)
           MOVE    "3 神経ブロック"
                                           TO  SPA-GMN-YKZKNRKBNCOB (3)
           MOVE    "4 生物学的製剤"
                                           TO  SPA-GMN-YKZKNRKBNCOB (4)
           MOVE    4                   TO  SPA-YKZKNRKBN-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YKZKNRKBN-MAX
               IF      SPA-GMN-YKZKNRKBN   =   SPA-GMN-YKZKNRKBNCOB
                                                         (IDX)(1:2)
                   MOVE    SPA-GMN-YKZKNRKBNCOB (IDX)
                                               TO  SPA-GMN-YKZKNRKBN-G
                   MOVE    SPA-YKZKNRKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
      *    指導管理コード
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   IDX     >       SDO-MAX
               MOVE    TBL-SDOKNRYO  (IDX)
                                       TO  SPA-GMN-SDOKNRYOCOB(IDX)
                                                             (1:2)
               MOVE    TBL-SDOKNRYOMEI(IDX)
                                       TO  SPA-GMN-SDOKNRYOCOB(IDX)
                                                            (4:)
               MOVE    IDX             TO  SPA-SDOKNRYO-MAX
           END-PERFORM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SDOKNRYO-MAX
               IF      SPA-GMN-SDOKNRYO    =   SPA-GMN-SDOKNRYOCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-SDOKNRYOCOB (IDX)
                                               TO  SPA-GMN-SDOKNRYO-G
                   MOVE    SPA-SDOKNRYO-MAX    TO  IDX
               END-IF
           END-PERFORM
      *
      *
           .
      *
       30012-COB-HEN-EXT.
           EXIT.
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
           MOVE    SPACE           TO  SPA-ZID1-FLG
           MOVE    SPACE           TO  SPA-ZIDCD
           .
       330-ZID1-SET-EXT.
           EXIT.
      *
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
      *        老人一般コード振替設定
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM  407-SRYCDCHG-SEC
      *        確定
               WHEN    "CLICKED"       ALSO    "B12"
                   PERFORM  412-TOUROKU-SEC
      *ver.5.0
      *H29.01
      *        ユーザー項目登録
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM  407-B10-SYORI-SEC
      *ver.4.7 基本情報
      *        WHEN    "CLICKED"       ALSO    "BJ1"
      *            PERFORM 420-KIRIKAE-SEC
      *            IF      SPA-ERRCD       =   SPACE
      *                MOVE    0               TO  SPA-GMN-PAGENO
      *                MOVE    ZERO            TO  SPA-GMN-CUR
      *            END-IF
      *        回数、年齢
      *        WHEN    "CLICKED"       ALSO    "BJ2"
      *            PERFORM 420-KIRIKAE-SEC
      *            IF      SPA-ERRCD       =   SPACE
      *                MOVE    1               TO  SPA-GMN-PAGENO
      *                MOVE    ZERO            TO  SPA-GMN-CUR
      *            END-IF
      *        手術、検査、入院
      *        WHEN    "CLICKED"       ALSO    "BJ3"
      *            PERFORM 420-KIRIKAE-SEC
      *            IF      SPA-ERRCD       =   SPACE
      *                MOVE    2               TO  SPA-GMN-PAGENO
      *                MOVE    ZERO            TO  SPA-GMN-CUR
      *            END-IF
      *        各種番号等
      *        WHEN    "CLICKED"       ALSO    "BJ4"
      *            PERFORM 420-KIRIKAE-SEC
      *            IF      SPA-ERRCD       =   SPACE
      *                MOVE    3               TO  SPA-GMN-PAGENO
      *                MOVE    ZERO            TO  SPA-GMN-CUR
      *******      END-IF
      *        タブ切替
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-BETUGAMEN-SEC
           END-EVALUATE.
      *
       200-GMNSENI-EXT.
           EXIT.
      *****************************************************************
      *    会話　処理
      *****************************************************************
       200-ENTRY-SEC                             SECTION.
      *
      *    入力個所のセット
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *ver.4.7 
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        基本情報
               WHEN    "SWITCH"       ALSO    "not1"
                   PERFORM 420-KIRIKAE-SEC
      *
               WHEN    OTHER
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
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
           IF      SPA-ERRCD       NOT =   SPACE
      *        登録時、内容変更の無い場合、エラーとしない
               IF      FLG-TOUROKU         =   1
                   PERFORM 4103-HENKOU-CHK-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      410-INPUT-CHK-EXT
           END-IF
      *
      *    登録時、必須入力チェック
           IF      FLG-TOUROKU         =   1
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
      *    画面ページ
           MOVE    Z03-PAGENO          TO  SPA-GMN-PAGENO
      *R01.10
      *    前回検査日の変更時のみ、「ユーザー」へ
           IF     (Z03-ZENKNSKBN   NOT =   SPA-GMN-ZENKNSKBN-G )
      *       ユーザーへ
               MOVE    1                TO  FLG-USERIN
           END-IF
      *R01.10
      *
      *H29.1
      *    ユーザー点数識別（2：ユーザーマスタ）のみ反映
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               CONTINUE
           ELSE
               GO      TO      4101-GMN-SPA-SET-EXT
           END-IF
      *
           MOVE    Z03-YUKOSTYMD       TO  SPA-GMN-YUKOSTYMD
           MOVE    Z03-YUKOEDYMD       TO  SPA-GMN-YUKOEDYMD
      *    診療区分
           MOVE    Z03-SRYKBN          TO  SPA-GMN-SRYKBN-G
      *    診療種別
           MOVE    Z03-SRYSYUKBN       TO  SPA-GMN-SRYSYUKBN-G
           MOVE    Z03-DATAKBN         TO  SPA-GMN-DATAKBN-G
           MOVE    Z03-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    Z03-NAME            TO  SPA-GMN-NAME
           MOVE    Z03-FORMALNAME      TO  SPA-GMN-FORMALNAME
      *H29.1  付加情報不可
      **** MOVE    Z03-SHONAME         TO  SPA-GMN-SHONAME
           MOVE    Z03-TEN             TO  SPA-GMN-TEN
           MOVE    Z03-TENSIKIBETU     TO  SPA-GMN-TENSIKIBETU-G
           MOVE    Z03-OLDTEN          TO  SPA-GMN-OLDTEN
           MOVE    Z03-OLDTENSKB       TO  SPA-GMN-OLDTENSKB-G
           MOVE    Z03-TANICD          TO  SPA-GMN-TANICD-G
           MOVE    Z03-HKNTEKKBN       TO  SPA-GMN-HKNTEKKBN-G
           MOVE    Z03-BYOKANRENKBN    TO  SPA-GMN-BYOKANRENKBN-G
           MOVE    Z03-SDOKNRYO        TO  SPA-GMN-SDOKNRYO-G
           MOVE    Z03-YKZKNRKBN       TO  SPA-GMN-YKZKNRKBN-G
           MOVE    Z03-NYUGAITEKKBN    TO  SPA-GMN-NYUGAITEKKBN-G
           MOVE    Z03-ROUTEKKBN       TO  SPA-GMN-ROUTEKKBN-G
           MOVE    Z03-HOSPSRYKBN      TO  SPA-GMN-HOSPSRYKBN-G
      *    医療観察法
           MOVE    Z03-IRYOKAN         TO  SPA-GMN-IRYOKAN-G
           MOVE    Z03-OSINKBN         TO  SPA-GMN-OSINKBN-G
           MOVE    Z03-BYOSYOKBN       TO  SPA-GMN-BYOSYOKBN
           MOVE    Z03-SANTEIRRKKBN    TO  SPA-GMN-SANTEIRRKKBN-G
           MOVE    Z03-TIMEKSNKBN      TO  SPA-GMN-TIMEKSNKBN-G
           MOVE    Z03-GAIKANRIKBN     TO  SPA-GMN-GAIKANRIKBN-G
           MOVE    Z03-TSUSOKUAGE      TO  SPA-GMN-TSUSOKUAGE
           MOVE    Z03-KGNAGE          TO  SPA-GMN-KGNAGE
           MOVE    Z03-JGNAGE          TO  SPA-GMN-JGNAGE
      *H29.1  付加情報不可
      *    MOVE    Z03-SAIKETUKBN      TO  SPA-GMN-SAIKETUKBN-G
      *    MOVE    Z03-ZENKNSKBN       TO  SPA-GMN-ZENKNSKBN-G
      *    MOVE    Z03-INPCHKKBN       TO  SPA-GMN-INPCHKKBN
      *
           MOVE    Z03-JITUDAY         TO  SPA-GMN-JITUDAY
           MOVE    Z03-DAYCNT          TO  SPA-GMN-DAYCNT
           MOVE    Z03-JGNCNT          TO  SPA-GMN-JGNCNT
           MOVE    Z03-JGNCNT1D        TO  SPA-GMN-JGNCNT1D
           MOVE    Z03-JGNCNT1W        TO  SPA-GMN-JGNCNT1W
           MOVE    Z03-JGNCNTETCM      TO  SPA-GMN-JGNCNTETCM
           MOVE    Z03-JGNCNTETC       TO  SPA-GMN-JGNCNTETC 
           MOVE    Z03-JGNCNTERR       TO  SPA-GMN-JGNCNTERR
      *
      *H29.1  付加情報不可
      *    MOVE    Z03-PLUS-SANTEIRRK  TO  SPA-GMN-PLUS-SANTEIRRK-G
      *    MOVE    Z03-PLUS-JGNCNT     TO  SPA-GMN-PLUS-JGNCNT
      *    MOVE    Z03-PLUS-JGNCNT1D   TO  SPA-GMN-PLUS-JGNCNT1D
      *    MOVE    Z03-PLUS-JGNCNT1W   TO  SPA-GMN-PLUS-JGNCNT1W
      *    MOVE    Z03-PLUS-JGNCNTETCM TO  SPA-GMN-PLUS-JGNCNTETCM
      *    MOVE    Z03-PLUS-JGNCNTETC  TO  SPA-GMN-PLUS-JGNCNTETC
      *    MOVE    Z03-PLUS-JGNCNTERR  TO  SPA-GMN-PLUS-JGNCNTERR
      *
           MOVE    Z03-KZMCOMPSIKIBETU TO  SPA-GMN-KZMCOMPSIKIBETU
           MOVE    Z03-KZMKGN          TO  SPA-GMN-KZMKGN
           MOVE    Z03-KZMJGN          TO  SPA-GMN-KZMJGN
           MOVE    Z03-KZM             TO  SPA-GMN-KZM    
           MOVE    Z03-KZMTEN          TO  SPA-GMN-KZMTEN
           MOVE    Z03-KZMERR          TO  SPA-GMN-KZMERR
           MOVE    Z03-CHUKSNCD        TO  SPA-GMN-CHUKSNCD
           MOVE    Z03-CHUKSNTSUBAN    TO  SPA-GMN-CHUKSNTSUBAN
           MOVE    Z03-AGEKSNKGN1      TO  SPA-GMN-AGEKSNKGN(1)
           MOVE    Z03-AGEKSNJGN1      TO  SPA-GMN-AGEKSNJGN(1)
           MOVE    Z03-AGEKSNSRYCD1    TO  SPA-GMN-AGEKSNSRYCD(1)
           MOVE    Z03-AGEKSNKGN2      TO  SPA-GMN-AGEKSNKGN(2)
           MOVE    Z03-AGEKSNJGN2      TO  SPA-GMN-AGEKSNJGN(2)
           MOVE    Z03-AGEKSNSRYCD2    TO  SPA-GMN-AGEKSNSRYCD(2)
           MOVE    Z03-AGEKSNKGN3      TO  SPA-GMN-AGEKSNKGN(3)
           MOVE    Z03-AGEKSNJGN3      TO  SPA-GMN-AGEKSNJGN(3)
           MOVE    Z03-AGEKSNSRYCD3    TO  SPA-GMN-AGEKSNSRYCD(3)
           MOVE    Z03-AGEKSNKGN4      TO  SPA-GMN-AGEKSNKGN(4)
           MOVE    Z03-AGEKSNJGN4      TO  SPA-GMN-AGEKSNJGN(4)
           MOVE    Z03-AGEKSNSRYCD4    TO  SPA-GMN-AGEKSNSRYCD(4)
      *
      *        通則加算対象外
           MOVE    Z03-TUSOKUGAIKBN    TO  SPA-GMN-TUSOKUGAIKBN
      *        処置乳幼児加算（レーザー照射）
           MOVE    Z03-LASERKSN        TO  SPA-GMN-LASERKSN
      *        極低出生体重児加算（超音波メス）
           MOVE    Z03-CHPMESUKSN      TO  SPA-GMN-CHPMESUKSN
      *        ドナー分集計区分
           MOVE    Z03-DONORKBN        TO  SPA-GMN-DONORKBN 
      *        入院基本料減算（マイクロ波メス）
           MOVE    Z03-MICMESUKSN      TO  SPA-GMN-MICMESUKSN 
      *        脊髄測定
           MOVE    Z03-SEKIZUISOKUTEI  TO  SPA-GMN-SEKIZUISOKUTEI
      *        頸部術
           MOVE    Z03-KEIBUJYUTU      TO  SPA-GMN-KEIBUJYUTU
      *        自動縫合器
           MOVE    Z03-AUTOHOUGO       TO  SPA-GMN-AUTOHOUGO
      *        自動吻合器
           MOVE    Z03-AUTOFUNGO       TO  SPA-GMN-AUTOFUNGO
      *        超音波石灰装置
           MOVE    Z03-CHPGYOKOSOTIKSN TO  SPA-GMN-CHPGYOKOSOTIKSN
      *        手術帰宅加算
           MOVE    Z03-SHORTSTAYOPE    TO  SPA-GMN-SHORTSTAYOPE
      *        施設基準不適合区分
           MOVE    Z03-KIJUNTEIGENKBN  TO  SPA-GMN-KIJUNTEIGENKBN
      *        基準不適合逓減対象施設基準コード
           MOVE    Z03-KIJUNTEIGENCD   TO  SPA-GMN-KIJUNTEIGENCD
      *H26.4
      *    ＤＰＣ区分
           MOVE    Z03-DPCKBN          TO  SPA-GMN-DPCKBN
      *    副鼻腔手術用内視鏡加算
           MOVE    Z03-FUKUBIKUNAIKBN  TO  SPA-GMN-FUKUBIKUNAIKBN
      *    副鼻腔手術用軟骨部組織切除機器加算
           MOVE    Z03-FUKUBIKUKOTUKBN TO  SPA-GMN-FUKUBIKUKOTUKBN
      *    長時間麻酔管理加算
           MOVE    Z03-TIMEMASUIKBN    TO  SPA-GMN-TIMEMASUIKBN
      *
      *        検査等実施判断
           MOVE    Z03-KNSJISKBN       TO  SPA-GMN-KNSJISKBN
      *        検査等実施判断グループ
           MOVE    Z03-KNSJISGRPKBN    TO  SPA-GMN-KNSJISGRPKBN
      *        検体検査コメント
           MOVE    Z03-KENTAICOMMENT   TO  SPA-GMN-KENTAICOMMENT
      *        逓減対象
           MOVE    Z03-TEIGENKBN       TO  SPA-GMN-TEIGENKBN
      *        包括逓減
           MOVE    Z03-HOKATUTEIGENKBN TO  SPA-GMN-HOKATUTEIGENKBN
      *        包括対象検査
           MOVE    Z03-HOUKNSKBN       TO  SPA-GMN-HOUKNSKBN
      *        超音波内視鏡
           MOVE    Z03-CHPNAISIKSN     TO  SPA-GMN-CHPNAISIKSN
      *        入院基本料区分
           MOVE    Z03-NYUKHNKBN       TO  SPA-GMN-NYUKHNKBN
      *        入院基本料加算区分
           MOVE    Z03-NYUKHNKSNKBN    TO  SPA-GMN-NYUKHNKSNKBN
      *        看護加算
           MOVE    Z03-KANGOKSN        TO  SPA-GMN-KANGOKSN
      *        地域加算
           MOVE    Z03-CHIIKIKSN       TO  SPA-GMN-CHIIKIKSN
      *    画像等手術支援加算
           MOVE    Z03-GAZOOPESUP      TO  SPA-GMN-GAZOOPESUP
      *    麻酔識別
           MOVE    Z03-MASUISKBKBN     TO  SPA-GMN-MASUISKBKBN
      *H28.4
      *    非侵襲的血行動態モニタリング加算
           MOVE    Z03-HISINSYUMONKSN      TO  SPA-GMN-HISINSYUMONKSN
           MOVE    Z03-TOUKETUHOZONKSN     TO  SPA-GMN-TOUKETUHOZONKSN
           MOVE    Z03-KUBUNBANGOU         TO  SPA-GMN-KUBUNBANGOU
      *    労災算定不可
           MOVE    Z03-ROSAIKBN            TO  SPA-GMN-ROSAIKBN
           MOVE    Z03-SISIKSN             TO  SPA-GMN-SISIKSN
      *H30.4
      *    悪性腫瘍病理組織標本加算
           MOVE    Z03-AKUSEIBYORIKSN      TO  SPA-GMN-AKUSEIBYORIKSN
      *R02.4
      *    創外固定器加算
           MOVE    Z03-SOGAIKOTEIKSN       TO  SPA-GMN-SOGAIKOTEIKSN
      *    超音波切削器加算
           MOVE    Z03-CHPSESSAKUKSN       TO  SPA-GMN-CHPSESSAKUKSN
      *    左心耳閉鎖術併施区分
           MOVE    Z03-SASINJIHEISIKBN     TO  SPA-GMN-SASINJIHEISIKBN
      *
      *        施設基準コード１〜１０
           MOVE    Z03-SSTKIJUNCD1     TO  SPA-GMN-SSTKIJUNCD(1)
           MOVE    Z03-SSTKIJUNCD2     TO  SPA-GMN-SSTKIJUNCD(2)
           MOVE    Z03-SSTKIJUNCD3     TO  SPA-GMN-SSTKIJUNCD(3)
           MOVE    Z03-SSTKIJUNCD4     TO  SPA-GMN-SSTKIJUNCD(4)
           MOVE    Z03-SSTKIJUNCD5     TO  SPA-GMN-SSTKIJUNCD(5)
           MOVE    Z03-SSTKIJUNCD6     TO  SPA-GMN-SSTKIJUNCD(6)
           MOVE    Z03-SSTKIJUNCD7     TO  SPA-GMN-SSTKIJUNCD(7)
           MOVE    Z03-SSTKIJUNCD8     TO  SPA-GMN-SSTKIJUNCD(8)
           MOVE    Z03-SSTKIJUNCD9     TO  SPA-GMN-SSTKIJUNCD(9)
           MOVE    Z03-SSTKIJUNCD10    TO  SPA-GMN-SSTKIJUNCD(10)
      *        点数欄集計先識別（外来）
           MOVE    Z03-GAITENTTLKBN    TO  SPA-GMN-GAITENTTLKBN
      *        点数欄集計先識別（入院）
           MOVE    Z03-NYUTENTTLSKB    TO  SPA-GMN-NYUTENTTLSKB
      *        公表順序番号
           MOVE    Z03-KOUHYOJYUNNUM   TO  SPA-GMN-KOUHYOJYUNNUM
      *        コード表用番号
           MOVE    Z03-CDKBN-KBN       TO  SPA-GMN-CDKBN-KBN
           MOVE    Z03-CDKBN-SYO       TO  SPA-GMN-CDKBN-SYO
           MOVE    Z03-CDKBN-BU        TO  SPA-GMN-CDKBN-BU
           MOVE    Z03-CDKBN-KBNNUM    TO  SPA-GMN-CDKBN-KBNNUM
           MOVE    Z03-CDKBN-KBNNUM-EDA
                                       TO  SPA-GMN-CDKBN-KBNNUM-EDA
           MOVE    Z03-CDKBN-KOUBAN    TO  SPA-GMN-CDKBN-KOUBAN
      *        告示・通知関連番号
           MOVE    Z03-KOKUCHI-KBN     TO  SPA-GMN-KOKUCHI-KBN
           MOVE    Z03-KOKUCHI-SYO     TO  SPA-GMN-KOKUCHI-SYO
           MOVE    Z03-KOKUCHI-BU      TO  SPA-GMN-KOKUCHI-BU
           MOVE    Z03-KOKUCHI-KBNNUM  TO  SPA-GMN-KOKUCHI-KBNNUM
           MOVE    Z03-KOKUCHI-KBNNUM-EDA
                                       TO   SPA-GMN-KOKUCHI-KBNNUM-EDA
           MOVE    Z03-KOKUCHI-KOUBAN  TO  SPA-GMN-KOKUCHI-KOUBAN
      *        告示等識別区分
           MOVE    Z03-KOKUJISKBKBN1   TO  SPA-GMN-KOKUJISKBKBN1
           MOVE    Z03-KOKUJISKBKBN2   TO  SPA-GMN-KOKUJISKBKBN2
      *
           .
      *
       4101-GMN-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本チェック処理
      *****************************************************************
       4102-KIHON-CHK-SEC          SECTION.
      *
      *    項目名テーブル順にチェックを行う
           PERFORM VARYING     IDX-T   FROM    1   BY  1
                   UNTIL      (IDX-T   >   TBL-GMN-MAX  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               MOVE    IDX-T               TO  CNT-CHK
               PERFORM 41021-KIHON-ALL-SEC
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    CNT-CHK         TO  SPA-GMN-CUR
           END-IF
      *
           .
      *
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　項目別エラーチェック
      *****************************************************************
       41021-KIHON-ALL-SEC                            SECTION.
      *
           SET     IDX-GMN             TO  CNT-CHK
      *
           EVALUATE    TBL-WIDGET (IDX-GMN)
      *        有効開始年月日チェック
               WHEN    "YUKOSTYMD"
                   PERFORM  501-YUKOSTYMDCHK-SEC
      *        有効終了年月日チェック
               WHEN    "YUKOEDYMD"
                   PERFORM  502-YUKOEDYMDCHK-SEC
      *        診療区分
               WHEN    "SRYKBN"
                   PERFORM 503-SRYKBN-CHK-SEC
      *        診療種別
               WHEN    "SRYSYUKBN"
                   PERFORM 504-SRYSYUKBN-CHK-SEC
      *        データ区分
               WHEN    "DATAKBN"
                   PERFORM 5041-DATAKBN-CHK-SEC
      *        カナ氏名チェック
               WHEN    "KANANAME"
                   PERFORM 503-KANANAME-CHK-SEC
      *        氏名チェック
               WHEN    "NAME"
                   PERFORM 504-NAME-CHK-SEC
      *        正式名
               WHEN    "FORMALNAME"
                   PERFORM 505-FORMALNAME-CHK-SEC
      *        処方名称
               WHEN    "SHONAME"
                   PERFORM 505-SHONAME-CHK-SEC
      *        金額
               WHEN    "TEN"
                   CONTINUE
      *        金額種別
               WHEN    "TENSIKIBETU"
                   PERFORM 505-TENSIKIBETU-CHK-SEC
      *        旧金額
               WHEN    "OLDTEN"
                   CONTINUE
      *        旧金額種別
               WHEN    "OLDTENSKB"
                   PERFORM 505-OLDTENSKB-CHK-SEC
      *        単位コード
               WHEN   "TANICD"
                   PERFORM 5012-TANICD-CHK-SEC
      *        保険適用
               WHEN    "HKNTEKKBN"
                   PERFORM 505-HKNTEKKBN-CHK-SEC
      *        傷病名関連
               WHEN    "BYOKANRENKBN"
                   PERFORM 505-BYOKANRENKBN-CHK-SEC
      *        指導管理料
               WHEN    "SDOKNRYO"
                   PERFORM 505-SDOKNRYO-CHK-SEC
      *        医薬品関連
               WHEN    "YKZKNRKBN"
                   PERFORM 505-YKZKNRKBN-CHK-SEC
      *        入外適用
               WHEN    "NYUGAITEKKBN"
                   PERFORM 505-NYUGAITEKKBN-CHK-SEC
      *        老人適用
               WHEN    "ROUTEKKBN"
                   PERFORM 505-ROUTEKKBN-CHK-SEC
      *        病院診療所
               WHEN    "HOSPSRYKBN"
                   PERFORM 505-HOSPSRYKBN-CHK-SEC
      *        医療観察法対象
               WHEN    "IRYOKAN"
                   PERFORM 505-IRYOKAN-CHK-SEC
      *        ＤＰＣ区分
               WHEN    "DPCKBN"
                   CONTINUE
      *        往診
               WHEN    "OSINKBN"
                   PERFORM 505-OSINKBN-CHK-SEC
      *        病床数
               WHEN    "BYOSYOKBN"
                   CONTINUE
      *        算定履歴
               WHEN    "SANTEIRRKKBN"
                   PERFORM 505-SANTEIRRKKBN-CHK-SEC
      *        時間加算
               WHEN    "TIMEKSNKBN"
                   PERFORM 505-TIMEKSNKBN-CHK-SEC
      *        外来管理加算
               WHEN    "GAIKANRIKBN"
                   PERFORM 505-GAIKANRIKBN-CHK-SEC
      *        通則年齢
               WHEN    "TSUSOKUAGE"
                   PERFORM 505-TSUSOKUAGE-CHK-SEC
      *        下限年齢
               WHEN    "KGNAGE"
                   PERFORM 5026-KGNAGE-CHK-SEC
      *        上限年齢
               WHEN    "JGNAGE"
                   PERFORM 5027-JGNAGE-CHK-SEC
      *        採血料区分
               WHEN    "SAIKETUKBN"
                   PERFORM 5030-SAIKETUKBN-CHK-SEC
      *        前回検査日区分
               WHEN    "ZENKNSKBN"
                   PERFORM 5030-ZENKNSKBN-CHK-SEC
      *        入力チェック区分
               WHEN    "INPCHKKBN"
                   CONTINUE
      *
      *        実日数
               WHEN    "JITUDAY"
      *        日数・回数
               WHEN    "DAYCNT"
      *        上限回数月
               WHEN    "JGNCNT"
      *        上限回数日
               WHEN    "JGNCNT1D"
      *        上限回数週
               WHEN    "JGNCNT1W"
      *        他月数
               WHEN    "JGNCNTETCM"
      *        他
               WHEN    "JGNCNTETC"
      *        上限エラー処理
               WHEN    "JGNCNTERR"
                   CONTINUE
      *        ユーザ設定算定履歴
               WHEN    "PLUS_SANTEIRRK"
                   PERFORM 5030-PLUS-SANTEIRRK-SEC
      *        ユーザ設定 上限回数
               WHEN    "PLUS_JGNCNT"
               WHEN    "PLUS_JGNCNT1D"
               WHEN    "PLUS_JGNCNT1W"
               WHEN    "PLUS_JGNCNTETCM"
               WHEN    "PLUS_JGNCNTETC"
                   CONTINUE
      *        ユーザ設定エラー処理
               WHEN    "PLUS_JGNCNTERR"
                   PERFORM 5030-JGNCNTERR-CHK-SEC
      *        きざみ値識別
               WHEN    "KZMCOMPSIKIBETU"
      *        きざみ値下限値
               WHEN    "KZMKGN"
      *        きざみ値上限値
               WHEN    "KZMJGN"
      *        きざみ値
               WHEN    "KZM"
      *        きざみ値きざみ点数
               WHEN    "KZMTEN"
      *        きざみ値エラー処理
               WHEN    "KZMERR"
                   CONTINUE
      *        注加算コード
               WHEN    "CHUKSNCD"
                   PERFORM 5042-CHUKSNCD-CHK-SEC
      *        注加算通番
               WHEN    "CHUKSNTSUBAN"
                   IF    SPA-GMN-CHUKSNTSUBAN  =   SPACE
                       MOVE    ZERO            TO  SPA-GMN-CHUKSNTSUBAN
                   END-IF
                   IF  (SPA-GMN-CHUKSNTSUBAN   >= "0" AND <= "9")  OR
                       (SPA-GMN-CHUKSNTSUBAN   >= "A" AND <= "Z")
                       CONTINUE
                   ELSE
                       MOVE    "0003"                TO  SPA-ERRCD
                   END-IF
      *        年齢加算
               WHEN    "AGEKSNKGN1"
                   MOVE    SPA-GMN-AGEKSNKGN(1)  TO  WRK-AGEKSNKGN
                   PERFORM 5044-AGEKSNKGN-CHK-SEC
                   MOVE    WRK-AGEKSNKGN     TO  SPA-GMN-AGEKSNKGN(1)
               WHEN    "AGEKSNJGN1"
                   MOVE    SPA-GMN-AGEKSNJGN(1)  TO  WRK-AGEKSNJGN
                   PERFORM 5045-AGEKSNJGN-CHK-SEC
                   MOVE    WRK-AGEKSNJGN     TO  SPA-GMN-AGEKSNJGN(1)
               WHEN    "AGEKSNSRYCD1"
                   CONTINUE
               WHEN    "AGEKSNKGN2"
                   MOVE    SPA-GMN-AGEKSNKGN(2)  TO  WRK-AGEKSNKGN
                   PERFORM 5044-AGEKSNKGN-CHK-SEC
                   MOVE    WRK-AGEKSNKGN     TO  SPA-GMN-AGEKSNKGN(2)
               WHEN    "AGEKSNJGN2"
                   MOVE    SPA-GMN-AGEKSNJGN(2)  TO  WRK-AGEKSNJGN
                   PERFORM 5045-AGEKSNJGN-CHK-SEC
                   MOVE    WRK-AGEKSNJGN     TO  SPA-GMN-AGEKSNJGN(2)
               WHEN    "AGEKSNSRYCD2"
                   CONTINUE
               WHEN    "AGEKSNKGN3"
                   MOVE    SPA-GMN-AGEKSNKGN(3)  TO  WRK-AGEKSNKGN
                   PERFORM 5044-AGEKSNKGN-CHK-SEC
                   MOVE    WRK-AGEKSNKGN     TO  SPA-GMN-AGEKSNKGN(3)
               WHEN    "AGEKSNJGN3"
                   MOVE    SPA-GMN-AGEKSNJGN(3)  TO  WRK-AGEKSNJGN
                   PERFORM 5045-AGEKSNJGN-CHK-SEC
                   MOVE    WRK-AGEKSNJGN     TO  SPA-GMN-AGEKSNJGN(3)
               WHEN    "AGEKSNSRYCD3"
                   CONTINUE
               WHEN    "AGEKSNKGN4"
                   MOVE    SPA-GMN-AGEKSNKGN(4)  TO  WRK-AGEKSNKGN
                   PERFORM 5044-AGEKSNKGN-CHK-SEC
                   MOVE    WRK-AGEKSNKGN     TO  SPA-GMN-AGEKSNKGN(4)
               WHEN    "AGEKSNJGN4"
                   MOVE    SPA-GMN-AGEKSNJGN(4)  TO  WRK-AGEKSNJGN
                   PERFORM 5045-AGEKSNJGN-CHK-SEC
                   MOVE    WRK-AGEKSNJGN     TO  SPA-GMN-AGEKSNJGN(4)
               WHEN    "AGEKSNSRYCD4"
                   CONTINUE
      *
      *        通則加算対象外
               WHEN    "TUSOKUGAIKBN"
      *        レーザー照射
               WHEN    "LASERKSN"
      *        超音波メス
               WHEN    "CHPMESUKSN"
      *        ドナー分集計区分
               WHEN    "MICMESUKSN"
      *        マイクロ波メス
               WHEN    "DONORKBN"
      *        脊髄測定
               WHEN    "SEKIZUISOKUTEI"
      *        頸部術
               WHEN    "KEIBUJYUTU6"
      *        自動縫合器
               WHEN    "AUTOHOUGO"
      *        自動吻合器
               WHEN    "AUTOFUNGO"
      *        超音波石灰装置
               WHEN    "CHPGYOKOSOTIKSN"
      *        手術帰宅加算
               WHEN    "SHORTSTAYOPE"
      *        施設基準不適合区分
               WHEN    "KIJUNTEIGENKBN"
      *        基準不適合逓減対象施設基準コード
               WHEN    "KIJUNTEIGENCD"
      *        副鼻腔手術用内視鏡加算
               WHEN    "FUKUBIKUNAIKBN"
      *        副鼻腔手術用切除機器加算
               WHEN    "FUKUBIKUKOTUKBN"
      *        長時間麻酔管理加算
               WHEN    "TIMEMASUIKBN"
      *        検査等実施判断
               WHEN    "KNSJISKBN"
      *        検査等実施判断グループ
               WHEN    "KNSJISGRPKBN"
      *        検体検査コメント
               WHEN    "KENTAICOMMENT"
      *        逓減対象
               WHEN    "TEIGENKBN"
      *        包括逓減
               WHEN    "HOKATUTEIGENKBN"
      *        包括対象検査
               WHEN    "HOUKNSKBN"
      *        超音波内視鏡
               WHEN    "CHPNAISIKSN"
      *        入院基本料区分
               WHEN    "NYUKHNKBN"
      *        入院基本料加算区分
               WHEN    "NYUKHNKSNKBN"
      *        看護加算
               WHEN    "KANGOKSN"
      *        地域加算
               WHEN    "CHIIKIKSN"
      *        画像等手術支援加算
               WHEN    "GAZOOPESUP"
      *        麻酔識別区分
               WHEN    "MASUISKBKBN"
      *
      *        施設基準コード１〜１０
               WHEN    "SSTKIJUNCD1"
               WHEN    "SSTKIJUNCD2"
               WHEN    "SSTKIJUNCD3"
               WHEN    "SSTKIJUNCD4"
               WHEN    "SSTKIJUNCD5"
               WHEN    "SSTKIJUNCD6"
               WHEN    "SSTKIJUNCD7"
               WHEN    "SSTKIJUNCD6"
               WHEN    "SSTKIJUNCD9"
               WHEN    "SSTKIJUNCD10"
      *        点数欄集計先識別（外来）
               WHEN    "GAITENTTLKBN"
      *        点数欄集計先識別（入院）
               WHEN    "NYUTENTTLSKB"
      *        公表順序番号
               WHEN    "KOUHYOJYUNNUM"
      *        コード表用番号
               WHEN    "CDKBN-KBN"
               WHEN    "CDKBN-SYO"
               WHEN    "CDKBN-BU"
               WHEN    "CDKBN-KBNNUM"
               WHEN    "CDKBN-KBNNUM-EDA"
               WHEN    "CDKBN-KOUBAN"
      *        告示・通知関連番号
               WHEN    "KOKUCHI-KBN "
               WHEN    "KOKUCHI-SYO"
               WHEN    "KOKUCHI-BU"
               WHEN    "KOKUCHI-KBNNUM"
               WHEN    "KOKUCHI-KBNNUM-EDA"
               WHEN    "KOKUCHI-KOUBAN"
      *        告示等識別区分
               WHEN    "KOKUJISKBKBN1"
               WHEN    "KOKUJISKBKBN2"
                   CONTINUE
           END-EVALUATE.
      *
           .    
      *
       41021-KIHON-ALL-EXT.
           EXIT.
      *
      *****************************************************************
      *  　有効開始年月日チェック
      *****************************************************************
       501-YUKOSTYMDCHK-SEC            SECTION.
      *
           IF      SPA-GMN-YUKOSTYMD   =   SPACE
               MOVE    "0000000"           TO  SPA-GMN-YUKOSTYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOSTYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOSTYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOSTYMD
           ELSE
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO      501-YUKOSTYMDCHK-EXT
           END-IF
      *
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
           IF      SPA-GMN-YUKOEDYMD   =   SPACE
               MOVE    "9999999"           TO  SPA-GMN-YUKOEDYMD
           END-IF
      *
           MOVE    SPA-GMN-YUKOEDYMD   TO  WRK-LNKYMD
           PERFORM 831-YMDCONV12-SEC
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-SYMD            TO  SPA-NAI-YUKOEDYMD
               MOVE    WRK-LNKYMD          TO  SPA-GMN-YUKOEDYMD
           ELSE
               MOVE    "0002"              TO  SPA-ERRCD
               GO      TO      502-YUKOEDYMDCHK-EXT
           END-IF
      *
           .
      *
       502-YUKOEDYMDCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    カナ名称チェック処理
      *****************************************************************
       503-KANANAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-KANANAME    TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-KANANAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-OUT-INPUT       TO  SPA-GMN-KANANAME
           ELSE
               MOVE    "0003"              TO  SPA-ERRCD
           END-IF
           .
      *
       503-KANANAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    漢字名称チェック処理
      *****************************************************************
       504-NAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-NAME        TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-NAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-OUT-INPUT       TO  SPA-GMN-NAME
           ELSE
               MOVE    "0004"              TO  SPA-ERRCD
           END-IF
           .
      *
       504-NAME-CHK-EXT.
           EXIT.
      *****************************************************************
      *    通称名称チェック処理
      *****************************************************************
       505-FORMALNAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-FORMALNAME  TO  WRK-MAE-INPUT
           MOVE    200                 TO  WRK-LEN
           PERFORM 400-ZENCHK-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-FORMALNAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-OUT-INPUT       TO  SPA-GMN-FORMALNAME
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-FORMALNAME-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方名称チェック処理
      *****************************************************************
       505-SHONAME-CHK-SEC          SECTION.
      *
           MOVE    SPA-GMN-SHONAME     TO  WRK-MAE-INPUT
           MOVE    100                 TO  WRK-LEN
           PERFORM 410-ZENCHK-HEN-SEC
           MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SHONAME
           IF      FLG-ERR             =   ZERO
               MOVE    WRK-OUT-INPUT       TO  SPA-GMN-SHONAME
           ELSE
               MOVE    "0010"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-SHONAME-CHK-EXT.
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
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT 
               MOVE    ZERO                TO  FLG-ERR
           END-IF
           .
      *
       400-ZENCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    半角変換（チェック）処理
      *****************************************************************
       410-ZENCHK-HEN-SEC             SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MAE-INPUT       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )
               MOVE    1                   TO  FLG-ERR
               MOVE    KANACHK-MAE-INPUT   TO  WRK-OUT-INPUT 
           ELSE
               MOVE    ZERO                TO  FLG-ERR
               MOVE    KANACHK-OUT-INPUT   TO  WRK-OUT-INPUT 
           END-IF
           .
       410-ZENCHK-HEN-EXT.
           EXIT.
      *****************************************************************
      *  　診療区分チェック
      *****************************************************************
       503-SRYKBN-CHK-SEC                      SECTION.
      *
           IF     (SPA-Z01-SRYCD (1:1) NOT =   "1" )  AND
                  (SPA-GMN-SRYKBN          =   SPACE)
               GO      TO  503-SRYKBN-CHK-EXT
           END-IF
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT  END
                       MOVE    1               TO  FLG-ERR
                   WHEN    SPA-GMN-SRYKBN      =   MSYU-SRYKBN
                                                           (MSYU-IDX)
                       MOVE    ZERO            TO  FLG-ERR
           END-SEARCH
      *
      *    入院のチェック
           IF      SPA-GMN-SRYKBN      =   "90"
               MOVE    ZERO            TO  FLG-ERR
           END-IF
      *
           IF      FLG-ERR             =   1
               MOVE    "0011"          TO  SPA-ERRCD
            END-IF
      *
           .
      *
       503-SRYKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　診療種別チェック
      *****************************************************************
       504-SRYSYUKBN-CHK-SEC                      SECTION.
      *
      *
           IF     (SPA-Z01-SRYCD (1:1) NOT =   "1" )  AND
                  (SPA-GMN-SRYSYUKBN       =   SPACE)
               GO      TO  504-SRYSYUKBN-CHK-EXT
           END-IF
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT  END
                       MOVE    "0012"              TO  SPA-ERRCD
                       MOVE    SPACE               TO  WRK-SRYKBN
                   WHEN    SPA-GMN-SRYSYUKBN   =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN (MSYU-IDX)    TO  WRK-SRYKBN
           END-SEARCH
      *
      *    入院のチェック
           IF      WRK-SRYKBN          =   SPACE
               IF      SPA-GMN-SRYSYUKBN   =   "900"
                   MOVE    "90"                TO  WRK-SRYKBN
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      WRK-SRYKBN      NOT =   SPACE
               IF      WRK-SRYKBN      NOT =   SPA-GMN-SRYKBN
                   MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   SPA-SRYKBN-MAX
                       IF      SPA-GMN-SRYKBN  =   SPA-GMN-SRYKBN-L(IDX)
                           MOVE    SPA-GMN-SRYKBNCOB (IDX)
                                                   TO  SPA-GMN-SRYKBN-G
                           MOVE    SPA-SRYKBN-MAX  TO  IDX
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
           .
      *
       504-SRYSYUKBN-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　データ区分チェック
      *****************************************************************
       5041-DATAKBN-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               IF      SPA-GMN-DATAKBN     =   SPA-GMN-DATAKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                               TO  SPA-GMN-DATAKBN-G
                   MOVE    SPA-DATAKBN-MAX     TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO      TO      5041-DATAKBN-CHK-EXT
           END-IF
      *
      *    時間外
           MOVE    SPACE               TO  SPA-GMN-TIMEKSNKBNCOB-G
           MOVE    ZERO                TO  SPA-TIMEKSNKBN-MAX
           IF      SPA-GMN-DATAKBN     =   1  OR  ZERO
               MOVE    "0 時間外算定不可"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (1)
               MOVE    "1 時間外算定可能"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (2)
      ****     MOVE    "2 初診料の休日に係る"
               MOVE    "3 初診料の休日に係る"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (3)
               MOVE    3                 TO  SPA-TIMEKSNKBN-MAX
           END-IF
           IF     (SPA-GMN-DATAKBN        =   2  )
      *H29.5
      *        手技料で時間区分が時間外区分である時
           OR    ((SPA-GMN-DATAKBN        =   1   )  AND
                  (SPA-GMN-KOKUJISKBKBN2  =   "7" )  AND
                  (SPA-GMN-TIMEKSNKBN     <>  "0" )  AND
                  (SPA-GMN-SRYKBN         =   "11"
                                          OR  "12"
                                          OR  "13"
                                          OR  "14" ))
               MOVE    "0 下記以外の診療行為"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (1)
               MOVE    "1 時間外加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (2)
               MOVE    "2 休日加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (3)
               MOVE    "3 初診料の休日加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (4)
               MOVE    "4 深夜加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (5)
               MOVE    "5 時間外特定加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (6)
               MOVE    "6 夜間・早朝加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (7)
               MOVE    "7 夜間加算"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (8)
               MOVE    "8 時間外等加算（100点以上）"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (9)
               MOVE    "9 休日加算（100点以上）"
                                         TO  SPA-GMN-TIMEKSNKBNCOB (10)
               MOVE    10                TO  SPA-TIMEKSNKBN-MAX
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TIMEKSNKBN-MAX
               IF      SPA-GMN-TIMEKSNKBN  =   SPA-GMN-TIMEKSNKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TIMEKSNKBNCOB (IDX)
                                           TO  SPA-GMN-TIMEKSNKBN-G
                   MOVE    SPA-TIMEKSNKBN-MAX  TO  IDX
               END-IF
           END-PERFORM
           .
      *
       5041-DATAKBN-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　金額種別チェック
      *****************************************************************
       505-TENSIKIBETU-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               IF      SPA-GMN-TENSIKIBETU =   SPA-GMN-TENSIKIBETUCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                               TO  SPA-GMN-TENSIKIBETU-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-TENSIKI-MAX     TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO      TO      505-TENSIKIBETU-CHK-EXT
           END-IF
           .
      *
       505-TENSIKIBETU-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　旧金額種別チェック
      *****************************************************************
       505-OLDTENSKB-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSKB-MAX
               IF      SPA-GMN-OLDTENSKB =   SPA-GMN-OLDTENSKBCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                               TO  SPA-GMN-OLDTENSKB-G
                   MOVE    SPA-OLDTENSKB-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
               GO      TO      505-OLDTENSKB-CHK-EXT
           END-IF
           .
      *
       505-OLDTENSKB-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　単位コードチェック
      *****************************************************************
       5012-TANICD-CHK-SEC             SECTION.
      *
      *
           IF      SPA-GMN-TANICD      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-TANICD
           END-IF
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    SPA-GMN-TANICD      TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN     NOT =   SPACE )   OR
                  (SNUM-SYOKBN     NOT =   SPACE )
               MOVE    "0007"                  TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTEDT(13:3)       TO  SPA-GMN-TANICD
           END-IF
      *
           IF      SPA-GMN-TANICD      =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TANINAME
               GO      TO              5012-TANICD-CHK-EXT
           END-IF
      *
           SET     MTANI-IDX           TO  1
           SEARCH      MTANI-TBL-OCC    VARYING   MTANI-IDX
                   AT  END
                       MOVE    "0013"              TO  SPA-ERRCD
                       MOVE    SPACE               TO  SPA-GMN-TANINAME
                   WHEN    SPA-GMN-TANICD      =   MTANI-TANICD 
                                                           (MTANI-IDX)
                       MOVE    MTANI-TANINAME (MTANI-IDX)
                                               TO  SPA-GMN-TANINAME
           END-SEARCH
           .
      *
       5012-TANICD-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　保険適用チェック
      *****************************************************************
       505-HKNTEKKBN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNTEKKBN-MAX
               IF      SPA-GMN-HKNTEKKBN   =   SPA-GMN-HKNTEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HKNTEKKBNCOB (IDX)
                                               TO  SPA-GMN-HKNTEKKBN-G
                   MOVE    1                   TO  FLG-OK
                   MOVE    SPA-HKNTEKKBN-MAX   TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
       505-HKNTEKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　傷病名関連チェック
      *****************************************************************
       505-BYOKANRENKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-BYOKANRENKBN    =   SPACE
                   MOVE    "00"            TO  SPA-GMN-BYOKANRENKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-BYOKANRENKBN-MAX
               IF      SPA-GMN-BYOKANRENKBN  =   SPA-GMN-BYOKANRENKBNCOB
                                                         (IDX)(1:2)
                   MOVE    SPA-GMN-BYOKANRENKBNCOB (IDX)
                                           TO  SPA-GMN-BYOKANRENKBN-G
                   MOVE    1                       TO  FLG-OK
                   MOVE    SPA-BYOKANRENKBN-MAX    TO  IDX
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-BYOKANRENKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　指導管理料関連チェック
      *****************************************************************
       505-SDOKNRYO-CHK-SEC         SECTION.
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-SDOKNRYO    =   SPACE
                   MOVE    ZERO            TO  SPA-GMN-SDOKNRYO
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SDOKNRYO-MAX
               IF      SPA-GMN-SDOKNRYO    =   SPA-GMN-SDOKNRYOCOB
                                                         (IDX)(1:3)
                   MOVE    SPA-GMN-SDOKNRYOCOB (IDX)
                                               TO  SPA-GMN-SDOKNRYO-G
                   MOVE    SPA-SDOKNRYO-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-SDOKNRYO-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　医薬品関連チェック
      *****************************************************************
       505-YKZKNRKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-YKZKNRKBN   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-YKZKNRKBN
               END-IF
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YKZKNRKBN-MAX
               IF      SPA-GMN-YKZKNRKBN   =   SPA-GMN-YKZKNRKBNCOB
                                                         (IDX)(1:2)
                   MOVE    SPA-GMN-YKZKNRKBNCOB (IDX)
                                               TO  SPA-GMN-YKZKNRKBN-G
                   MOVE    SPA-YKZKNRKBN-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-YKZKNRKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　入外適用チェック
      *****************************************************************
       505-NYUGAITEKKBN-CHK-SEC         SECTION.
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-NYUGAITEKKBN    =   SPACE
                   MOVE    "0"             TO  SPA-GMN-NYUGAITEKKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAITEKKBN-MAX
               IF      SPA-GMN-NYUGAITEKKBN
                                           =   SPA-GMN-NYUGAITEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-NYUGAITEKKBNCOB (IDX)
                                           TO  SPA-GMN-NYUGAITEKKBN-G
                   MOVE    SPA-NYUGAITEKKBN-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-NYUGAITEKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　老人適用チェック
      *****************************************************************
       505-ROUTEKKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-ROUTEKKBN   =   SPACE
                   MOVE    "0"             TO  SPA-GMN-ROUTEKKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ROUTEKKBN-MAX
               IF      SPA-GMN-ROUTEKKBN   =   SPA-GMN-ROUTEKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-ROUTEKKBNCOB (IDX)
                                               TO  SPA-GMN-ROUTEKKBN-G
                   MOVE    SPA-ROUTEKKBN-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-ROUTEKKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　病院診療所チェック
      *****************************************************************
       505-HOSPSRYKBN-CHK-SEC         SECTION.
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-HOSPSRYKBN  =   SPACE
                   MOVE    "0"             TO  SPA-GMN-HOSPSRYKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HOSPSRYKBN-MAX
               IF      SPA-GMN-HOSPSRYKBN  =   SPA-GMN-HOSPSRYKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-HOSPSRYKBNCOB (IDX)
                                           TO  SPA-GMN-HOSPSRYKBN-G
                   MOVE    SPA-HOSPSRYKBN-MAX  TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-HOSPSRYKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　医療観察法対象チェック
      *****************************************************************
       505-IRYOKAN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-IRYOKAN-MAX
               IF      SPA-GMN-IRYOKAN     =   SPA-GMN-IRYOKANCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-IRYOKANCOB (IDX)
                                               TO  SPA-GMN-IRYOKAN-G
                   MOVE    SPA-IRYOKAN-MAX     TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-IRYOKAN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　往診チェック
      *****************************************************************
       505-OSINKBN-CHK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OSINKBN-MAX
               IF      SPA-GMN-OSINKBN  =   SPA-GMN-OSINKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-OSINKBNCOB (IDX)
                                               TO  SPA-GMN-OSINKBN-G
                   MOVE    SPA-OSINKBN-MAX     TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
       505-OSINKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　算定履歴チェック
      *****************************************************************
       505-SANTEIRRKKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-SANTEIRRKKBN    =   SPACE
                   MOVE    "0"             TO  SPA-GMN-SANTEIRRKKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIRRKKBN-MAX
               IF      SPA-GMN-SANTEIRRKKBN  =   SPA-GMN-SANTEIRRKKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-SANTEIRRKKBNCOB (IDX)
                                           TO  SPA-GMN-SANTEIRRKKBN-G
                   MOVE    SPA-SANTEIRRKKBN-MAX    TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-SANTEIRRKKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　時間加算チェック
      *****************************************************************
       505-TIMEKSNKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-TIMEKSNKBN  =   SPACE
                   MOVE    "0"             TO  SPA-GMN-TIMEKSNKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TIMEKSNKBN-MAX
               IF      SPA-GMN-TIMEKSNKBN  =   SPA-GMN-TIMEKSNKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-TIMEKSNKBNCOB (IDX)
                                               TO  SPA-GMN-TIMEKSNKBN-G
                   MOVE    SPA-TIMEKSNKBN-MAX  TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-TIMEKSNKBN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　外来管理加算チェック
      *****************************************************************
       505-GAIKANRIKBN-CHK-SEC         SECTION.
      *
      *ver.4.7
      *    予約コードのみ初期表示
           IF      SPA-Z01-SRYCD(1:3)  =   "099"
               IF      SPA-GMN-GAIKANRIKBN =   SPACE
                   MOVE    "0"             TO  SPA-GMN-GAIKANRIKBN
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >    SPA-GAIKANRIKBN-MAX
               IF      SPA-GMN-GAIKANRIKBN  =   SPA-GMN-GAIKANRIKBNCOB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-GAIKANRIKBNCOB (IDX)
                                           TO  SPA-GMN-GAIKANRIKBN-G
                   MOVE    SPA-GAIKANRIKBN-MAX TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
           .
      *
       505-GAIKANRIKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  ユーザ設定　算定履歴チェック
      *****************************************************************
       5030-PLUS-SANTEIRRK-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PLUS-SANTEI-MAX
               IF      SPA-GMN-PLUS-SANTEIRRK
                                       =   SPA-GMN-PLUS-SANTEICOMB
                                                         (IDX)(1:1)
                   MOVE    SPA-GMN-PLUS-SANTEICOMB (IDX)
                                           TO  SPA-GMN-PLUS-SANTEIRRK-G
                   MOVE    SPA-PLUS-SANTEI-MAX TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           ELSE
      *        システム予約は指定附加
               IF      SPA-GMN-PLUS-SANTEIRRK  =   1
                   IF      SPA-Z01-SRYCD(1:1)  NOT =   "1"
                       MOVE    "0019"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
      *
       5030-PLUS-SANTEIRRK-EXT.
           EXIT.
      *****************************************************************
      *  ユーザ設定　エラー処理チェック
      *****************************************************************
       5030-JGNCNTERR-CHK-SEC         SECTION.
      *
           EVALUATE    SPA-GMN-PLUS-JGNCNTERR
               WHEN    ZERO
                   CONTINUE
               WHEN    9
                   IF       SPA-GMN-PLUS-SANTEIRRK =   ZERO
                       MOVE    "0016"              TO  SPA-ERRCD
                   ELSE
                       IF   (SPA-GMN-PLUS-JGNCNT   =   ZERO )  AND
                            (SPA-GMN-PLUS-JGNCNT1D =   ZERO )  AND
                            (SPA-GMN-PLUS-JGNCNT1W =   ZERO )  AND
                            (SPA-GMN-PLUS-JGNCNTETCM   =   ZERO ) AND
                            (SPA-GMN-PLUS-JGNCNTETC    =   ZERO )
                           MOVE    "0017"              TO  SPA-ERRCD
                       END-IF
                   END-IF
               WHEN    OTHER
                   MOVE    "0015"              TO  SPA-ERRCD
           END-EVALUATE
           .
      *
       5030-JGNCNTERR-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　通則年齢算チェック
      *****************************************************************
       505-TSUSOKUAGE-CHK-SEC         SECTION.
      *
           IF    SPA-GMN-TSUSOKUAGE    =   0  OR  1
               CONTINUE
           ELSE
               MOVE    "0025"          TO  SPA-ERRCD
           END-IF
           .
      *
       505-TSUSOKUAGE-CHK-EXT.
           EXIT.
      *        
      *****************************************************************
      *  　下限年齢チェック
      *****************************************************************
       5026-KGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-KGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-KGNAGE
           END-IF
      *
           IF      SPA-GMN-KGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
                                       OR  "BK"
                                       OR  "AE"
      *R02.4
                                       OR  "B6"
                                       OR  "MG"
               CONTINUE
           ELSE
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-KGNAGE      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)       TO  SPA-GMN-KGNAGE
               END-IF
           END-IF
      *
           .
      *
       5026-KGNAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　上限年齢チェック
      *****************************************************************
       5027-JGNAGE-CHK-SEC                      SECTION.
      *
           IF      SPA-GMN-JGNAGE      =   SPACE
               MOVE    ZERO            TO  SPA-GMN-JGNAGE
           END-IF
      *
           IF      SPA-GMN-JGNAGE      =   "AA"
                                       OR  "B3"
                                       OR  "BF"
                                       OR  "BK"
                                       OR  "AE"
      *R02.4
                                       OR  "B6"
                                       OR  "MG"
               CONTINUE
           ELSE
               MOVE    SPA-GMN-JGNAGE      TO  WRK-SNUM-INX
      *
               INITIALIZE                      ORCSNUMAREA
               MOVE    SPA-GMN-JGNAGE      TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)       TO  SPA-GMN-JGNAGE
               END-IF
           END-IF
           .    
      *
       5027-JGNAGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *  　注加算コードチェック処理
      *****************************************************************
       5042-CHUKSNCD-CHK-SEC                            SECTION.
      *
           IF      SPA-GMN-CHUKSNCD    =   SPACE
               MOVE    ZERO                TO  SPA-GMN-CHUKSNCD
            END-IF
      *
      *H26.2 労災対応でチェック削除
      *    INITIALIZE                      ORCSNUMAREA
      *    MOVE    SPA-GMN-CHUKSNCD    TO  SNUM-INX
      *    CALL    "ORCSNUM"           USING
      *                                ORCSNUMAREA
      *    IF     (SNUM-RC         NOT =   ZERO  )   OR
      *           (SNUM-MINKBN     NOT =   SPACE )   OR
      *           (SNUM-SYOKBN     NOT =   SPACE )
      *        MOVE    "0007"              TO  SPA-ERRCD
      *    ELSE
      *        MOVE    SNUM-OUTEDT(12:4)       TO  SPA-GMN-CHUKSNCD
      *****END-IF
           .    
      *
       5042-CHUKSNCD-CHK-EXT.
      *
      *****************************************************************
      *  　上限年齢チェック
      *****************************************************************
       5044-AGEKSNKGN-CHK-SEC                      SECTION.
      *
           IF      WRK-AGEKSNKGN       =   SPACE
               MOVE    ZERO            TO  WRK-AGEKSNKGN
           END-IF
      *
           IF      WRK-AGEKSNKGN       =   "AA"
                                       OR  "B3"
                                       OR  "BF"
                                       OR  "BK"
                                       OR  "AE"
      *R02.4
                                       OR  "B6"
                                       OR  "MG"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-AGEKSNKGN       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)       TO  WRK-AGEKSNKGN
               END-IF
           END-IF
           .    
      *
       5044-AGEKSNKGN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  　上限年齢チェック
      *****************************************************************
       5045-AGEKSNJGN-CHK-SEC                      SECTION.
      *
           IF      WRK-AGEKSNJGN       =   SPACE
               MOVE    ZERO            TO  WRK-AGEKSNJGN
           END-IF
      *
           IF      WRK-AGEKSNJGN       =   "AA"
                                       OR  "B3"
                                       OR  "BF"
                                       OR  "BK"
                                       OR  "AE"
      *R02.4
                                       OR  "B6"
                                       OR  "MG"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    WRK-AGEKSNJGN       TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "0007"              TO  SPA-ERRCD
               ELSE
                   MOVE    SNUM-OUTEDT(14:2)       TO  WRK-AGEKSNJGN
               END-IF
           END-IF
           .    
      *
       5045-AGEKSNJGN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  採血料区分  チェック
      *****************************************************************
       5030-SAIKETUKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SAIKETUKBN-MAX
               IF      SPA-GMN-SAIKETUKBN  =   SPA-GMN-SAIKETUCOMB
                                                            (IDX)(1:1)
                   MOVE    SPA-GMN-SAIKETUCOMB (IDX)
                                           TO  SPA-GMN-SAIKETUKBN-G
                   MOVE    SPA-SAIKETUKBN-MAX  TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *    採血料区分は、検査の時のみ入力可能
           IF      SPA-GMN-SAIKETUKBN  NOT =   "0"
               IF      SPA-GMN-SRYKBN      =   "60"
                   CONTINUE
               ELSE
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0014"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .    
       5030-SAIKETUKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *  採血料区分  チェック
      *****************************************************************
       5030-ZENKNSKBN-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ZENKNSKBN-MAX
               IF      SPA-GMN-ZENKNSKBN   =   SPA-GMN-ZENKNSKBN-COMB
                                                            (IDX)(1:1)
                   MOVE    SPA-GMN-ZENKNSKBN-COMB (IDX)
                                           TO  SPA-GMN-ZENKNSKBN-G
                   MOVE    SPA-ZENKNSKBN-MAX   TO  IDX
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0006"              TO  SPA-ERRCD
           END-IF
      *    採血料区分は、検査の時のみ入力可能
           IF      SPA-GMN-ZENKNSKBN   NOT =   "0"
               IF      SPA-GMN-SRYKBN      =   "60"
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-ZENKNSKBN-COMB (1)
                                               TO  SPA-GMN-ZENKNSKBN-G
               END-IF
           END-IF
           .
       5030-ZENKNSKBN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    切替え処理
      *****************************************************************
       420-KIRIKAE-SEC                              SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-TOUROKU
      *
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *****入力チェック処理
      *****PERFORM 410-INPUT-CHK-SEC
      *
      *
           .
       420-KIRIKAE-EXT.
           EXIT.
      *****************************************************************
      *    タグ切替処理
      *****************************************************************
       480-BETUGAMEN-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    ZERO                TO  FLG-TOUROKU
      *    画面をＳＰＡセット
           PERFORM 4101-GMN-SPA-SET-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-GMN-PAGENO
                   WHEN    0
                       MOVE    1               TO  SPA-GMN-PAGENO
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   WHEN    1
                       MOVE    2               TO  SPA-GMN-PAGENO
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   WHEN    2
                       MOVE    3               TO  SPA-GMN-PAGENO
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   WHEN    3
                       MOVE    0               TO  SPA-GMN-PAGENO
                       MOVE    ZERO            TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           .
       480-BETUGAMEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       401-BACK-SEC                              SECTION.
      *
           MOVE  "Z01"                           TO  SPA-SAKIPG.
      *
           MOVE  "Z03"                           TO  SPA-MOTOPG.
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
      *H30.9
      *    新規・改正処理はなし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               GO      TO     403-DEL-EXT
           END-IF
      *
           IF      SPA-NAI-KEIFLG1     =   ZERO
               IF      SPA-Z01-SRYCD(1:1)  =   "1"
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
       4031-DEL-SEC                              SECTION.
      *
           IF      SPA-MASTER-CLASS    =   2
      *        ユーザ点数設定分
               PERFORM 40311-TENSU-USER-DEL-SEC
           ELSE
      *        点数マスタ削除
               PERFORM 40312-TENSU-MAST-DEL-SEC
           END-IF
      *
           IF      SPA-ERRCD          =   SPACE
               MOVE  "JOIN"               TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
      *
           .
       4031-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザ点数設定分削除処理
      *****************************************************************
       40311-TENSU-USER-DEL-SEC        SECTION.
      *
      *    点数マスタ削除
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_user"    TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z03 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
           .
       40311-TENSU-USER-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ削除処理
      *****************************************************************
       40312-TENSU-MAST-DEL-SEC        SECTION.
      *
      *    点数マスタ削除
           MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
           MOVE    SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOSTYMD
           MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_tensu_master"  TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "Z03 TENSU DEL ERR!!"   MCP-RC
                       ",KEY:"   TNS-KEY
               MOVE    "9001"                TO  SPA-ERRCD
           END-IF
      *
      *    入力コード削除
           IF     (SPA-Z01-SELNUM      =   1    )  AND
                  (SPA-Z01-MAX         =   1    )
               INITIALIZE                  INPUTCD-REC
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
               INITIALIZE                  TENSUPLUS-REC
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
      *        一般老人コード削除
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   5  )    OR
                                  (SPA-Z90-IPNSRYCD (IDX)  =   SPACE )
                   MOVE    SPA-Z90-IPNSRYCD (IDX)  TO  CHG-IPNSRYCD
                   MOVE    SPA-Z90-RJNSRYCD (IDX)  TO  CHG-RJNSRYCD
      *
                   MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
                   MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
      **********   MOVE    "tbl_srycdchg"      TO  MCP-TABLE
                   MOVE    "tbl_srycdchg_master"   TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-PERFORM
           END-IF
           .
       40312-TENSU-MAST-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード　設定処理
      *****************************************************************
       405-INPUTCD-SEC                 SECTION.
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      405-INPUTCD-EXT
           END-IF
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-Z03             TO  SPA-Z01SONOTA
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
           MOVE    SPA-Z01SONOTA       TO  SPA-Z03
      *
      *    初期カーソル
           MOVE    "INPUTCD01"         TO  MCP-WIDGET
      *
           MOVE    "Z99"               TO  SPA-SAKIPG.
           MOVE    "Z03"               TO  SPA-MOTOPG.
      *
           MOVE    SPA-MOTOPG          TO  SPA-Z99GMN-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                             TO  FLG-END.
      *
       405-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    老人一般コード振替設定処理
      *****************************************************************
       407-SRYCDCHG-SEC                SECTION.
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
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      407-SRYCDCHG-EXT
           END-IF
      *
           EVALUATE    SPA-GMN-ROUTEKKBN
               WHEN    1
               WHEN    2
                   CONTINUE
               WHEN    OTHER
                   MOVE    "0021"              TO  SPA-ERRCD
                   GO      TO      407-SRYCDCHG-EXT
           END-EVALUATE
      *
      *R01.10
      *    提供対象はないので確認のみ
           IF      SPA-Z90-SRYCDCHG (1)    =   SPACE
               MOVE    "0022"              TO  SPA-ERRCD
               GO      TO      407-SRYCDCHG-EXT
           END-IF
      *
      *    Ｚ９０初期画面編集
           MOVE    SPACE               TO  SPA-Z90GMN-AREA.
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    SPA-Z90-SRYCDCHG (IDX)
                               TO  SPA-Z90GMN-SRYCDCHG (IDX)
               MOVE    SPA-Z90-SRYNAME  (IDX)
                               TO  SPA-Z90GMN-SRYNAME (IDX)
           END-PERFORM
           MOVE    1                   TO  SPA-Z90GMN-CUR
      *
      *    画面編集
           MOVE    SPACE               TO  Z90
           INITIALIZE                      Z90
           MOVE    SPA-Z01-SRYCD       TO  Z90-SRYCD
           MOVE    SPA-Z01-NAMEH       TO  Z90-SRYNAME
           PERFORM VARYING  IDX    FROM  1  BY  1  
                   UNTIL    IDX        >  5
               MOVE    SPA-Z90GMN-SRYCDCHG (IDX) TO  Z90-SRYCDCHG (IDX)
               MOVE    SPA-Z90GMN-SRYNAME  (IDX) TO  Z90-SRYCDNAME(IDX)
           END-PERFORM
      *
      *    初期カーソル
           MOVE    "SRYCDCHG1"         TO  MCP-WIDGET
      *
           MOVE    "Z90"               TO  SPA-SAKIPG
      *
           MOVE    "Z03"               TO  SPA-MOTOPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                             TO  FLG-END.
      *
       407-SRYCDCHG-EXT.
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
           IF      SPA-Z01-SRYCD(1:1)  NOT =   "1"
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
           MOVE    "Z03"               TO  SPA-MOTOPG
      *
           MOVE    SPA-Z01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *****MOVE    SPA-Z03             TO  SPA-Z01SONOTA
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
           MOVE    SPA-Z01SONOTA       TO  SPA-Z03
           MOVE    SPA-Z010FREE        TO  SPA-Z010
      *    画面情報編集
           MOVE    SPA-GMN-SRYKBN      TO  SPA-Z010-SRYKBN
           MOVE    SPA-GMN-DATAKBN     TO  SPA-Z010-DATAKBN
           MOVE    SPA-GMN-JGNCNT      TO  SPA-Z010-JGNCNT
           MOVE    SPA-GMN-JGNCNT1D    TO  SPA-Z010-JGNCNT1D
           MOVE    SPA-GMN-SANTEIRRKKBN    TO  SPA-Z010-SANTEIRRKKBN
      *
      *    初期カーソル
           IF      SPA-Z010-RRKMAX     =   ZERO
               MOVE    "YUKOSTYMD"         TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM"            TO  MCP-WIDGET
           END-IF
      *
           MOVE    "Z010"              TO  SPA-SAKIPG
           MOVE    "Z03"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    登録処理
      *****************************************************************
       412-TOUROKU-SEC                           SECTION.
      *
           MOVE    SPACE               TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           MOVE    1                   TO  FLG-TOUROKU
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      412-TOUROKU-EXT
           END-IF
      *
           IF      SPA-Z01-SYORIKBN    =   1
      *        新規処理
               IF      SPA-NAI-KEIFLG2     =   ZERO
                   IF      SPA-Z01-SRYCD(1:1)  =   "0"
                       MOVE    "K002"          TO  SPA-ERRCD
                    ELSE
                       MOVE    "K001"          TO  SPA-ERRCD
                   END-IF
                   MOVE    1               TO  SPA-NAI-KEIFLG2
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0002"              TO  SPA-ZIDCD
           END-IF
           .
       412-TOUROKU-EXT.
           EXIT.
      *
      ****************************************************************
      *    登録前、内容変更チェック処理
      ****************************************************************
       4103-HENKOU-CHK-SEC      SECTION.
      *
           IF      SPA-Z01-SYORIKBN    =   2
      *        更新処理の時のみ
               CONTINUE
           ELSE
               GO      TO      4103-HENKOU-CHK-EXT
           END-IF
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
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
               MOVE    TNS-TENSU-REC(1:1290)   TO  WRK-TNS-TENSU-REC
      *        更新項目編集
               PERFORM 4121-KOUMOKUISO-SEC
      *
               IF      TNS-TENSU-REC(1:1290)   =   WRK-TNS-TENSU-REC
                   MOVE    SPACE           TO  SPA-ERRCD
      *H29.1   チェックなし
      *            入力チェック区分はエラーとする
      *            採血料区分・前回検査日チェックは行う
      *            PERFORM 5030-SAIKETUKBN-CHK-SEC
      *            IF      SPA-ERRCD           =   SPACE
      *                PERFORM 5030-ZENKNSKBN-CHK-SEC
      *                IF      SPA-ERRCD       NOT =   SPACE
      *                    MOVE    "ZENKNSKBN"     TO  WRK-MCP-WIDGET
      *                END-IF
      *            ELSE
      *                MOVE    "SAIKETUKBN"        TO  WRK-MCP-WIDGET
      *            END-IF
      *            IF      SPA-ERRCD       NOT =   SPACE
      *            エラーの次のカーソル設定
      *                SET     IDX-GMN             TO  1
      *                SEARCH      TBL-GMN-OCC     VARYING   IDX-GMN
      *                AT  END
      *                    MOVE    ZERO            TO  SPA-GMN-CUR
      *                WHEN    WRK-MCP-WIDGET  =   TBL-WIDGET (IDX-GMN)
      *                    MOVE    IDX-GMN         TO  SPA-GMN-CUR
      *                END-SEARCH 
      ****         END-IF
               END-IF
           END-IF
      *
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
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-YUKOEDYMD   =   SPACE
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-YUKOSTYMD   >=  SPA-NAI-YUKOEDYMD
                   MOVE    "1010"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    有効期限範囲チェック
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-Z01-SYORIKBN
      *            更新チェック
                   WHEN    2
                       PERFORM 41091-YUKOKIKAN-CHK2-SEC
                   WHEN    3
      *            改正チェック
                       PERFORM 41092-YUKOKIKAN-CHK3-SEC
               END-EVALUATE
           END-IF
      *H24.8
      *    期間変更チェック
           IF      SPA-Z01-SELNUM      >=  1
               IF      (SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOSTYMD)
               OR      (SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM)
                                       NOT =   SPA-NAI-YUKOEDYMD)
                   IF     (SPA-NAI-KEIFLGK     =   ZERO  )
                     AND  (SPA-ERRCD           =   SPACE )
                       MOVE    "K004"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-NAI-KEIFLGK
                       MOVE    1               TO  SPA-GMN-CUR
      *H29.1
      *               更新不可
                       IF      (SPA-MASTER-CLASS   =   2  )
                          OR   (SPA-MENTE-OK       =   1  )
                           CONTINUE
                       ELSE
                           MOVE    "0051"      TO  SPA-ERRCD
                           MOVE    ZERO        TO  SPA-NAI-KEIFLGK
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-GMN-NAME        =   SPACE
                   MOVE    "1014"              TO  SPA-ERRCD
                   MOVE    7                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *H29.1
      *    点数付加情報はチェックなし
           GO      TO      4109-TOUROKU-CHK-EXT
      *
      *    前回検査日と算定履歴作成有無判定
           IF      SPA-GMN-ZENKNSKBN   NOT =   ZERO
               IF      (SPA-ERRCD          =   SPACE )
                   AND (SPA-NAI-KEIFLG3    =   ZERO  )
                   IF     (SPA-GMN-SANTEIRRKKBN    =   ZERO )
                     AND  (SPA-GMN-PLUS-SANTEIRRK  =   ZERO )
                       MOVE    "K100"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-NAI-KEIFLG3
                       MOVE    "PLUS_SANTEIRRK"    TO  WRK-MCP-WIDGET
                   END-IF
               END-IF
           END-IF
      *    ユーザ設定の上限回数
           IF       SPA-GMN-PLUS-SANTEIRRK NOT =   ZERO
               IF     (SPA-GMN-JGNCNT   =   ZERO )  AND
                      (SPA-GMN-JGNCNT1D =   ZERO )
                   CONTINUE
               ELSE
                   IF    (SPA-GMN-JGNCNT   NOT =   ZERO )  AND
                         (SPA-GMN-PLUS-JGNCNT  =   ZERO )
                     AND (SPA-GMN-PLUS-JGNCNTETC    =   ZERO )
                     AND (SPA-GMN-PLUS-JGNCNT1W     =   ZERO )
                     AND (SPA-ERRCD                 =   SPACE )
                       MOVE    "0018"              TO  SPA-ERRCD
                       MOVE    "PLUS_JGNCNT"       TO  WRK-MCP-WIDGET
                   END-IF
                   IF    (SPA-GMN-JGNCNT1D     NOT =   ZERO )  AND
                         (SPA-GMN-PLUS-JGNCNT1D    =   ZERO )
                     AND (SPA-ERRCD                =   SPACE )
                       MOVE    "0018"              TO  SPA-ERRCD
                       MOVE    "PLUS_JGNCNT1D"     TO  WRK-MCP-WIDGET
                   END-IF
               END-IF
           END-IF
           .
      *
       4109-TOUROKU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新有効期限範囲チェック処理
      *****************************************************************
       41091-YUKOKIKAN-CHK2-SEC        SECTION.
      *
      *    前チェック
           IF      SPA-Z01-SELNUM      >   1
               MOVE    SPA-Z01-TYUKOEDYMD(SPA-Z01-SELNUM - 1)
                                           TO  WRK-YMD
      *        対象日＋１日
               PERFORM 800-DAYSET-SEC
               IF      SPA-NAI-YUKOSTYMD       <   WRK-OTYMD
                   MOVE    "1011"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    後チェック
           IF      SPA-Z01-SELNUM      <   SPA-Z01-MAX
               MOVE    SPA-NAI-YUKOEDYMD   TO  WRK-YMD
      *        対象日＋１日
               PERFORM 800-DAYSET-SEC
      *****    IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
      *****                                NOT =   WRK-OTYMD
               IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM + 1)
                                               <   WRK-OTYMD
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "1011"              TO  SPA-ERRCD
                       MOVE    2                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
      *
       41091-YUKOKIKAN-CHK2-EXT.
           EXIT.
      *
      *****************************************************************
      *    改正有効期限範囲チェック処理
      *****************************************************************
       41092-YUKOKIKAN-CHK3-SEC        SECTION.
      *
           IF      SPA-Z01-TYUKOSTYMD(SPA-Z01-SELNUM)
                                           >=  SPA-NAI-YUKOSTYMD
               MOVE    "1012"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-NAI-YUKOEDYMD   NOT =  ALL "9"
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "1013"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *H24.8
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-KEIFLGK     =   ZERO
                   MOVE    "K005"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-NAI-KEIFLGK
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
      *
       41092-YUKOKIKAN-CHK3-EXT.
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
      *    前画面に戻る
           IF      SPA-ERRCD           =   SPACE
               MOVE     "JOIN"            TO  MCP-PUTTYPE
               PERFORM  401-BACK-SEC
           END-IF
      *
           .
       4121-TOUROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       41211-TENSU-USER-UPD-SEC        SECTION.
      *
           MOVE    "tbl_tensu_user"    TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
      *H29.1
      *    点数マスタ付加情報更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 4129-TENSUPLUS-UPD-SEC
      *    END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *
      *R01.10  削除
      *    老人一般コード変換マスタ更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 41212-SRYCDCHG-UPD-SEC
      *****END-IF
      *
           .
       41211-TENSU-USER-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録処理
      *****************************************************************
       41212-TENSU-MAST-UPD-SEC        SECTION.
      *
           IF     (SPA-MENTE-OK        =   1 )
      *    メンテナンスモードのみ、マスタ更新
      *
           MOVE    "tbl_tensu_master"  TO  WRK-MCP-TABLE
           MOVE    "key"               TO  WRK-MCP-PATHNAME
           EVALUATE    SPA-Z01-SYORIKBN
      *        改正時処理
               WHEN    3
                   PERFORM 41211-KAISEI-SYORI-SEC
      *　          新規処理
                   PERFORM 41213-INSERT-SYORI-SEC
      *　      更新処理
               WHEN    2
                   PERFORM 41212-UPDATE-SYORI-SEC
      *　      新規処理
               WHEN    1
                   PERFORM 41213-INSERT-SYORI-SEC
           END-EVALUATE
      *
           END-IF
      *H29.1
      *    点数マスタ付加情報更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 4129-TENSUPLUS-UPD-SEC
      *    END-IF
      *
      *    入力コード更新
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCGZS01"          USING
                                           MCPAREA
                                           SPAAREA
           END-IF
      *
      *R01.10  削除
      *    老人一般コード変換マスタ更新
      *    IF      SPA-ERRCD           =   SPACE
      *        PERFORM 41212-SRYCDCHG-UPD-SEC
      ***  END-IF
      *
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
      *****    MOVE    "tbl_tensu"         TO  MCP-TABLE
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
           PERFORM 990-DBLOSEC-SEC
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
      *****    MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
                           ",KEY:" TNSUP-KEY
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
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
      **** MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
      *****    MOVE    "tbl_tensu"         TO  MCP-TABLE
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
           PERFORM 990-DBLOSEC-SEC
      *
           IF      FLG-TENSU           =   ZERO
      *        更新項目編集
               MOVE    SPA-Z01-SRYCD       TO  TNS-SRYCD
               MOVE    SPA-NAI-YUKOSTYMD   TO  TNS-YUKOSTYMD
               MOVE    SPA-NAI-YUKOEDYMD   TO  TNS-YUKOEDYMD
               MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
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
      ******   MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    WRK-MCP-TABLE       TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC              NOT =   ZERO
                   DISPLAY "Z02 TENSU UPD ERR:" MCP-RC
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
               DISPLAY "Z02 TENSU INS ERR:" MCP-RC
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
           MOVE  SPA-GMN-SRYKBN          TO  TNS-SRYKBN         
           MOVE  SPA-GMN-SRYSYUKBN       TO  TNS-SRYSYUKBN      
           MOVE  SPA-GMN-DATAKBN         TO  TNS-DATAKBN        
      *
           MOVE  SPA-GMN-KANANAME        TO  TNS-KANANAME       
                                                     WRK-STRINGTBL
           MOVE  100                     TO  CNT-STRING
           PERFORM 891-STRCOUNT-SEC
           MOVE  CNT-STRING              TO  TNS-KANAYUKOKETA
           MOVE  SPA-GMN-NAME            TO  TNS-NAME           
                                                    WRK-STRINGTBL
           MOVE  100                     TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING              TO  TNS-YUKOKETA
      *    正式名称
           MOVE  SPA-GMN-FORMALNAME      TO  TNS-FORMALNAME           
                                                    WRK-STRINGTBL
           MOVE  200                     TO  CNT-STRING
           PERFORM  891-STRCOUNT-SEC
           MOVE  CNT-STRING              TO  TNS-FORMALYUKOKETA
      *
           MOVE  SPA-GMN-TEN             TO  TNS-TEN
      *
           MOVE  SPA-GMN-TENSIKIBETU     TO  TNS-TENSIKIBETU
           MOVE  SPA-GMN-OLDTEN          TO  TNS-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB       TO  TNS-OLDTENSKB
           MOVE  SPA-GMN-TANICD          TO  TNS-TANICD
           MOVE  SPA-GMN-TANINAME        TO  TNS-TANINAME
           MOVE  SPA-GMN-HKNTEKKBN       TO  TNS-HKNTEKKBN
           MOVE  SPA-GMN-BYOKANRENKBN    TO  TNS-BYOKANRENKBN
      *
           MOVE  SPA-GMN-SDOKNRYO        TO  TNS-SDOKNRYO
           MOVE  SPA-GMN-YKZKNRKBN       TO  TNS-YKZKNRKBN
           MOVE  SPA-GMN-NYUGAITEKKBN    TO  TNS-NYUGAITEKKBN
           MOVE  SPA-GMN-ROUTEKKBN       TO  TNS-ROUTEKKBN
           MOVE  SPA-GMN-HOSPSRYKBN      TO  TNS-HOSPSRYKBN
           MOVE  SPA-GMN-OSINKBN         TO  TNS-OSINKBN
      *
           MOVE  SPA-GMN-BYOSYOKBN       TO  TNS-BYOSYOKBN
      *
           MOVE  SPA-GMN-SANTEIRRKKBN    TO  TNS-SANTEIRRKKBN
      *
           MOVE  SPA-GMN-SANTEIRRKKBN    TO  TNS-SANTEIRRKKBN
      *
           MOVE  SPA-GMN-TIMEKSNKBN      TO  TNS-TIMEKSNKBN
      *
           MOVE  SPA-GMN-GAIKANRIKBN     TO  TNS-GAIKANRIKBN
      *
           MOVE  SPA-GMN-TSUSOKUAGE      TO  TNS-TSUSOKUAGE
      *
           IF    SPA-GMN-KGNAGE            =   SPACE
               MOVE    ZERO              TO  SPA-GMN-KGNAGE
           END-IF
           MOVE  SPA-GMN-KGNAGE          TO  TNS-KGNAGE         
           IF    SPA-GMN-JGNAGE            =   SPACE
               MOVE    ZERO              TO  SPA-GMN-JGNAGE
           END-IF
           MOVE  SPA-GMN-JGNAGE          TO  TNS-JGNAGE         
           MOVE  SPA-GMN-JITUDAY         TO  TNS-JITUDAY
           MOVE  SPA-GMN-DAYCNT          TO  TNS-DAYCNT         
           MOVE  SPA-GMN-JGNCNT          TO  TNS-JGNCNT         
           MOVE  SPA-GMN-JGNCNT1D        TO  TNS-JGNCNT1D       
           MOVE  SPA-GMN-JGNCNT1W        TO  TNS-JGNCNT1W       
           MOVE  SPA-GMN-JGNCNTETCM      TO  TNS-JGNCNTETCM     
           MOVE  SPA-GMN-JGNCNTETC       TO  TNS-JGNCNTETC      
      *
           MOVE  SPA-GMN-JGNCNTERR       TO  TNS-JGNCNTERR      
           MOVE  SPA-GMN-KZMCOMPSIKIBETU TO  TNS-KZMCOMPSIKIBETU
      *
           MOVE  SPA-GMN-KZMKGN          TO  TNS-KZMKGN         
           MOVE  SPA-GMN-KZMJGN          TO  TNS-KZMJGN         
           MOVE  SPA-GMN-KZM             TO  TNS-KZM            
           MOVE  SPA-GMN-KZMTEN          TO  TNS-KZMTEN         
      *
           MOVE  SPA-GMN-KZMERR          TO  TNS-KZMERR         
      *
           MOVE  SPA-GMN-CHUKSNCD        TO  TNS-CHUKSNCD       
           MOVE  SPA-GMN-CHUKSNTSUBAN    TO  TNS-CHUKSNTUBAN    
      *
           PERFORM  VARYING  IDXX
                    FROM  1  BY  1  UNTIL  IDXX  >  4
               MOVE  SPA-GMN-AGEKSNKGN(IDXX)   TO  TNS-AGEKSNKGN(IDXX)
               MOVE  SPA-GMN-AGEKSNJGN(IDXX)   TO  TNS-AGEKSNJGN(IDXX)
               MOVE  SPA-GMN-AGEKSNSRYCD(IDXX) TO
                                               TNS-AGEKSNSRYCD(IDXX)
           END-PERFORM
      *                                          
           MOVE  SPA-GMN-TUSOKUGAIKBN          TO  TNS-TUSOKUGAIKBN
      *
           MOVE  SPA-GMN-LASERKSN              TO  TNS-LASERKBN
      *
           MOVE  SPA-GMN-CHPMESUKSN            TO  TNS-CHPMESUKSN      
      *
           MOVE  SPA-GMN-MICMESUKSN            TO  TNS-MICMESUKSN      
      *
           MOVE  SPA-GMN-DONORKBN              TO  TNS-DONORKBN      
      *
           MOVE  SPA-GMN-SEKIZUISOKUTEI        TO  TNS-SEKIZUISOKUTEI  
      *
           MOVE  SPA-GMN-KEIBUJYUTU            TO  TNS-KEIBUJYUTU      
      *
           MOVE  SPA-GMN-AUTOHOUGO             TO  TNS-AUTOHOUGO       
      *
           MOVE  SPA-GMN-AUTOFUNGO             TO  TNS-AUTOFUNGO       
      *
           MOVE  SPA-GMN-CHPGYOKOSOTIKSN       TO  TNS-CHPGYOKOSOTIKSN 
      *
           MOVE  SPA-GMN-SHORTSTAYOPE          TO  TNS-SHORTSTAYOPE    
      *    施設基準不適合区分
           MOVE  SPA-GMN-KIJUNTEIGENKBN        TO  TNS-KIJUNTEIGENKBN
      *     基準不適合逓減対象施設基準コード
           MOVE  SPA-GMN-KIJUNTEIGENCD         TO  TNS-KIJUNTEIGENCD
      *
           MOVE  SPA-GMN-KNSJISKBN             TO  TNS-KNSJISKBN       
      *
           MOVE  SPA-GMN-KNSJISGRPKBN          TO  TNS-KNSJISGRPKBN    
      *
           MOVE  SPA-GMN-KENTAICOMMENT         TO  TNS-KENTAICOMMENT   
      *
           MOVE  SPA-GMN-TEIGENKBN             TO  TNS-TEIGENKBN       
      *
           MOVE  SPA-GMN-HOKATUTEIGENKBN       TO  TNS-HOUKATUTEIGENKBN
      *
           MOVE  SPA-GMN-HOUKNSKBN             TO  TNS-HOUKNSKBN       
      *
           MOVE  SPA-GMN-CHPNAISIKSN           TO  TNS-CHPNAISIKSN     
      *
           MOVE  SPA-GMN-NYUKHNKBN             TO  TNS-NYUKHNKBN
      *
           MOVE  SPA-GMN-NYUKHNKSNKBN          TO  TNS-NYUKHNKSNKBN
      *
           MOVE  SPA-GMN-KANGOKSN              TO  TNS-KANGOKSN 
      *
           MOVE  SPA-GMN-CHIIKIKSN             TO  TNS-CHIIKIKSN 
      *H28.4
      *    非侵襲的血行動態モニタリング加算
           MOVE  SPA-GMN-HISINSYUMONKSN    TO  TNS-HISINSYUMONITORKSN
           MOVE  SPA-GMN-TOUKETUHOZONKSN   TO  TNS-TOUKETUHOZONKSN
      *    点数表区分番号
           MOVE  SPA-GMN-KUBUNBANGOU       TO  TNS-KUBUNBANGOU
      *    労災算定不可
           MOVE  SPA-GMN-ROSAIKBN          TO  TNS-ROSAIKBN
           MOVE  SPA-GMN-SISIKSN           TO  TNS-SISIKSN
      *    悪性腫瘍病理組織標本加算
           MOVE  SPA-GMN-AKUSEIBYORIKSN    TO  TNS-AKUSEIBYORIKSN
      *R02.4
      *    創外固定器加算
           MOVE  SPA-GMN-SOGAIKOTEIKSN     TO  TNS-SOGAIKOTEIKSN
      *    超音波切削器加算
           MOVE  SPA-GMN-CHPSESSAKUKSN     TO  TNS-CHPSESSAKUKSN
      *    左心耳閉鎖術併施区分
           MOVE  SPA-GMN-SASINJIHEISIKBN   TO  TNS-SASINJIHEISIKBN
      *
           PERFORM  VARYING  IDXX
                    FROM  1  BY  1  UNTIL  IDXX  >  10
               MOVE  SPA-GMN-SSTKIJUNCD(IDXX)  TO  TNS-SSTKIJUNCD
                                                           (IDXX)
           END-PERFORM
      *
           MOVE  SPA-GMN-GAITENTTLKBN          TO  TNS-GAITENTTLKBN
           MOVE  SPA-GMN-NYUTENTTLSKB          TO  TNS-NYUTENTTLKBN
           MOVE  SPA-GMN-KOUHYOJYUNNUM         TO  TNS-KOUHYOJYUNNUM
           MOVE  SPA-GMN-CDKBN-KBN             TO  TNS-CDKBN-KBN
           MOVE  SPA-GMN-CDKBN-SYO             TO  TNS-CDKBN-SYO
           MOVE  SPA-GMN-CDKBN-BU              TO  TNS-CDKBN-BU
           MOVE  SPA-GMN-CDKBN-KBNNUM          TO  TNS-CDKBN-KBNNUM
           MOVE  SPA-GMN-CDKBN-KBNNUM-EDA      TO  TNS-CDKBN-KBNNUM-EDA
           MOVE  SPA-GMN-CDKBN-KOUBAN          TO  TNS-CDKBN-KOUBAN
      *
           MOVE  SPA-GMN-KOKUCHI-KBN           TO  TNS-KOKUCHI-KBN
           MOVE  SPA-GMN-KOKUCHI-SYO           TO  TNS-KOKUCHI-SYO
           MOVE  SPA-GMN-KOKUCHI-BU            TO  TNS-KOKUCHI-BU
           MOVE  SPA-GMN-KOKUCHI-KBNNUM        TO  TNS-KOKUCHI-KBNNUM
           MOVE  SPA-GMN-KOKUCHI-KBNNUM-EDA  TO  TNS-KOKUCHI-KBNNUM-EDA
           MOVE  SPA-GMN-KOKUCHI-KOUBAN        TO  TNS-KOKUCHI-KOUBAN
      *
           MOVE  SPA-GMN-KOKUJISKBKBN1         TO  TNS-KOKUJISKBKBN1
           MOVE  SPA-GMN-KOKUJISKBKBN2         TO  TNS-KOKUJISKBKBN2 
      *Ｈ２０．４から
      *    医療観察法
           MOVE  SPA-GMN-IRYOKAN           TO  TNS-IRYOKANSATUKBN 
           MOVE  SPA-GMN-GAZOOPESUP        TO  TNS-GAZOOPESUP 
           MOVE  SPA-GMN-MASUISKBKBN       TO  TNS-MASUISKBKBN 
      *H26.4　から
      *    ＤＰＣ区分
           MOVE  SPA-GMN-DPCKBN            TO  TNS-DPCKBN
      *    副鼻腔内視鏡加算
           MOVE  SPA-GMN-FUKUBIKUNAIKBN    TO  TNS-FUKUBIKUNAIKBN
      *    副鼻腔手術用切除機器加算
           MOVE  SPA-GMN-FUKUBIKUKOTUKBN   TO  TNS-FUKUBIKUKOTUKBN
      *    長時間麻酔管理加算
           MOVE  SPA-GMN-TIMEMASUIKBN      TO  TNS-TIMEMASUIKBN
      *
           .
      *
       4121-KOUMOKUISO-EXT.
           EXIT.
      *
      *****************************************************************
      *     点数マスタ付加情報更新処理
      *****************************************************************
      *4129-TENSUPLUS-UPD-SEC                       SECTION.
      *
      *    INITIALIZE                  ORCGZS02AREA
      *    MOVE    SPA-NAI-YUKOSTYMD   TO  ZS02-NAI-YUKOSTYMD
      *    MOVE    SPA-NAI-YUKOEDYMD   TO  ZS02-NAI-YUKOEDYMD
      *    MOVE    WRK-YUKOEDYMD       TO  ZS02-YUKOEDYMD
      *    採血料区分
      *    INITIALIZE                  TENSUPLUS-REC
      *    MOVE    SPA-GMN-SAIKETUKBN  TO  TNSP-SAIKETUKBN
      *    MOVE    SPA-GMN-ZENKNSKBN   TO  TNSP-ZENKNSKBN
      *    MOVE    SPA-GMN-INPCHKKBN   TO  TNSP-INPCHKKBN
      *    MOVE    SPA-GMN-PLUS-SANTEIRRK
      *                                TO  TNSP-SANTEIRRKKBN
      *    MOVE    SPA-GMN-PLUS-JGNCNT
      *                                TO  TNSP-JGNCNT
      *    MOVE    SPA-GMN-PLUS-JGNCNT1D
      *                                TO  TNSP-JGNCNT1D
      *    MOVE    SPA-GMN-PLUS-JGNCNT1W
      *                                TO  TNSP-JGNCNT1W
      *    MOVE    SPA-GMN-PLUS-JGNCNTETCM
      *                                TO  TNSP-JGNCNTETCM
      *    MOVE    SPA-GMN-PLUS-JGNCNTETC
      *                                TO  TNSP-JGNCNTETC
      *    MOVE    SPA-GMN-PLUS-JGNCNTERR
      *                                TO  TNSP-JGNCNTERR
      *    他月数
      *    MOVE    SPA-GMN-PLUS-JGNCNTETCM
      *                                TO  TNSP-JGNCNTETCM
      *    他回数
      *    MOVE    SPA-GMN-PLUS-JGNCNTETC
      *                                TO  TNSP-JGNCNTETC
      *    処方名称
      *    MOVE    SPA-GMN-SHONAME     TO  TNSP-SHONAME
      *                                    WRK-STRINGTBL
      *    MOVE    100                 TO  CNT-STRING
      *    PERFORM 891-STRCOUNT-SEC
      *    MOVE    CNT-STRING          TO  TNSP-SHOYUKOKETA
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
      **** EXIT.
      *
      *R01.10  削除（現在対象なしの為）
      *****************************************************************
      *  老人一般コード変換マスタ更新
      *****************************************************************
      *41212-SRYCDCHG-UPD-SEC          SECTION.
      *
      *    コード削除
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX     >   5  )    OR
      *                       (SPA-Z90-IPNSRYCD (IDX)  =   SPACE )
      *        MOVE    SPA-Z90-IPNSRYCD (IDX)  TO  CHG-IPNSRYCD
      *        MOVE    SPA-Z90-RJNSRYCD (IDX)  TO  CHG-RJNSRYCD
      *
      *        MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
      *        MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
      *        MOVE    "DBDELETE"          TO  MCP-FUNC
      *        MOVE    "tbl_srycdchg"      TO  MCP-TABLE
      *        MOVE    "key"               TO  MCP-PATHNAME
grpsys*        CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                            MCPDATA-REC
      *                                            SPA-AREA
      *    END-PERFORM
      *
      *    レコード追加
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL      (IDX     >   5  )    OR
      *                       (SPA-Z90-SRYCDCHG (IDX)  =   SPACE )  OR
      *                       (SPA-ERRCD           NOT =   SPACE )
      *        INITIALIZE                  SRYCDCHG-REC 
      *        EVALUATE    SPA-GMN-ROUTEKKBN
      *            WHEN    1
      *                MOVE    SPA-Z01-SRYCD           TO  CHG-IPNSRYCD
      *                MOVE    SPA-Z90-SRYCDCHG (IDX)  TO  CHG-RJNSRYCD
      *            WHEN    2
      *                MOVE    SPA-Z90-SRYCDCHG (IDX)  TO  CHG-IPNSRYCD
      *                MOVE    SPA-Z01-SRYCD           TO  CHG-RJNSRYCD
      *        END-EVALUATE
      *
      *        IF      CHG-IPNSRYCD        NOT =   SPACE
      *
      *            MOVE    SPA-OPID            TO  CHG-OPID
      *            MOVE    SMCNDATE-YMD        TO  CHG-CREYMD
      *            MOVE    SMCNDATE-YMD        TO  CHG-UPYMD
      *            MOVE    SMCNDATE-HMS        TO  CHG-UPHMS
      *
      *            MOVE    SPA-HOSPNUM         TO  CHG-HOSPNUM
      *            MOVE    SRYCDCHG-REC        TO  MCPDATA-REC
      *            MOVE    "DBINSERT"          TO  MCP-FUNC
      *            MOVE    "tbl_srycdchg"      TO  MCP-TABLE
      *            MOVE    "key"               TO  MCP-PATHNAME
grpsys*            CALL    "ORCDBMAIN"         USING   MCPAREA
      *                                                MCPDATA-REC
      *                                                SPA-AREA
      *            IF      MCP-RC          NOT =   ZERO
      *                MOVE    "9005"              TO  SPA-ERRCD
      *                DISPLAY "Z03 SRYCDCHG INS ERR:"  MCP-RC
      *                        ",KEYU:" CHG-KEY
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *    .    
      *
      *41212-SRYCDCHG-UPD-EXT.
      *    EXIT.
      *
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
      ***     MOVE  SPA-GMN-CUR           TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "Z03    "           TO  MCP-WINDOW.
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
      *
      *     画面ワークエリアにセット
           INITIALIZE                            Z03.
      *
           MOVE  SPA-GMN-SRYCDH                TO  Z03-SRYCDH
           MOVE  SPA-GMN-NAMEH                 TO  Z03-NAMEH
           MOVE  SPA-GMN-YUKOSTYMDH            TO  Z03-YUKOSTYMDH
           MOVE  SPA-GMN-YUKOEDYMDH            TO  Z03-YUKOEDYMDH
      *                                         
           MOVE  SPA-GMN-YUKOSTYMD             TO  Z03-YUKOSTYMD
           MOVE  SPA-GMN-YUKOEDYMD             TO  Z03-YUKOEDYMD
           MOVE  SPA-GMN-SRYKBN-G              TO  Z03-SRYKBN   
           MOVE  SPA-GMN-SRYSYUKBN-G           TO  Z03-SRYSYUKBN
           MOVE  SPA-GMN-DATAKBN-G             TO  Z03-DATAKBN  
           MOVE  SPA-GMN-KANANAME              TO  Z03-KANANAME
           MOVE  SPA-GMN-NAME                  TO  Z03-NAME
           MOVE  SPA-GMN-TEN                   TO  Z03-TEN
           MOVE  SPA-GMN-TENSIKIBETU-G         TO  Z03-TENSIKIBETU
           MOVE  SPA-GMN-OLDTEN                TO  Z03-OLDTEN
           MOVE  SPA-GMN-OLDTENSKB-G           TO  Z03-OLDTENSKB
           MOVE  SPA-GMN-TANICD-G              TO  Z03-TANICD
      *****MOVE  SPA-GMN-TANINAME              TO  Z03-TANINAME
           MOVE  SPA-GMN-HKNTEKKBN-G           TO  Z03-HKNTEKKBN   
           MOVE  SPA-GMN-BYOKANRENKBN-G        TO  Z03-BYOKANRENKBN
           MOVE  SPA-GMN-SDOKNRYO-G            TO  Z03-SDOKNRYO    
           MOVE  SPA-GMN-YKZKNRKBN-G           TO  Z03-YKZKNRKBN   
           MOVE  SPA-GMN-NYUGAITEKKBN-G        TO  Z03-NYUGAITEKKBN
           MOVE  SPA-GMN-ROUTEKKBN-G           TO  Z03-ROUTEKKBN   
           MOVE  SPA-GMN-HOSPSRYKBN-G          TO  Z03-HOSPSRYKBN  
           MOVE  SPA-GMN-OSINKBN-G             TO  Z03-OSINKBN     
           MOVE  SPA-GMN-BYOSYOKBN             TO  Z03-BYOSYOKBN   
           MOVE  SPA-GMN-SANTEIRRKKBN-G        TO  Z03-SANTEIRRKKBN
           MOVE  SPA-GMN-TIMEKSNKBN-G          TO  Z03-TIMEKSNKBN  
           MOVE  SPA-GMN-GAIKANRIKBN-G         TO  Z03-GAIKANRIKBN 
           MOVE  SPA-GMN-TSUSOKUAGE            TO  Z03-TSUSOKUAGE  
           MOVE  SPA-GMN-KGNAGE                TO  Z03-KGNAGE      
           MOVE  SPA-GMN-JGNAGE                TO  Z03-JGNAGE      
      *
           MOVE  SPA-GMN-FORMALNAME            TO  Z03-FORMALNAME
           MOVE  SPA-GMN-SHONAME               TO  Z03-SHONAME
      *****MOVE  SPA-GMN-INPCHKKBN             TO  Z03-INPCHKKBN
      *    採血料区分
           MOVE  SPA-GMN-SAIKETUKBN-G          TO  Z03-SAIKETUKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SAIKETUKBN-MAX
               MOVE    SPA-GMN-SAIKETUCOMB (IDX)
                                          TO  Z03-SAIKETUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SAIKETUKBN-MAX  TO  Z03-SAIKETUKBN-COUNT
      *    前回検査日区分
           MOVE  SPA-GMN-ZENKNSKBN-G   TO  Z03-ZENKNSKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ZENKNSKBN-MAX
               MOVE    SPA-GMN-ZENKNSKBN-COMB (IDX)
                                          TO  Z03-ZENKNSKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-ZENKNSKBN-MAX   TO  Z03-ZENKNSKBN-COUNT
      *    診療区分
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKBN-MAX
               MOVE    SPA-GMN-SRYKBNCOB (IDX)
                                           TO  Z03-SRYKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYKBN-MAX      TO  Z03-SRYKBN-COUNT
      *
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYSYUKBN-MAX
               MOVE    SPA-GMN-SRYSYUKBNCOB (IDX)
                                           TO  Z03-SRYSYUKBN-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-SRYSYUKBN-MAX   TO  Z03-SRYSYUKBN-COUNT
      *    データ区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DATAKBN-MAX
               MOVE    SPA-GMN-DATAKBNCOB (IDX)
                                           TO  Z03-DATAKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-DATAKBN-MAX     TO  Z03-DATAKBN-COUNT
      *    データ規格
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TANICD-MAX
               MOVE    SPA-GMN-TANICDCOB (IDX)
                                            TO  Z03-TANICD-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-TANICD-MAX           TO  Z03-TANICD-COUNT
      *    点数識別
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TENSIKI-MAX
               MOVE    SPA-GMN-TENSIKIBETUCOB (IDX)
                                       TO  Z03-TENSIKIBETU-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-TENSIKI-MAX     TO  Z03-TENSIKIBETU-COUNT
      *    旧点数識別
           MOVE    ZERO                TO  Z03-OLDTENSKB-COUNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OLDTENSKB-MAX
               MOVE    SPA-GMN-OLDTENSKBCOB (IDX)
                                           TO  Z03-OLDTENSKB-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-OLDTENSKB-MAX  TO  Z03-OLDTENSKB-COUNT
      *
      *    保険適用
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HKNTEKKBN-MAX
               MOVE    SPA-GMN-HKNTEKKBNCOB (IDX)
                                           TO  Z03-HKNTEKKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-HKNTEKKBN-MAX   TO  Z03-HKNTEKKBN-COUNT
      *    傷病名
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-BYOKANRENKBN-MAX
               MOVE    SPA-GMN-BYOKANRENKBNCOB (IDX)
                                           TO  Z03-BYOKANRENKBN-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-BYOKANRENKBN-MAX    TO  Z03-BYOKANRENKBN-COUNT
      *    指導管理
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SDOKNRYO-MAX
               IF      SPA-GMN-SDOKNRYOCOB (IDX)   NOT =   SPACE
                   ADD     1               TO  IDX-1
                   MOVE    SPA-GMN-SDOKNRYOCOB (IDX)
                                           TO  Z03-SDOKNRYO-ITEM
                                                         (IDX-1)
               END-IF
           END-PERFORM
           MOVE    SPA-SDOKNRYO-MAX    TO  Z03-SDOKNRYO-COUNT
      *    医薬品関連
           MOVE    ZERO                TO  IDX-1
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-YKZKNRKBN-MAX
               MOVE    SPA-GMN-YKZKNRKBNCOB (IDX)
                                           TO  Z03-YKZKNRKBN-ITEM
                                                         (IDX)
           END-PERFORM
           MOVE    SPA-YKZKNRKBN-MAX   TO  Z03-YKZKNRKBN-COUNT
      *
      *    入外区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-NYUGAITEKKBN-MAX
               MOVE    SPA-GMN-NYUGAITEKKBNCOB (IDX)
                                       TO  Z03-NYUGAITEKKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-NYUGAITEKKBN-MAX    TO  Z03-NYUGAITEKKBN-COUNT
      *    老人適応
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-ROUTEKKBN-MAX
               MOVE    SPA-GMN-ROUTEKKBNCOB (IDX)
                                           TO  Z03-ROUTEKKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-ROUTEKKBN-MAX       TO  Z03-ROUTEKKBN-COUNT
      *
      *    病院適応
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-HOSPSRYKBN-MAX
               MOVE    SPA-GMN-HOSPSRYKBNCOB (IDX)
                                           TO  Z03-HOSPSRYKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-HOSPSRYKBN-MAX  TO  Z03-HOSPSRYKBN-COUNT
      *
      *    医療観察法
           MOVE    SPA-GMN-IRYOKAN-G   TO  Z03-IRYOKAN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-IRYOKAN-MAX
               MOVE    SPA-GMN-IRYOKANCOB (IDX)
                                           TO  Z03-IRYOKAN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-IRYOKAN-MAX     TO  Z03-IRYOKAN-COUNT
      *    往診区分
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-OSINKBN-MAX
               MOVE    SPA-GMN-OSINKBNCOB (IDX)
                                           TO  Z03-OSINKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-OSINKBN-MAX     TO  Z03-OSINKBN-COUNT
      *    算定履歴
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SANTEIRRKKBN-MAX
               MOVE    SPA-GMN-SANTEIRRKKBNCOB (IDX)
                                       TO  Z03-SANTEIRRKKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-SANTEIRRKKBN-MAX    TO  Z03-SANTEIRRKKBN-COUNT
      *    時間外
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-TIMEKSNKBN-MAX
               MOVE    SPA-GMN-TIMEKSNKBNCOB (IDX)
                                           TO  Z03-TIMEKSNKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-TIMEKSNKBN-MAX      TO  Z03-TIMEKSNKBN-COUNT
      *    外来管理加算
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GAIKANRIKBN-MAX
               MOVE    SPA-GMN-GAIKANRIKBNCOB (IDX)
                                       TO  Z03-GAIKANRIKBN-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-GAIKANRIKBN-MAX     TO  Z03-GAIKANRIKBN-COUNT
      *
      *
           MOVE  SPA-GMN-JITUDAY               TO  Z03-JITUDAY        
           MOVE  SPA-GMN-DAYCNT                TO  Z03-DAYCNT         
           MOVE  SPA-GMN-JGNCNT                TO  Z03-JGNCNT         
           MOVE  SPA-GMN-JGNCNT1D              TO  Z03-JGNCNT1D       
           MOVE  SPA-GMN-JGNCNT1W              TO  Z03-JGNCNT1W       
           MOVE  SPA-GMN-JGNCNTETCM            TO  Z03-JGNCNTETCM     
           MOVE  SPA-GMN-JGNCNTETC             TO  Z03-JGNCNTETC      
           MOVE  SPA-GMN-JGNCNTERR             TO  Z03-JGNCNTERR      
      *    ユーザ設定上限回数
           MOVE  SPA-GMN-PLUS-SANTEIRRK-G      TO  Z03-PLUS-SANTEIRRK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-PLUS-SANTEI-MAX
               MOVE    SPA-GMN-PLUS-SANTEICOMB (IDX)
                                       TO  Z03-PLUS-SANTEI-ITEM (IDX)
           END-PERFORM
           MOVE  SPA-PLUS-SANTEI-MAX       TO  Z03-PLUS-SANTEI-COUNT
           MOVE  SPA-GMN-PLUS-JGNCNT           TO  Z03-PLUS-JGNCNT
           MOVE  SPA-GMN-PLUS-JGNCNT1D         TO  Z03-PLUS-JGNCNT1D
           MOVE  SPA-GMN-PLUS-JGNCNT1W         TO  Z03-PLUS-JGNCNT1W
           MOVE  SPA-GMN-PLUS-JGNCNTETCM       TO  Z03-PLUS-JGNCNTETCM
           MOVE  SPA-GMN-PLUS-JGNCNTETC        TO  Z03-PLUS-JGNCNTETC
           MOVE  SPA-GMN-PLUS-JGNCNTERR        TO  Z03-PLUS-JGNCNTERR 
      *
           MOVE  SPA-GMN-KZMCOMPSIKIBETU       TO  Z03-KZMCOMPSIKIBETU
           MOVE  SPA-GMN-KZMKGN                TO  Z03-KZMKGN         
           MOVE  SPA-GMN-KZMJGN                TO  Z03-KZMJGN         
           MOVE  SPA-GMN-KZM                   TO  Z03-KZM            
           MOVE  SPA-GMN-KZMTEN                TO  Z03-KZMTEN         
           MOVE  SPA-GMN-KZMERR                TO  Z03-KZMERR         
           MOVE  SPA-GMN-CHUKSNCD              TO  Z03-CHUKSNCD       
           MOVE  SPA-GMN-CHUKSNTSUBAN          TO  Z03-CHUKSNTSUBAN   
           MOVE  SPA-GMN-AGEKSNKGN(1)          TO  Z03-AGEKSNKGN1
           MOVE  SPA-GMN-AGEKSNJGN(1)          TO  Z03-AGEKSNJGN1
           MOVE  SPA-GMN-AGEKSNSRYCD(1)        TO  Z03-AGEKSNSRYCD1
           MOVE  SPA-GMN-AGEKSNKGN(2)          TO  Z03-AGEKSNKGN2
           MOVE  SPA-GMN-AGEKSNJGN(2)          TO  Z03-AGEKSNJGN2
           MOVE  SPA-GMN-AGEKSNSRYCD(2)        TO  Z03-AGEKSNSRYCD2
           MOVE  SPA-GMN-AGEKSNKGN(3)          TO  Z03-AGEKSNKGN3
           MOVE  SPA-GMN-AGEKSNJGN(3)          TO  Z03-AGEKSNJGN3
           MOVE  SPA-GMN-AGEKSNSRYCD(3)        TO  Z03-AGEKSNSRYCD3
           MOVE  SPA-GMN-AGEKSNKGN(4)          TO  Z03-AGEKSNKGN4
           MOVE  SPA-GMN-AGEKSNJGN(4)          TO  Z03-AGEKSNJGN4
           MOVE  SPA-GMN-AGEKSNSRYCD(4)        TO  Z03-AGEKSNSRYCD4
      *                                          
           MOVE  SPA-GMN-TUSOKUGAIKBN          TO  Z03-TUSOKUGAIKBN    
           MOVE  SPA-GMN-LASERKSN              TO  Z03-LASERKSN        
           MOVE  SPA-GMN-CHPMESUKSN            TO  Z03-CHPMESUKSN      
           MOVE  SPA-GMN-MICMESUKSN            TO  Z03-MICMESUKSN      
           MOVE  SPA-GMN-DONORKBN              TO  Z03-DONORKBN      
           MOVE  SPA-GMN-SEKIZUISOKUTEI        TO  Z03-SEKIZUISOKUTEI  
           MOVE  SPA-GMN-KEIBUJYUTU            TO  Z03-KEIBUJYUTU      
           MOVE  SPA-GMN-AUTOHOUGO             TO  Z03-AUTOHOUGO       
           MOVE  SPA-GMN-AUTOFUNGO             TO  Z03-AUTOFUNGO       
           MOVE  SPA-GMN-CHPGYOKOSOTIKSN       TO  Z03-CHPGYOKOSOTIKSN 
           MOVE  SPA-GMN-SHORTSTAYOPE          TO  Z03-SHORTSTAYOPE    
      *    画像等手術支援加算
           MOVE    SPA-GMN-GAZOOPESUP      TO  Z03-GAZOOPESUP
           MOVE    SPA-GMN-MASUISKBKBN     TO  Z03-MASUISKBKBN
      *    施設基準不適合区分
           MOVE  SPA-GMN-KIJUNTEIGENKBN    TO  Z03-KIJUNTEIGENKBN
      *     基準不適合逓減対象施設基準コード
           MOVE  SPA-GMN-KIJUNTEIGENCD     TO  Z03-KIJUNTEIGENCD    
           MOVE  SPA-GMN-KNSJISKBN             TO  Z03-KNSJISKBN       
           MOVE  SPA-GMN-KNSJISGRPKBN          TO  Z03-KNSJISGRPKBN    
           MOVE  SPA-GMN-KENTAICOMMENT         TO  Z03-KENTAICOMMENT   
           MOVE  SPA-GMN-TEIGENKBN             TO  Z03-TEIGENKBN       
           MOVE  SPA-GMN-HOKATUTEIGENKBN       TO  Z03-HOKATUTEIGENKBN 
           MOVE  SPA-GMN-HOUKNSKBN             TO  Z03-HOUKNSKBN       
           MOVE  SPA-GMN-CHPNAISIKSN           TO  Z03-CHPNAISIKSN     
           MOVE  SPA-GMN-NYUKHNKBN             TO  Z03-NYUKHNKBN
           MOVE  SPA-GMN-NYUKHNKSNKBN          TO  Z03-NYUKHNKSNKBN
           MOVE  SPA-GMN-KANGOKSN              TO  Z03-KANGOKSN 
           MOVE  SPA-GMN-CHIIKIKSN             TO  Z03-CHIIKIKSN       
      *    非侵襲的血行動態モニタリング加算                                      
           MOVE  SPA-GMN-HISINSYUMONKSN        TO  Z03-HISINSYUMONKSN       
           MOVE  SPA-GMN-TOUKETUHOZONKSN       TO  Z03-TOUKETUHOZONKSN       
           MOVE  SPA-GMN-KUBUNBANGOU           TO  Z03-KUBUNBANGOU       
      *    労災算定不可
           MOVE  SPA-GMN-ROSAIKBN              TO  Z03-ROSAIKBN
           MOVE  SPA-GMN-SISIKSN               TO  Z03-SISIKSN
      *H30.4
      *    悪性腫瘍病理組織標本加算
           MOVE  SPA-GMN-AKUSEIBYORIKSN        TO  Z03-AKUSEIBYORIKSN
      *R02.4
      *    創外固定器加算
           MOVE  SPA-GMN-SOGAIKOTEIKSN         TO  Z03-SOGAIKOTEIKSN
      *    超音波切削器加算
           MOVE  SPA-GMN-CHPSESSAKUKSN         TO  Z03-CHPSESSAKUKSN
      *    左心耳閉鎖術併施区分
           MOVE  SPA-GMN-SASINJIHEISIKBN       TO  Z03-SASINJIHEISIKBN
      *                                          
           MOVE  SPA-GMN-SSTKIJUNCD(1)         TO  Z03-SSTKIJUNCD1     
           MOVE  SPA-GMN-SSTKIJUNCD(2)         TO  Z03-SSTKIJUNCD2     
           MOVE  SPA-GMN-SSTKIJUNCD(3)         TO  Z03-SSTKIJUNCD3     
           MOVE  SPA-GMN-SSTKIJUNCD(4)         TO  Z03-SSTKIJUNCD4     
           MOVE  SPA-GMN-SSTKIJUNCD(5)         TO  Z03-SSTKIJUNCD5     
           MOVE  SPA-GMN-SSTKIJUNCD(6)         TO  Z03-SSTKIJUNCD6     
           MOVE  SPA-GMN-SSTKIJUNCD(7)         TO  Z03-SSTKIJUNCD7     
           MOVE  SPA-GMN-SSTKIJUNCD(8)         TO  Z03-SSTKIJUNCD8     
           MOVE  SPA-GMN-SSTKIJUNCD(9)         TO  Z03-SSTKIJUNCD9     
           MOVE  SPA-GMN-SSTKIJUNCD(10)        TO  Z03-SSTKIJUNCD10    
           MOVE  SPA-GMN-GAITENTTLKBN          TO  Z03-GAITENTTLKBN    
           MOVE  SPA-GMN-NYUTENTTLSKB          TO  Z03-NYUTENTTLSKB
           MOVE  SPA-GMN-KOUHYOJYUNNUM         TO  Z03-KOUHYOJYUNNUM   
           MOVE  SPA-GMN-CDKBN-KBN             TO  Z03-CDKBN-KBN       
           MOVE  SPA-GMN-CDKBN-SYO             TO  Z03-CDKBN-SYO       
           MOVE  SPA-GMN-CDKBN-BU              TO  Z03-CDKBN-BU        
           MOVE  SPA-GMN-CDKBN-KBNNUM          TO  Z03-CDKBN-KBNNUM    
           MOVE  SPA-GMN-CDKBN-KBNNUM-EDA      TO  Z03-CDKBN-KBNNUM-EDA
           MOVE  SPA-GMN-CDKBN-KOUBAN          TO  Z03-CDKBN-KOUBAN    
           MOVE  SPA-GMN-KOKUCHI-KBN           TO  Z03-KOKUCHI-KBN     
           MOVE  SPA-GMN-KOKUCHI-SYO           TO  Z03-KOKUCHI-SYO     
           MOVE  SPA-GMN-KOKUCHI-BU            TO  Z03-KOKUCHI-BU      
           MOVE  SPA-GMN-KOKUCHI-KBNNUM        TO  Z03-KOKUCHI-KBNNUM  
           MOVE  SPA-GMN-KOKUCHI-KBNNUM-EDA    TO
                                               Z03-KOKUCHI-KBNNUM-EDA
           MOVE  SPA-GMN-KOKUCHI-KOUBAN        TO  Z03-KOKUCHI-KOUBAN  
           MOVE  SPA-GMN-KOKUJISKBKBN1         TO  Z03-KOKUJISKBKBN1   
           MOVE  SPA-GMN-KOKUJISKBKBN2         TO  Z03-KOKUJISKBKBN2   
      *H26.4
      *    ＤＰＣ区分
           MOVE    SPA-GMN-DPCKBN          TO  Z03-DPCKBN
      *    副鼻腔手術用内視鏡加算
           MOVE    SPA-GMN-FUKUBIKUNAIKBN  TO  Z03-FUKUBIKUNAIKBN
      *    副鼻腔手術用軟骨部組織切除機器加算
           MOVE    SPA-GMN-FUKUBIKUKOTUKBN TO  Z03-FUKUBIKUKOTUKBN
      *    長時間麻酔管理加算
           MOVE    SPA-GMN-TIMEMASUIKBN    TO  Z03-TIMEMASUIKBN
      *H29.1
      *    ユーザー
           IF      SPA-MASTER-CLASS    =   2
               MOVE    "【ユーザー管理】"  TO  Z03-MIDASHI-DATA
               MOVE    "red"               TO  Z03-MIDASHI-STYLE
           END-IF
      *
           PERFORM 5001-MAPCUR-SEC
      *H29.1
      *    ユーザー点数識別（ユーザーマスタ）のみ更新
           IF     (SPA-MASTER-CLASS    =   2 )
               OR (SPA-MENTE-OK        =   1 )
               MOVE    WIDGET-NORMAL           TO  Z03-B12-STATE
               MOVE    WIDGET-NORMAL           TO  Z03-B03-STATE
           ELSE
      ****     MOVE    WIDGET-INSENSITIVE      TO  Z03-B12-STATE
               MOVE    WIDGET-NORMAL           TO  Z03-B12-STATE
               MOVE    WIDGET-INSENSITIVE      TO  Z03-B03-STATE
           END-IF
      *H30.9
      *    新規・改正処理は削除なし
           IF      SPA-Z01-SYORIKBN    =   1
                                       OR  3
               MOVE    WIDGET-INSENSITIVE      TO  Z03-B03-STATE
           END-IF
      *
           MOVE  SPA-GMN-PAGENO                TO  Z03-PAGENO
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
      *R01.10
      *    前回検査日の変更時のみ、「ユーザー」へ
           IF     (FLG-USERIN          =   1     )
            AND   (SPA-ERRCD           =   SPACE )
               MOVE    "B10"               TO  MCP-WIDGET
               GO      TO  5001-MAPCUR-EXT
           END-IF
      *
      *    エラー位置決定
           IF      SPA-GMN-CUR         >   ZERO
               CONTINUE
           ELSE
               IF     (SPA-GMN-CUR         =   ZERO )  AND
                      (WRK-MCP-WIDGET      =   SPACE )
      *            各タグの初期位置
                   SET     IDX-GMN             TO  1
                   SEARCH      TBL-GMN-OCC     VARYING   IDX-GMN
                       AT  END
                           MOVE    ZERO            TO  SPA-GMN-CUR
                       WHEN    SPA-GMN-PAGENO  =   TBL-PAGE  (IDX-GMN)
                           MOVE    IDX-GMN         TO  SPA-GMN-CUR
                   END-SEARCH 
               ELSE
      *            今回入力位置より次のカーソル設定
                   SET     IDX-GMN             TO  1
                   SEARCH      TBL-GMN-OCC     VARYING   IDX-GMN
                       AT  END
                           MOVE    ZERO            TO  SPA-GMN-CUR
                       WHEN    WRK-MCP-WIDGET  =   TBL-WIDGET (IDX-GMN)
                           MOVE    IDX-GMN         TO  SPA-GMN-CUR
                   END-SEARCH
      *            エラー以外
                   IF      SPA-ERRCD           =   SPACE
                     ADD     1                   TO  SPA-GMN-CUR
      *              採血料区分は、検査の時のみカーソル移動とする
                     IF     (SPA-GMN-CUR             <=  TBL-GMN-MAX )
                       AND  (TBL-WIDGET(SPA-GMN-CUR) =   "SAIKETUKBN" )
                       AND  (SPA-GMN-SRYKBN      NOT =   "60"  )
                       ADD     1                   TO  SPA-GMN-CUR
                     END-IF
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-GMN-CUR         >   TBL-GMN-MAX
               MOVE    "B12"               TO  MCP-WIDGET
           ELSE
               SET     IDX-GMN                 TO  SPA-GMN-CUR
               MOVE    TBL-WIDGET (IDX-GMN)    TO  MCP-WIDGET
               MOVE    TBL-PAGE   (IDX-GMN)    TO  SPA-GMN-PAGENO
           END-IF
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示処理
      *****************************************************************
       510-ERRSET-SEC                            SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG
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
               WHEN    "0010"
                   MOVE    "出力名称は全角で入力して下さい"
                                                 TO  SPA-ERRMSG
      *
               WHEN    "0006"
                   MOVE    "選択種別がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "数字のみ入力可能です"
                                                 TO  SPA-ERRMSG
               WHEN    "0011"
                   MOVE    "診療区分が登録されていません"
                                                 TO  SPA-ERRMSG
               WHEN    "0012"
                   MOVE    "診療種別が登録されていません"
                                                 TO  SPA-ERRMSG
               WHEN    "0013"
                   MOVE    "単位がありません"
                                                 TO  SPA-ERRMSG
               WHEN    "0014"
                   MOVE    "採血料区分は検査のみの入力です。"
                                                 TO  SPA-ERRMSG
               WHEN    "0020"
                   MOVE    "老人一般コード変換更新エラー"
                                                 TO  SPA-ERRMSG
               WHEN    "0021"
                   MOVE    "老人一般コード変換はできません"
                                                 TO  SPA-ERRMSG
      *R01.10 
               WHEN    "0022"
                   MOVE    "老人一般コード変換はありません。"
                                                 TO  SPA-ERRMSG
      *
               WHEN    "0025"
                   MOVE    "通則年齢は０、１のみ入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0015"
                   MOVE    "エラー処理は、０、９のみ入力して下さい"
                                                 TO  SPA-ERRMSG
               WHEN    "0016"
                   STRING  "ユーザー設定が有効になっていません。"
                           "有効にして下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0017"
                   STRING  "ユーザー設定の上限回数が設定されていません"
                           "。設定して下さい。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0018"
                   STRING  "上限回数がゼロではありません。"
                           "ユーザ設定の上限回数はゼロにできません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0019"
                   STRING  "システム予約コードはユーザ設定できません。"
                                                 DELIMITED  BY  SIZE
                                                 INTO  SPA-ERRMSG
                   END-STRING
      *
               WHEN    "K100"
                   STRING   "警告！算定履歴作成の設定がありません。"
                            "前回の判定ができません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *H29.1
               WHEN    "0050"
                   STRING   "ユーザー設定はできません。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0051"
                   STRING    "厚生労働省から提供される範囲のコードです"
                             "。有効年月日の変更はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
      *
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
               WHEN    "K001"
                   STRING    "警告！厚生労働省から提供される"
                             "コードの範囲です。"
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING    "警告！ユーザ登録できる範囲のコード"
                             "ではありません。"
                             "新規登録はしないで下さい。"
                                                   DELIMITED BY  SIZE
                                                 INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "K003"
                   STRING   "警告！厚生労働省から提供されたデータです。"
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
           MOVE    SPACE               TO  ZERR
           MOVE    SPA-ERRCD           TO  ZERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  ZERR-ERRMSG
      *
           MOVE    "Z03"               TO  SPA-MOTOPG
           MOVE    "ZERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "ZERR"              TO  MCP-WINDOW
           MOVE    "B01"               TO  MCP-WIDGET
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
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
           END-EVALUATE
      *
           MOVE    SPACE                TO  ZID1
           INITIALIZE                       ZID1
           MOVE    SPA-ZIDCD            TO  ZID1-ID1CODE
           MOVE    WRK-ZIDMSG           TO  ZID1-ID1MSG
           MOVE    "B12"                TO  MCP-WIDGET
      *
           MOVE    "Z03"                TO  SPA-MOTOPG
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
      *    ＤＢ　処理
      *****************************************************************
      *
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
       990-DBLOSEC-SEC                 SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       990-DBLOSEC-EXT.
           EXIT.
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
      *
      *************IF  WRK-LNKYMD(1:7)           NOT NUMERIC
      *                MOVE    WRK-LNKYMD(1:1)   TO  WRK-WGO
      *                MOVE    WRK-LNKYMD(2:2)   TO  WRK-WYY
      *                MOVE    WRK-LNKYMD(5:2)   TO  WRK-WMM
      *                MOVE    WRK-LNKYMD(8:2)   TO  WRK-WDD
      *                INSPECT  WRK-WYMD  REPLACING   ALL " " BY  "0"
      *            ELSE
      *                MOVE    WRK-LNKYMD(1:7)   TO  WRK-WYMD
      *            END-IF
      *            INITIALIZE                    LNK-DAY1-AREA
      *            MOVE  "12"                    TO  LNK-DAY1-IRAI
      *            MOVE  ZERO                    TO  LNK-DAY1-YMD
      *            MOVE  WRK-WYMD                TO  LNK-DAY1-YMD(2:7)
      *            CALL  "ORCSDAY"               USING   STS-AREA-DAY
      *                                          LNK-DAY1-AREA
      *            IF      STS-DAY-RC1           =   ZERO
      *                MOVE    LNK-DAY1-YMD        TO  WRK-SYMD
      *************END-IF
      *----(01.00.01) LINE ADD START --------------------------------
      *            日付画面チェックサブ
                   INITIALIZE                      ORCSGDAYAREA
                   MOVE    WRK-LNKYMD          TO  SGDAY-INDAY
                   CALL    "ORCSGDAY"          USING
                                               ORCSGDAYAREA
                   IF      SGDAY-RC            =   ZERO
                       MOVE    ZERO                TO  FLG-ERR
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
