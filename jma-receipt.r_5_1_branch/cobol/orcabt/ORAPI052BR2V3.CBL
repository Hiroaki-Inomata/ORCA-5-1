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
      * this licence along with this program. If not, stop using this
      * program and contact JMA, 2-28-16 Honkomagome, Bunkyo-ku, Tokyo,
      * 113-8621, Japan.
      ********************************************************************
       IDENTIFICATION          DIVISION.
       PROGRAM-ID.             ORAPI052BR2V3.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : API連携用モジュール
      *  コンポーネント名  : 点数マスタ返却
      *  管理者            : 
      *  作成日付    作業者        記述
      *  17/06/30    NACL−藤原    新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  05.00.01    NACL-藤原    18/09/05  ＣＳＶ対応
      *                                     悪性腫瘍病理組織標本加算の追加
      *                                     抽出条件の追加
      *  05.00.02    NACL-藤原    19/02/05  ＣＳＶデータの最後のデータの
      *                                     編集方法の修正
      *  05.00.03    NACL-藤原    20/03/25  平成３２年４月改正対応
      *                                     創外固定器加算
      *                                     超音波切削器加算
      *                                     左心耳閉鎖術併施区分の追加
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *    ＣＳＶファイル
           SELECT  CSV-FILE        ASSIGN  CSVPARA
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-CSV.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    ＣＳＶファイル
       FD  CSV-FILE.
       01  CSV-REC                 PIC X(3000).
      *
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC             PIC X(200).
      *
       WORKING-STORAGE             SECTION.
      *
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //CSVPARA//.
      *
      *    一時ファイル対応
           COPY    "CPRECEERR.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-CSV             PIC X(02).
           03  STS-RECEERR         PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-OUT             PIC 9(10).
           03  CNT-OUT1            PIC 9(10).
           03  CNT-XMLCOUNT        PIC 9(06).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
      *
           03  IDW2                PIC 9(04).
      *
      *    システム領域
       01  SYS-AREA.
           03  SYS-YMD.
               05  SYS-YY          PIC 9(02).
               05  SYS-MM          PIC 9(02).
               05  SYS-DD          PIC 9(02).
      *
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID      PIC 9(07).
           03  WRK-PARA-SHELLID    PIC X(08).
           03  WRK-PARA-HOSPNUM    PIC X(02).
           03  WRK-PARA-TERMID     PIC X(64).
           03  WRK-PARA-BASE-YMD   PIC X(08).
           03  WRK-PARA-DATA-FORMAT
                                   PIC X(03).
           03  WRK-PARA-EFFECTIVE-MODE
                                   PIC X(05).
           03  WRK-PARA-ERRFILE    PIC X(64).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC X(04).
               05  WRK-SMM         PIC X(02).
               05  WRK-SDD         PIC X(02).
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY      PIC X(04).
               05  WRK-HEN-H1      PIC X(01).
               05  WRK-HEN-MM      PIC X(02).
               05  WRK-HEN-H2      PIC X(01).
               05  WRK-HEN-DD      PIC X(02).
      *
           03  WRK-THMS.
               05  WRK-THH         PIC X(02).
               05  WRK-TMM         PIC X(02).
               05  WRK-TSS         PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH     PIC X(02).
               05  WRK-HEN-T1      PIC X(01).
               05  WRK-HEN-TMM     PIC X(02).
               05  WRK-HEN-T2      PIC X(01).
               05  WRK-HEN-TSS     PIC X(02).
      *
      *     右づめ編集用
           03  WRK-HENKAN              PIC X(20).
           03  WRK-LENGTH              PIC 9(02).
           03  WRK-HENSYU              PIC X(20).
      *    出力データ編集用
           03  WRK-REC                 PIC X(3000).
           03  WRK-REC-LENGTH          PIC 9(04).
           03  WRK-HENSYU-AREA.
               05  WRK-DATA            PIC X(2400).
               05  WRK-NAGASA          PIC 9(04).
               05  WRK-ST              PIC 9(02). 
               05  WRK-SYOSUU          PIC 9(01).
               05  WRK-END             PIC 9(01).
               05  WRK-MOJI-CHK        PIC 9(01).
      *
       01  WRK-ERR-AREA.
           03  WRK-RECEERR             PIC X(200).
           03  WRK-ERRMSG              PIC X(300).
           03  WRK-MCP-RC              PIC 9(9).
           03  WRK-ERR-FILE-STS        PIC X(02).
           03  WRK-ERR-FILE-MSG        PIC X(100).
           03  WRK-ERR-FILE-MSG1       PIC X(17).
           03  WRK-ERR-FLG             PIC 9(01).
      *    エラーファイル名称領域 
           COPY    "CPTEMPFL.INC"  REPLACING  //TEMPFLPARA//
                                   BY         //WRK-ERR-FILE-NM//.
      *
       01  WRK-SGETTEMP-TEMPDIR    PIC X(1024).
      *
      *    FULLNAME内のBASENAME開始位置
       01  WRK-SGETTEMP-ST         PIC 9(05).
      *
      *    点数データ(1301 byte)
       01  TENSULST-REC.
           03  TENSULST-OCC          OCCURS   1000.
      *1-20
               05  TENSULST-SRYCD              PIC X(09).
               05  TENSULST-YUKOSTYMD          PIC X(10).
               05  TENSULST-YUKOEDYMD          PIC X(10).
               05  TENSULST-SRYKBN             PIC X(02).
               05  TENSULST-SRYSYUKBN          PIC X(03).
               05  TENSULST-YUKOKETA           PIC 9(02).
               05  TENSULST-NAME               PIC X(200).
               05  TENSULST-KANAYUKOKETA       PIC 9(02).
               05  TENSULST-KANANAME           PIC X(200).
               05  TENSULST-FORMALYUKOKETA     PIC 9(03).
               05  TENSULST-FORMALNAME         PIC X(400).
               05  TENSULST-TENSIKIBETU        PIC 9(01).
               05  TENSULST-TEN                PIC 999999999.99.
               05  TENSULST-TANICD             PIC X(03).
               05  TENSULST-TANIMOJISU         PIC 9(01).
               05  TENSULST-TANINAME           PIC X(24).
               05  TENSULST-DATAKBN            PIC 9(01).
               05  TENSULST-HKNTEKKBN          PIC 9(01).
               05  TENSULST-NYUGAITEKKBN       PIC 9(01).
               05  TENSULST-ROUTEKKBN          PIC 9(01).
      *21-40
               05  TENSULST-HOSPSRYKBN         PIC 9(01).
               05  TENSULST-OSINKBN            PIC 9(01).
               05  TENSULST-HOUKNSKBN          PIC 9(02).
               05  TENSULST-BYOKANRENKBN       PIC 9(02).
               05  TENSULST-SDOKNRYO           PIC 9(02).
               05  TENSULST-JITUDAY            PIC 9(01).
               05  TENSULST-DAYCNT             PIC 9(01).
               05  TENSULST-YKZKNRKBN          PIC 9(01).
               05  TENSULST-KZMCOMPSIKIBETU    PIC 9(01).
               05  TENSULST-KZMKGN             PIC 9(08).
               05  TENSULST-KZMJGN             PIC 9(08).
               05  TENSULST-KZM                PIC 9(08).
               05  TENSULST-KZMTEN             PIC 9999999.99.
               05  TENSULST-KZMERR             PIC 9(01).
               05  TENSULST-JGNCNT             PIC 9(03).
               05  TENSULST-JGNCNT1D           PIC 9(03).
               05  TENSULST-JGNCNT1W           PIC 9(03).
               05  TENSULST-JGNCNTETCM         PIC 9(02).
               05  TENSULST-JGNCNTETC          PIC 9(03).
               05  TENSULST-JGNCNTERR          PIC 9(01).
      *41-60
               05  TENSULST-CHUKSNCD           PIC X(04).
               05  TENSULST-CHUKSNTUBAN        PIC X(01).
               05  TENSULST-TSUSOKUAGE         PIC 9(01).
               05  TENSULST-KGNAGE             PIC X(02).
               05  TENSULST-JGNAGE             PIC X(02).
               05  TENSULST-TIMEKSNKBN         PIC 9(01).
               05  TENSULST-KIJUNTEIGENKBN     PIC 9(01).
               05  TENSULST-KIJUNTEIGENCD      PIC 9(04).
               05  TENSULST-LASERKBN           PIC 9(01).
               05  TENSULST-CHPMESUKSN         PIC 9(01).
               05  TENSULST-MICMESUKSN         PIC 9(01).
               05  TENSULST-DONORKBN           PIC 9(01).
               05  TENSULST-KNSJISKBN          PIC 9(01).
               05  TENSULST-KNSJISGRPKBN       PIC 9(02).
               05  TENSULST-TEIGENKBN          PIC 9(01).
               05  TENSULST-SEKIZUISOKUTEI     PIC 9(01).
               05  TENSULST-KEIBUJYUTU         PIC 9(01).
               05  TENSULST-AUTOHOUGO          PIC 9(01).
               05  TENSULST-GAIKANRIKBN        PIC 9(01).
               05  TENSULST-TUSOKUGAIKBN       PIC 9(01).
      *61-91
               05  TENSULST-HOUKATUTEIGENKBN   PIC 9(03).
               05  TENSULST-CHPNAISIKSN        PIC 9(01).
               05  TENSULST-AUTOFUNGO          PIC 9(01).
               05  TENSULST-CHIIKIKSN          PIC 9(01).
               05  TENSULST-BYOSYOKBN          PIC 9(01).
               05  TENSULST-CHPGYOKOSOTIKSN    PIC 9(01).
               05  TENSULST-SHORTSTAYOPE       PIC 9(01).
               05  TENSULST-BUIKBN             PIC 9(01).
               05  TENSULST-SANTEIRRKKBN       PIC 9(01).
               05  TENSULST-SSTKIJUNCDTBL      OCCURS 10.
                   07  TENSULST-SSTKIJUNCD         PIC 9(04).
               05  TENSULST-AGEKSNTBL          OCCURS 4.
                   07  TENSULST-AGEKSNKGN          PIC X(02).
                   07  TENSULST-AGEKSNJGN          PIC X(02).
                   07  TENSULST-AGEKSNSRYCD        PIC X(09).
      *92-110
               05  TENSULST-KENTAICOMMENT      PIC 9(01).
               05  TENSULST-NYUKHNKBN          PIC 9(02).
               05  TENSULST-NYUKHNKSNKBN       PIC 9(03).
               05  TENSULST-KANGOKSN           PIC 9(02).
               05  TENSULST-OLDTENSKB          PIC 9(01).
               05  TENSULST-OLDTEN             PIC 999999999.99.
               05  TENSULST-MADOKUKBN          PIC 9(01).
               05  TENSULST-SINKEIHAKAIKBN     PIC 9(01).
               05  TENSULST-SEIBUTUGAKUKBN     PIC 9(01).
               05  TENSULST-ZOEIZAIKBN         PIC 9(01).
               05  TENSULST-CSYYOURYO          PIC 9(05).
               05  TENSULST-YKZKBN             PIC 9(01).
               05  TENSULST-ZAIGATAKBN         PIC 9(01).
               05  TENSULST-KOUHATUKBN         PIC 9(01).
               05  TENSULST-LONGTOYOKBN        PIC 9(01).
               05  TENSULST-MEIUSESKB          PIC 9(01).
               05  TENSULST-TOKUKIZAIAGEKSNKBN PIC 9(01).
               05  TENSULST-SANSOKBN           PIC 9(01).
               05  TENSULST-TOKUKIZAISBT1      PIC 9(01).
      *111-130
               05  TENSULST-TOKUKIZAISBT2      PIC 9(01).
               05  TENSULST-JGNTEN             PIC 9(07).
               05  TENSULST-GAITENTTLKBN       PIC 9(03).
               05  TENSULST-NYUTENTTLKBN       PIC 9(03).
               05  TENSULST-CDKBN-KBN          PIC X(01).
               05  TENSULST-CDKBN-SYO          PIC 9(01).
               05  TENSULST-CDKBN-BU           PIC 9(02).
               05  TENSULST-CDKBN-KBNNUM       PIC 9(03).
               05  TENSULST-CDKBN-KBNNUM-EDA   PIC 9(02).
               05  TENSULST-CDKBN-KOUBAN       PIC 9(02).
               05  TENSULST-KOKUCHI-KBN        PIC X(01).
               05  TENSULST-KOKUCHI-SYO        PIC 9(01).
               05  TENSULST-KOKUCHI-BU         PIC 9(02).
               05  TENSULST-KOKUCHI-KBNNUM     PIC 9(03).
               05  TENSULST-KOKUCHI-KBNNUM-EDA PIC 9(02).
               05  TENSULST-KOKUCHI-KOUBAN     PIC 9(02).
               05  TENSULST-KOKUJISKBKBN1      PIC 9(01).
               05  TENSULST-KOKUJISKBKBN2      PIC 9(01).
               05  TENSULST-NAMECHGKBN         PIC 9(01).
               05  TENSULST-KANANAMECHGKBN     PIC 9(01).
      *131-157
               05  TENSULST-IDOKANREN          PIC X(09).
               05  TENSULST-KOUHYOJYUNNUM      PIC 9(09).
               05  TENSULST-YAKKAKJNCD         PIC X(12).
               05  TENSULST-SYUSAISKB          PIC 9(01).
               05  TENSULST-SYOMEIKANREN       PIC X(09).
               05  TENSULST-CHGYMD             PIC X(10).
               05  TENSULST-HAISIYMD           PIC X(10).
               05  TENSULST-KEIKASOCHIYMD      PIC X(10).
      ******** 05  TENSULST-TERMID             PIC X(16).
      ******** 05  TENSULST-OPID               PIC X(16).
               05  TENSULST-CREYMD             PIC X(10).
               05  TENSULST-UPYMD              PIC X(10).
               05  TENSULST-UPHMS              PIC X(08).
      *********05  TENSULST-HOSPNUM            PIC 9(02).
      *********05  TENSULST-MASTER-CLASS       PIC 9(01).
               05  TENSULST-GAZOOPESUP         PIC 9(01).
               05  TENSULST-IRYOKANSATUKBN     PIC 9(01).
               05  TENSULST-MASUISKBKBN        PIC 9(01).
               05  TENSULST-FUKUBIKUNAIKBN     PIC 9(01).
               05  TENSULST-FUKUBIKUKOTUKBN    PIC 9(01).
               05  TENSULST-TIMEMASUIKBN       PIC 9(01).
               05  TENSULST-DPCKBN             PIC 9(01).
               05  TENSULST-HISINSYUMONITORKSN PIC 9(01).
               05  TENSULST-TOUKETUHOZONKSN    PIC 9(01).
               05  TENSULST-KUBUNBANGOU        PIC X(30).
               05  TENSULST-ROSAIKBN           PIC 9(01).
               05  TENSULST-SISIKSN            PIC 9(01).
               05  TENSULST-AKUSEIBYORIKSN     PIC 9(01).
               05  TENSULST-SOGAIKOTEIKSN      PIC 9(01).
               05  TENSULST-CHPSESSAKUKSN      PIC 9(01).
               05  TENSULST-SASINJIHEISIKBN    PIC 9(01).
      *
       01  TITLE-01.
      *1-20
           03  FILLER                 PIC X(14)    VALUE
               "診療行為コード".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "有効開始年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "有効終了年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "診療行為区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "診療種別区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "漢字有効桁数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "漢字名称".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "カナ有効桁数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "カナ名称".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "正式名称有効桁数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "正式名称".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "点数識別".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(04)    VALUE
               "点数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "単位コード".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "単位文字数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "単位名称".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "データ区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "保険適用区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "入外適用区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "後期高齢者医療適用区分".
      *21-40
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "病院診療所区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "往診区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "包括対象検査".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "傷病名関連区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "指導管理料、医学管理料".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(06)    VALUE
               "実日数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "日数回数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "医薬品関連区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "きざみ値計算識別".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "きざみ値下限値".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "きざみ値上限値".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "きざみ値".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "きざみ点数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "きざみ値上下限エラー処理".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "上限回数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "上限回数１日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "上限回数１週".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "上限回数他月数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "上限回数他".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "上限回数エラー処理".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *41-60
           03  FILLER                 PIC X(12)    VALUE
               "注加算コード".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "注加算通番".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "通則年齢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "上下限年齢下限年齢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "上下限年齢上限年齢".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "時間加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "基準不適合逓減区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "基準不適合逓減対象施設区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "処置乳幼児加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "極低出生体重児加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "入院基本料等減算対象識別".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "ドナー分集計区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "検査等実施判断区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "検査等実施判断グループ区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "逓減対象区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(26)    VALUE
               "脊髄誘発電位測定等加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "頸部郭清術併施加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "自動縫合器加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "外来管理加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(44)    VALUE
               "通則加算対象外区分、通則加算所定点数対象区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *61-91
           03  FILLER                 PIC X(12)    VALUE
               "包括逓減区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "超音波内視鏡加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "自動吻合器加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "地域加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "病床数区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(28)    VALUE
               "超音波凝固切開装置等加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "短期滞在手術".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "部位区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "算定履歴区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード１".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード２".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード３".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード４".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード５".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード６".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード７".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード８".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "施設基準コード９".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "施設基準コード１０".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算下限年齢１".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算上限年齢１".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "年齢加算注加算診療行為コード１".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算下限年齢２".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算上限年齢２".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "年齢加算注加算診療行為コード２".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算下限年齢３".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算上限年齢３".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "年齢加算注加算診療行為コード３".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算下限年齢４".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "年齢加算上限年齢４".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "年齢加算注加算診療行為コード４".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *92-110
           03  FILLER                 PIC X(16)    VALUE
               "検体検査コメント".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "入院基本料区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "入院基本料加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "看護加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "旧点数識別".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(06)    VALUE
               "旧点数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "麻毒区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "神経破壊剤区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "生物学的製剤区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "造影剤区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "注射容量".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "薬剤区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "剤型区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "後発医薬品区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "長期投与区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "名称使用識別".
      *******         「湿布薬区分」「選択式コメント識別」
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "特定器材年齢加算区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "酸素等区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "特定器材種別１".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *111-130
           03  FILLER                 PIC X(24)    VALUE
               "特定器材種別２、上限価格".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "上限点数".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "点数欄集計先識別（外来）".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "点数欄集計先識別（入院）".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "コード表用区分−区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "コード表用区分−章".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "コード表用区分−部".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "コード表用区分−区分番号".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(30)    VALUE
               "コード表用区分−区分番号−枝番".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "コード表用区分−項番".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "告知・通知関連番号−区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "告知・通知関連番号−章".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "告知・通知関連番号−部".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(28)    VALUE
               "告知・通知関連番号−区分番号".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "告知・通知関連番号−区分番号−枝番".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "告知・通知関連番号−項番".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "告示等識別区分（１）".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "告示等識別区分（２）".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "漢字名称変更区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "カナ名称変更区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
      *131-157
           03  FILLER                 PIC X(08)    VALUE
               "異動関連".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "公表順序番号".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "薬価基準コード".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "収載方式等識別".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "商品名等関連".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "変更年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "廃止年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "経過措置年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "作成年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(10)    VALUE
               "更新年月日".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(08)    VALUE
               "更新時間".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "画像等手術支援加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "医療観察法対象区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(12)    VALUE
               "麻酔識別区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(22)    VALUE
               "副鼻腔手術用内視鏡加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(34)    VALUE
               "副鼻腔手術用骨軟部組織切除機器加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(18)    VALUE
               "長時間麻酔管理加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "ＤＰＣ適用区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(32)    VALUE
               "非侵襲的血行動態モニタリング加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "凍結保存同種組織加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "点数表区分番号".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "労災算定不可区分".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "四肢加算区分（労災）".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(24)    VALUE
               "悪性腫瘍病理組織標本加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(14)    VALUE
               "創外固定器加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(16)    VALUE
               "超音波切削器加算".
           03  FILLER                 PIC X(01)    VALUE   ",".
           03  FILLER                 PIC X(20)    VALUE
               "左心耳閉鎖術併施区分".
           03  FILLER                 PIC X(01)    VALUE   X"0D".
      *
       01  WRK-CONS-AREA.
      *    区切り文字
           03  WRK-KUGIRI             PIC X(01)    VALUE   ",".
      *    改行コード
           03  WRK-KAIGYO             PIC X(01)    VALUE   X"0D".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    点数
           COPY    "CPTENSU.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *
      *****************************************************************
      *    サブプロ用領域
      *****************************************************************
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    一時ファイル名取得
           COPY    "CPORCSGETTEMP.INC".
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *
           COPY    "MCPDATA.INC".
           COPY    "CPORCMCP.INC".
      *
           COPY    "COMMON-SPA".
      *
           COPY    "MONBLOB".
           COPY    "XMLDATA.INC".
           COPY    "ORCA-BLOB".
      *
           COPY    "CPORCSAPIFILE.INC".
      *
      *****************************************************************
      *    連絡領域
      *****************************************************************
       LINKAGE                     SECTION.
      *
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      *
      ******************************************************************
      *
       PROCEDURE               DIVISION
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
                   UNTIL       FLG-END     =   1
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  SYS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-AREA
                                       CNT-AREA
      *
           UNSTRING   COMMAND-PARAM    DELIMITED  BY  ","
                                       INTO    LNK-PRTKANRI-RENNUM
                                               LNK-PRTKANRI-TBL-KEY
                                               LNK-PRTKANRI-TBL-GROUP
                                               LNK-PRTKANRI-SHORI-RENNUM
                                               LNK-PRTKANRI-SRYYM
                                               LNK-PRTKANRI-SKYYMD
                                               LNK-PRTKANRI-SHELLID
                                               LNK-PRTKANRI-PRIORITY
                                               LNK-PRTKANRI-TERMID
                                               LNK-PRTKANRI-OPID
                                               LNK-PRTKANRI-PRTNM
                                               WRK-PARA-JOBID
                                               WRK-PARA-SHELLID
                                               WRK-PARA-HOSPNUM
                                               RECEERR
                                               WRK-PARA-BASE-YMD
                                               WRK-PARA-DATA-FORMAT
                                               WRK-PARA-EFFECTIVE-MODE
           END-UNSTRING
      *
           DISPLAY "PARA-BASE-YMD      =" WRK-PARA-BASE-YMD
           DISPLAY "PARA-DATA-FORMAT   =" WRK-PARA-DATA-FORMAT
           DISPLAY "PARA-EFFECTIVE-MODE=" WRK-PARA-EFFECTIVE-MODE
           DISPLAY "LNK-PRTKANRI-SKYYMD=" LNK-PRTKANRI-SKYYMD
      *
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           INITIALIZE                      SGETTEMP-AREA
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES  (1)
           MOVE    "medication_masterlst.csv"
                                       TO  SGETTEMP-BASENAMES  (2)
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SPACE               TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  RECEERR
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  CSVPARA
      *
           PERFORM 100-DBOPEN-SEC
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    "ORAPI052BR2V3"     TO  JOB-PGID
           MOVE    "点数マスタ返却"    TO  JOB-SHELLMSG
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
               OPEN    OUTPUT      CSV-FILE
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *      
           INITIALIZE                      TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    WRK-PARA-BASE-YMD   TO  TNS-UPYMD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key35"             TO  MCP-PATHNAME
               PERFORM 900-TENSU-READ-N-SEC
           ELSE
               INITIALIZE                      TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
      *
      *    点数マスタ編集処理
           PERFORM     UNTIL   FLG-TENSU  =   1
               MOVE    ZERO                TO  CNT-OUT1
      *
               MOVE    SPACE               TO  TENSULST-REC
      *
               PERFORM         UNTIL   FLG-TENSU   =   1
                               OR      CNT-OUT1    >=  1000
                   PERFORM 2001-TENSULST-HENSYU-SEC
               END-PERFORM
      *
               IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
                   PERFORM 2002-TENSULST-CSV-SEC
               ELSE
                   ADD     1               TO  CNT-XMLCOUNT
                   PERFORM 910-XMLEXPORT-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    1                   TO  FLG-END
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタ編集処理
      *****************************************************************
       2001-TENSULST-HENSYU-SEC          SECTION.
      *
           IF    ( WRK-PARA-DATA-FORMAT
                                       =   "csv" )
           AND   ( CNT-OUT             =   ZERO  )
               MOVE    TITLE-01            TO  CSV-REC
               WRITE   CSV-REC
           END-IF
      *
           ADD     1                   TO  CNT-OUT1
                                           CNT-OUT
           MOVE    CNT-OUT1            TO  IDX
      *
           INITIALIZE                      TENSULST-OCC (IDX)
      *
           MOVE    TNS-SRYCD           TO  TENSULST-SRYCD          (IDX)
           MOVE    TNS-YUKOSTYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOSTYMD      (IDX)
           MOVE    TNS-YUKOEDYMD       TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-YUKOEDYMD      (IDX)
           MOVE    TNS-SRYKBN          TO  TENSULST-SRYKBN         (IDX)
           MOVE    TNS-SRYSYUKBN       TO  TENSULST-SRYSYUKBN      (IDX)
           MOVE    TNS-YUKOKETA        TO  TENSULST-YUKOKETA       (IDX)
           MOVE    TNS-NAME            TO  TENSULST-NAME           (IDX)
           MOVE    TNS-KANAYUKOKETA    TO  TENSULST-KANAYUKOKETA   (IDX)
           MOVE    TNS-KANANAME        TO  TENSULST-KANANAME       (IDX)
           MOVE    TNS-FORMALYUKOKETA  TO  TENSULST-FORMALYUKOKETA (IDX)
           MOVE    TNS-FORMALNAME      TO  TENSULST-FORMALNAME     (IDX)
           MOVE    TNS-TENSIKIBETU     TO  TENSULST-TENSIKIBETU    (IDX)
           MOVE    TNS-TEN             TO  TENSULST-TEN            (IDX)
           MOVE    TNS-TANICD          TO  TENSULST-TANICD         (IDX)
           MOVE    TNS-TANIMOJISU      TO  TENSULST-TANIMOJISU     (IDX)
           MOVE    TNS-TANINAME        TO  TENSULST-TANINAME       (IDX)
           MOVE    TNS-DATAKBN         TO  TENSULST-DATAKBN        (IDX)
           MOVE    TNS-HKNTEKKBN       TO  TENSULST-HKNTEKKBN      (IDX)
           MOVE    TNS-NYUGAITEKKBN    TO  TENSULST-NYUGAITEKKBN   (IDX)
           MOVE    TNS-ROUTEKKBN       TO  TENSULST-ROUTEKKBN      (IDX)
           MOVE    TNS-HOSPSRYKBN      TO  TENSULST-HOSPSRYKBN     (IDX)
           MOVE    TNS-OSINKBN         TO  TENSULST-OSINKBN        (IDX)
           MOVE    TNS-HOUKNSKBN       TO  TENSULST-HOUKNSKBN      (IDX)
           MOVE    TNS-BYOKANRENKBN    TO  TENSULST-BYOKANRENKBN   (IDX)
           MOVE    TNS-SDOKNRYO        TO  TENSULST-SDOKNRYO       (IDX)
           MOVE    TNS-JITUDAY         TO  TENSULST-JITUDAY        (IDX)
           MOVE    TNS-DAYCNT          TO  TENSULST-DAYCNT         (IDX)
           MOVE    TNS-YKZKNRKBN       TO  TENSULST-YKZKNRKBN      (IDX)
           MOVE    TNS-KZMCOMPSIKIBETU TO  TENSULST-KZMCOMPSIKIBETU(IDX)
           MOVE    TNS-KZMKGN          TO  TENSULST-KZMKGN         (IDX)
           MOVE    TNS-KZMJGN          TO  TENSULST-KZMJGN         (IDX)
           MOVE    TNS-KZM             TO  TENSULST-KZM            (IDX)
           MOVE    TNS-KZMTEN          TO  TENSULST-KZMTEN         (IDX)
           MOVE    TNS-KZMERR          TO  TENSULST-KZMERR         (IDX)
           MOVE    TNS-JGNCNT          TO  TENSULST-JGNCNT         (IDX)
           MOVE    TNS-JGNCNT1D        TO  TENSULST-JGNCNT1D       (IDX)
           MOVE    TNS-JGNCNT1W        TO  TENSULST-JGNCNT1W       (IDX)
           MOVE    TNS-JGNCNTETCM      TO  TENSULST-JGNCNTETCM     (IDX)
           MOVE    TNS-JGNCNTETC       TO  TENSULST-JGNCNTETC      (IDX)
           MOVE    TNS-JGNCNTERR       TO  TENSULST-JGNCNTERR      (IDX)
           MOVE    TNS-CHUKSNCD        TO  TENSULST-CHUKSNCD       (IDX)
           MOVE    TNS-CHUKSNTUBAN     TO  TENSULST-CHUKSNTUBAN    (IDX)
           MOVE    TNS-TSUSOKUAGE      TO  TENSULST-TSUSOKUAGE     (IDX)
           MOVE    TNS-KGNAGE          TO  TENSULST-KGNAGE         (IDX)
           MOVE    TNS-JGNAGE          TO  TENSULST-JGNAGE         (IDX)
           MOVE    TNS-TIMEKSNKBN      TO  TENSULST-TIMEKSNKBN     (IDX)
           MOVE    TNS-KIJUNTEIGENKBN  TO  TENSULST-KIJUNTEIGENKBN (IDX)
           MOVE    TNS-KIJUNTEIGENCD   TO  TENSULST-KIJUNTEIGENCD  (IDX)
           MOVE    TNS-LASERKBN        TO  TENSULST-LASERKBN       (IDX)
           MOVE    TNS-CHPMESUKSN      TO  TENSULST-CHPMESUKSN     (IDX)
           MOVE    TNS-MICMESUKSN      TO  TENSULST-MICMESUKSN     (IDX)
           MOVE    TNS-DONORKBN        TO  TENSULST-DONORKBN       (IDX)
           MOVE    TNS-KNSJISKBN       TO  TENSULST-KNSJISKBN      (IDX)
           MOVE    TNS-KNSJISGRPKBN    TO  TENSULST-KNSJISGRPKBN   (IDX)
           MOVE    TNS-TEIGENKBN       TO  TENSULST-TEIGENKBN      (IDX)
           MOVE    TNS-SEKIZUISOKUTEI  TO  TENSULST-SEKIZUISOKUTEI (IDX)
           MOVE    TNS-KEIBUJYUTU      TO  TENSULST-KEIBUJYUTU     (IDX)
           MOVE    TNS-AUTOHOUGO       TO  TENSULST-AUTOHOUGO      (IDX)
           MOVE    TNS-GAIKANRIKBN     TO  TENSULST-GAIKANRIKBN    (IDX)
           MOVE    TNS-TUSOKUGAIKBN    TO  TENSULST-TUSOKUGAIKBN   (IDX)
           MOVE    TNS-HOUKATUTEIGENKBN
                                       TO  TENSULST-HOUKATUTEIGENKBN
                                                                   (IDX)
           MOVE    TNS-CHPNAISIKSN     TO  TENSULST-CHPNAISIKSN    (IDX)
           MOVE    TNS-AUTOFUNGO       TO  TENSULST-AUTOFUNGO      (IDX)
           MOVE    TNS-CHIIKIKSN       TO  TENSULST-CHIIKIKSN      (IDX)
           MOVE    TNS-BYOSYOKBN       TO  TENSULST-BYOSYOKBN      (IDX)
           MOVE    TNS-CHPGYOKOSOTIKSN TO  TENSULST-CHPGYOKOSOTIKSN(IDX)
           MOVE    TNS-SHORTSTAYOPE    TO  TENSULST-SHORTSTAYOPE   (IDX)
           MOVE    TNS-BUIKBN          TO  TENSULST-BUIKBN         (IDX)
           MOVE    TNS-SANTEIRRKKBN    TO  TENSULST-SANTEIRRKKBN   (IDX)
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       10
               MOVE    TNS-SSTKIJUNCD (IDY)
                                       TO  TENSULST-SSTKIJUNCD (IDX IDY)
           END-PERFORM
           PERFORM VARYING IDY FROM    1   BY  1
                   UNTIL   IDY >       4
               MOVE    TNS-AGEKSNKGN (IDY)
                                       TO  TENSULST-AGEKSNKGN  (IDX IDY)
               MOVE    TNS-AGEKSNJGN (IDY)
                                       TO  TENSULST-AGEKSNJGN  (IDX IDY)
               MOVE    TNS-AGEKSNSRYCD (IDY)
                                       TO  TENSULST-AGEKSNSRYCD(IDX IDY)
           END-PERFORM
           MOVE    TNS-KENTAICOMMENT   TO  TENSULST-KENTAICOMMENT  (IDX)
           MOVE    TNS-NYUKHNKBN       TO  TENSULST-NYUKHNKBN      (IDX)
           MOVE    TNS-NYUKHNKSNKBN    TO  TENSULST-NYUKHNKSNKBN   (IDX)
           MOVE    TNS-KANGOKSN        TO  TENSULST-KANGOKSN       (IDX)
           MOVE    TNS-OLDTENSKB       TO  TENSULST-OLDTENSKB      (IDX)
           MOVE    TNS-OLDTEN          TO  TENSULST-OLDTEN         (IDX)
           MOVE    TNS-MADOKUKBN       TO  TENSULST-MADOKUKBN      (IDX)
           MOVE    TNS-SINKEIHAKAIKBN  TO  TENSULST-SINKEIHAKAIKBN (IDX)
           MOVE    TNS-SEIBUTUGAKUKBN  TO  TENSULST-SEIBUTUGAKUKBN (IDX)
           MOVE    TNS-ZOEIZAIKBN      TO  TENSULST-ZOEIZAIKBN     (IDX)
           MOVE    TNS-CSYYOURYO       TO  TENSULST-CSYYOURYO      (IDX)
           MOVE    TNS-YKZKBN          TO  TENSULST-YKZKBN         (IDX)
           MOVE    TNS-ZAIGATAKBN      TO  TENSULST-ZAIGATAKBN     (IDX)
           MOVE    TNS-KOUHATUKBN      TO  TENSULST-KOUHATUKBN     (IDX)
           MOVE    TNS-LONGTOYOKBN     TO  TENSULST-LONGTOYOKBN    (IDX)
           MOVE    TNS-MEIUSESKB       TO  TENSULST-MEIUSESKB      (IDX)
           MOVE    TNS-TOKUKIZAIAGEKSNKBN
                                       TO  TENSULST-TOKUKIZAIAGEKSNKBN
                                                                   (IDX)
           MOVE    TNS-SANSOKBN        TO  TENSULST-SANSOKBN       (IDX)
           MOVE    TNS-TOKUKIZAISBT1   TO  TENSULST-TOKUKIZAISBT1  (IDX)
           MOVE    TNS-TOKUKIZAISBT2   TO  TENSULST-TOKUKIZAISBT2  (IDX)
           MOVE    TNS-JGNTEN          TO  TENSULST-JGNTEN         (IDX)
           MOVE    TNS-GAITENTTLKBN    TO  TENSULST-GAITENTTLKBN   (IDX)
           MOVE    TNS-NYUTENTTLKBN    TO  TENSULST-NYUTENTTLKBN   (IDX)
           MOVE    TNS-CDKBN-KBN       TO  TENSULST-CDKBN-KBN      (IDX)
           MOVE    TNS-CDKBN-SYO       TO  TENSULST-CDKBN-SYO      (IDX)
           MOVE    TNS-CDKBN-BU        TO  TENSULST-CDKBN-BU       (IDX)
           MOVE    TNS-CDKBN-KBNNUM    TO  TENSULST-CDKBN-KBNNUM   (IDX)
           MOVE    TNS-CDKBN-KBNNUM-EDA
                                       TO  TENSULST-CDKBN-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-CDKBN-KOUBAN    TO  TENSULST-CDKBN-KOUBAN   (IDX)
           MOVE    TNS-KOKUCHI-KBN     TO  TENSULST-KOKUCHI-KBN    (IDX)
           MOVE    TNS-KOKUCHI-SYO     TO  TENSULST-KOKUCHI-SYO    (IDX)
           MOVE    TNS-KOKUCHI-BU      TO  TENSULST-KOKUCHI-BU     (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM  TO  TENSULST-KOKUCHI-KBNNUM (IDX)
           MOVE    TNS-KOKUCHI-KBNNUM-EDA
                                       TO  TENSULST-KOKUCHI-KBNNUM-EDA
                                                                   (IDX)
           MOVE    TNS-KOKUCHI-KOUBAN  TO  TENSULST-KOKUCHI-KOUBAN (IDX)
           MOVE    TNS-KOKUJISKBKBN1   TO  TENSULST-KOKUJISKBKBN1  (IDX)
           MOVE    TNS-KOKUJISKBKBN2   TO  TENSULST-KOKUJISKBKBN2  (IDX)
           MOVE    TNS-NAMECHGKBN      TO  TENSULST-NAMECHGKBN     (IDX)
           MOVE    TNS-KANANAMECHGKBN  TO  TENSULST-KANANAMECHGKBN (IDX)
           MOVE    TNS-IDOKANREN       TO  TENSULST-IDOKANREN      (IDX)
           MOVE    TNS-KOUHYOJYUNNUM   TO  TENSULST-KOUHYOJYUNNUM  (IDX)
           MOVE    TNS-YAKKAKJNCD      TO  TENSULST-YAKKAKJNCD     (IDX)
           MOVE    TNS-SYUSAISKB       TO  TENSULST-SYUSAISKB      (IDX)
           MOVE    TNS-SYOMEIKANREN    TO  TENSULST-SYOMEIKANREN   (IDX)
           MOVE    TNS-CHGYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CHGYMD         (IDX)
           MOVE    TNS-HAISIYMD        TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-HAISIYMD       (IDX)
           MOVE    TNS-KEIKASOCHIYMD   TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-KEIKASOCHIYMD  (IDX)
      ***  MOVE    TNS-TERMID          TO  TENSULST-TERMID         (IDX)
      ***  MOVE    TNS-OPID            TO  TENSULST-OPID           (IDX)
           MOVE    TNS-CREYMD          TO  WRK-SYMD
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-CREYMD         (IDX)
           MOVE    TNS-UPYMD           TO  WRK-SYMD
           MOVE    TNS-UPHMS           TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  TENSULST-UPYMD          (IDX)
           MOVE    WRK-HEN-TIME        TO  TENSULST-UPHMS          (IDX)
      *****MOVE    TNS-MASTER-CLASS    TO  TENSULST-MASTER-CLASS   (IDX)
           MOVE    TNS-GAZOOPESUP      TO  TENSULST-GAZOOPESUP     (IDX)
           MOVE    TNS-IRYOKANSATUKBN  TO  TENSULST-IRYOKANSATUKBN (IDX)
           MOVE    TNS-MASUISKBKBN     TO  TENSULST-MASUISKBKBN    (IDX)
           MOVE    TNS-FUKUBIKUNAIKBN  TO  TENSULST-FUKUBIKUNAIKBN (IDX)
           MOVE    TNS-FUKUBIKUKOTUKBN TO  TENSULST-FUKUBIKUKOTUKBN(IDX)
           MOVE    TNS-TIMEMASUIKBN    TO  TENSULST-TIMEMASUIKBN   (IDX)
           MOVE    TNS-DPCKBN          TO  TENSULST-DPCKBN         (IDX)
           MOVE    TNS-HISINSYUMONITORKSN
                                       TO  TENSULST-HISINSYUMONITORKSN
                                                                   (IDX)
           MOVE    TNS-TOUKETUHOZONKSN TO  TENSULST-TOUKETUHOZONKSN(IDX)
           MOVE    TNS-KUBUNBANGOU     TO  TENSULST-KUBUNBANGOU    (IDX)
           MOVE    TNS-ROSAIKBN        TO  TENSULST-ROSAIKBN       (IDX)
           MOVE    TNS-SISIKSN         TO  TENSULST-SISIKSN        (IDX)
           MOVE    TNS-AKUSEIBYORIKSN  TO  TENSULST-AKUSEIBYORIKSN (IDX)
           MOVE    TNS-SOGAIKOTEIKSN   TO  TENSULST-SOGAIKOTEIKSN  (IDX)
           MOVE    TNS-CHPSESSAKUKSN   TO  TENSULST-CHPSESSAKUKSN  (IDX)
           MOVE    TNS-SASINJIHEISIKBN TO  TENSULST-SASINJIHEISIKBN(IDX)
      *
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key35"             TO  MCP-PATHNAME
           PERFORM 900-TENSU-READ-N-SEC
           .
       2001-TENSULST-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスタＣＳＶ編集処理
      *****************************************************************
       2002-TENSULST-CSV-SEC       SECTION.
      *
           PERFORM VARYING IDX FROM    1   BY  1
                   UNTIL   IDX >       CNT-OUT1
               MOVE    SPACE               TO  WRK-REC
               MOVE    ZERO                TO  WRK-REC-LENGTH
      *
               MOVE    TENSULST-SRYCD      (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOSTYMD  (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOEDYMD  (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SRYKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SRYSYUKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YUKOKETA   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NAME       (IDX)
                                           TO  WRK-DATA
               MOVE    200                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANAYUKOKETA   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANANAME   (IDX)
                                           TO  WRK-DATA
               MOVE    200                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FORMALYUKOKETA (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FORMALNAME (IDX)
                                           TO  WRK-DATA
               MOVE    400                 TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TENSIKIBETU    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TEN        (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANICD     (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANIMOJISU (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TANINAME   (IDX)
                                           TO  WRK-DATA
               MOVE    24                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DATAKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HKNTEKKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUGAITEKKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ROUTEKKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *21-40
               MOVE    TENSULST-HOSPSRYKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OSINKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HOUKNSKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BYOKANRENKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SDOKNRYO   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JITUDAY    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DAYCNT     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YKZKNRKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMCOMPSIKIBETU     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMKGN     (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMJGN     (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZM        (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KZMERR     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT     (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT1D   (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNT1W   (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTETCM (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTETC  (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNCNTERR  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *41-60
               MOVE    TENSULST-CHUKSNCD   (IDX)
                                           TO  WRK-DATA
               MOVE    4                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHUKSNTUBAN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TSUSOKUAGE (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KGNAGE     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNAGE     (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TIMEKSNKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KIJUNTEIGENKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KIJUNTEIGENCD       (IDX)
                                           TO  WRK-DATA
               MOVE    4                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-LASERKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPMESUKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MICMESUKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DONORKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KNSJISKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KNSJISGRPKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TEIGENKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SEKIZUISOKUTEI      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KEIBUJYUTU (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AUTOHOUGO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAIKANRIKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TUSOKUGAIKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *61-91
               MOVE    TENSULST-HOUKATUTEIGENKBN    (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPNAISIKSN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AUTOFUNGO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHIIKIKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BYOSYOKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPGYOKOSOTIKSN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SHORTSTAYOPE        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-BUIKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SANTEIRRKKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL    IDY       >   10
                   MOVE    TENSULST-SSTKIJUNCD     (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    4                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               END-PERFORM
               PERFORM VARYING IDY FROM   1   BY  1
                       UNTIL    IDY       >   4
                   MOVE    TENSULST-AGEKSNKGN      (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
                   MOVE    TENSULST-AGEKSNJGN      (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    2                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
                   MOVE    TENSULST-AGEKSNSRYCD    (IDX IDY)
                                               TO  WRK-DATA
                   MOVE    9                   TO  WRK-NAGASA
                   PERFORM 5004-MOJI-HENSYU-SEC
               END-PERFORM
      *92-110
               MOVE    TENSULST-KENTAICOMMENT   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUKHNKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUKHNKSNKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANGOKSN   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OLDTENSKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-OLDTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MADOKUKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SINKEIHAKAIKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SEIBUTUGAKUKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ZOEIZAIKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CSYYOURYO  (IDX)
                                           TO  WRK-DATA
               MOVE    5                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YKZKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ZAIGATAKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOUHATUKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-LONGTOYOKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MEIUSESKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOKUKIZAIAGEKSNKBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SANSOKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOKUKIZAISBT1       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *111-130
               MOVE    TENSULST-TOKUKIZAISBT2       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-JGNTEN     (IDX)
                                           TO  WRK-DATA
               MOVE    7                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAITENTTLKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NYUTENTTLKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-SYO  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-BU   (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBNNUM        (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KBNNUM-EDA    (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CDKBN-KOUBAN        (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-SYO(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-BU (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBNNUM      (IDX)
                                           TO  WRK-DATA
               MOVE    3                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KBNNUM-EDA  (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUCHI-KOUBAN      (IDX)
                                           TO  WRK-DATA
               MOVE    2                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUJISKBKBN1       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOKUJISKBKBN2       (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-NAMECHGKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KANANAMECHGKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
      *131-157
               MOVE    TENSULST-IDOKANREN  (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KOUHYOJYUNNUM       (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-YAKKAKJNCD (IDX)
                                           TO  WRK-DATA
               MOVE    12                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SYUSAISKB  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SYOMEIKANREN        (IDX)
                                           TO  WRK-DATA
               MOVE    9                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHGYMD     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HAISIYMD   (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KEIKASOCHIYMD       (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CREYMD     (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-UPYMD      (IDX)
                                           TO  WRK-DATA
               MOVE    10                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-UPHMS      (IDX)
                                           TO  WRK-DATA
               MOVE    8                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-GAZOOPESUP (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-IRYOKANSATUKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-MASUISKBKBN(IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FUKUBIKUNAIKBN      (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-FUKUBIKUKOTUKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TIMEMASUIKBN        (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-DPCKBN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-HISINSYUMONITORKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-TOUKETUHOZONKSN     (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-KUBUNBANGOU(IDX)
                                           TO  WRK-DATA
               MOVE    30                  TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-ROSAIKBN   (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SISIKSN    (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-AKUSEIBYORIKSN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SOGAIKOTEIKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-CHPSESSAKUKSN  (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               PERFORM 5004-MOJI-HENSYU-SEC
               MOVE    TENSULST-SASINJIHEISIKBN (IDX)
                                           TO  WRK-DATA
               MOVE    1                   TO  WRK-NAGASA
               MOVE    1                   TO  WRK-END
               PERFORM 5004-MOJI-HENSYU-SEC
      *
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KAIGYO                DELIMITED  BY  SIZE
                       INTO        WRK-REC
               END-STRING
      *
               MOVE    WRK-REC             TO  CSV-REC
               WRITE   CSV-REC
      *
               IF      STS-CSV             =   "00"
                   CONTINUE
               ELSE
                   CALL "coblog" USING   "file write err " CSVPARA
                   MOVE    SPACE               TO  WRK-ERR-AREA
                   INITIALIZE                      WRK-ERR-AREA
                   MOVE    3                   TO  WRK-ERR-FLG
                   MOVE    CSVPARA             TO  WRK-ERR-FILE-NM
                   MOVE    STS-CSV             TO  WRK-ERR-FILE-STS
                   PERFORM 500-FILE-ERR-ABORT-SEC
               END-IF
           END-PERFORM
           .
       2002-TENSULST-CSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    EXPORT 処理
      *****************************************************************
       910-XMLEXPORT-SEC           SECTION.
      *
           INITIALIZE                  XMLDATA-REC
           MOVE   TENSULST-REC     TO  XMLDATA-DATA
           MOVE    1               TO  XMLDATA-MODE
           MOVE    "xml_medication_masterlst"
                                   TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "medication_masterlst"
                                   TO  XMLDATA-RECORDNAME
           MOVE    "XMLWRITE"      TO  MCP-FUNC
           CALL    "MONFUNC"           USING
                                       MCPAREA
                                       XMLDATA-REC
      *
           INITIALIZE                   ORCA-BLOB
           MOVE    "key"           TO  MCP-PATHNAME
           MOVE    "blob"          TO  MCP-TABLE
           MOVE    XMLDATA-OBJECT  TO  ORCA-BLOB-OBJECT
           INITIALIZE                  SGETTEMP-AREA
           STRING  "medication_masterlst"
                                   DELIMITED   BY  SIZE
                   CNT-XMLCOUNT    DELIMITED   BY  SIZE
                   ".xml"          DELIMITED   BY  SIZE
                                   INTO        SGETTEMP-BASENAMES  (1)
           END-STRING
           CALL    "ORCSGETTEMP"       USING   SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  ORCA-BLOB-FILE
           MOVE    "BLOBEXPORT"    TO  MCP-FUNC
           CALL    "MONFUNC"       USING
                                       MCPAREA
                                       ORCA-BLOB
           .
       910-XMLEXPORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
           INITIALIZE                  WRK-HEN-DATE
           MOVE    "-"                 TO  WRK-HEN-H1
           MOVE    "-"                 TO  WRK-HEN-H2
           IF      WRK-SYMD            =   SPACE   OR  "00000000"
               MOVE    "1900"              TO  WRK-HEN-YY
               MOVE    "01"                TO  WRK-HEN-MM
               MOVE    "01"                TO  WRK-HEN-DD
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
           INITIALIZE                  WRK-HEN-TIME
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           IF      WRK-THMS (1:3)      =   SPACE   OR  "000"
               MOVE    "00"                TO  WRK-HEN-THH
                                               WRK-HEN-TMM
                                               WRK-HEN-TSS
           ELSE
               MOVE    WRK-THH             TO  WRK-HEN-THH
               MOVE    WRK-TMM             TO  WRK-HEN-TMM
               MOVE    WRK-TSS             TO  WRK-HEN-TSS
           END-IF
           .
       801-DAYHEN01-EXT.
           EXIT.
      *****************************************************************
      *    日付変換編集処理
      *****************************************************************
       802-DAYHEN02-SEC                SECTION.
      *
           INITIALIZE                  WRK-SYMD
           MOVE    WRK-HEN-YY          TO  WRK-SYY
           MOVE    WRK-HEN-MM          TO  WRK-SMM
           MOVE    WRK-HEN-DD          TO  WRK-SDD
           IF      WRK-SYMD            =   "99991231"
               MOVE    "99"                TO  WRK-SMM
               MOVE    "99"                TO  WRK-SDD
           END-IF
      *
           INITIALIZE                  WRK-THMS
           MOVE    WRK-HEN-THH         TO  WRK-THH
           MOVE    WRK-HEN-TMM         TO  WRK-TMM
           MOVE    WRK-HEN-TSS         TO  WRK-TSS
           .
       802-DAYHEN02-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF      CNT-OUT         =   ZERO
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
             IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
                   CONTINUE
             ELSE
               INITIALIZE                 ORCSAPIFILEAREA
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-IN-DIRNAME
               MOVE    "medication_masterlst"
                                       TO  SAPIFILE-IN-FILENAME
               MOVE    "xml"           TO  SAPIFILE-IN-FILE-TYPE
               move    "2"             TO  SAPIFILE-IN-KBN
               MOVE    1               TO  SAPIFILE-IN-RENNUM
               MOVE    CNT-XMLCOUNT    TO  SAPIFILE-IN-COUNT
               MOVE    SGETTEMP-TEMPDIR
                                       TO  SAPIFILE-OT-DIRNAME
               MOVE    "medication_masterlst.xml"
                                       TO  SAPIFILE-OT-FILENAME
               CALL    "ORCSAPIFILE"   USING   ORCSAPIFILEAREA
      *
               IF      SAPIFILE-RC     =   0
                   CONTINUE
               ELSE
                   STRING   "ｘｍｌファイル作成エラー RC="
                                           DELIMITED   BY  SIZE
                            SAPIFILE-RC    DELIMITED   BY  SIZE
                                           INTO    WRK-RECEERR
                   END-STRING
                   PERFORM 500-ERR-HENSYU-SEC
               END-IF
             END-IF
           END-IF
      *
           IF      WRK-PARA-DATA-FORMAT
                                       =   "csv"
               CLOSE           CSV-FILE
           END-IF
      *
      *    ステップ管理終了処理
           MOVE    "STE"           TO  SJOBKANRI-MODE
           INITIALIZE                  JOBKANRI-REC
           MOVE    CNT-OUT         TO  JOB-UPDCNT
           PERFORM 900-CALL-ORCSJOB-SEC
      *
           PERFORM 900-DBDISCONNECT-SEC
      *
           DISPLAY "*** ORAPI052BR2V3 OUT:" CNT-OUT
           DISPLAY "*** ORAPI052BR2V3 END ***"
      *
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
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
               PERFORM 900-CALL-ORCSJOB-SEC
           END-IF
      *
           MOVE    1                   TO  FLG-END
      *
           .
       500-ERR-HENSYU-EXT.
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
           STRING  "ORAPI052BR2V3 "        DELIMITED   BY  SIZE
                   WRK-ERR-FILE-MSG1       DELIMITED   BY  SIZE
                   " FILE="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-NM (WRK-SGETTEMP-ST:)
                                           DELIMITED   BY  SPACE
                   "  STS="                DELIMITED   BY  SIZE
                   WRK-ERR-FILE-STS        DELIMITED   BY  SIZE
                   LOW-VALUE               DELIMITED   BY  SIZE
                                           INTO    WRK-ERRMSG
           END-STRING
           CALL    "cobabort"          USING   WRK-ERRMSG
      *
           .
       500-FILE-ERR-ABORT-EXT.
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
               IF      WRK-MOJI-CHK    =   1
      *            後ろの全角空白を削除する
                   INITIALIZE                      ORCSKANACHKAREA
                   MOVE    "1"                 TO  KANACHK-SYORI
                   MOVE    "1"                 TO  KANACHK-SYORIKBN
                   MOVE    WRK-NAGASA          TO  KANACHK-MAX-CNT
                   MOVE    WRK-DATA            TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"       USING   ORCSKANACHKAREA
                   IF    ( KANACHK-RC          =   ZERO )
                   AND   ( KANACHK-MAX         >   ZERO )
                       MOVE    SPACE               TO  WRK-DATA
                       MOVE    KANACHK-OUT-INPUT (1:KANACHK-MAX)
                                                   TO  WRK-DATA
                   END-IF
               END-IF
      *
               PERFORM VARYING IDW2    FROM    WRK-NAGASA  BY  -1
                       UNTIL   IDW2    <       1
                   IF      WRK-DATA (IDW2:1)   NOT =   SPACE
                       IF      WRK-REC-LENGTH      =   ZERO
                           MOVE    WRK-DATA (1:IDW2)
                                               TO  WRK-REC
                       ELSE
                           STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED
                                                             BY  SIZE
                                   WRK-DATA(1:IDW2)          DELIMITED
                                                             BY  SIZE
                                   INTO        WRK-REC
                           END-STRING
                       END-IF
                       ADD     IDW2                TO  WRK-REC-LENGTH
                       MOVE    1                   TO  IDW2
                   END-IF
               END-PERFORM
           END-IF
      *     
           IF      WRK-END         =   ZERO
               STRING  WRK-REC(1:WRK-REC-LENGTH) DELIMITED  BY  SIZE
                       WRK-KUGIRI                DELIMITED  BY  SIZE
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
      *    点数マスター読込
      *****************************************************************
       900-TENSU-READ-N-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO            TO  FLG-TENSU
               MOVE    MCPDATA-REC     TO  TNS-TENSU-REC
               EVALUATE    WRK-PARA-EFFECTIVE-MODE
                   WHEN    "True"
                       IF      LNK-PRTKANRI-SKYYMD >   TNS-YUKOEDYMD
                           GO  TO  900-TENSU-READ-N-SEC
                       END-IF
                   WHEN    "False"
                       IF      LNK-PRTKANRI-SKYYMD <=  TNS-YUKOEDYMD
                           GO  TO  900-TENSU-READ-N-SEC
                       END-IF
               END-EVALUATE
           ELSE
              MOVE    1               TO  FLG-TENSU
           END-IF
           .
       900-TENSU-READ-N-EXT.
           EXIT.
      *
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-DBSELECT-SEC                SECTION.
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
      *****************************************************************
      *    ＤＢ読み込み処理
      *****************************************************************
       920-DBFETCH-SEC                 SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
      *
       920-DBFETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-DBCLOSE-SEC           SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC                SECTION.
      *
           MOVE    "DBOPEN"            TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBSTART"           TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC            SECTION.
      *
           MOVE    "DBCOMMIT"          TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
      *
           MOVE    "DBDISCONNECT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE           TO  MCP-TABLE.
           MOVE    LOW-VALUE           TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"         USING
                                       MCPAREA
                                       MCPDATA-REC
                                       SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
