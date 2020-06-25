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
       PROGRAM-ID.             ORCBG014.
      *****************************************************************
      *  システム名        :   ＯＲＣＡ
      *  サブシステム名    :   月次帳票
      *  コンポーネント名  :   保険請求確認リスト
      *  管理者            : 
      *  作成日付    作業者        記述
      *  05/06/30    NACL-森脇     新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者        日付      内容
      *  00.00.01    NACL-森脇     05/10/05  出力順パラメタ追加
      *                                      ＣＳＶ出力
      *                                      統計ファイルをひとつにまとめる
      *  00.00.02    NACL-森脇     05/10/26  特別療養費追加
      *                                      MONFUNCに変更
      *  03.03.01    NACL-森脇     06/10/20  老人３割対応
      *  03.04.01    NACL-森脇     06/11/20  請求チェック
      *  03.04.02    NACL-森脇     07/01/16  未請求患者分対応
      *  03.05.01    NACL-森脇     07/06/04  グループ診療対応
      *  04.01.01    NACL-森脇     07/12/26  公害対応
      *  04.02.01    NACL-森脇     08/04/15  平成２０年４月改正
      *  04.03.00    NACL-森脇     08/10/02  協会けんぽ対応
      *  04.04.00    NACL-森脇     09/03/10  オンライン返戻対応
      *  04.05.00    NACL-森脇     09/06/09  食事療養費、食事負担金追加
      *  04.05.00    NACL-太田     10/01/29  拡張漢字対応
      *  04.07.00    NACL-森脇     11/10/04  労災、アフターケア対応
      *  04.07.00    NACL-森脇     12/02/07  新モンペ、０点公費記載対応
      *  04.07.01    NACL-森脇     12/03/30  健保組合への直接請求と審査
      *                                      支払機関請求対応
      *  04.07.02    NACL-森脇     13/06/14  在総診区分分類なし追加   
      *  04.08.00    NACL-森脇     14/05/01  平成２６年４月改正
      *  04.08.01    NACL-森脇     14/07/22  一時ファイルディレクトリ設定
      *  04.08.02    NACL-森脇     14/11/07  公費と医保の併用（単独入力分）追加←中止
      *  04.08.03    NACL-森脇     16/12/06  自賠責追加
      *****************************************************************
      *
       ENVIRONMENT             DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    統計用ファイル
           SELECT  TOKEI-FILE      ASSIGN  TOKEIPARA
                                   ORGANIZATION    IS  INDEXED
                                   ACCESS  MODE    IS  DYNAMIC
                                   RECORD  KEY     IS  TOKEI-KEY
                                   FILE    STATUS  IS  STS-TOKEI.
      *    エラーファイル
           SELECT  RECEERR-FILE    ASSIGN  RECEERR
                                   FILE    STATUS  IS  STS-RECEERR.
      *    ＣＳＶファイル
           SELECT  RECECSV-FILE    ASSIGN  RECECSV
                                   ORGANIZATION    IS  LINE
                                                       SEQUENTIAL
                                   FILE    STATUS  IS  STS-RECECSV.
      *
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *KEYの説明-------------------------------------------------------------------------
      *        SKYKBN      請求区分    1:請求済、未請求、2:請求しない、3:未請求患者分
      *        TEISYUTU    提出先区分  1:社保、2:国保、3:特別療養費、4:未請求設定、5:未請求解除、9:公害
      *        HKNJANUM    保険者番号  国保の時だけ使用、社保＆請求しない場合はオールゼロ
      *        SYOKBN      処理区分    1:通常分、2:返戻分、3:オンライン返戻分、4:月遅れ分
      *        HKNJANUM2   保険者番号  広域の時だけ使用、それ以外はオールゼロ
      *        NYUGAIKBN   入外区分    1:外来分、2:入院分
      *        ZAIKBN      在総診区分  1:その他のレセ、2:在総診または在医総を算定したレセ
      *        SRYYMFLG    診療月フラグ
      *        HKNKBN      保険区分    1:医保、2:老人、3:公費
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:７０歳以上一般・低所得、2:７０歳以上７割、3:本人、4:家族、5:３歳
      *                          老人  1:老人９割、2:老人８割
      *                          公費  1:公費と公費の併用、2:公費単独
      *                    国保  一般  1:７０歳以上一般・低所得、2:７０歳以上７割、3:被保険者、4:３歳、
      *                          退職  5:退職本人、6:７０歳以上一般・低所得、7:７０歳以上７割、8:被扶養者、9:三歳
      *                          老人  1:老人９割、2:老人８割
      *        KBN         詳細区分
      *                    社保  医保  001:公費併用合計、002:政、003:船、004:日、005:日特、
      *                                006:共、007:組、008:自、009:退
      *                          老人  001:公費併用合計、002:単独合計
      *                          公費  001〜999
      *                    国保        001
      *        KIGO        公害の記号
      *----(平成２０年４月以降)----
      *        SKYKBN      請求区分    1:請求済、未請求、2:請求しない、3:未請求患者分
      *        TEISYUTU    提出先区分  1:社保、2:国保、3:特別療養費、4:未請求設定、5:未請求解除、6:広域連合、
      *                                7:労災、8:アフターケア、9:公害、10:自賠責
      *        HKNJANUM    保険者番号  国保と広域連合と労災と直接請求の時だけ使用、社保＆請求しない場合はオールゼロ
      *        SYOKBN      処理区分    1:通常分、2:返戻分、3:オンライン返戻分、4:月遅れ分
      *        HKNJANUM2   保険者番号  広域の時と労災の時だけ使用、それ以外はオールゼロ
      *        NYUGAIKBN   入外区分    1:外来分、2:入院分
      *        ZAIKBN      在総診区分  1:その他のレセ、2:在総診または在医総を算定したレセ
      *        SRYYMFLG    診療月フラグ
      *        HKNKBN      保険区分    1:医保、2:老人、3:公費
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:７０歳以上一般・低所得、2:７０歳以上７割、3:本人、4:家族、5:６歳
      *                          公費  1:公費と公費の併用、2:公費単独、3:公費と医保の併用
      *                    国保  一般  1:７０歳以上一般・低所得、2:７０歳以上７割、3:一般、4:６歳
      *                          退職  5:本人、8:被扶養者、9:６歳
      *                    広域連合    1:後期高齢者９割、2:後期高齢者７割
      *        KBN         詳細区分
      *                    社保  医保  001:公費併用合計、002:政・協会、003:船、004:日、005:日特
      *                                006:共、007:組、008:自、009:退
      *                          公費  001〜999
      *                    国保        001
      *        KIGO        公害の記号
      *----------------------------------------------------------------------------------
      *    統計用ファイル
       FD  TOKEI-FILE.
       01  TOKEI-REC. 
           03  TOKEI-KEY.
               05  TOKEI-SKYKBN        PIC X(01).
      *----(04.08.03)--UPD-START---
               05  TOKEI-TEISYUTU      PIC X(02).
      *----(04.08.03)--UPD-END-----
               05  TOKEI-HKNJANUM      PIC X(08).
               05  TOKEI-SYOKBN        PIC X(01).
               05  TOKEI-HKNJANUM2     PIC X(08).
               05  TOKEI-NYUGAIKBN     PIC X(01).
               05  TOKEI-ZAIKBN        PIC X(01).
               05  TOKEI-SRYYMFLG      PIC 9(03).
               05  TOKEI-HKNKBN        PIC X(01).
               05  TOKEI-SYUBETUKBN    PIC X(01).
               05  TOKEI-KBN           PIC X(03).
               05  TOKEI-KIGO          PIC X(80).
               05  TOKEI-JYUNBAN       PIC X(131).
           03  TOKEI-SRYYM             PIC X(06).
           03  TOKEI-PTNUM             PIC X(20).
           03  TOKEI-RENNUM            PIC 9(01).
           03  TOKEI-NAME              PIC X(26).
           03  TOKEI-KANANAME          PIC X(100).
           03  TOKEI-NISSU             PIC 9(06).
           03  TOKEI-TENSU             PIC 9(10).
           03  TOKEI-RYOYOHI           PIC 9(09).
           03  TOKEI-SHOKUJIFTN        PIC 9(09).
      *    エラーファイル
       FD  RECEERR-FILE.
       01  RECEERR-REC                 PIC X(200).
      *    ＣＳＶファイル
       FD  RECECSV-FILE.
       01  RECECSV-REC                 PIC X(400).
      *
       WORKING-STORAGE             SECTION.
      *
      *    統計ファイル
           COPY    "CPCOMMONDAT2.INC"  REPLACING  //RECE01//
                                       BY         //TOKEI//.
      *----(04.08.01)--UPD-START---
      *     03  FILLER                  PIC X(04)   VALUE   ".dat".
      *----(04.08.01)--UPD-END-----
       01  RECECSV.
           03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
           03  RECECSV-HOSPNUM     PIC 9(02).
           03  RECECSV-FILE-ID     PIC X(08).
           03  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *----(04.08.01)--ADD-START---
           COPY    "CPRECEERR.INC".
      *----(04.08.01)--ADD-END-----
      *
           COPY    "HCMSL55.INC".
      *
      *    スパ領域
       01  STS-AREA.
           03  STS-TOKEI               PIC X(02).
           03  STS-RECEERR             PIC X(02).
           03  STS-RECECSV             PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END                 PIC 9(01).
           03  FLG-ERR                 PIC 9(01).
           03  FLG-CSV                 PIC 9(01).
           03  FLG-TOKEI               PIC 9(01).
           03  FLG-PTINF               PIC 9(01).
           03  FLG-RECE10              PIC 9(01).
           03  FLG-HKNNUM              PIC 9(01).
           03  FLG-HKNJAINF            PIC 9(01).
           03  FLG-SYSKANRI            PIC 9(01).
           03  FLG-SYUKA               PIC 9(01).
           03  FLG-PTNUM               PIC 9(01).
           03  FLG-LABOR-SIO           PIC 9(01).
           03  FLG-OK                  PIC 9(01).
           03  FLG-DIRECT              PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                     PIC 9(04).
           03  IDY                     PIC 9(04).
           03  IDZ                     PIC 9(04).
           03  IDZZ                    PIC 9(04).
      *
      *PARAの説明---------------------------------------------------------------------
      *        DENPPRTYM   請求日付
      *        TEISYUTU    提出先区分  0:全部、1:社保、2:国保、3:特別療養費、9:公害
      *        HKNJANUM    保険者番号  国保の時だけ使用
      *        SYOKBN      処理区分    0:全部、1:通常分、2:返戻分、3:月遅れ分、4:請求しない
      *        NYUGAIKBN   入外区分    O:全部、1:入院分、2:外来分
      *        ZAIKBN      在総診区分  0:全部、1:その他のレセ、2:在総診または在医総を算定したレセ
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:老人９割、2:老人８割、3:保本人、4:家族、5:三歳
      *                          老人  6:老人９割、7:老人８割
      *                          公費  8:公費と公費の併用、9:公費単独
      *                    国保  一般 10:老人９割、11:老人８割、12:被保険者、13:３歳
      *                          退職 14:本人、15:老人９割、16:老人８割、17:被扶養者、18:三歳
      *                          老人 19:老人９割、20:老人８割
      *        KBN         詳細区分
      *                    社保  医保  1:公費併用合計、2:政、3:船、4:日、5:日特、
      *                                6:共、7:組、8:自、9:退
      *                          老人  1:公費併用合計、2:単独合計
      *        JYUNBAN     出力順      0:患者カナ氏名順、1:患者番号順、2:点数順
      *----平成２０年４月以降------
      *        DENPPRTYM   請求日付
      *        TEISYUTU    提出先区分  0:全部、1:社保、2:国保、3:特別療養費、4:広域連合、5:労災、6:アフターケア
      *                                9:公害
      *        HKNJANUM    保険者番号  国保と広域連合の時だけ使用
      *        SYOKBN      処理区分    0:全部、1:通常分、2:返戻分、3:月遅れ分、4:請求しない
      *        NYUGAIKBN   入外区分    O:全部、1:入院分、2:外来分
      *        ZAIKBN      在総診区分  0:全部、1:その他のレセ、2:在総診または在医総を算定したレセ、3:分類なし
      *        SYUBETUKBN  種別区分
      *                    社保  医保  1:７０歳以上一般・低所得、2:７０歳以上７割、3:本人、4:家族、5:６歳
      *                          公費  8:公費と公費の併用、9:公費単独
      *                    国保  一般 10:７０歳以上一般・低所得、11:７０歳以上７割、12:一般、13:６歳
      *                          退職 14:本人、17:被扶養者、18:６歳
      *                    広域連合   21:後期高齢者９割、22:後期高齢者７割
      *        KBN         詳細区分
      *                    社保  医保  1:公費併用合計、2:政・協会、3:船、4:日、5:日特、
      *                                6:共、7:組、8:自、9:退
      *        JYUNBAN     出力順      0:患者カナ氏名順、1:患者番号順、2:点数順
      *-------------------------------------------------------------------------------
      *    パラメタ領域
       01  WRK-PARA.
           COPY    "CPORCSPRTLNK.INC".
           03  WRK-PARA-JOBID          PIC 9(07).
           03  WRK-PARA-SHELLID        PIC X(08).
           03  WRK-PARA-HOSPNUM        PIC 9(02).
      *----(04.08.01)--UPD-START---
      *     03  RECEERR                 PIC X(100).
      *----(04.08.01)--UPD-END-----
           03  WRK-PARA-DENPPRTYM      PIC X(06).
           03  WRK-PARA-TEISYUTU       PIC X(01).
           03  WRK-PARA-HKNJANUM       PIC X(08).
           03  WRK-PARA-SYOKBN         PIC X(01).
           03  WRK-PARA-NYUGAIKBN      PIC X(01).
           03  WRK-PARA-ZAIKBN         PIC X(01).
           03  WRK-PARA-SYUBETU-G.
               05  WRK-PARA-SYUBETUKBN PIC X(02).
               05  WRK-PARA-KBN        PIC X(01).
           03  WRK-PARA-JYUNBAN        PIC X(01).
      *
      *    カウント領域
       01  CNT-AREA.
           03  CNT-RECE10              PIC 9(06).
           03  CNT-PAGE                PIC 9(04).
           03  CNT-LINE                PIC 9(03).
           03  CNT-KETA                PIC 9(02).
           03  CNT-KETA2               PIC 9(02).
      *
      *    集計領域  （１）小計、（２）合計、（３）レセプト合計
       01  SUM-AREA.
           03  SUM-GOK-TBL             OCCURS  3.
               05  SUM-GOK-KENSU       PIC 9(06).
               05  SUM-GOK-NISSU       PIC 9(06).
               05  SUM-GOK-TENSU       PIC 9(10).
               05  SUM-GOK-RYOYOHI     PIC 9(09).
               05  SUM-GOK-SHOKUJIFTN  PIC 9(09).
      *----(04.08.03)--UPD-START---
           03  SUM-GOK-TEISYUTU        PIC X(02).
      *----(04.08.03)--UPD-END-----
      *
      *    出力先ファイル名
       01  WRK-TO-DATA-G.
           03  WRK-TO-DATA.
               05  WRK-TO-DATA-HOSPNUM PIC 9(02).
               05  FILLER              PIC X(15)   VALUE
                                                   "seikyu_kakunin_".
               05  WRK-TO-DATA-STRYM   PIC X(06).
               05  FILLER              PIC X(04)   VALUE   ".csv".
      *
      *    ヘッダー情報
       01  CSV-HEAD-01.
           03  FILLER              PIC X(08)   VALUE   "請求年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "診療科コード".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "処理区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "処理区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "提出先区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "提出先名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "保険者名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "入外区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "入外区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "在総診区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(10)   VALUE   "在総診名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "診療年月".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "種別区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "種別区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "詳細区分".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(12)   VALUE   "詳細区分名称".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者番号".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "連番".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "患者氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(08)   VALUE   "カナ氏名".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "日数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(04)   VALUE   "点数".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(16)   VALUE
                                               "食事・生活療養費".
           03  FILLER              PIC X(01)   VALUE   ",".
           03  FILLER              PIC X(20)   VALUE
                                               "食事・生活標準負担額".
      *
      *    一時領域
       01  WRK-AREA.
      *    編集用キー
           03  WRK-KEY.
               05  WRK-SKYKBN          PIC X(01).
      *----(04.08.03)--UPD-START---
               05  WRK-TEISYUTU        PIC X(02).
      *----(04.08.03)--UPD-END-----
               05  WRK-HKNJANUM        PIC X(08).
               05  WRK-SYOKBN          PIC X(01).
               05  WRK-HKNJANUM2       PIC X(08).
               05  WRK-NYUGAIKBN       PIC X(01).
               05  WRK-ZAIKBN          PIC X(01).
               05  WRK-SRYYMFLG        PIC 9(03).
               05  WRK-HKNKBN          PIC X(01).
               05  WRK-SYUBETUKBN      PIC X(01).
               05  WRK-KBN             PIC X(03).
               05  WRK-SRYYM           PIC X(06).
               05  WRK-RENNUM          PIC 9(01).
               05  WRK-KIGO            PIC X(80).
               05  WRK-SYOKAIFLG       PIC X(01).
      *    患者カナ氏名順
           03  WRK-KEY1.
               05  WRK-KEY1-KANANAME   PIC X(100).
               05  WRK-KEY1-PTNUM      PIC X(20).
               05  WRK-KEY1-RENNUM     PIC 9(01).
               05  WRK-KEY1-TENSU      PIC 9(10).
      *    患者番号順
           03  WRK-KEY2.
               05  WRK-KEY2-PTNUM      PIC X(20).
               05  WRK-KEY2-RENNUM     PIC 9(01).
               05  WRK-KEY2-KANANAME   PIC X(100).
               05  WRK-KEY2-TENSU      PIC 9(10).
      *    点数順
           03  WRK-KEY3.
               05  WRK-KEY3-TENSU      PIC 9(10).
               05  WRK-KEY3-KANANAME   PIC X(100).
               05  WRK-KEY3-PTNUM      PIC X(20).
               05  WRK-KEY3-RENNUM     PIC 9(01).
      *
      *    エラー情報
           03  WRK-RECEERR             PIC X(200).
           03  WRK-TOUKEIERR           PIC X(200).
      *    検索パス
           03  WRK-PATH                PIC X(64).
           03  WRK-PATH-TABLE          PIC X(64).
           03  WRK-PATH-PATHNAME       PIC X(64).
           03  WRK-MISYUFLG            PIC X(02).
           03  WRK-TENSU               PIC 9(10).
           03  WRK-TEISYUTUSAKI-KBN    PIC 9(01).
           03  WRK-RYOYOHI             PIC X(09).
           03  WRK-SHOKUJIFTN          PIC X(09).
           03  WRK-REC                 PIC X(20000).
      *
      *    日付編集用
           03  WRK-SYSYMD              PIC X(08).
           03  WRK-SYMD.
               05  WRK-SYY             PIC 9(04).
               05  WRK-SMM             PIC 9(02).
               05  WRK-SDD             PIC 9(02).
           03  WRK-SYMD2.
               05  WRK-SYY2            PIC 9(02).
               05  WRK-SMM2            PIC 9(02).
               05  WRK-SDD2            PIC 9(02).
           03  WRK-HENYMDG             PIC X(22).
           03  WRK-DENPYMD             PIC X(16).
           03  WRK-PRTYMD              PIC X(09).
           03  WRK-PRTTIME             PIC X(08).
           03  WRK-HENTIME.
               05  WRK-HH              PIC X(02).
               05  WRK-FILLER1         PIC X(01).
               05  WRK-MM              PIC X(02).
               05  WRK-FILLER2         PIC X(01).
               05  WRK-SS              PIC X(02).
      *
      *    診療年月編集用
           03  WRK-HENSRYYM-G.
               05  WRK-HENSRYYM.
                   07  WRK-HENSRY-YY   PIC 9(04).
                   07  WRK-HENSRY-MM   PIC 9(02).
               05  WRK-HENDENPYM.
                   07  WRK-HENDENP-YY  PIC 9(04).
                   07  WRK-HENDENP-MM  PIC 9(02).
               05  WRK-HENSYY          PIC S9(04).
               05  WRK-HENSMM          PIC S9(02).
      *
      *    数字編集
           03  WRK-Z4                  PIC ZZZZ.
           03  WRK-Z6                  PIC ZZZZZ9.
           03  WRK-Z8                  PIC ZZZZZZZ9.
           03  WRK-Z9                  PIC ZZZZZZZZ9.
           03  WRK-HENSURYO            PIC X(10).
           03  WRK-SURYO               PIC ZZZZZZZZZ9.
      *
       01  WRK-HEN-YMD-OT.
           03  WRK-HEN-YY-OT       PIC X(04).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-MM-OT       PIC X(02).
           03  FILLER              PIC X(01)   VALUE   "/". 
           03  WRK-HEN-DD-OT       PIC X(02).
      *
      *    印刷編集領域
       01  WRK-PRT-AREA.
      *    編集行
           03  WRK-PRT-LINE            PIC X(158).
      *    編集用レセプト種別行
           03  WRK-PRT-LINE-AREA.
               05  WRK-PRT-GOKEI       PIC X(22).
               05  WRK-PRT-KENSU       PIC 9(06).
               05  WRK-PRT-NISSU       PIC 9(06).
               05  WRK-PRT-TENSU       PIC 9(10).
               05  WRK-PRT-RYOYOHI     PIC 9(09).
               05  WRK-PRT-SHOKUJIFTN  PIC 9(09).
      *----(04.08.03)--UPD-START---
      *    帳票タイトル
           03  WRK-PRT1-AREA.
               05  WRK-PRT1-HOSPNAME   PIC X(50).
               05  WRK-PRT1-FILLER1    PIC X(06).
               05  WRK-PRT1-DENPPRTYM  PIC X(16).
               05  WRK-PRT1-TITLE      PIC X(28).
               05  WRK-PRT1-BUNRUI     PIC X(28).
               05  WRK-PRT1-FILLER2    PIC X(01).
               05  WRK-PRT1-PRTYMD     PIC X(09).
               05  WRK-PRT1-FILLER3    PIC X(02).
               05  WRK-PRT1-PRTTIME    PIC X(08).
               05  WRK-PRT1-FILLER4    PIC X(04).
               05  WRK-PRT1-PAGE       PIC X(04).
               05  WRK-PRT1-FILLER5    PIC X(02).
      *----(04.08.03)--UPD-END-----
      *    保険者タイトル
           03  WRK-PRT2-AREA.
               05  WRK-PRT2-HKNJANUM   PIC X(08).
               05  WRK-PRT2-FILLER     PIC X(02).
               05  WRK-PRT2-HKNJANAME  PIC X(40).
      *    レセプト種別明細
           03  WRK-PRT3-AREA.
               05  WRK-PRT3-FILLER1    PIC X(22).
               05  WRK-PRT3-GOKEI      PIC X(22).
               05  WRK-PRT3-KENSU      PIC X(06).
               05  WRK-PRT3-FILLER2    PIC X(04).
               05  WRK-PRT3-NISSU      PIC X(06).
               05  WRK-PRT3-FILLER3    PIC X(04).
               05  WRK-PRT3-TENSU      PIC X(08).
               05  WRK-PRT3-FILLER4    PIC X(02).
               05  WRK-PRT3-FILLER5    PIC X(10).
               05  WRK-PRT3-FILLER6    PIC X(12).
               05  WRK-PRT3-RYOYOHI    PIC X(09).
               05  WRK-PRT3-FILLER7    PIC X(08).
               05  WRK-PRT3-FILLER8    PIC X(12).
               05  WRK-PRT3-SHOKUJIFTN PIC X(09).
               05  WRK-PRT3-FILLER9    PIC X(02).
      *    患者明細
           03  WRK-PRT4-AREA.
               05  WRK-PRT4-G              OCCURS  2.
                   07  WRK-PRT4-FILLER1    PIC X(10).
                   07  WRK-PRT4-PTNUM      PIC X(20).
                   07  WRK-PRT4-FILLER2    PIC X(02).
                   07  WRK-PRT4-NAME       PIC X(20).
                   07  WRK-PRT4-FILLER5    PIC X(02).
                   07  WRK-PRT4-NISSU      PIC X(06).
                   07  WRK-PRT4-FILLER3    PIC X(04).
                   07  WRK-PRT4-TENSU      PIC X(08).
                   07  WRK-PRT4-FILLER4    PIC X(02).
      *    患者明細２
           03  WRK-PRT5-AREA.
               05  WRK-PRT5-G.
                   07  FILLER          PIC X(74).
               05  WRK-PRT5-FILLER1    PIC X(10).
               05  WRK-PRT5-FILLER2    PIC X(12).
               05  WRK-PRT5-RYOYOHI    PIC X(09).
               05  WRK-PRT5-FILLER3    PIC X(08).
               05  WRK-PRT5-FILLER4    PIC X(12).
               05  WRK-PRT5-SHOKUJIFTN PIC X(09).
               05  WRK-PRT5-FILLER5    PIC X(02).
      *    CSV編集用
      *----(04.08.03)--UPD-START---
       01  CSV-REC.
           03  CSV-SKYYM           PIC X(06).
           03  CSV-SRYKA           PIC X(02).
           03  CSV-SYOKBN          PIC X(01).
           03  CSV-TEISYUTU        PIC X(02).
           03  CSV-HKNJANUM        PIC X(08).
           03  CSV-HKNJANAME       PIC X(200).
           03  CSV-NYUGAIKBN       PIC X(01).
           03  CSV-ZAIKBN          PIC X(01).
           03  CSV-SRYYM           PIC X(06).
           03  CSV-SYUBETUKBN      PIC X(01).
           03  CSV-KBN             PIC X(03).
           03  CSV-PTNUM           PIC X(20).
           03  CSV-RENNUM          PIC 9(01).
           03  CSV-NAME            PIC X(26).
           03  CSV-NISSU           PIC X(6).
           03  CSV-TENSU           PIC X(10).
           03  CSV-RYOYOHI         PIC X(9).
           03  CSV-SHOKUJIFTN      PIC X(9).
           03  CSV-SKYYMHEN        PIC X(07).
           03  CSV-SYOKBNMEI       PIC X(10).
           03  CSV-NYUGAIMEI       PIC X(02).
           03  CSV-TEISYUTUMEI     PIC X(20).
           03  CSV-ZAIKBNMEI       PIC X(14).
           03  CSV-SYUBETUMEI      PIC X(30).
           03  CSV-KBNMEI          PIC X(30).
           03  CSV-SRYYMHEN        PIC X(07).
      *----(04.08.03)--UPD-END-----
      *
           COPY    "CPSHELLTBL.INC".
      *    広域連合番号テーブル
           COPY    "CMKOUIKITBL.INC".
      *
       01  CONST-AREA.
         03  CONST-H2004           PIC X(06)   VALUE   "200804".
         03  CONST-H2010           PIC X(06)   VALUE   "200810".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    医療機関情報情報
           COPY    "CPSK1001.INC".
      *
      *    請求管理
           COPY    "CPRCF010.INC".
      *
      *    患者
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    保険番号
       01  HKNNUM-REC.
           COPY    "CPHKNNUM.INC".
      *
      *    保険者
       01  HKNJAINF-REC.
           COPY    "CPHKNJAINF.INC".
      *
      *    患者番号
       01  PTNUM-REC.
           COPY    "CPPTNUM.INC".
      *
      *    主科
       01  SYUKA-REC.
           COPY    "CPSYUKA.INC".
      *
      *    労働基準監督署マスタ
       01  LABOR-SIO-REC.
           COPY    "CPLABOR-SIO.INC".
      *
      *    ジョブ管理マスタ
       01  JOBKANRI-REC.
           COPY    "CPJOBKANRI.INC".
      *
      *    印刷管理
       01  PRTKANRI-REC.
           COPY    "CPPRTKANRI.INC".
      *
      *    印刷用データ
       01  PRTDATA-REC.
           COPY    "CPPRTDATA.INC".
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *    ジョブ管理ＤＢ制御サブ
           COPY    "CPORCSJOBKANRI.INC".
      *
      *    印刷ＤＢ更新サブ
           COPY    "CPORCSPRT.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    半角チェックサブ
           COPY    "CPORCSKANACHK2.INC".
      *
      *    日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    統計ＣＳＶ制御サブ
           COPY    "CPORCSTOUKEICSV.INC".
      *
      *----(04.08.01)--ADD-START---
      *    一時ファイル名編集
           COPY    "CPORCSGETTEMP.INC".
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢ検索
           COPY    "MCPAREA".
      *    LIKE使用KEY対応
           COPY    "CPORCSADDSIGN.INC".
      *
           COPY    "MCPDATA.INC".
      *
           COPY    "COMMON-SPA".
      *
      ****************************************************************
       LINKAGE                     SECTION.
       01  COMMAND-PARAM.
           02  FILLER              PIC X(256).
      ****************************************************************
       PROCEDURE               DIVISION
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
           PERFORM 120-SEIKYU-SYOKICHK-SEC
      *
           IF      FLG-RECE10      =   ZERO
               MOVE
               "０点公費記載の対象レセプトがあるため処理できません。"
                                           TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           ELSE
               PERFORM 200-MAIN-SEC
           END-IF
      *
           PERFORM 300-END-SEC
      *
           STOP    RUN
           .
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  CNT-AREA
           INITIALIZE                  SUM-AREA
           INITIALIZE                  WRK-PARA
           INITIALIZE                  SPA-AREA
      *----(04.08.01)--ADD-START---
           INITIALIZE                  RECEERR
      *----(04.08.01)--ADD-END-----
      *
      *    ＤＢオープン処理
           PERFORM 100-DBOPEN-SEC
      *
      *    パラメタセット
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
                                               WRK-PARA-DENPPRTYM
                                               WRK-PARA-TEISYUTU
                                               WRK-PARA-HKNJANUM
                                               WRK-PARA-SYOKBN
                                               WRK-PARA-NYUGAIKBN
                                               WRK-PARA-ZAIKBN
                                               WRK-PARA-SYUBETU-G
                                               WRK-PARA-JYUNBAN
           END-UNSTRING
           MOVE    WRK-PARA-HOSPNUM    TO  SPA-HOSPNUM
      *
           CALL    "ORCSENCODING"      USING
                                       MCPAREA
                                       SPA-AREA
      *
      *    ステップ管理開始処理
           MOVE    "STS"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    "ORCBG014"          TO  JOB-PGID
           MOVE    "保険請求確認リスト"
                                       TO  JOB-SHELLMSG
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
      *    ファイル設定
           MOVE    "BG01401"           TO  TOKEIPARA-FILE-ID
           MOVE    LNK-PRTKANRI-TERMID TO  TOKEIPARA-TERMID
           MOVE    SPA-HOSPNUM         TO  TOKEIPARA-HOSPNUM
      *
      *----(04.08.01)--ADD-START---
           INITIALIZE                      SGETTEMP-AREA
           MOVE    TOKEIPARA-BASENAME  TO  SGETTEMP-BASENAMES (1)
           MOVE    RECEERR             TO  SGETTEMP-BASENAMES (2)
           CALL    "ORCSGETTEMP"       USING
                                       SGETTEMP-AREA
           MOVE    SGETTEMP-FULLNAMES (1)
                                       TO  TOKEIPARA-FULLNAME
           MOVE    SGETTEMP-FULLNAMES (2)
                                       TO  RECEERR
      *----(04.08.01)--ADD-END-----
      *
           MOVE    "ORCBG014"          TO  RECECSV-FILE-ID
           MOVE    SPA-HOSPNUM         TO  RECECSV-HOSPNUM
      *
      *    パラメタ編集処理
           PERFORM 110-PARA-HENSYU-SEC
      *
           .
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタ編集処理
      *****************************************************************
       110-PARA-HENSYU-SEC         SECTION.
      *
      *    システム日付セット
           MOVE    LNK-PRTKANRI-SKYYMD TO  WRK-SYSYMD
      *
      *    対象年月セット
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG(1:16)   TO  WRK-DENPYMD
      *
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-TO-DATA-STRYM
           MOVE    SPA-HOSPNUM     TO  WRK-TO-DATA-HOSPNUM
      *
      *    印刷日セット
           PERFORM 5002-HIZUKE-CHK-SEC
      *
           .
       110-PARA-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスター初期チェック
      *****************************************************************
       120-SEIKYU-SYOKICHK-SEC     SECTION.
      *
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  RECE10-REC
           INITIALIZE                      RECE10-REC
           MOVE    SPA-HOSPNUM         TO  RECE10-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYM  TO  RECE10-SKYYM
           MOVE    "%"                 TO  RECE10-NYUGAIKBN
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    "tbl_seikyu"        TO  MCP-TABLE
               MOVE    "key79"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO                TO  FLG-RECE10
               ELSE
                   MOVE    1                   TO  FLG-RECE10
               END-IF
           ELSE
               MOVE    1                   TO  FLG-RECE10
           END-IF
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    "tbl_seikyu"        TO  MCP-TABLE
           MOVE    "key79"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       120-SEIKYU-SYOKICHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       200-MAIN-SEC                SECTION.
      *
      *    直接請求フラグクリア
           MOVE    0               TO  FLG-DIRECT
      *
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    統計用ファイルを開く
           OPEN    I-O                 TOKEI-FILE
      *
      *    請求社保分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               AND   ( WRK-PARA-HKNJANUM       =   SPACE )
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key3"              TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *
      *    請求国保分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key4"              TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *    請求特別療養費分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key41"             TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *    広域連合集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               MOVE    6                   TO  WRK-TEISYUTUSAKI-KBN
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key60"             TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *    労災集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               MOVE    3                   TO  WRK-TEISYUTUSAKI-KBN
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key77"             TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *----(04.08.03)--ADD-START---
      *    自賠責集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "9" )
               MOVE    4                   TO  WRK-TEISYUTUSAKI-KBN
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key77"             TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *----(04.08.03)--ADD-END-----
      *    公害集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "1" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "2" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "3" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "4" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "5" )
               AND   ( WRK-PARA-TEISYUTU   NOT =   "6" )
      *----(04.08.03)--ADD-START---
               AND   ( WRK-PARA-TEISYUTU   NOT =   "8" )
      *----(04.08.03)--ADD-END-----
               MOVE    5                   TO  WRK-TEISYUTUSAKI-KBN
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key60"             TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
           END-IF
      *    請求しない分集計処理
           IF        ( WRK-PARA-SYOKBN     NOT =   "1" )
               AND   ( WRK-PARA-SYOKBN     NOT =   "2" )
               AND   ( WRK-PARA-SYOKBN     NOT =   "3" )
               MOVE    "tbl_seikyu"        TO  WRK-PATH-TABLE
               MOVE    "key5"              TO  WRK-PATH-PATHNAME
               PERFORM 210-SYUKEI-READ-SEC
               IF      WRK-PARA-ZAIKBN     NOT =   "2"
      *            未請求設定分
                   MOVE    "01"            TO  WRK-MISYUFLG
                   PERFORM 210-SYUKA-READ-SEC
      *            未請求解除分
                   MOVE    "02"            TO  WRK-MISYUFLG
                   PERFORM 210-SYUKA-READ-SEC
               END-IF
           END-IF
      *
      *    統計用ファイルを閉じる
           CLOSE                       TOKEI-FILE
      *    統計用ファイルを開く
           OPEN    INPUT               TOKEI-FILE
      *    ＣＳＶファイルを開く
           OPEN    OUTPUT              RECECSV-FILE
      *
      *    印刷処理
           PERFORM 300-PRINT-HENSYU-SEC
      *
      *    統計用ファイルを閉じる
           CLOSE                       RECECSV-FILE
           CLOSE                       TOKEI-FILE
      *    統計用ファイルをクリアする
           OPEN    OUTPUT              TOKEI-FILE
           CLOSE                       TOKEI-FILE
      *    ファイル削除
           MOVE    ZERO            TO  IF-RESULT
           MOVE    TOKEIPARA       TO  IF-FILENAME
           CALL    "orcfiledel"        USING
                                       ORCSFDELAREA
      *
           MOVE    1                   TO  FLG-END
      *
           .
       200-MAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科集計処理
      *****************************************************************
       210-SYUKA-READ-SEC          SECTION.
      *
      *    主科読み込み
           PERFORM 900-SYUKA-SELECT-SEC
      *
           PERFORM UNTIL FLG-SYUKA = 1
      *        集計
               PERFORM 240-PTINF-SYUKA-SEC
      *        主科読み込み
               PERFORM 900-SYUKA-READ-SEC
           END-PERFORM
      *
      *    主科読み込み
           PERFORM 900-SYUKA-CLOSE-SEC
      *
           .
       210-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者別主科集計処理
      *****************************************************************
       240-PTINF-SYUKA-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOKEI
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    患者連番最大９まで
           PERFORM UNTIL ( FLG-TOKEI   =    1 )
                   OR    ( WRK-RENNUM  >=   9 )
               ADD     1               TO  WRK-RENNUM
      *        統計読み込み
               INITIALIZE                  TOKEI-REC
               MOVE    WRK-SKYKBN      TO  TOKEI-SKYKBN
               MOVE    WRK-TEISYUTU    TO  TOKEI-TEISYUTU
               MOVE    WRK-HKNJANUM    TO  TOKEI-HKNJANUM
               MOVE    WRK-SYOKBN      TO  TOKEI-SYOKBN
               MOVE    WRK-HKNJANUM2   TO  TOKEI-HKNJANUM2
               MOVE    WRK-SRYYMFLG    TO  TOKEI-SRYYMFLG
               MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
               MOVE    WRK-ZAIKBN      TO  TOKEI-ZAIKBN
               MOVE    WRK-HKNKBN      TO  TOKEI-HKNKBN
               MOVE    WRK-SYUBETUKBN  TO  TOKEI-SYUBETUKBN
               MOVE    WRK-KBN         TO  TOKEI-KBN
               MOVE    SPACE           TO  TOKEI-KIGO
               EVALUATE    WRK-PARA-JYUNBAN
                   WHEN    "1"
                       MOVE    SPACE           TO  WRK-KEY2
                       MOVE    PTNUM-PTNUM     TO  WRK-KEY2-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY2-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY2-RENNUM
                       MOVE    0               TO  WRK-KEY2-TENSU
                       MOVE    WRK-KEY2        TO  TOKEI-JYUNBAN
                   WHEN    "2"
                       MOVE    SPACE           TO  WRK-KEY3
                       MOVE    PTNUM-PTNUM     TO  WRK-KEY3-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY3-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY3-RENNUM
                       MOVE    0               TO  WRK-KEY3-TENSU
                       MOVE    WRK-KEY3        TO  TOKEI-JYUNBAN
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-KEY1
                       MOVE    PTNUM-PTNUM     TO  WRK-KEY1-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY1-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY1-RENNUM
                       MOVE    0               TO  WRK-KEY1-TENSU
                       MOVE    WRK-KEY1        TO  TOKEI-JYUNBAN
               END-EVALUATE
               PERFORM 900-TOKEI-READ-SEC
           END-PERFORM
      *
      *    患者データ無ければ新規作成
           MOVE    WRK-SRYYM               TO  TOKEI-SRYYM
           MOVE    PTNUM-PTNUM             TO  TOKEI-PTNUM
           MOVE    WRK-RENNUM              TO  TOKEI-RENNUM
           MOVE    PTINF-NAME              TO  TOKEI-NAME
           MOVE    PTINF-KANANAME          TO  TOKEI-KANANAME
           MOVE    0                       TO  TOKEI-NISSU
           MOVE    0                       TO  TOKEI-TENSU
           WRITE   TOKEI-REC
      *
           .
       240-PTINF-SYUKA-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       210-SYUKEI-READ-SEC         SECTION.
      *
      *    請求読み込み
           PERFORM 900-SEIKYU-SELECT-SEC
      *
           PERFORM UNTIL ( FLG-RECE10  =   1 )
                   OR    ( FLG-ERR     =   1 )
               IF      RECE10-SRYYM    <   CONST-H2004
                   PERFORM 210-SYUKEI-H2004-MAE-READ-SEC
               ELSE
                   PERFORM 210-SYUKEI-H2004-ATO-READ-SEC
               END-IF
      *        請求読み込み
               PERFORM 900-SEIKYU-READ-SEC
           END-PERFORM
      *
      *    請求読み込み
           PERFORM 900-SEIKYU-CLOSE-SEC
      *
           .
       210-SYUKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       210-SYUKEI-H2004-MAE-READ-SEC         SECTION.
      *
      *    社保分集計処理
           IF        ( WRK-TEISYUTU    =   "1" )
      *        医保
               PERFORM 220-IHO-SYAHO-SYUKEI-SEC
      *        老人
               PERFORM 220-ROU-SYAHO-SYUKEI-SEC
      *        公費
               PERFORM 220-KOH-SYAHO-SYUKEI-SEC
           END-IF
      *
      *    国保分、特別療養費分集計処理
           IF        ( WRK-TEISYUTU    =   "2" )
               OR    ( WRK-TEISYUTU    =   "3" )
      *        一般・退職
               PERFORM 220-IHO-KOKHO-SYUKEI-SEC
      *        老人
               PERFORM 220-ROU-KOKHO-SYUKEI-SEC
           END-IF
      *    公害集計処理
           IF        ( WRK-TEISYUTU    =   "9" )
               MOVE    "0"                 TO  WRK-HKNKBN
               MOVE    SPACE               TO  WRK-SYUBETUKBN
               MOVE    SPACE               TO  WRK-KBN
               MOVE    1                   TO  IDX
               MOVE    RECE10-FTNMONEY(1)  TO  WRK-TENSU
               MOVE    RECE10-KIGO         TO  WRK-KIGO
               MOVE    RECE10-SHOKUJIFTN(1)    TO  WRK-SHOKUJIFTN
               MOVE    RECE10-SHOKUJIRYOYOHI(1)    TO  WRK-RYOYOHI
               PERFORM 240-PTINF-SYUKEI-SEC
           END-IF
      *
           .
       210-SYUKEI-H2004-MAE-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    集計処理
      *****************************************************************
       210-SYUKEI-H2004-ATO-READ-SEC         SECTION.
      *
      *    社保分集計処理
           IF        ( WRK-TEISYUTU    =   "1" )
      *        医保
               PERFORM 220-IHO-SYAHO-SYUKEI-ATO-SEC
      *        公費
               PERFORM 220-KOH-SYAHO-SYUKEI-ATO-SEC
           END-IF
      *
      *    国保分、特別療養費分集計処理
           IF        ( WRK-TEISYUTU    =   "2" )
               OR    ( WRK-TEISYUTU    =   "3" )
      *        一般・退職
               PERFORM 220-IHO-KOKHO-SYUKEI-ATO-SEC
           END-IF
      *    広域連合集計処理
           IF        ( WRK-TEISYUTU    =   "6" )
               OR    ( WRK-TEISYUTU    =   "3" )
               PERFORM 220-ROU-KOUIKI-SYUKEI-SEC
           END-IF
      *----(04.08.03)--UPD-START---
      *    アフターケア集計処理
           IF        ( WRK-TEISYUTU    =   "8" )
               MOVE    "0"                 TO  WRK-HKNKBN
               MOVE    SPACE               TO  WRK-SYUBETUKBN
               MOVE    SPACE               TO  WRK-KBN
               MOVE    1                   TO  IDX
               MOVE    RECE10-FTNMONEY(1)  TO  WRK-TENSU
               MOVE    SPACE               TO  WRK-KIGO
               MOVE    SPACE               TO  WRK-SHOKUJIFTN
               MOVE    SPACE               TO  WRK-RYOYOHI
               PERFORM 240-PTINF-SYUKEI-SEC
           END-IF
      *    短期、傷病集計処理、自賠責
           IF        ( WRK-TEISYUTU    =   "7" )
               OR    ( WRK-TEISYUTU    =   "10" )
               MOVE    "0"                 TO  WRK-HKNKBN
               MOVE    SPACE               TO  WRK-SYUBETUKBN
               MOVE    SPACE               TO  WRK-KBN
               MOVE    1                   TO  IDX
               MOVE    RECE10-FTNMONEY(1)  TO  WRK-TENSU
               MOVE    SPACE               TO  WRK-KIGO
               MOVE    RECE10-SHOKUJIFTN(1)    TO  WRK-SHOKUJIFTN
               MOVE    RECE10-SHOKUJIRYOYOHI(1)    TO  WRK-RYOYOHI
               PERFORM 240-PTINF-SYUKEI-SEC
           END-IF
      *----(04.08.03)--UPD-END-----
      *    公害集計処理
           IF        ( WRK-TEISYUTU    =   "9" )
               MOVE    "0"                 TO  WRK-HKNKBN
               MOVE    SPACE               TO  WRK-SYUBETUKBN
               MOVE    SPACE               TO  WRK-KBN
               MOVE    1                   TO  IDX
               MOVE    RECE10-FTNMONEY(1)  TO  WRK-TENSU
               MOVE    RECE10-KIGO         TO  WRK-KIGO
               MOVE    RECE10-SHOKUJIFTN(1)    TO  WRK-SHOKUJIFTN
               MOVE    RECE10-SHOKUJIRYOYOHI(1)    TO  WRK-RYOYOHI
               PERFORM 240-PTINF-SYUKEI-SEC
           END-IF
      *
           .
       210-SYUKEI-H2004-ATO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分医療保険分集計処理
      *****************************************************************
       220-IHO-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    医保でない場合は処理抜ける
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM           =   SPACE )
               GO  TO  220-IHO-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
      *    船員保険の補助区分が前期高齢者以外の時
           IF        ( RECE10-HKNNUM       =   "002" )
               AND   (( RECE10-HOJOKBN     =   "1"   )
               OR     ( RECE10-HOJOKBN     =   "2"   )
               OR     ( RECE10-HOJOKBN     =   "3"   )
               OR     ( RECE10-HOJOKBN     =   SPACE ))
      *        三歳以下の時
               IF      RECE10-AGE      <   3
                   MOVE    "5"             TO  WRK-SYUBETUKBN
               ELSE
      *            本人の時
                   IF      RECE10-HONKZKKBN        =   "1"
                       MOVE    "3"             TO  WRK-SYUBETUKBN
                   ELSE
      *            家族の時
                       MOVE    "4"             TO  WRK-SYUBETUKBN
                   END-IF
               END-IF
           ELSE
      *        共済組合の補助区分が前期高齢者以外の時
               IF         ( RECE10-HKNNUM  >=  "031" )
                   AND    ( RECE10-HKNNUM  <=  "034" )
                   AND   (( RECE10-HOJOKBN =   "2" )
                   OR     ( RECE10-HOJOKBN =   SPACE ))
      *            三歳以下の時
                   IF      RECE10-AGE      <   3
                       MOVE    "5"             TO  WRK-SYUBETUKBN
                   ELSE
      *                本人の時
                       IF      RECE10-HONKZKKBN        =   "1"
                           MOVE    "3"             TO  WRK-SYUBETUKBN
                       ELSE
      *                家族の時
                           MOVE    "4"             TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               ELSE
                   IF        ( RECE10-AGE      >=  70 )
                       AND   ( RECE10-AGE      <=  74 )
      *                船員保険の補助区分が前期高齢者の時
                       IF      RECE10-HKNNUM           =   "002"
                           EVALUATE    RECE10-HOJOKBN
                               WHEN    "9"
                               WHEN    "A"
                               WHEN    "B"
                               WHEN    "C"
                                   MOVE    "1"     TO  WRK-SYUBETUKBN
                               WHEN    "8"
                               WHEN    "D"
                               WHEN    "E"
                               WHEN    "F"
                               WHEN    "7"
                               WHEN    "G"
                               WHEN    "H"
                               WHEN    "I"
                                   MOVE    "2"     TO  WRK-SYUBETUKBN
                           END-EVALUATE
                       ELSE
      *                共済組合の補助区分が前期高齢者の時
                           IF        ( RECE10-HKNNUM   >=  "031" )
                               AND   ( RECE10-HKNNUM   <=  "034" )
                               EVALUATE    RECE10-HOJOKBN
                                   WHEN    "9"
                                   WHEN    "B"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    "8"
                                   WHEN    "E"
                                   WHEN    "7"
                                   WHEN    "H"
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           ELSE
                               EVALUATE    RECE10-KYURATE
                                   WHEN    9
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    8
                                   WHEN    7
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           END-IF
                       END-IF
                   ELSE
      *                三歳以下の時
                       IF      RECE10-AGE      <   3
                           MOVE    "5"             TO  WRK-SYUBETUKBN
                       ELSE
      *                    本人の時
                           IF      RECE10-HONKZKKBN    =   "1"
                               MOVE    "3"         TO  WRK-SYUBETUKBN
                           ELSE
      *                    家族の時
                               MOVE    "4"         TO  WRK-SYUBETUKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    詳細区分分類
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
      *        公費と併用
               MOVE    "001"           TO  WRK-KBN
           ELSE
      *        単独
               EVALUATE    RECE10-HKNNUM
      *            政管は協会に変更になる
                   WHEN    "001"
                   WHEN    "009"
                       MOVE    "002"           TO  WRK-KBN
                   WHEN    "002"
                       MOVE    "003"           TO  WRK-KBN
                   WHEN    "003"
                       MOVE    "004"           TO  WRK-KBN
                   WHEN    "004"
                       MOVE    "005"           TO  WRK-KBN
                   WHEN    "031"  THRU  "034"
                       MOVE    "006"           TO  WRK-KBN
                   WHEN    "006"
                       MOVE    "007"           TO  WRK-KBN
                   WHEN    "007"
                       MOVE    "008"           TO  WRK-KBN
                   WHEN    "063"
                   WHEN    "072"  THRU  "075"
                       MOVE    "009"           TO  WRK-KBN
               END-EVALUATE
           END-IF
      *
      *    詳細区分が違う場合は処理抜ける
           IF        ( WRK-PARA-KBN    NOT =   SPACE)
               AND   ( WRK-PARA-KBN    NOT =   "0" )
               IF      WRK-KBN(3:1)    NOT =   WRK-PARA-KBN
                   GO  TO  220-IHO-SYAHO-SYUKEI-EXT
               END-IF
           END-IF
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分医療保険分集計処理
      *****************************************************************
       220-IHO-SYAHO-SYUKEI-ATO-SEC    SECTION.
      *
      *    医保でない場合は処理抜ける
           IF          RECE10-HKNNUM           =   SPACE
               GO  TO  220-IHO-SYAHO-SYUKEI-ATO-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
      *    船員保険の補助区分が前期高齢者以外の時
           IF        ( RECE10-HKNNUM       =   "002" )
               AND   (( RECE10-HOJOKBN     =   "1"   )
               OR     ( RECE10-HOJOKBN     =   "2"   )
               OR     ( RECE10-HOJOKBN     =   "3"   )
               OR     ( RECE10-HOJOKBN     =   SPACE ))
      *        未就学時
               IF      RECE10-FLG(1)       =   1
                   MOVE    "5"             TO  WRK-SYUBETUKBN
               ELSE
      *            本人の時
                   IF      RECE10-HONKZKKBN        =   "1"
                       MOVE    "3"             TO  WRK-SYUBETUKBN
                   ELSE
      *            家族の時
                       MOVE    "4"             TO  WRK-SYUBETUKBN
                   END-IF
               END-IF
           ELSE
      *        共済組合の補助区分が前期高齢者以外の時
               IF         ( RECE10-HKNNUM  >=  "031" )
                   AND    ( RECE10-HKNNUM  <=  "034" )
                   AND   (( RECE10-HOJOKBN =   "2" )
                   OR     ( RECE10-HOJOKBN =   SPACE ))
      *            未就学時
                   IF      RECE10-FLG(1)       =   1
                       MOVE    "5"             TO  WRK-SYUBETUKBN
                   ELSE
      *                本人の時
                       IF      RECE10-HONKZKKBN        =   "1"
                           MOVE    "3"             TO  WRK-SYUBETUKBN
                       ELSE
      *                家族の時
                           MOVE    "4"             TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               ELSE
                   IF        ( RECE10-AGE      >=  70 )
      *                海外在住の高齢者は前期高齢扱いのため
      *                 AND   ( RECE10-AGE      <=  74 )
      *                船員保険の補助区分が前期高齢者の時
                       IF      RECE10-HKNNUM           =   "002"
                           EVALUATE    RECE10-HOJOKBN
                               WHEN    "9"
                               WHEN    "A"
                               WHEN    "B"
                               WHEN    "C"
                                   MOVE    "1"     TO  WRK-SYUBETUKBN
                               WHEN    "8"
                               WHEN    "D"
                               WHEN    "E"
                               WHEN    "F"
      *----(04.08.00)--ADD-START---
                                   IF      RECE10-RECESYUBETU (4:1)
                                                   =   "7" OR  "8"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   ELSE
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                                   END-IF
      *----(04.08.00)--ADD-END-----
                               WHEN    "7"
                               WHEN    "G"
                               WHEN    "H"
                               WHEN    "I"
                                   MOVE    "2"     TO  WRK-SYUBETUKBN
                           END-EVALUATE
                       ELSE
      *                共済組合の補助区分が前期高齢者の時
                           IF        ( RECE10-HKNNUM   >=  "031" )
                               AND   ( RECE10-HKNNUM   <=  "034" )
                               EVALUATE    RECE10-HOJOKBN
                                   WHEN    "9"
                                   WHEN    "B"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    "8"
                                   WHEN    "E"
      *----(04.08.00)--ADD-START---
                                   IF      RECE10-RECESYUBETU (4:1)
                                                   =   "7" OR  "8"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   ELSE
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                                   END-IF
      *----(04.08.00)--ADD-END-----
                                   WHEN    "7"
                                   WHEN    "H"
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           ELSE
                               EVALUATE    RECE10-KYURATE
                                   WHEN    9
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   WHEN    8
      *----(04.08.00)--ADD-START---
                                   IF      RECE10-RECESYUBETU (4:1)
                                                   =   "7" OR  "8"
                                       MOVE    "1" TO  WRK-SYUBETUKBN
                                   ELSE
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                                   END-IF
      *----(04.08.00)--ADD-END-----
                                   WHEN    7
                                       MOVE    "2" TO  WRK-SYUBETUKBN
                               END-EVALUATE
                           END-IF
                       END-IF
                   ELSE
      *                未就学時
                       IF      RECE10-FLG(1)       =   1
                           MOVE    "5"             TO  WRK-SYUBETUKBN
                       ELSE
      *                    本人の時
                           IF      RECE10-HONKZKKBN    =   "1"
                               MOVE    "3"         TO  WRK-SYUBETUKBN
                           ELSE
      *                    家族の時
                               MOVE    "4"         TO  WRK-SYUBETUKBN
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
      *    詳細区分分類
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
      *        公費と併用
               MOVE    "001"           TO  WRK-KBN
           ELSE
      *        単独
               EVALUATE    RECE10-HKNNUM
      *            政管は協会に変更する
                   WHEN    "001"
                   WHEN    "009"
                       MOVE    "002"           TO  WRK-KBN
                   WHEN    "002"
                       MOVE    "003"           TO  WRK-KBN
                   WHEN    "003"
                       MOVE    "004"           TO  WRK-KBN
                   WHEN    "004"
                       MOVE    "005"           TO  WRK-KBN
                   WHEN    "031"  THRU  "034"
                       MOVE    "006"           TO  WRK-KBN
                   WHEN    "006"
                       MOVE    "007"           TO  WRK-KBN
                   WHEN    "007"
                       MOVE    "008"           TO  WRK-KBN
                   WHEN    "063"
                   WHEN    "072"  THRU  "075"
                       MOVE    "009"           TO  WRK-KBN
               END-EVALUATE
           END-IF
      *
      *    詳細区分が違う場合は処理抜ける
           IF        ( WRK-PARA-KBN    NOT =   SPACE)
               AND   ( WRK-PARA-KBN    NOT =   "0" )
               IF      WRK-KBN(3:1)    NOT =   WRK-PARA-KBN
                   GO  TO  220-IHO-SYAHO-SYUKEI-ATO-EXT
               END-IF
           END-IF
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-SYAHO-SYUKEI-ATO-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分老人保健分集計処理
      *****************************************************************
       220-ROU-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    老人でない場合処理抜ける
           IF      RECE10-RJNHKNNUM    =   SPACE
               GO  TO  220-ROU-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               WHEN    8
               WHEN    7
                   MOVE    "2"             TO  WRK-SYUBETUKBN
           END-EVALUATE
      *
      *    詳細区分分類
           IF      RECE10-KOHNUM(1)    NOT =   SPACE
               MOVE    "001"           TO  WRK-KBN
           ELSE
               MOVE    "002"           TO  WRK-KBN
           END-IF
      *
      *    詳細区分が違う場合は処理抜ける
           IF        ( WRK-PARA-KBN    NOT =   SPACE)
               AND   ( WRK-PARA-KBN    NOT =   "0" )
               IF      WRK-KBN(3:1)    NOT =   WRK-PARA-KBN
                   GO  TO  220-ROU-SYAHO-SYUKEI-EXT
               END-IF
           END-IF
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-ROU-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分公費保険分集計処理
      *****************************************************************
       220-KOH-SYAHO-SYUKEI-SEC    SECTION.
      *
      *    公費でない場合処理抜ける
           IF      RECE10-KOHNUM(1)    =   SPACE
               GO  TO  220-KOH-SYAHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "3"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
      *    レセプト種別区分分類
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM       NOT =   SPACE )
               GO  TO  220-KOH-SYAHO-SYUKEI-EXT
           ELSE
               IF      RECE10-KOHNUM(2)    NOT =   SPACE
      *            公費と公費の併用欄
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               ELSE
      *            公費単独欄
                   MOVE    "2"             TO  WRK-SYUBETUKBN
               END-IF
           END-IF
      *
      *    詳細区分に公費番号セット
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
                   OR  RECE10-KOHNUM(IDY)  =   SPACE
      *
      *        特定疾患無しのとき
               IF      RECE10-KOHNUM(IDY)  =   "091"
                   MOVE    "051"           TO  RECE10-KOHNUM(IDY)
               END-IF
      *
               MOVE    RECE10-KOHNUM(IDY)  TO  WRK-KBN
               COMPUTE IDX     =   IDY     +   1
      *
      *        保健集計処理
               PERFORM 230-SYUKEI-CHK-SEC
           END-PERFORM
      *
           .
       220-KOH-SYAHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    社保分公費保険分集計処理
      *****************************************************************
       220-KOH-SYAHO-SYUKEI-ATO-SEC    SECTION.
      *
      *    公費でない場合処理抜ける
           IF      RECE10-KOHNUM(1)    =   SPACE
               GO  TO  220-KOH-SYAHO-SYUKEI-ATO-EXT
           END-IF
      *
           MOVE    "3"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
      *    レセプト種別区分分類
           IF          RECE10-HKNNUM       NOT =   SPACE
      *----(04.08.02)--UPD-START---
      *         IF          RECE10-TOTALTEN(1)  =   RECE10-TOTALTEN(2)
                   GO  TO  220-KOH-SYAHO-SYUKEI-ATO-EXT
      *         ELSE
      *            公費と医保の併用
      *             MOVE    "3"             TO  WRK-SYUBETUKBN
      *         END-IF
      *----(04.08.02)--UPD-END-----
           ELSE
               IF      RECE10-KOHNUM(2)    NOT =   SPACE
      *            公費と公費の併用欄
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               ELSE
      *            公費単独欄
                   MOVE    "2"             TO  WRK-SYUBETUKBN
               END-IF
           END-IF
      *
      *    詳細区分に公費番号セット
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY     >       4
                   OR  RECE10-KOHNUM(IDY)  =   SPACE
      *
      *        特定疾患無しのとき
               IF      RECE10-KOHNUM(IDY)  =   "091"
                   MOVE    "051"           TO  RECE10-KOHNUM(IDY)
               END-IF
      *
               MOVE    RECE10-KOHNUM(IDY)  TO  WRK-KBN
               COMPUTE IDX     =   IDY     +   1
      *
      *        保健集計処理
               PERFORM 230-SYUKEI-CHK-SEC
           END-PERFORM
      *
           .
       220-KOH-SYAHO-SYUKEI-ATO-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保分医療保険分集計処理
      *****************************************************************
       220-IHO-KOKHO-SYUKEI-SEC   SECTION.
      *
      *    一般・退職でない場合は処理抜ける
           IF        ( RECE10-RJNHKNNUM    NOT =   SPACE )
               OR    ( RECE10-HKNNUM-S         =   SPACE )
               GO  TO  220-IHO-KOKHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-HKNNUM-S
               WHEN    "060"
               WHEN    "068"
                   IF        ( RECE10-AGE      >=  70 )
                       AND   ( RECE10-AGE      <=  74 )
                       EVALUATE    RECE10-KYURATE
                           WHEN    9
                               MOVE    "1"         TO  WRK-SYUBETUKBN
                           WHEN    8
                           WHEN    7
                               MOVE    "2"         TO  WRK-SYUBETUKBN
                       END-EVALUATE
                   ELSE
                       IF      RECE10-AGE      <   3
                           MOVE    "4"         TO  WRK-SYUBETUKBN
                       ELSE
                           MOVE    "3"         TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               WHEN    "067"
               WHEN    "069"
                   IF        ( RECE10-AGE      >=  70 )
                       AND   ( RECE10-AGE      <=  74 )
                       EVALUATE    RECE10-KYURATE
                           WHEN    9
                               MOVE    "6"         TO  WRK-SYUBETUKBN
                           WHEN    8
                           WHEN    7
                               MOVE    "7"         TO  WRK-SYUBETUKBN
                       END-EVALUATE
                   ELSE
                       IF      RECE10-AGE      <   3
                           MOVE    "9"         TO  WRK-SYUBETUKBN
                       ELSE
                           IF      RECE10-HONKZKKBN    =   "1"
                               MOVE    "5"         TO  WRK-SYUBETUKBN
                           ELSE
                               MOVE    "8"         TO  WRK-SYUBETUKBN
                           END-IF
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-KOKHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保分医療保険分集計処理
      *****************************************************************
       220-IHO-KOKHO-SYUKEI-ATO-SEC   SECTION.
      *
      *    一般・退職でない場合は処理抜ける
           IF       ( RECE10-HKNNUM-S         =   SPACE )
               OR   ( RECE10-HKNNUM-S         =   "040" )
               GO  TO  220-IHO-KOKHO-SYUKEI-ATO-EXT
           END-IF
      *
           MOVE    "1"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-HKNNUM-S
               WHEN    "060"
               WHEN    "068"
      *            一般前期高齢者
                   IF        ( RECE10-AGE      >=  70 )
      *                海外在住の高齢者は前期高齢扱いのため
      *                 AND   ( RECE10-AGE      <=  74 )
                       EVALUATE    RECE10-KYURATE
                           WHEN    9
                               MOVE    "1"         TO  WRK-SYUBETUKBN
                           WHEN    8
      *----(04.08.00)--ADD-START---
                               IF      RECE10-RECESYUBETU (4:1)
                                               =   "7" OR  "8"
                                   MOVE    "1" TO  WRK-SYUBETUKBN
                               ELSE
                                   MOVE    "2" TO  WRK-SYUBETUKBN
                               END-IF
      *----(04.08.00)--ADD-END-----
                           WHEN    7
                               MOVE    "2"         TO  WRK-SYUBETUKBN
                       END-EVALUATE
                   ELSE
      *                未就学時
                       IF      RECE10-FLG(1)       =   1
                           MOVE    "4"         TO  WRK-SYUBETUKBN
                       ELSE
                           MOVE    "3"         TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
               WHEN    "067"
               WHEN    "069"
      *            未就学時
                   IF      RECE10-FLG(1)       =   1
                       MOVE    "9"         TO  WRK-SYUBETUKBN
                   ELSE
                       IF      RECE10-HONKZKKBN    =   "1"
                           MOVE    "5"         TO  WRK-SYUBETUKBN
                       ELSE
                           MOVE    "8"         TO  WRK-SYUBETUKBN
                       END-IF
                   END-IF
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-IHO-KOKHO-SYUKEI-ATO-EXT.
           EXIT.
      *
      *****************************************************************
      *    国保分老人保健分集計処理
      *****************************************************************
       220-ROU-KOKHO-SYUKEI-SEC    SECTION.
      *
      *    老人でない場合は処理抜ける
           IF      RECE10-RJNHKNNUM    =   SPACE
               GO  TO  220-ROU-KOKHO-SYUKEI-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               WHEN    8
               WHEN    7
                   MOVE    "2"             TO  WRK-SYUBETUKBN
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-ROU-KOKHO-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    広域連合分医療保険分集計処理
      *****************************************************************
       220-ROU-KOUIKI-SYUKEI-SEC   SECTION.
      *
      *    後期高齢者でない場合は処理抜ける
           IF        ( RECE10-HKNNUM-S         =   SPACE )
               OR   (( RECE10-HKNNUM-S     NOT =   "039" )
               AND   ( RECE10-HKNNUM-S     NOT =   "040" ))
               GO  TO  220-ROU-KOUIKI-SYUKEI-EXT
           END-IF
      *
           MOVE    "2"                 TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    "001"               TO  WRK-KBN
           MOVE    1                   TO  IDX
      *
      *    レセプト種別区分分類
           EVALUATE    RECE10-KYURATE
               WHEN    9
                   MOVE    "1"             TO  WRK-SYUBETUKBN
               WHEN    7
                   MOVE    "2"             TO  WRK-SYUBETUKBN
           END-EVALUATE
      *
      *    保健集計処理
           PERFORM 230-SYUKEI-CHK-SEC
      *
           .
       220-ROU-KOUIKI-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保健集計処理
      *****************************************************************
       230-SYUKEI-CHK-SEC          SECTION.
      *
      *    キーがない場合は処理抜ける
           IF        ( WRK-SYUBETUKBN  =   SPACE )
               OR    ( WRK-KBN         =   SPACE )
               OR    ( IDX             =   ZERO  )
               GO  TO  230-SYUKEI-CHK-EXT
           END-IF
      *
           IF      RECE10-SRYYM    <   CONST-H2004
               EVALUATE    WRK-TEISYUTU    ALSO    WRK-HKNKBN
               ALSO    WRK-SYUBETUKBN  ALSO    WRK-PARA-SYUBETUKBN
      *        指定なし
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    SPACE
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    ZERO
      *        社保医保前期高齢者９割
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "01"
      *        社保医保前期高齢者８割・７割
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "02"
      *        社保医保本人
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "03"
      *        社保医保家族
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "04"
      *        社保医保三歳
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "05"
      *        社保老人９割
               WHEN    "1" ALSO    "2" ALSO    "1" ALSO    "06"
               WHEN    "1" ALSO    "2" ALSO    "1" ALSO    "21"
      *        社保老人８割・７割
               WHEN    "1" ALSO    "2" ALSO    "2" ALSO    "07"
               WHEN    "1" ALSO    "2" ALSO    "2" ALSO    "22"
      *        社保公費と公費の併用
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    "08"
      *        社保公費単独
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    "09"
      *        国保、特別療養費一般前期高齢者９割
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    "10"
               WHEN    "3" ALSO    "1" ALSO    "1" ALSO    "10"
      *        国保、特別療養費一般前期高齢者８割・７割
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    "11"
               WHEN    "3" ALSO    "1" ALSO    "2" ALSO    "11"
      *        国保、特別療養費一般被保険者
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    "12"
               WHEN    "3" ALSO    "1" ALSO    "3" ALSO    "12"
      *        国保、特別療養費一般三歳
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    "13"
               WHEN    "3" ALSO    "1" ALSO    "4" ALSO    "13"
      *        国保、特別療養費退職本人
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    "14"
               WHEN    "3" ALSO    "1" ALSO    "5" ALSO    "14"
      *        国保、特別療養費退職老人９割
               WHEN    "2" ALSO    "1" ALSO    "6" ALSO    "15"
               WHEN    "3" ALSO    "1" ALSO    "6" ALSO    "15"
               WHEN    "2" ALSO    "1" ALSO    "6" ALSO    "21"
               WHEN    "3" ALSO    "1" ALSO    "6" ALSO    "21"
      *        国保、特別療養費退職老人８割・７割
               WHEN    "2" ALSO    "1" ALSO    "7" ALSO    "16"
               WHEN    "3" ALSO    "1" ALSO    "7" ALSO    "16"
               WHEN    "2" ALSO    "1" ALSO    "7" ALSO    "22"
               WHEN    "3" ALSO    "1" ALSO    "7" ALSO    "22"
      *        国保、特別療養費退職被扶養者
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    "17"
               WHEN    "3" ALSO    "1" ALSO    "8" ALSO    "17"
      *        国保、特別療養費退職三歳
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    "18"
               WHEN    "3" ALSO    "1" ALSO    "9" ALSO    "18"
      *        国保、特別療養費老人９割
               WHEN    "2" ALSO    "2" ALSO    "1" ALSO    "19"
               WHEN    "3" ALSO    "2" ALSO    "1" ALSO    "19"
               WHEN    "2" ALSO    "2" ALSO    "1" ALSO    "21"
               WHEN    "3" ALSO    "2" ALSO    "1" ALSO    "21"
      *        国保、特別療養費老人８割・７割
               WHEN    "2" ALSO    "2" ALSO    "2" ALSO    "20"
               WHEN    "3" ALSO    "2" ALSO    "2" ALSO    "20"
               WHEN    "2" ALSO    "2" ALSO    "2" ALSO    "22"
               WHEN    "3" ALSO    "2" ALSO    "2" ALSO    "22"
                   CONTINUE
      *        指定したレセ種別が正しくない場合は処理抜ける
               WHEN    OTHER
                   GO  TO  230-SYUKEI-CHK-EXT
               END-EVALUATE
           ELSE
               EVALUATE    WRK-TEISYUTU    ALSO    WRK-HKNKBN
               ALSO    WRK-SYUBETUKBN  ALSO    WRK-PARA-SYUBETUKBN
      *        指定なし
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    SPACE
               WHEN    ANY ALSO    ANY ALSO    ANY ALSO    ZERO
      *        社保医保前期高齢者一般・低所得
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "01"
      *        社保医保前期高齢者７割
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "02"
      *        社保医保本人
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "03"
      *        社保医保家族
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "04"
      *        社保医保未就学者
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "05"
      *        社保公費と公費の併用
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    "08"
      *        社保公費単独
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    "09"
      *        国保、特別療養費前期高齢者一般・低所得
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    "10"
               WHEN    "3" ALSO    "1" ALSO    "1" ALSO    "10"
      *        国保、特別療養費前期高齢者７・８割
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    "11"
               WHEN    "3" ALSO    "1" ALSO    "2" ALSO    "11"
      *        国保、特別療養費世帯主
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    "12"
               WHEN    "3" ALSO    "1" ALSO    "3" ALSO    "12"
      *        国保、特別療養費未就学者
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    "13"
               WHEN    "3" ALSO    "1" ALSO    "4" ALSO    "13"
      *        国保、特別療養費退職本人
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    "14"
               WHEN    "3" ALSO    "1" ALSO    "5" ALSO    "14"
      *        国保、特別療養費退職家族
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    "17"
               WHEN    "3" ALSO    "1" ALSO    "8" ALSO    "17"
      *        国保、特別療養費退職未就学者
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    "18"
               WHEN    "3" ALSO    "1" ALSO    "9" ALSO    "18"
      *        特別療養費、広域連合後期高齢者一般・低所得
               WHEN    "3" ALSO    "2" ALSO    "1" ALSO    "21"
               WHEN    "6" ALSO    "2" ALSO    "1" ALSO    "21"
      *        特別療養費、広域連合後期高齢者７割
               WHEN    "3" ALSO    "2" ALSO    "2" ALSO    "22"
               WHEN    "6" ALSO    "2" ALSO    "2" ALSO    "22"
                   CONTINUE
      *        指定したレセ種別が正しくない場合は処理抜ける
               WHEN    OTHER
                   GO  TO  230-SYUKEI-CHK-EXT
               END-EVALUATE
           END-IF
      *
           MOVE    RECE10-TOTALTEN(IDX)    TO  WRK-TENSU
           MOVE    SPACE                   TO  WRK-KIGO
           MOVE    RECE10-SHOKUJIFTN(IDX)  TO  WRK-SHOKUJIFTN
           MOVE    RECE10-SHOKUJIRYOYOHI(IDX)  TO  WRK-RYOYOHI
      *    患者別集計処理
           PERFORM 240-PTINF-SYUKEI-SEC
      *
           .
       230-SYUKEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者別集計処理
      *****************************************************************
       240-PTINF-SYUKEI-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-TOKEI
           MOVE    ZERO                TO  WRK-RENNUM
      *
      *    患者連番最大９まで
           PERFORM UNTIL ( FLG-TOKEI   =    1 )
                   OR    ( WRK-RENNUM  >=   9 )
               ADD     1               TO  WRK-RENNUM
      *        統計読み込み
               INITIALIZE                  TOKEI-REC
               MOVE    WRK-SKYKBN      TO  TOKEI-SKYKBN
               MOVE    WRK-TEISYUTU    TO  TOKEI-TEISYUTU
               MOVE    WRK-HKNJANUM    TO  TOKEI-HKNJANUM
               MOVE    WRK-SYOKBN      TO  TOKEI-SYOKBN
               MOVE    WRK-HKNJANUM2   TO  TOKEI-HKNJANUM2
               MOVE    WRK-SRYYMFLG    TO  TOKEI-SRYYMFLG
               MOVE    WRK-NYUGAIKBN   TO  TOKEI-NYUGAIKBN
               MOVE    WRK-ZAIKBN      TO  TOKEI-ZAIKBN
               MOVE    WRK-HKNKBN      TO  TOKEI-HKNKBN
               MOVE    WRK-SYUBETUKBN  TO  TOKEI-SYUBETUKBN
               MOVE    WRK-KBN         TO  TOKEI-KBN
               MOVE    WRK-KIGO        TO  TOKEI-KIGO
               EVALUATE    WRK-PARA-JYUNBAN
                   WHEN    "1"
                       MOVE    SPACE           TO  WRK-KEY2
                       MOVE    RECE10-PTNUM    TO  WRK-KEY2-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY2-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY2-RENNUM
                       MOVE    WRK-TENSU       TO  WRK-KEY2-TENSU
                       MOVE    WRK-KEY2        TO  TOKEI-JYUNBAN
                   WHEN    "2"
                       MOVE    SPACE           TO  WRK-KEY3
                       MOVE    RECE10-PTNUM    TO  WRK-KEY3-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY3-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY3-RENNUM
                       MOVE    WRK-TENSU       TO  WRK-KEY3-TENSU
                       MOVE    WRK-KEY3        TO  TOKEI-JYUNBAN
                   WHEN    OTHER
                       MOVE    SPACE           TO  WRK-KEY1
                       MOVE    RECE10-PTNUM    TO  WRK-KEY1-PTNUM
                       MOVE    PTINF-KANANAME  TO  WRK-KEY1-KANANAME
                       MOVE    WRK-RENNUM      TO  WRK-KEY1-RENNUM
                       MOVE    WRK-TENSU       TO  WRK-KEY1-TENSU
                       MOVE    WRK-KEY1        TO  TOKEI-JYUNBAN
               END-EVALUATE
               PERFORM 900-TOKEI-READ-SEC
           END-PERFORM
      *
      *    患者データ無ければ新規作成
           MOVE    WRK-SRYYM               TO  TOKEI-SRYYM
           MOVE    RECE10-PTNUM            TO  TOKEI-PTNUM
           MOVE    WRK-RENNUM              TO  TOKEI-RENNUM
           MOVE    PTINF-NAME              TO  TOKEI-NAME
           MOVE    PTINF-KANANAME          TO  TOKEI-KANANAME
           MOVE    RECE10-JNISSU(IDX)      TO  TOKEI-NISSU
           MOVE    WRK-TENSU               TO  TOKEI-TENSU
           MOVE    WRK-RYOYOHI             TO  TOKEI-RYOYOHI
           MOVE    WRK-SHOKUJIFTN          TO  TOKEI-SHOKUJIFTN
           WRITE   TOKEI-REC
      *
           .
       240-PTINF-SYUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷編集処理
      *****************************************************************
       300-PRINT-HENSYU-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-KEY
           MOVE    ZERO                TO  CNT-PAGE
           MOVE    ZERO                TO  CNT-LINE
      *
      *    統計ファイル読み込み
           INITIALIZE                  TOKEI-REC
           PERFORM 900-TOKEI-NEXT-SEC
           IF      FLG-TOKEI   =   ZERO
      *        ＣＳＶ出力処理
               INITIALIZE                  WRK-REC
               MOVE    CSV-HEAD-01     TO  WRK-REC
               PERFORM 500-TOUKEICSV-SEC
           END-IF
      *
           PERFORM UNTIL ( FLG-TOKEI   =   1 )
      *
      *        請求、未請求分
               IF     TOKEI-SKYKBN     =   "1"
                   EVALUATE    TOKEI-TEISYUTU
      *            広域分
                   WHEN    "6"
      *                帳票編集<見出し>処理４
                       PERFORM 310-HEAD-HEN4-SEC
      *                帳票編集<明細>処理４
                       PERFORM 320-BODY-HEN4-SEC
                           UNTIL ( FLG-TOKEI   =   1 )
                           OR    ( TOKEI-SKYKBN    NOT =  WRK-SKYKBN )
                           OR    ( TOKEI-TEISYUTU  NOT =  WRK-TEISYUTU )
                           OR    ( TOKEI-HKNJANUM  NOT =  WRK-HKNJANUM )
                           OR    ( TOKEI-SYOKBN    NOT =  WRK-SYOKBN )
      *            短期、傷病分
                   WHEN    "7"
      *                帳票編集<見出し>処理６
                       PERFORM 310-HEAD-HEN6-SEC
      *                帳票編集<明細>処理６
                       PERFORM 320-BODY-HEN6-SEC
                           UNTIL ( FLG-TOKEI   =   1 )
                           OR    ( TOKEI-SKYKBN    NOT =  WRK-SKYKBN )
                           OR    ( TOKEI-TEISYUTU  NOT =  WRK-TEISYUTU )
                           OR    ( TOKEI-HKNJANUM  NOT =  WRK-HKNJANUM )
                           OR    ( TOKEI-SYOKBN    NOT =  WRK-SYOKBN )
      *            アフターケア分
                   WHEN    "8"
      *                帳票編集<見出し>処理５
                       PERFORM 310-HEAD-HEN5-SEC
      *                帳票編集<明細>処理５
                       PERFORM 320-BODY-HEN5-SEC
                           UNTIL ( FLG-TOKEI   =   1 )
                           OR    ( TOKEI-SKYKBN    NOT =  WRK-SKYKBN )
                           OR    ( TOKEI-TEISYUTU  NOT =  WRK-TEISYUTU )
                           OR    ( TOKEI-SYOKBN    NOT =  WRK-SYOKBN )
      *----(04.08.03)--ADD-START---
      *            自賠責分
                   WHEN    "10"
      *                帳票編集<見出し>処理７
                       PERFORM 310-HEAD-HEN7-SEC
      *                帳票編集<明細>処理７
                       PERFORM 320-BODY-HEN7-SEC
                           UNTIL ( FLG-TOKEI   =   1 )
                           OR    ( TOKEI-SKYKBN    NOT =  WRK-SKYKBN )
                           OR    ( TOKEI-TEISYUTU  NOT =  WRK-TEISYUTU )
                           OR    ( TOKEI-HKNJANUM  NOT =  WRK-HKNJANUM )
                           OR    ( TOKEI-SYOKBN    NOT =  WRK-SYOKBN )
      *----(04.08.03)--ADD-END-----
                   WHEN    OTHER
      *                帳票編集<見出し>処理
                       PERFORM 310-HEAD-HEN-SEC
      *                帳票編集<明細>処理
                       PERFORM 320-BODY-HEN-SEC
                           UNTIL ( FLG-TOKEI   =   1 )
                           OR    ( TOKEI-SKYKBN    NOT =  WRK-SKYKBN )
                           OR    ( TOKEI-TEISYUTU  NOT =  WRK-TEISYUTU )
                           OR    ( TOKEI-HKNJANUM  NOT =  WRK-HKNJANUM )
                           OR    ( TOKEI-SYOKBN    NOT =  WRK-SYOKBN )
                   END-EVALUATE
               END-IF
      *
      *        請求しない分
               IF      TOKEI-SKYKBN    =   "2"
      *            帳票編集<見出し>処理２
                   PERFORM 310-HEAD-HEN2-SEC
      *            帳票編集<明細>処理２
                   PERFORM 320-BODY-HEN2-SEC
                       UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               END-IF
      *
      *        未請求患者分
               IF      TOKEI-SKYKBN    =   "3"
      *            帳票編集<見出し>処理3
                   PERFORM 310-HEAD-HEN3-SEC
      *            帳票編集<明細>処理3
                   PERFORM 320-BODY-HEN3-SEC
                       UNTIL ( FLG-TOKEI   =   1 )
                       OR    ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               END-IF
           END-PERFORM
      *
           .
       300-PRINT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理
      *****************************************************************
       310-HEAD-HEN-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO    WRK-SYOKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "（社保分）"        TO  WRK-PRT1-BUNRUI
               WHEN    "1"     ALSO    "2"
                   MOVE    "（社保返戻分）"    TO  WRK-PRT1-BUNRUI
               WHEN    "1"     ALSO    "3"
                   MOVE    "（社保オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "1"     ALSO    "4"
                   MOVE    "（社保月遅れ分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "1"
                   MOVE    "（国保分）"        TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "2"
                   MOVE    "（国保返戻分）"    TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "3"
                   MOVE    "（国保オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "2"     ALSO    "4"
                   MOVE    "（国保月遅れ分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "3"     ALSO    "1"
                   MOVE    "（特別療養費分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "3"     ALSO    "2"
                   MOVE    "（特別返戻分）"    TO  WRK-PRT1-BUNRUI
               WHEN    "3"     ALSO    "3"
                   MOVE    "（特別オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "3"     ALSO    "4"
                   MOVE    "（特別月遅れ分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "9"     ALSO    "1"
                   MOVE    "（公害分）"        TO  WRK-PRT1-BUNRUI
               WHEN    "9"     ALSO    "2"
                   MOVE    "（公害返戻分）"    TO  WRK-PRT1-BUNRUI
               WHEN    "9"     ALSO    "3"
                   MOVE    "（公害オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "9"     ALSO    "4"
                   MOVE    "（公害月遅れ分）"  TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "1"         TO  CSV-SYOKBN
               WHEN    "2"
               WHEN    "3"
                   MOVE    "2"         TO  CSV-SYOKBN
               WHEN    "4"
                   MOVE    "3"         TO  CSV-SYOKBN
           END-EVALUATE
      *
      *    国保分、特別療養費分、社保直接請求分は保険者名セット
           IF        ( WRK-TEISYUTU    =   "2" )
               OR    ( WRK-TEISYUTU    =   "3" )
               OR   (( WRK-TEISYUTU    =   "1" )
               AND   ( FLG-DIRECT      =   1 ))
               IF      WRK-HKNJANUM    =   "00000000"
                   IF      WRK-TEISYUTU    =   "1"
                       MOVE    "直接請求する保険者以外"
                                               TO  WRK-PRT2-HKNJANAME
                                                   CSV-HKNJANAME
                   ELSE
                       MOVE    WRK-HKNJANUM    TO  WRK-PRT2-HKNJANUM
                       MOVE    SPACE           TO  WRK-PRT2-FILLER
                       MOVE    "保険者番号入力なし"
                                               TO  WRK-PRT2-HKNJANAME
                   END-IF
               ELSE
                   MOVE    SPACE           TO  HKNJAINF-REC
                   INITIALIZE                  HKNJAINF-REC
                   MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
                   MOVE    WRK-HKNJANUM    TO  HKNJA-HKNJANUM
                   MOVE    HKNJAINF-REC    TO  MCPDATA-REC
                   PERFORM 900-HKNJAINF-READ-SEC
                   IF      FLG-HKNJAINF        =   ZERO
                       INITIALIZE                  ORCSKANACHKAREA
                       MOVE    "2"             TO  KANACHK-SYORI
                       MOVE    HKNJA-HKNJANAME TO  KANACHK-MAE-INPUT
                       CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                       IF      KANACHK-RC      =   0
                           MOVE    KANACHK-OUT-INPUT
                                               TO  WRK-PRT2-HKNJANAME
                                                   CSV-HKNJANAME
                       END-IF
                   END-IF
                   MOVE    WRK-HKNJANUM    TO  WRK-PRT2-HKNJANUM
                                               CSV-HKNJANUM
                   MOVE    SPACE           TO  WRK-PRT2-FILLER
               END-IF
           END-IF
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理２
      *****************************************************************
       310-HEAD-HEN2-SEC           SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           MOVE    "（請求しない分）"  TO  WRK-PRT1-BUNRUI
      *
           MOVE    "4"                 TO  CSV-SYOKBN
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *----(04.08.03)--UPD-START---
      *     IF      TOKEI-TEISYUTU  =   "7"
      *         MOVE    SPACE           TO  WRK-HKNJANUM
      *     END-IF
           MOVE    SPACE               TO  WRK-KEY
           MOVE    TOKEI-SKYKBN        TO  WRK-SKYKBN 
      *----(04.08.03)--UPD-END-----
           .
       310-HEAD-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理3
      *****************************************************************
       310-HEAD-HEN3-SEC           SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           MOVE    "（未請求患者分）"  TO  WRK-PRT1-BUNRUI
      *
           MOVE    SPACE               TO  WRK-TEISYUTU
           MOVE    WRK-TEISYUTU        TO  CSV-TEISYUTU
           MOVE    "5"                 TO  CSV-SYOKBN
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理4
      *****************************************************************
       310-HEAD-HEN4-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO    WRK-SYOKBN
               WHEN    "6"     ALSO    "1"
                   MOVE    "（広域分）"        TO  WRK-PRT1-BUNRUI
               WHEN    "6"     ALSO    "2"
                   MOVE    "（広域返戻分）"    TO  WRK-PRT1-BUNRUI
               WHEN    "6"     ALSO    "3"
                   MOVE    "（広域オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "6"     ALSO    "4"
                   MOVE    "（広域月遅れ分）"  TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "1"         TO  CSV-SYOKBN
               WHEN    "2"
               WHEN    "3"
                   MOVE    "2"         TO  CSV-SYOKBN
               WHEN    "4"
                   MOVE    "3"         TO  CSV-SYOKBN
           END-EVALUATE
      *
      *    広域連合名セット
           IF      WRK-HKNJANUM    =   "00000000"
               MOVE    WRK-HKNJANUM    TO  WRK-PRT2-HKNJANUM
               MOVE    SPACE           TO  WRK-PRT2-FILLER
               MOVE     "保険者番号入力なし"
                                       TO  WRK-PRT2-HKNJANAME
           ELSE
               MOVE    SPACE               TO  WRK-PRT2-FILLER
               PERFORM VARYING IDZZ    FROM    1   BY  1
                       UNTIL   IDZZ    >       CONST-KOUIKI-MAX
                   IF      WRK-HKNJANUM(1:4)   =
                                           CONST-KOUIKI-NUM (IDZZ) (1:4)
                       MOVE    CONST-KOUIKI-NUM (IDZZ)
                                                TO  WRK-PRT2-HKNJANUM
                       MOVE    CONST-KOUIKI-NAME (IDZZ)
                                                TO  WRK-PRT2-HKNJANAME
                       MOVE    CONST-KOUIKI-MAX     TO  IDZZ
                   END-IF
               END-PERFORM
           END-IF
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           IF      WRK-HKNJANUM2   NOT =   "00000000"
      *        保険者名セット
               PERFORM 3201-HKNJANUM2-BODY-HEN-SEC
           END-IF
      *
           .
       310-HEAD-HEN4-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理5
      *****************************************************************
       310-HEAD-HEN5-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
      *----(04.08.03)--UPD-START---
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO    WRK-SYOKBN
               WHEN    "8"     ALSO    "1"
                   MOVE    "（アフターケア分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "8"     ALSO    "2"
                   MOVE    "（アフター返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "8"     ALSO    "3"
                   MOVE    "（アフターオンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "8"     ALSO    "4"
                   MOVE    "（アフター月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *----(04.08.03)--UPD-START---
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "1"         TO  CSV-SYOKBN
               WHEN    "2"
               WHEN    "3"
                   MOVE    "2"         TO  CSV-SYOKBN
               WHEN    "4"
                   MOVE    "3"         TO  CSV-SYOKBN
           END-EVALUATE
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN5-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>処理６
      *****************************************************************
       310-HEAD-HEN6-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
           IF      WRK-HKNJANUM(1:5)   =   "99999"
               MOVE    "2"             TO  WRK-SYOKAIFLG
           ELSE
               MOVE    "1"             TO  WRK-SYOKAIFLG
           END-IF
      *
      *----(04.08.03)--UPD-START---
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO
                       WRK-SYOKAIFLG   ALSO  WRK-SYOKBN
               WHEN    "7"     ALSO    "1"     ALSO    "1"
                   MOVE    "（短期傷病初回分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "1"     ALSO    "2"
                   MOVE    "（短傷初回返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "1"     ALSO    "3"
                   MOVE    "（短傷初回オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "1"     ALSO    "4"
                   MOVE    "（短傷初回月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "2"     ALSO    "1"
                   MOVE    "（短期傷病継続分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "2"     ALSO    "2"
                   MOVE    "（短傷継続返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "2"     ALSO    "3"
                   MOVE    "（短傷継続オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "7"     ALSO    "2"     ALSO    "4"
                   MOVE    "（短傷継続月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *----(04.08.03)--UPD-END-----
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "1"         TO  CSV-SYOKBN
               WHEN    "2"
               WHEN    "3"
                   MOVE    "2"         TO  CSV-SYOKBN
               WHEN    "4"
                   MOVE    "3"         TO  CSV-SYOKBN
           END-EVALUATE
      *
           MOVE    SPACE           TO  WRK-PRT2-FILLER
      *
      *    労働基準監督署名セット
           IF      WRK-HKNJANUM(1:5)   NOT =   "99999"
               MOVE    SPACE           TO  LABOR-SIO-REC
               INITIALIZE                  LABOR-SIO-REC
               MOVE    WRK-HKNJANUM(1:5)
                                       TO  LABOR-SIO-SYOCD
               MOVE    TOKEI-SRYYM     TO  LABOR-SIO-YUKOSTYMD(1:6)
               MOVE    "01"            TO  LABOR-SIO-YUKOSTYMD(7:2)
               MOVE    TOKEI-SRYYM     TO  LABOR-SIO-YUKOEDYMD(1:6)
               MOVE    "01"            TO  LABOR-SIO-YUKOEDYMD(7:2)
               MOVE    LABOR-SIO-REC   TO  MCPDATA-REC
               PERFORM 900-LABOR-SIO-READ-SEC
               IF      FLG-LABOR-SIO       =   ZERO
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    LABOR-SIO-NAME  TO  KANACHK-MAE-INPUT
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF      KANACHK-RC      =   0
                       STRING  KANACHK-OUT-INPUT   DELIMITED BY SPACE
                               "署分"              DELIMITED BY SIZE
                                           INTO    WRK-PRT2-HKNJANAME
                       END-STRING
                       STRING  "短期傷病初回分（"  DELIMITED BY SIZE
                               KANACHK-OUT-INPUT   DELIMITED BY SPACE
                               "署分）"            DELIMITED BY SIZE
                                       INTO    CSV-HKNJANAME
                       END-STRING
                   END-IF
               END-IF
               MOVE    WRK-HKNJANUM(1:5)   TO  WRK-PRT2-HKNJANUM
                                               CSV-HKNJANUM
               MOVE    SPACE           TO  WRK-PRT2-FILLER
           ELSE
               MOVE    "短期傷病継続分"    TO   CSV-HKNJANAME
               MOVE    SPACE               TO   CSV-HKNJANUM
           END-IF
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN6-EXT.
           EXIT.
      *----(04.08.03)--ADD-START---
      *****************************************************************
      *    帳票編集<見出し>処理7
      *****************************************************************
       310-HEAD-HEN7-SEC            SECTION.
      *
      *    初期処理
           PERFORM 310-HEAD-KEY-SET-SEC
      *
           MOVE    WRK-HKNJANUM(1:1)   TO  WRK-SYOKAIFLG
      *
      *    分類セット
           EVALUATE    WRK-TEISYUTU    ALSO
                       WRK-SYOKAIFLG   ALSO  WRK-SYOKBN
               WHEN    "10"     ALSO    "1"     ALSO    "1"
                   MOVE    "（自賠責新様式分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "1"     ALSO    "2"
                   MOVE    "（自賠新返戻分）"  TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "1"     ALSO    "3"
                   MOVE    "（自賠新オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "1"     ALSO    "4"
                   MOVE    "（自賠新月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "2"     ALSO    "1"
                   MOVE    "（自賠責従来様式分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "2"     ALSO    "2"
                   MOVE    "（自賠従来返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "2"     ALSO    "3"
                   MOVE    "（自賠従来オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "2"     ALSO    "4"
                   MOVE    "（自賠従来月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "3"     ALSO    "1"
                   MOVE    "（自賠責第三者行為分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "3"     ALSO    "2"
                   MOVE    "（自賠第三返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "3"     ALSO    "3"
                   MOVE    "（自賠第三オンライン返戻分）"
                                               TO  WRK-PRT1-BUNRUI
               WHEN    "10"     ALSO    "3"     ALSO    "4"
                   MOVE    "（自賠第三月遅れ分）"
                                               TO  WRK-PRT1-BUNRUI
           END-EVALUATE
      *
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-SYOKBN
               WHEN    "1"
                   MOVE    "1"         TO  CSV-SYOKBN
               WHEN    "2"
               WHEN    "3"
                   MOVE    "2"         TO  CSV-SYOKBN
               WHEN    "4"
                   MOVE    "3"         TO  CSV-SYOKBN
           END-EVALUATE
      *
      *    医療機関名セット
           PERFORM 310-HEAD-SYSKANRI-HEN-SEC
      *
      *    帳票クリア
           PERFORM 310-HEAD-SET-SEC
      *
           .
       310-HEAD-HEN7-EXT.
           EXIT.
      *
      *----(04.08.03)--ADD-END-----
      *****************************************************************
      *    帳票編集<見出し>初期処理
      *****************************************************************
       310-HEAD-KEY-SET-SEC        SECTION.
      *
      *    キーセット
           MOVE    SPACE               TO  WRK-KEY
           MOVE    ZERO                TO  WRK-SRYYMFLG
           MOVE    TOKEI-SKYKBN        TO  WRK-SKYKBN
           MOVE    TOKEI-TEISYUTU      TO  WRK-TEISYUTU
           MOVE    TOKEI-HKNJANUM      TO  WRK-HKNJANUM
           MOVE    TOKEI-SYOKBN        TO  WRK-SYOKBN
           MOVE    TOKEI-HKNJANUM2     TO  WRK-HKNJANUM2
           MOVE    ZERO                TO  SUM-AREA
      *
      *    帳票タイトル初期セット
           MOVE    SPACE               TO  WRK-PRT1-AREA
           MOVE    SPACE               TO  WRK-PRT1-HOSPNAME
           MOVE    "＜＜"              TO  WRK-PRT1-FILLER1
           MOVE    WRK-DENPYMD         TO  WRK-PRT1-DENPPRTYM
           MOVE    "分　保険請求確認リスト　＞＞"
                                       TO  WRK-PRT1-TITLE
           MOVE    SPACE               TO  WRK-PRT1-BUNRUI
           MOVE    SPACE               TO  WRK-PRT1-FILLER2
           MOVE    WRK-PRTYMD          TO  WRK-PRT1-PRTYMD
           MOVE    SPACE               TO  WRK-PRT1-FILLER3
           MOVE    WRK-PRTTIME         TO  WRK-PRT1-PRTTIME
           MOVE    "作成"              TO  WRK-PRT1-FILLER4
           MOVE    SPACE               TO  WRK-PRT1-PAGE
           MOVE    "頁"                TO  WRK-PRT1-FILLER5
      *
      *    保険者名クリア
           MOVE    SPACE               TO  WRK-PRT2-AREA
      *
           INITIALIZE                      CSV-REC
           MOVE    0                   TO  CSV-RENNUM
           MOVE    "0"                 TO  CSV-NISSU
                                           CSV-TENSU
           MOVE    WRK-PARA-DENPPRTYM(1:6)
                                       TO  CSV-SKYYM
           MOVE    "00"                TO  CSV-SRYKA
      *
           .
       310-HEAD-KEY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<見出し>医療機関情報処理
      *****************************************************************
       310-HEAD-SYSKANRI-HEN-SEC   SECTION.
      *
      *    医療機関基本情報読み込み
           PERFORM 900-SYS1001-READ-SEC
      *
      *    医療機関名セット
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    SYS-1001-HOSPNAME   TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
           IF      KANACHK-RC      =   0
               COMPUTE KANACHK-MAX     =   KANACHK-MAX +   1
               PERFORM UNTIL ( KANACHK-MAX >=  50 )
                   MOVE    "　"
                           TO  KANACHK-OUT-INPUT(KANACHK-MAX:2)
                   COMPUTE KANACHK-MAX     =   KANACHK-MAX +   2
               END-PERFORM
               MOVE    KANACHK-OUT-INPUT(1:50)
                                       TO  WRK-PRT1-HOSPNAME
           END-IF
      *
           .
       310-HEAD-SYSKANRI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票クリア処理
      *****************************************************************
       310-HEAD-SET-SEC            SECTION.
      *
           MOVE    5               TO  CNT-LINE
      *
           INITIALIZE                  HCMSL55
           MOVE    SPACE           TO  HCMSL55
      *
           .
       310-HEAD-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN-SEC            SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN-SEC
      *
      *    区分合計
           PERFORM 3202-KBN-BODY-END-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END-SEC
      *
           .
       320-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       320-BODY-HEN2-SEC           SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR2-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN2-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END2-SEC
      *
           .
       320-BODY-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理3
      *****************************************************************
       320-BODY-HEN3-SEC           SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR3-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN3-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END3-SEC
      *
           .
       320-BODY-HEN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN4-SEC           SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR4-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN-SEC
      *
      *    区分合計
           PERFORM 3202-KBN-BODY-END-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END4-SEC
      *
           .
       320-BODY-HEN4-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       320-BODY-HEN5-SEC           SECTION.
      *
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN5-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END5-SEC
      *
           .
       320-BODY-HEN5-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理6
      *****************************************************************
       320-BODY-HEN6-SEC           SECTION.
      *
      *    グループ見出しセット
           PERFORM 3201-BODY-HEN-STR6-SEC
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN6-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END6-SEC
      *
           .
       320-BODY-HEN6-EXT.
           EXIT.
      *
      *----(04.08.03)--ADD-START---
      *****************************************************************
      *    帳票編集<明細行>処理7
      *****************************************************************
       320-BODY-HEN7-SEC           SECTION.
      *
      *    入外区分見出し
           IF      TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN
               PERFORM 3201-NYUGAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF      TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
      *
      *    患者情報
           PERFORM 3202-BODY-SUB-HEN6-SEC
      *
      *    グループ合計セット
           PERFORM 3203-BODY-HEN-END6-SEC
      *
           .
       320-BODY-HEN7-EXT.
           EXIT.
      *----(04.08.03)--ADD-END-----
      *****************************************************************
      *    帳票編集<明細行>セット処理
      *****************************************************************
       320-BODY-SET-SEC             SECTION.
      *
      *    データがない時は処理抜ける
           IF      WRK-PRT-LINE     =   SPACE
               GO  TO  320-BODY-SET-EXT
           END-IF
      *
           ADD     1                   TO  CNT-LINE
      *
           IF      CNT-LINE        >=  55
               PERFORM 390-PRINT-OUT-SEC
               PERFORM 310-HEAD-SET-SEC
           END-IF
      *
      *    データを帳票にセットする
           MOVE    WRK-PRT-LINE        TO  HCMSL55-MOJI(CNT-LINE)
           .
       320-BODY-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理
      *****************************************************************
       3201-BODY-HEN-STR-SEC       SECTION.
      *
      *    入外区分見出し
           IF      TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN
               PERFORM 3201-NYUGAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    在総診区分見出し
           IF      TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN
               PERFORM 3201-ZAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF      TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
      *    保険区分見出し
           IF      TOKEI-HKNKBN    NOT =   WRK-HKNKBN
               MOVE    TOKEI-HKNKBN    TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    種別区分見出し
           IF      TOKEI-SYUBETUKBN    NOT =   WRK-SYUBETUKBN
               PERFORM 3201-SYUBETU-BODY-HEN-SEC
           END-IF
      *
      *    詳細区分見出し
           IF      TOKEI-KBN           NOT =   WRK-KBN
               PERFORM 3201-KBN-BODY-HEN-SEC
           END-IF
      *
      *    公害見出し
           IF      TOKEI-KIGO          NOT =   WRK-KIGO
               PERFORM 3201-KIGO-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理２
      *****************************************************************
       3201-BODY-HEN-STR2-SEC      SECTION.
      *
           EVALUATE    TOKEI-TEISYUTU
               WHEN    "7"
      *            短期傷病監督署見出し
                   IF    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM  )
                       PERFORM 3201-LABOR-SIO-BODY-HEN-SEC
                   END-IF
      *            短期傷病入外区分見出し
                   IF    ( TOKEI-HKNJANUM2 NOT =   WRK-HKNJANUM2 ) OR
                         ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       PERFORM 3201-ROUSAI-BODY-HEN-SEC
                   END-IF
               WHEN    "8"
                   IF        ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       PERFORM 3201-AFTER-BODY-HEN-SEC
                   END-IF
      *----(04.08.03)--ADD-START---
               WHEN    "10"
      *            自賠責
                   IF        ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM  )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       PERFORM 3201-JIBAI-BODY-HEN-SEC
                   END-IF
      *----(04.08.03)--ADD-END-----
               WHEN    OTHER
      *            提出先入外区分見出し
                   IF        ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       MOVE    SPACE           TO  CSV-HKNJANAME
                                                   CSV-HKNJANUM
                                                   CSV-SYUBETUKBN
                                                   CSV-KBN
                       PERFORM 3201-TEISYUTU-BODY-HEN-SEC
                   END-IF
           END-EVALUATE
      *
      *    診療月見出し
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
           MOVE    TOKEI-SRYYM     TO  CSV-SRYYM
      *
      *
      *    保険区分見出し
           IF      TOKEI-HKNKBN    NOT =   WRK-HKNKBN
               MOVE    TOKEI-HKNKBN    TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    種別区分見出し
           IF      TOKEI-SYUBETUKBN    NOT =   WRK-SYUBETUKBN
               PERFORM 3201-SYUBETU-BODY-HEN-SEC
           END-IF
      *
      *    詳細区分見出し
           IF      TOKEI-KBN           NOT =   WRK-KBN
               PERFORM 3201-KBN-BODY-HEN-SEC
           END-IF
      *
      *    公害見出し
           IF      TOKEI-KIGO          NOT =   WRK-KIGO
               PERFORM 3201-KIGO-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理３
      *****************************************************************
       3201-BODY-HEN-STR3-SEC      SECTION.
      *
      *    未請求設定入外区分見出し
           IF        ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               PERFORM 3201-MISETTEI-BODY-HEN-SEC
           END-IF
      *
      *    入外区分見出し
      *     IF        ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
      *         PERFORM 3201-NYUGAIKBN-BODY-HEN-SEC
      *     END-IF
      *
      *    診療月見出し
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR3-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理
      *****************************************************************
       3201-BODY-HEN-STR4-SEC      SECTION.
      *
      *    保険者見出し
           IF      TOKEI-HKNJANUM2 NOT =   WRK-HKNJANUM2
               ADD     1                   TO  CNT-LINE
               PERFORM 3201-HKNJANUM2-BODY-HEN-SEC
           END-IF
      *
      *    入外区分見出し
           IF      TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN
               PERFORM 3201-NYUGAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    在総診区分見出し
           IF      TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN
               PERFORM 3201-ZAIKBN-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF      TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
      *    保険区分見出し
           IF      TOKEI-HKNKBN    NOT =   WRK-HKNKBN
               MOVE    TOKEI-HKNKBN    TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    種別区分見出し
           IF      TOKEI-SYUBETUKBN    NOT =   WRK-SYUBETUKBN
               PERFORM 3201-SYUBETU-BODY-HEN-SEC
           END-IF
      *
      *    詳細区分見出し
           IF      TOKEI-KBN           NOT =   WRK-KBN
               PERFORM 3201-KBN-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR4-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>見出し処理6
      *****************************************************************
       3201-BODY-HEN-STR6-SEC       SECTION.
      *
      *    短期傷病入外区分見出し
           IF    ( TOKEI-HKNJANUM2 NOT =   WRK-HKNJANUM2 ) OR
                 ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               PERFORM 3201-ROUSAI-BODY-HEN-SEC
           END-IF
      *
      *    診療月見出し
           IF      TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG
               PERFORM 3201-SRYYM-BODY-HEN-SEC
           END-IF
      *
           .
       3201-BODY-HEN-STR6-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-AFTER-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  CSV-HKNJANAME
                                       CSV-HKNJANUM
                                       CSV-SYUBETUKBN
                                       CSV-KBN
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
      *----(04.08.03)--UPD-START---
      *     MOVE    "アフターケア分"    TO  WRK-PRT-LINE
      *                                     CSV-HKNJANAME
           MOVE    "＜アフターケア＞"  TO  WRK-PRT-LINE
      *----(04.08.03)--UPD-END-----
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-TEISYUTU  TO  CSV-TEISYUTU
      *
           MOVE    TOKEI-HKNJANUM  TO  WRK-HKNJANUM
           MOVE    TOKEI-HKNJANUM2 TO  WRK-HKNJANUM2
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-AFTER-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-ROUSAI-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-HKNJANUM2(1:1)    ALSO    TOKEI-NYUGAIKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "＜短期　外来＞"    TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "2"
                   MOVE    "＜短期　入院＞"    TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "1"
                   MOVE    "＜傷病　外来＞"    TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "2"
                   MOVE    "＜傷病　入院＞"    TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-HKNJANUM  TO  WRK-HKNJANUM
           MOVE    TOKEI-HKNJANUM2 TO  WRK-HKNJANUM2
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
      *----(04.08.03)--ADD-START---
           MOVE    TOKEI-TEISYUTU  TO  CSV-TEISYUTU
      *----(04.08.03)--ADD-END-----
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-ROUSAI-BODY-HEN-EXT.
           EXIT.
      *
      *----(04.08.03)--ADD-START---
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-JIBAI-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-HKNJANUM(1:1)     ALSO    TOKEI-NYUGAIKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "＜自賠責新様式　外来＞"    TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "2"
                   MOVE    "＜自賠責新様式　入院＞"    TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "1"
                   MOVE    "＜自賠責従来様式　外来＞"  TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "2"
                   MOVE    "＜自賠責従来様式　入院＞"  TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    "1"
                   MOVE    "＜自賠責第三者行為　外来＞"
                                                       TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    "2"
                   MOVE    "＜自賠責第三者行為　入院＞"
                                                       TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-HKNJANUM  TO  WRK-HKNJANUM
           MOVE    TOKEI-HKNJANUM2 TO  WRK-HKNJANUM2
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    TOKEI-TEISYUTU  TO  CSV-TEISYUTU
           MOVE    SPACE           TO  CSV-HKNJANAME
                                       CSV-HKNJANUM
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-JIBAI-BODY-HEN-EXT.
           EXIT.
      *----(04.08.03)--ADD-END-----
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-MISETTEI-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-TEISYUTU  ALSO    TOKEI-NYUGAIKBN
               WHEN    "4"     ALSO    "1"
                   MOVE    "＜未請求設定　外来＞"  TO  WRK-PRT-LINE
               WHEN    "4"     ALSO    "2"
                   MOVE    "＜未請求設定　入院＞"  TO  WRK-PRT-LINE
               WHEN    "5"     ALSO    "1"
                   MOVE    "＜未請求解除　外来＞"  TO  WRK-PRT-LINE
               WHEN    "5"     ALSO    "2"
                   MOVE    "＜未請求解除　入院＞"  TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    SPACE           TO  CSV-TEISYUTU
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-MISETTEI-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-TEISYUTU-BODY-HEN-SEC  SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-TEISYUTU  ALSO    TOKEI-NYUGAIKBN
               WHEN    "1"     ALSO    "1"
                   MOVE    "＜社保　外来＞"          TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "2"
                   MOVE    "＜社保　入院＞"          TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "1"
                   MOVE    "＜国保　外来＞"          TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "2"
                   MOVE    "＜国保　入院＞"          TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    "1"
                   MOVE    "＜特別療養費　外来＞"    TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    "2"
                   MOVE    "＜特別療養費　入院＞"    TO  WRK-PRT-LINE
               WHEN    "6"     ALSO    "1"
                   MOVE    "＜広域　外来＞"          TO  WRK-PRT-LINE
               WHEN    "6"     ALSO    "2"
                   MOVE    "＜広域　入院＞"          TO  WRK-PRT-LINE
               WHEN    "9"     ALSO    "1"
                   MOVE    "＜公害　外来＞"          TO  WRK-PRT-LINE
               WHEN    "9"     ALSO    "2"
                   MOVE    "＜公害　入院＞"          TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-TEISYUTU  TO  WRK-TEISYUTU
           MOVE    TOKEI-HKNJANUM  TO  WRK-HKNJANUM
           MOVE    TOKEI-HKNJANUM2 TO  WRK-HKNJANUM2
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    WRK-TEISYUTU    TO  CSV-TEISYUTU
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-TEISYUTU-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-LABOR-SIO-BODY-HEN-SEC SECTION.
      *
           MOVE    SPACE           TO  CSV-HKNJANAME
                                       CSV-HKNJANUM
                                       CSV-SYUBETUKBN
                                       CSV-KBN
      *
           MOVE    TOKEI-HKNJANUM  TO  WRK-HKNJANUM
           MOVE    TOKEI-TEISYUTU  TO  CSV-TEISYUTU
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           IF      WRK-HKNJANUM(1:5)   NOT =   "99999"
           MOVE    SPACE           TO  LABOR-SIO-REC
           INITIALIZE                  LABOR-SIO-REC
           MOVE    WRK-HKNJANUM(1:5)
                                   TO  LABOR-SIO-SYOCD
           MOVE    TOKEI-SRYYM     TO  LABOR-SIO-YUKOSTYMD(1:6)
           MOVE    "01"            TO  LABOR-SIO-YUKOSTYMD(7:2)
           MOVE    TOKEI-SRYYM     TO  LABOR-SIO-YUKOEDYMD(1:6)
           MOVE    "01"            TO  LABOR-SIO-YUKOEDYMD(7:2)
           MOVE    LABOR-SIO-REC   TO  MCPDATA-REC
           PERFORM 900-LABOR-SIO-READ-SEC
           MOVE    "短期傷病初回分　"      TO  WRK-PRT-LINE(1:16)
           IF      FLG-LABOR-SIO       =   ZERO
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    LABOR-SIO-NAME  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   0
                   STRING  KANACHK-OUT-INPUT   DELIMITED BY SPACE
                           "署分"              DELIMITED BY SIZE
                                       INTO    WRK-PRT-LINE(24:40)
                   END-STRING
                   STRING  "短期傷病初回分（"  DELIMITED BY SIZE
                           KANACHK-OUT-INPUT   DELIMITED BY SPACE
                           "署分）"            DELIMITED BY SIZE
                                       INTO    CSV-HKNJANAME
                   END-STRING
               END-IF
           END-IF
           MOVE    WRK-HKNJANUM(1:5)   TO  WRK-PRT-LINE(17:5)
                                       CSV-HKNJANUM
           MOVE    "　"                TO  WRK-PRT-LINE(22:2)
           ELSE
               MOVE    "短期傷病継続分"    TO  WRK-PRT-LINE
                                               CSV-HKNJANAME
           END-IF
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    SPACE           TO  WRK-TEISYUTU
           MOVE    SPACE           TO  WRK-NYUGAIKBN
      *
            .
      *
       3201-LABOR-SIO-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-HKNJANUM2-BODY-HEN-SEC SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           MOVE    SPACE           TO  HKNJAINF-REC
           INITIALIZE                  HKNJAINF-REC
           MOVE    SPA-HOSPNUM     TO  HKNJA-HOSPNUM
           MOVE    TOKEI-HKNJANUM2 TO  HKNJA-HKNJANUM
           MOVE    HKNJAINF-REC    TO  MCPDATA-REC
           PERFORM 900-HKNJAINF-READ-SEC
           IF      FLG-HKNJAINF        =   ZERO
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"             TO  KANACHK-SYORI
               MOVE    HKNJA-HKNJANAME TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
               IF      KANACHK-RC      =   0
                   MOVE    KANACHK-OUT-INPUT(1:40)
                                           TO  WRK-PRT-LINE(11:40)
                                               CSV-HKNJANAME
               END-IF
           END-IF
           MOVE    TOKEI-HKNJANUM2 TO  WRK-PRT-LINE(1:8)
                                       CSV-HKNJANUM
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-HKNJANUM2 TO  WRK-HKNJANUM2
           MOVE    SPACE           TO  WRK-NYUGAIKBN
           .
      *
       3201-HKNJANUM2-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-NYUGAIKBN-BODY-HEN-SEC SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-NYUGAIKBN
               WHEN    "1"
                   MOVE    "＜外来＞"  TO  WRK-PRT-LINE
               WHEN    "2"
                   MOVE    "＜入院＞"  TO  WRK-PRT-LINE
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-NYUGAIKBN TO  WRK-NYUGAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-ZAIKBN
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           EVALUATE    WRK-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"     TO  CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"     TO  CSV-NYUGAIKBN
           END-EVALUATE
      *
           .
       3201-NYUGAIKBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3201-ZAIKBN-BODY-HEN-SEC    SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT-LINE
      *
           IF      TOKEI-ZAIKBN    =   "2"
               MOVE    "＜在総診または在医総を算定したレセ＞"
                                   TO  WRK-PRT-LINE
           END-IF
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    0               TO  WRK-SRYYMFLG
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    WRK-ZAIKBN      TO  CSV-ZAIKBN
      *
           .
       3201-ZAIKBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    診療年月見出し
      *****************************************************************
       3201-SRYYM-BODY-HEN-SEC     SECTION.
      *
      *    月遅れ分以外は処理抜ける
      *     IF      TOKEI-SYOKBN    NOT =   "3"
           IF      TOKEI-SYOKBN    =   "1"
               MOVE    TOKEI-SRYYMFLG  TO  WRK-SRYYMFLG
               MOVE    SPACE           TO  CSV-SRYYM
               GO  TO  3201-SRYYM-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
      *    対象年月編集
           MOVE    TOKEI-SRYYM         TO  WRK-SYMD(1:6)
           MOVE    "01"                TO  WRK-SYMD(7:2)
           PERFORM 31012-SEIWA-HEN-SEC
           STRING  "＜"                DELIMITED  BY  SIZE
                   WRK-HENYMDG(1:16)   DELIMITED  BY  SIZE
                   "＞"                DELIMITED  BY  SIZE
                               INTO    WRK-PRT-LINE
           END-STRING
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-SRYYMFLG  TO  WRK-SRYYMFLG
           MOVE    TOKEI-ZAIKBN    TO  WRK-ZAIKBN
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
           MOVE    TOKEI-SRYYM     TO  CSV-SRYYM
      *
           .
       3201-SRYYM-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    種別区分見出し
      *****************************************************************
       3201-SYUBETU-BODY-HEN-SEC   SECTION.
      *
      *    公害の場合は抜ける
           IF       ( WRK-TEISYUTU    =   "9" )
               MOVE    TOKEI-SYUBETUKBN    TO  WRK-SYUBETUKBN
               MOVE    WRK-SYUBETUKBN      TO  CSV-SYUBETUKBN
               MOVE    SPACE               TO  WRK-KBN
               GO  TO  3201-SYUBETU-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
           IF      TOKEI-SRYYM    <   CONST-H2004
               PERFORM 3201-SYUBETU-BODY-HEN-MAE-SEC
           ELSE
               PERFORM 3201-SYUBETU-BODY-HEN-ATO-SEC
           END-IF
      *
           .
       3201-SYUBETU-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    種別区分見出し
      *****************************************************************
       3201-SYUBETU-BODY-HEN-MAE-SEC   SECTION.
      *
           EVALUATE    TOKEI-TEISYUTU      ALSO    TOKEI-HKNKBN
               ALSO    TOKEI-SYUBETUKBN    ALSO    TOKEI-KBN
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "001"
                   MOVE    "＊医保（７０歳以上９割）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊医保単独（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "001"
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊医保（７０歳以上８割）と公費の併用"
                                                   TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊医保（７０歳以上７割）と公費の併用"
                                                   TO  WRK-PRT-LINE
                   END-IF
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    ANY
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊医保単独（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊医保単独（７０歳以上７割）"
                                                   TO  WRK-PRT-LINE
                   END-IF
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "001"
                   MOVE    "＊医保（本人）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊医保単独（本人）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "001"
                   MOVE    "＊医保（家族）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊医保単独（家族）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "001"
                   MOVE    "＊医保（３歳）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊医保単独（３歳）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "2" ALSO    "1" ALSO    ANY
                   MOVE    "＊老人９割"            TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "2" ALSO    "2" ALSO    ANY
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊老人８割"        TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊老人７割"        TO  WRK-PRT-LINE
                   END-IF
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    ANY
                   MOVE    "＊公費と公費の併用"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    ANY
                   MOVE    "＊公費単独"            TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊一般（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "2" ALSO    ANY
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊一般（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊一般（７０歳以上７割）"
                                                   TO  WRK-PRT-LINE
                   END-IF
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊一般被保険者"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊一般（３歳未満）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊退職（本人）"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "6" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "6" ALSO    ANY
                   MOVE    "＊退職（７０歳以上９割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "7" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "7" ALSO    ANY
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊退職（７０歳以上８割）"
                                                   TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊退職（７０歳以上７割）"
                                                   TO  WRK-PRT-LINE
                   END-IF
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "8" ALSO    ANY
                   MOVE    "＊退職（被扶養者）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "9" ALSO    ANY
                   MOVE    "＊退職（３歳未満）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "2" ALSO    "1" ALSO    ANY
               WHEN    "3" ALSO    "2" ALSO    "1" ALSO    ANY
                   MOVE    "＊老人９割"            TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "2" ALSO    "2" ALSO    ANY
               WHEN    "3" ALSO    "2" ALSO    "2" ALSO    ANY
                   IF      TOKEI-SRYYM     <     "200610"
                       MOVE    "＊老人８割"        TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "＊老人７割"        TO  WRK-PRT-LINE
                   END-IF
               WHEN    OTHER
                   GO  TO  3201-SYUBETU-BODY-HEN-MAE-EXT
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-SYUBETUKBN    TO  WRK-SYUBETUKBN
           MOVE    WRK-SYUBETUKBN      TO  CSV-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
           .
       3201-SYUBETU-BODY-HEN-MAE-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    種別区分見出し
      *****************************************************************
       3201-SYUBETU-BODY-HEN-ATO-SEC   SECTION.
      *
           EVALUATE    TOKEI-TEISYUTU      ALSO    TOKEI-HKNKBN
               ALSO    TOKEI-SYUBETUKBN    ALSO    TOKEI-KBN
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    "001"
               MOVE    "＊医保（７０歳以上一般・低所得）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊医保単独（７０歳以上一般・低所得）"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    "001"
                   MOVE    "＊医保（７０歳以上７割）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "2" ALSO    ANY
                   MOVE    "＊医保単独（７０歳以上７割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    "001"
                   MOVE    "＊医保（本人）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊医保単独（本人）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    "001"
                   MOVE    "＊医保（家族）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊医保単独（家族）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    "001"
                   MOVE    "＊医保（６歳）と公費の併用"
                                                   TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊医保単独（６歳）"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "3" ALSO    "1" ALSO    ANY
                   MOVE    "＊公費と公費の併用"    TO  WRK-PRT-LINE
               WHEN    "1" ALSO    "3" ALSO    "2" ALSO    ANY
                   MOVE    "＊公費単独"            TO  WRK-PRT-LINE
      *----(04.08.02)--ADD-START---
      *         WHEN    "1" ALSO    "3" ALSO    "3" ALSO    ANY
      *             MOVE    "＊公費と医保の併用（単独入力分）"
      *                                             TO  WRK-PRT-LINE
      *----(04.08.02)--ADD-END-----
               WHEN    "2" ALSO    "1" ALSO    "1" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "1" ALSO    ANY
                   MOVE    "＊一般（７０歳以上一般・低所得）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "2" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "2" ALSO    ANY
                   MOVE    "＊一般（７０歳以上７割）"
                                                   TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "3" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "3" ALSO    ANY
                   MOVE    "＊一般"                TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "4" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "4" ALSO    ANY
                   MOVE    "＊一般（６歳）"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "5" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "5" ALSO    ANY
                   MOVE    "＊退職（本人）"        TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "8" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "8" ALSO    ANY
                   MOVE    "＊退職（被扶養者）"    TO  WRK-PRT-LINE
               WHEN    "2" ALSO    "1" ALSO    "9" ALSO    ANY
               WHEN    "3" ALSO    "1" ALSO    "9" ALSO    ANY
                   MOVE    "＊退職（６歳）"        TO  WRK-PRT-LINE
               WHEN    "3" ALSO    "2" ALSO    "1" ALSO    ANY
               WHEN    "6" ALSO    "2" ALSO    "1" ALSO    ANY
                   MOVE    "＊後期高齢者９割"      TO  WRK-PRT-LINE
               WHEN    "3" ALSO    "2" ALSO    "2" ALSO    ANY
               WHEN    "6" ALSO    "2" ALSO    "2" ALSO    ANY
                   MOVE    "＊後期高齢者７割"      TO  WRK-PRT-LINE
               WHEN    OTHER
                   GO  TO  3201-SYUBETU-BODY-HEN-ATO-EXT
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-SYUBETUKBN    TO  WRK-SYUBETUKBN
           MOVE    WRK-SYUBETUKBN      TO  CSV-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
      *
           .
       3201-SYUBETU-BODY-HEN-ATO-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    公害
      *****************************************************************
       3201-KIGO-BODY-HEN-SEC       SECTION.
      *
      *    公害以外は抜ける
           IF        WRK-TEISYUTU   NOT =   "9"
               GO  TO  3201-KIGO-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE            TO  WRK-PRT-LINE
      *
           MOVE    "＊"             TO  WRK-PRT-LINE(1:2)
           MOVE    TOKEI-KIGO       TO  WRK-PRT-LINE(3:)
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-KIGO       TO  WRK-KIGO
      *
           .
       3201-KIGO-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    詳細区分
      *****************************************************************
       3201-KBN-BODY-HEN-SEC       SECTION.
      *
      *    国保、特別療養費の場合は抜ける、公害も、後期高齢者も
           IF        ( WRK-TEISYUTU    =   "2" )
               OR    ( WRK-TEISYUTU    =   "3" )
               OR    ( WRK-TEISYUTU    =   "6" )
               OR    ( WRK-TEISYUTU    =   "9" )
               MOVE    TOKEI-KBN       TO  WRK-KBN
               MOVE    SPACE           TO  CSV-KBN
               GO  TO  3201-KBN-BODY-HEN-EXT
           END-IF
      *
           MOVE    SPACE               TO  WRK-PRT-LINE
      *
           EVALUATE    TOKEI-HKNKBN    ALSO    TOKEI-KBN
               WHEN    "1"     ALSO    "001"
                   MOVE    TOKEI-KBN       TO  WRK-KBN
                   MOVE    WRK-KBN         TO  CSV-KBN
                   GO  TO  3201-KBN-BODY-HEN-EXT
               WHEN    "1"     ALSO    "002"
                   IF      TOKEI-SRYYM     <   CONST-H2010
                       MOVE    "・０１（政）"          TO  WRK-PRT-LINE
                   ELSE
                       MOVE    "・０１（協会）"        TO  WRK-PRT-LINE
                   END-IF
               WHEN    "1"     ALSO    "003"
                   MOVE    "・０２（船）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "004"
                   MOVE    "・０３（日）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "005"
                   MOVE    "・０４（日特）"            TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "006"
                   MOVE    "・３１〜３４（共）"        TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "007"
                   MOVE    "・０６（組）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "008"
                   MOVE    "・０７（自）"              TO  WRK-PRT-LINE
               WHEN    "1"     ALSO    "009"
                   MOVE    "・６３・７２〜７５（退）"  TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "001"
                   MOVE    "・老人と公費の併用"        TO  WRK-PRT-LINE
               WHEN    "2"     ALSO    "002"
                   MOVE    "・老人単独"                TO  WRK-PRT-LINE
               WHEN    "3"     ALSO    ANY
      *            公費区分
                   PERFORM 3201-HKNNUM-HENSYU-SEC
               WHEN    OTHER
                   GO  TO  3201-KBN-BODY-HEN-EXT
           END-EVALUATE
      *
      *    見出し行セット
           PERFORM 320-BODY-SET-SEC
      *
           MOVE    TOKEI-KBN       TO  WRK-KBN
           MOVE    WRK-KBN         TO  CSV-KBN
      *
           .
       3201-KBN-BODY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       3201-HKNNUM-HENSYU-SEC      SECTION.
      *
      *    保険番号読込み
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    TOKEI-KBN       TO  HKN-HKNNUM
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  WRK-PATH-TABLE
           MOVE    "key2"          TO  WRK-PATH-PATHNAME
           PERFORM 900-HKNNUM-SELECT-SEC
      *
           PERFORM UNTIL   FLG-HKNNUM      =   1
               IF        ( HKN-TEKSTYMD    <=  WRK-SYSYMD )
                   AND   ( HKN-TEKEDYMD    >=  WRK-SYSYMD )
                   IF      HKN-HKNNUM  =   "027"
      *                老人保険のとき
                       IF    ( SYS-1001-HOSPSBT    =   HKN-PAYKBN(1:1) )
                       AND   ( SYS-1001-ROUPAYKBN  =   HKN-PAYKBN(2:1) )
                           STRING  "（"            DELIMITED  BY  SIZE
                               HKN-TANSEIDONAME    DELIMITED  BY  SPACE
                                   "）"            DELIMITED  BY  SIZE
                                                   INTO   WRK-PRT-LINE
                           END-STRING
                           MOVE    1                   TO  FLG-HKNNUM
                       END-IF
                   ELSE
      *                特定疾患のとき
                       IF      HKN-HKNNUM  =   "051"
                           MOVE    "（特定疾患）"  TO   WRK-PRT-LINE
                       ELSE
      *                その他の保険のとき
                           STRING  "（"            DELIMITED  BY  SIZE
                               HKN-TANSEIDONAME    DELIMITED  BY  SPACE
                                   "）"            DELIMITED  BY  SIZE
                                                   INTO   WRK-PRT-LINE
                           END-STRING
                       END-IF
                       MOVE    1                   TO  FLG-HKNNUM
                   END-IF
               END-IF
      *
               IF      FLG-HKNNUM           =   ZERO
                   PERFORM 900-HKNNUM-FETCH-SEC
               ELSE
                   MOVE    HKN-HBTNUM      TO  WRK-Z4
                   INITIALIZE                  ORCSKANACHKAREA
                   MOVE    "2"             TO  KANACHK-SYORI
                   MOVE    "・"      TO  KANACHK-MAE-INPUT(1:2)
                   MOVE    WRK-Z4 (3:2)    TO  KANACHK-MAE-INPUT(3:2)
                   MOVE    WRK-PRT-LINE    TO  KANACHK-MAE-INPUT(5:)
                   CALL    "ORCSKANACHK"   USING   ORCSKANACHKAREA
                   IF          KANACHK-RC      =   0
                       MOVE    KANACHK-OUT-INPUT   TO  WRK-PRT-LINE
                   END-IF
               END-IF 
           END-PERFORM
      *
           PERFORM 900-HKNNUM-CLOSE-SEC
      *
           .
       3201-HKNNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    公費保険区分編集処理
      *****************************************************************
       3201-HKNNUM-HENSYU2-SEC      SECTION.
      *
      *    保険番号読込み
           INITIALIZE                  HKNNUM-REC
           MOVE    SPA-HOSPNUM     TO  HKN-HOSPNUM
           MOVE    TOKEI-KBN       TO  HKN-HKNNUM
           MOVE    HKNNUM-REC      TO  MCPDATA-REC
           MOVE    "tbl_hknnum"    TO  WRK-PATH-TABLE
           MOVE    "key2"          TO  WRK-PATH-PATHNAME
           PERFORM 900-HKNNUM-SELECT-SEC
      *
           PERFORM UNTIL   FLG-HKNNUM      =   1
               IF        ( HKN-TEKSTYMD    <=  WRK-SYSYMD )
                   AND   ( HKN-TEKEDYMD    >=  WRK-SYSYMD )
                   IF      HKN-HKNNUM  =   "027"
      *                老人保険のとき
                       IF    ( SYS-1001-HOSPSBT    =   HKN-PAYKBN(1:1) )
                       AND   ( SYS-1001-ROUPAYKBN  =   HKN-PAYKBN(2:1) )
                           MOVE    HKN-TANSEIDONAME    TO  CSV-KBNMEI
                           MOVE    1                   TO  FLG-HKNNUM
                       END-IF
                   ELSE
      *                特定疾患のとき
                       IF      HKN-HKNNUM  =   "051"
                           MOVE    "特定疾患"          TO  CSV-KBNMEI
                       ELSE
      *                その他の保険のとき
                           MOVE    HKN-TANSEIDONAME    TO  CSV-KBNMEI
                       END-IF
                       MOVE    1                   TO  FLG-HKNNUM
                   END-IF
               END-IF
      *
               IF      FLG-HKNNUM           =   ZERO
                   PERFORM 900-HKNNUM-FETCH-SEC
               END-IF 
           END-PERFORM
      *
           PERFORM 900-HKNNUM-CLOSE-SEC
      *
           .
       3201-HKNNUM-HENSYU2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-NYUIN-SEC SECTION.
      *
           ADD     TOKEI-RYOYOHI       TO  SUM-GOK-RYOYOHI(1)
                                           SUM-GOK-RYOYOHI(2)
                                           SUM-GOK-RYOYOHI(3)
           ADD     TOKEI-SHOKUJIFTN    TO  SUM-GOK-SHOKUJIFTN(1)
                                           SUM-GOK-SHOKUJIFTN(2)
                                           SUM-GOK-SHOKUJIFTN(3)
      *
           MOVE    SPACE           TO  WRK-PRT5-AREA
           MOVE    WRK-PRT4-G(IDX) TO  WRK-PRT5-G
           MOVE    SPACE           TO  WRK-PRT5-FILLER1
           MOVE    "食事療養費"    TO  WRK-PRT5-FILLER2
           MOVE    TOKEI-RYOYOHI   TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-PRT5-RYOYOHI
           MOVE    "円"            TO  WRK-PRT5-FILLER3
           MOVE    "食事負担金"    TO  WRK-PRT5-FILLER4
           MOVE    TOKEI-SHOKUJIFTN    TO  WRK-Z9
           MOVE    WRK-Z9          TO  WRK-PRT5-SHOKUJIFTN
           MOVE    "円"            TO  WRK-PRT5-FILLER5
           MOVE    WRK-PRT5-AREA   TO  WRK-PRT-LINE
           PERFORM 320-BODY-SET-SEC
           .
       3202-BODY-NYUIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-SUB-HEN-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-HKNJANUM    NOT =   TOKEI-HKNJANUM )
                   OR    ( WRK-SYOKBN      NOT =   TOKEI-SYOKBN )
                   OR    ( WRK-HKNJANUM2   NOT =   TOKEI-HKNJANUM2 )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( WRK-ZAIKBN      NOT =   TOKEI-ZAIKBN )
                   OR    ( WRK-HKNKBN      NOT =   TOKEI-HKNKBN )
                   OR    ( WRK-SYUBETUKBN  NOT =   TOKEI-SYUBETUKBN )
                   OR    ( WRK-KBN         NOT =   TOKEI-KBN )
                   OR    ( WRK-KIGO        NOT =   TOKEI-KIGO )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               IF      WRK-NYUGAIKBN   =   "2"
                   PERFORM 3202-BODY-NYUIN-SEC
               ELSE
                   MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
                   PERFORM 320-BODY-SET-SEC
               END-IF
           END-IF
      *
           .
       3202-BODY-SUB-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       3202-BODY-SUB-HEN2-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-HKNJANUM    NOT =   TOKEI-HKNJANUM )
                   OR    ( WRK-HKNJANUM2   NOT =   TOKEI-HKNJANUM2 )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( WRK-HKNKBN      NOT =   TOKEI-HKNKBN )
                   OR    ( WRK-SYUBETUKBN  NOT =   TOKEI-SYUBETUKBN )
                   OR    ( WRK-KBN         NOT =   TOKEI-KBN )
                   OR    ( WRK-KIGO        NOT =   TOKEI-KIGO )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *----(04.08.03)--UPD-START---
      *    アフターケアは対象外
           IF      IDX                 >   ZERO
               IF    ( WRK-NYUGAIKBN       =   "2" )   AND
      *               ( WRK-TEISYUTU    NOT =   "7" )   AND
                     ( WRK-TEISYUTU    NOT =   "8" )
      *----(04.08.03)--UPD-END-----
                   PERFORM 3202-BODY-NYUIN-SEC
               ELSE
                   MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
                   PERFORM 320-BODY-SET-SEC
               END-IF
           END-IF
      *
           .
       3202-BODY-SUB-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理3
      *****************************************************************
       3202-BODY-SUB-HEN3-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               IF      WRK-NYUGAIKBN   =   "2"
                   PERFORM 3202-BODY-NYUIN-SEC
               ELSE
                   MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
                   PERFORM 320-BODY-SET-SEC
               END-IF
           END-IF
      *
           .
       3202-BODY-SUB-HEN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理５
      *****************************************************************
       3202-BODY-SUB-HEN5-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-SYOKBN      NOT =   TOKEI-SYOKBN )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
               PERFORM 320-BODY-SET-SEC
           END-IF
      *
           .
       3202-BODY-SUB-HEN5-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-SUB-HEN6-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT4-AREA
           MOVE    ZERO            TO  IDX
      *
           PERFORM UNTIL ( FLG-TOKEI       =   1 )
                   OR    ( WRK-SKYKBN      NOT =   TOKEI-SKYKBN )
                   OR    ( WRK-TEISYUTU    NOT =   TOKEI-TEISYUTU )
                   OR    ( WRK-HKNJANUM    NOT =   TOKEI-HKNJANUM )
                   OR    ( WRK-SYOKBN      NOT =   TOKEI-SYOKBN )
                   OR    ( WRK-HKNJANUM2   NOT =   TOKEI-HKNJANUM2 )
                   OR    ( WRK-SRYYMFLG    NOT =   TOKEI-SRYYMFLG )
                   OR    ( WRK-NYUGAIKBN   NOT =   TOKEI-NYUGAIKBN )
      *        患者編集
               PERFORM 3202-BODY-SUB-SET-SEC
      *        患者別集計ファイル読み込み
               PERFORM 900-TOKEI-NEXT-SEC
           END-PERFORM
      *
      *    患者情報セット
           IF      IDX                 >   ZERO
               MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
               PERFORM 320-BODY-SET-SEC
           END-IF
      *
           .
       3202-BODY-SUB-HEN6-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3202-BODY-SUB-SET-SEC       SECTION.
      *
           ADD     1                   TO  IDX
           MOVE    SPACE               TO  WRK-PRT4-FILLER1(IDX)
           MOVE    SPACE               TO  WRK-PRT4-FILLER2(IDX)
           MOVE    SPACE               TO  WRK-PRT4-FILLER5(IDX)
           MOVE    "日"                TO  WRK-PRT4-FILLER3(IDX)
           IF    ( TOKEI-TEISYUTU      =   "7" )   OR
                 ( TOKEI-TEISYUTU      =   "8" )   OR
      *----(04.08.03)--ADD-START---
                 ( TOKEI-TEISYUTU      =   "10" )  OR
      *----(04.08.03)--ADD-END-----
                 ( TOKEI-TEISYUTU      =   "9" )
               MOVE    "円"                TO  WRK-PRT4-FILLER4(IDX)
           ELSE
               MOVE    "点"                TO  WRK-PRT4-FILLER4(IDX)
           END-IF
      *
           MOVE    TOKEI-PTNUM         TO  WRK-PRT4-PTNUM(IDX)
      *
           IF      TOKEI-SKYKBN        =   "3"
               MOVE    "    -"             TO  WRK-PRT4-NISSU(IDX)
               MOVE    "       -"          TO  WRK-PRT4-TENSU(IDX)
               MOVE    TOKEI-PTNUM         TO  CSV-PTNUM
               MOVE    TOKEI-RENNUM        TO  CSV-RENNUM
               MOVE    "0"                 TO  CSV-NISSU
                                               CSV-TENSU
                                               CSV-RYOYOHI
                                               CSV-SHOKUJIFTN
           ELSE
               MOVE    TOKEI-NISSU         TO  WRK-Z6
               MOVE    WRK-Z6              TO  WRK-PRT4-NISSU(IDX)
               MOVE    TOKEI-TENSU         TO  WRK-Z8
               MOVE    WRK-Z8              TO  WRK-PRT4-TENSU(IDX)
               MOVE    TOKEI-PTNUM         TO  CSV-PTNUM
               MOVE    TOKEI-RENNUM        TO  CSV-RENNUM
               MOVE    TOKEI-NISSU         TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-NISSU
               MOVE    TOKEI-TENSU         TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-TENSU
               MOVE    TOKEI-RYOYOHI       TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-RYOYOHI
               MOVE    TOKEI-SHOKUJIFTN    TO  WRK-SURYO
               PERFORM 400-ZHEN-SURYO-SET-SEC
               MOVE    WRK-HENSURYO        TO  CSV-SHOKUJIFTN
           END-IF
      *
      *    患者名セット
           MOVE    TOKEI-NAME          TO  WRK-PRT4-NAME(IDX)
           IF    ( SPA-ENCODING    =   2 )
               INITIALIZE                  ORCSKANACHK2AREA
               MOVE    "1"             TO  KANACHK2-SYORI
               MOVE    WRK-PRT4-NAME(IDX)
                                       TO  KANACHK2-MAE-INPUT
               CALL    "ORCSKANACHK2"      USING
                                           ORCSKANACHK2AREA
               MOVE    KANACHK2-MAE-INPUT
                                       TO  WRK-PRT4-NAME(IDX)
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    WRK-PRT4-NAME(IDX)  TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-MAE-INPUT   TO  WRK-PRT4-NAME(IDX)
           END-IF
           MOVE    WRK-PRT4-NAME(IDX)      TO  CSV-NAME
           INSPECT WRK-PRT4-NAME(IDX)  REPLACING  ALL "  " BY  "　"
      *
      *    ＣＳＶファイル出力
           INITIALIZE                RECECSV-REC
           STRING  CSV-SKYYM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYKA         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYOKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TEISYUTU      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANUM      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANAME     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NYUGAIKBN     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-ZAIKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYYM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYUBETUKBN    DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-KBN           DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-PTNUM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-RENNUM        DELIMITED  BY  SIZE
                   ","               DELIMITED  BY  SIZE
                   CSV-NAME          DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NISSU         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TENSU         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-RYOYOHI       DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SHOKUJIFTN    DELIMITED  BY  SPACE
                             INTO    RECECSV-REC
           END-STRING
      *
           WRITE   RECECSV-REC
      *
           MOVE    SPACE           TO  CSV-SKYYMHEN
                                       CSV-SYOKBNMEI
                                       CSV-TEISYUTUMEI
                                       CSV-NYUGAIMEI
                                       CSV-ZAIKBNMEI
                                       CSV-SRYYMHEN
                                       CSV-SYUBETUMEI
                                       CSV-KBNMEI
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  WRK-SYMD
           MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
           MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
           MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
           MOVE    WRK-HEN-YMD-OT(1:7)
                                   TO  CSV-SKYYMHEN
      *
           EVALUATE    CSV-SYOKBN
               WHEN    "1"
                   MOVE    "通常"          TO  CSV-SYOKBNMEI
               WHEN    "2"
                   MOVE    "返戻"          TO  CSV-SYOKBNMEI
               WHEN    "3"
                   MOVE    "月遅れ"        TO  CSV-SYOKBNMEI
               WHEN    "4"
                   MOVE    "請求しない"    TO  CSV-SYOKBNMEI
               WHEN    "5"
                   MOVE    "未請求患者"    TO  CSV-SYOKBNMEI
           END-EVALUATE
      *
           EVALUATE    CSV-TEISYUTU
               WHEN    "1"
                   MOVE    "社保"          TO  CSV-TEISYUTUMEI
               WHEN    "2"
                   MOVE    "国保"          TO  CSV-TEISYUTUMEI
               WHEN    "3"
                   MOVE    "特別療養費"    TO  CSV-TEISYUTUMEI
               WHEN    "4"
                   MOVE    "未請求設定"    TO  CSV-TEISYUTUMEI
               WHEN    "5"
                   MOVE    "未請求解除"    TO  CSV-TEISYUTUMEI
               WHEN    "6"
                   MOVE    "広域連合"      TO  CSV-TEISYUTUMEI
               WHEN    "7"
                   MOVE    "短期傷病"      TO  CSV-TEISYUTUMEI
               WHEN    "8"
                   MOVE    "アフターケア"  TO  CSV-TEISYUTUMEI
               WHEN    "9"
                   MOVE    "公害"          TO  CSV-TEISYUTUMEI
      *----(04.08.03)--ADD-START---
               WHEN    "10"
                   EVALUATE    TOKEI-HKNJANUM(1:1)
                   WHEN    "1"
                       MOVE    "自賠責新様式"  TO  CSV-TEISYUTUMEI
                   WHEN    "2"
                       MOVE    "自賠責従来様式"
                                               TO  CSV-TEISYUTUMEI
                   WHEN    "3"
                       MOVE    "自賠責第三者行為"
                                               TO  CSV-TEISYUTUMEI
                   END-EVALUATE
      *----(04.08.03)--ADD-END-----
           END-EVALUATE
      *
           EVALUATE    CSV-NYUGAIKBN
               WHEN    "2"
                   MOVE    "外"        TO  CSV-NYUGAIMEI
               WHEN    "1"
                   MOVE    "入"        TO  CSV-NYUGAIMEI
           END-EVALUATE
      *
           EVALUATE    CSV-ZAIKBN
               WHEN    "1"
                   MOVE    "その他"        TO  CSV-ZAIKBNMEI
               WHEN    "2"
                   MOVE    "在総診・在医総"    TO  CSV-ZAIKBNMEI
           END-EVALUATE
      *
           IF      CSV-SRYYM       NOT =   SPACE
               MOVE    CSV-SRYYM       TO  WRK-SYMD
               MOVE    WRK-SYY         TO  WRK-HEN-YY-OT
               MOVE    WRK-SMM         TO  WRK-HEN-MM-OT
               MOVE    WRK-SDD         TO  WRK-HEN-DD-OT
               MOVE    WRK-HEN-YMD-OT(1:7)
                                   TO  CSV-SRYYMHEN
           END-IF
      *
           EVALUATE    CSV-TEISYUTU      ALSO    TOKEI-HKNKBN
               ALSO    CSV-SYUBETUKBN
               WHEN    "1" ALSO    "1" ALSO    "1"
                   MOVE    "医保（７０歳以上一般・低所得）"
                                           TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "1" ALSO    "2"
                   MOVE    "医保（７０歳以上７割）"
                                           TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "1" ALSO    "3"
                   MOVE    "医保（本人）"  TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "1" ALSO    "4"
                   MOVE    "医保（家族）"  TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "1" ALSO    "5"
                   MOVE    "医保（６歳）"  TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "3" ALSO    "1"
                   MOVE    "公費と公費の併用"
                                           TO  CSV-SYUBETUMEI
               WHEN    "1" ALSO    "3" ALSO    "2"
                   MOVE    "公費単独"      TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "1"
               WHEN    "3" ALSO    "1" ALSO    "1"
                   MOVE    "一般（７０歳以上一般・低所得）"
                                           TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "2"
               WHEN    "3" ALSO    "1" ALSO    "2"
                   MOVE    "一般（７０歳以上７割）"
                                           TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "3"
               WHEN    "3" ALSO    "1" ALSO    "3"
                   MOVE    "一般"          TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "4"
               WHEN    "3" ALSO    "1" ALSO    "4"
                   MOVE    "一般（６歳）"  TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "5"
               WHEN    "3" ALSO    "1" ALSO    "5"
                   MOVE    "退職（本人）"  TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "8"
               WHEN    "3" ALSO    "1" ALSO    "8"
                   MOVE    "退職（被扶養者）"
                                           TO  CSV-SYUBETUMEI
               WHEN    "2" ALSO    "1" ALSO    "9"
               WHEN    "3" ALSO    "1" ALSO    "9"
                   MOVE    "退職（６歳）"  TO  CSV-SYUBETUMEI
               WHEN    "3" ALSO    "2" ALSO    "1"
               WHEN    "6" ALSO    "2" ALSO    "1"
                   MOVE    "後期高齢者９割"
                                           TO  CSV-SYUBETUMEI
               WHEN    "3" ALSO    "2" ALSO    "2"
               WHEN    "6" ALSO    "2" ALSO    "2"
                   MOVE    "後期高齢者７割"
                                           TO  CSV-SYUBETUMEI
           END-EVALUATE
      *
           EVALUATE    TOKEI-HKNKBN    ALSO    CSV-KBN
               WHEN    "1"     ALSO    "001"
                   MOVE    "公費との併用"  TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "002"
                   MOVE    "協会"          TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "003"
                   MOVE    "船"            TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "004"
                   MOVE    "日"            TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "005"
                   MOVE    "日特"          TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "006"
                   MOVE    "共"            TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "007"
                   MOVE    "組"            TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "008"
                   MOVE    "自"            TO  CSV-KBNMEI
               WHEN    "1"     ALSO    "009"
                   MOVE    "退"            TO  CSV-KBNMEI
               WHEN    "2"     ALSO    "001"
                   MOVE    "老人と公費の併用"
                                           TO  CSV-KBNMEI
               WHEN    "2"     ALSO    "002"
                   MOVE    "老人"          TO  CSV-KBNMEI
               WHEN    "3"     ALSO    ANY
      *            公費区分
                   PERFORM 3201-HKNNUM-HENSYU2-SEC
           END-EVALUATE
      *
      *    ＣＳＶファイル出力
           INITIALIZE                WRK-REC
           STRING  CSV-SKYYMHEN      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYKA         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYOKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYOKBNMEI     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TEISYUTU      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TEISYUTUMEI   DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANUM      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-HKNJANAME     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NYUGAIKBN     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NYUGAIMEI     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-ZAIKBN        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-ZAIKBNMEI     DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SRYYMHEN      DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYUBETUKBN    DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SYUBETUMEI    DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-KBN           DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-KBNMEI        DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-PTNUM         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-RENNUM        DELIMITED  BY  SIZE
                   ","               DELIMITED  BY  SIZE
                   CSV-NAME          DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   TOKEI-KANANAME    DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-NISSU         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-TENSU         DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-RYOYOHI       DELIMITED  BY  SPACE
                   ","               DELIMITED  BY  SIZE
                   CSV-SHOKUJIFTN    DELIMITED  BY  SPACE
                             INTO    WRK-REC
           END-STRING
      *
           PERFORM 500-TOUKEICSV-SEC
      *
      *    集計
           ADD     1                   TO  SUM-GOK-KENSU(1)
                                           SUM-GOK-KENSU(2)
                                           SUM-GOK-KENSU(3)
           ADD     TOKEI-NISSU         TO  SUM-GOK-NISSU(1)
                                           SUM-GOK-NISSU(2)
                                           SUM-GOK-NISSU(3)
           ADD     TOKEI-TENSU         TO  SUM-GOK-TENSU(1)
                                           SUM-GOK-TENSU(2)
                                           SUM-GOK-TENSU(3)
           MOVE    TOKEI-TEISYUTU      TO  SUM-GOK-TEISYUTU
      *
      *----(04.08.03)--UPD-START---
      *    アフターケアは対象外
           IF    ( WRK-NYUGAIKBN       =   "2" )   AND
      *           ( TOKEI-TEISYUTU  NOT =   "7" )   AND
                 ( TOKEI-TEISYUTU  NOT =   "8" )
      *----(04.08.03)--UPD-END-----
               PERFORM 3202-BODY-NYUIN-SEC
               MOVE    ZERO            TO  IDX
           ELSE
      *        患者情報セット
               IF      IDX             =   2
                   MOVE    WRK-PRT4-AREA   TO  WRK-PRT-LINE
                   PERFORM 320-BODY-SET-SEC
                   MOVE    SPACE           TO  WRK-PRT4-AREA
                   MOVE    ZERO            TO  IDX
               END-IF
           END-IF
      *
           .
       3202-BODY-SUB-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    区分合計行セット
      *****************************************************************
       3202-KBN-BODY-END-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-PRT-LINE-AREA
      *
           MOVE    SUM-GOK-KENSU(1)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(1)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(1)    TO  WRK-PRT-TENSU
           MOVE    SUM-GOK-RYOYOHI(1)  TO  WRK-PRT-RYOYOHI
           MOVE    SUM-GOK-SHOKUJIFTN(1)   TO  WRK-PRT-SHOKUJIFTN
           MOVE    "　　　　　　　　　小計"
                                       TO  WRK-PRT-GOKEI
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    ZERO                TO  SUM-GOK-TBL(1)
      *
           .
       3202-KBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    合計行セット
      *****************************************************************
       3203-BODY-END-SET-SEC       SECTION.
      *
           MOVE    SPACE           TO  WRK-PRT3-AREA
      *
           MOVE    SPACE           TO  WRK-PRT3-FILLER1
           MOVE    "件"            TO  WRK-PRT3-FILLER2
           MOVE    "日"            TO  WRK-PRT3-FILLER3
           IF    ( SUM-GOK-TEISYUTU    =   "7" )   OR
                 ( SUM-GOK-TEISYUTU    =   "8" )   OR
      *----(04.08.03)--ADD-START---
                 ( SUM-GOK-TEISYUTU    =   "10" )  OR
      *----(04.08.03)--ADD-END-----
                 ( SUM-GOK-TEISYUTU    =   "9" )
               MOVE    "円"            TO  WRK-PRT3-FILLER4
           ELSE
               MOVE    "点"            TO  WRK-PRT3-FILLER4
           END-IF
           MOVE    WRK-PRT-GOKEI   TO  WRK-PRT3-GOKEI
           MOVE    WRK-PRT-KENSU   TO  WRK-Z6
           MOVE    WRK-Z6          TO  WRK-PRT3-KENSU
           IF      WRK-SKYKBN      =   "3"
               MOVE    "    -"         TO  WRK-PRT3-NISSU
               MOVE    "       -"      TO  WRK-PRT3-TENSU
           ELSE
               MOVE    WRK-PRT-NISSU   TO  WRK-Z6
               MOVE    WRK-Z6          TO  WRK-PRT3-NISSU
               MOVE    WRK-PRT-TENSU   TO  WRK-Z8
               MOVE    WRK-Z8          TO  WRK-PRT3-TENSU
           END-IF
      *
           IF    ( WRK-PRT-RYOYOHI     NOT =  0 )  OR
                 ( WRK-PRT-SHOKUJIFTN  NOT =  0 )
               MOVE    SPACE           TO  WRK-PRT3-FILLER5
               MOVE    "食事療養費"    TO  WRK-PRT3-FILLER6
               MOVE    WRK-PRT-RYOYOHI     TO  WRK-Z9
               MOVE    WRK-Z9          TO  WRK-PRT3-RYOYOHI
               MOVE    "円"            TO  WRK-PRT3-FILLER7
               MOVE    "食事負担金"    TO  WRK-PRT3-FILLER8
               MOVE    WRK-PRT-SHOKUJIFTN  TO  WRK-Z9
               MOVE    WRK-Z9          TO  WRK-PRT3-SHOKUJIFTN
               MOVE    "円"            TO  WRK-PRT3-FILLER9
           END-IF
           MOVE    WRK-PRT3-AREA   TO  WRK-PRT-LINE
      *
      *    合計行セット
           PERFORM 320-BODY-SET-SEC
      *
           .
       3203-BODY-END-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3203-BODY-HEN-END-SEC       SECTION.
      *
      *    種別合計
           IF        ( WRK-KBN         =   "001" )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-SYUBETUKBN
           END-IF
      *
      *    公害合計
           IF      TOKEI-KIGO          NOT =   WRK-KIGO
               MOVE    SPACE           TO  WRK-KIGO
           END-IF
      *
      *    保険区分合計
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( TOKEI-HKNKBN    NOT =   WRK-HKNKBN )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-HKNKBN-BODY-END-SEC
           END-IF
      *
      *    レセプト合計、在総診合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN )
               OR    ( FLG-TOKEI       =   1 )
               IF      WRK-TEISYUTU    =   "1"
                   PERFORM 3203-ZAIKBN-BODY-END-SEC
                   IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                       OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                       OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
                       OR    ( FLG-TOKEI       =   1 )
                       OR    ( CNT-LINE        >=  55 )
                       PERFORM 390-PRINT-OUT-SEC
                       PERFORM 310-HEAD-SET-SEC
                   END-IF
               ELSE
                   PERFORM 3203-ZAIKBN-BODY-END-SEC
                   IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                       OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                       OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                       OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                       OR    ( FLG-TOKEI       =   1 )
                       OR    ( CNT-LINE        >=  55 )
                       PERFORM 390-PRINT-OUT-SEC
                       PERFORM 310-HEAD-SET-SEC
                   END-IF
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理２
      *****************************************************************
       3203-BODY-HEN-END2-SEC      SECTION.
      *
      *    種別合計
           IF        ( WRK-KBN         =   "001" )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    公害合計
           IF      TOKEI-KIGO          NOT =   WRK-KIGO
               MOVE    SPACE               TO  WRK-KIGO
           END-IF
      *
      *    保険区分合計
           IF        ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( TOKEI-HKNKBN    NOT =   WRK-HKNKBN )
               OR    ( FLG-TOKEI       =   1 )
               MOVE    SPACE           TO  WRK-HKNKBN
               MOVE    SPACE           TO  WRK-SYUBETUKBN
               MOVE    SPACE           TO  WRK-KBN
           END-IF
      *
      *    レセプト合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-HKNJANUM2 NOT =   WRK-HKNJANUM2 )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END2-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理３
      *****************************************************************
       3203-BODY-HEN-END3-SEC      SECTION.
      *
      *    レセプト合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-SRYYMFLG  NOT =   WRK-SRYYMFLG )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END3-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理
      *****************************************************************
       3203-BODY-HEN-END4-SEC      SECTION.
      *
      *    レセプト合計、在総診合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM2 NOT =   WRK-HKNJANUM2)
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( TOKEI-NYUGAIKBN NOT =   WRK-NYUGAIKBN )
               OR    ( TOKEI-ZAIKBN    NOT =   WRK-ZAIKBN )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                   OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                   OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END4-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理５
      *****************************************************************
       3203-BODY-HEN-END5-SEC      SECTION.
      *
      *    レセプト合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                   OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END5-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理６
      *****************************************************************
       3203-BODY-HEN-END6-SEC       SECTION.
      *
      *    レセプト合計、在総診合計
           IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
               OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
               OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
               OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
               OR    ( FLG-TOKEI       =   1 )
               PERFORM 3203-ZAIKBN-BODY-END-SEC
               IF        ( TOKEI-SKYKBN    NOT =   WRK-SKYKBN )
                   OR    ( TOKEI-HKNJANUM  NOT =   WRK-HKNJANUM )
                   OR    ( TOKEI-TEISYUTU  NOT =   WRK-TEISYUTU )
                   OR    ( TOKEI-SYOKBN    NOT =   WRK-SYOKBN )
                   OR    ( FLG-TOKEI       =   1 )
                   OR    ( CNT-LINE        >=  55 )
                   PERFORM 390-PRINT-OUT-SEC
                   PERFORM 310-HEAD-SET-SEC
               END-IF
           END-IF
      *
           .
       3203-BODY-HEN-END6-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    保険区分合計行セット
      *****************************************************************
       3203-HKNKBN-BODY-END-SEC    SECTION.
      *
      *    国保、特別療養費は処理抜ける、公害も抜ける、後期高齢も抜ける
           IF        ( WRK-TEISYUTU    =   "2" )
               OR    ( WRK-TEISYUTU    =   "3" )
               OR    ( WRK-TEISYUTU    =   "6" )
               OR    ( WRK-TEISYUTU    =   "9" )
               GO  TO  3203-HKNKBN-BODY-END-EXT
           END-IF
      *
           MOVE    SUM-GOK-KENSU(2)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(2)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(2)    TO  WRK-PRT-TENSU
           MOVE    SUM-GOK-RYOYOHI(2)  TO  WRK-PRT-RYOYOHI
           MOVE    SUM-GOK-SHOKUJIFTN(2)   TO  WRK-PRT-SHOKUJIFTN
           EVALUATE    WRK-HKNKBN
               WHEN    "1" 
                   MOVE    "　　　　　　　医保合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "2"
                   MOVE    "　　　　　　　老人合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "3"
                   MOVE    "　　　　　　　公費合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-PRT-GOKEI
           END-EVALUATE
      *
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    SPACE               TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    ZERO                TO  SUM-GOK-TBL(1)
           MOVE    ZERO                TO  SUM-GOK-TBL(2)
      *
           .
       3203-HKNKBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票編集<明細行>処理    在総診区分合計行セット
      *****************************************************************
       3203-ZAIKBN-BODY-END-SEC    SECTION.
      *
      *     MOVE    SPACE               TO  WRK-PRT-LINE-AREA
      *
           MOVE    SUM-GOK-KENSU(3)    TO  WRK-PRT-KENSU
           MOVE    SUM-GOK-NISSU(3)    TO  WRK-PRT-NISSU
           MOVE    SUM-GOK-TENSU(3)    TO  WRK-PRT-TENSU
           MOVE    SUM-GOK-RYOYOHI(3)  TO  WRK-PRT-RYOYOHI
           MOVE    SUM-GOK-SHOKUJIFTN(3)   TO  WRK-PRT-SHOKUJIFTN
           EVALUATE    WRK-ZAIKBN
               WHEN    "1"
                   MOVE    "　　　　　レセプト合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    "2"
                   MOVE    "在総診及び在医総分合計"
                                       TO  WRK-PRT-GOKEI
               WHEN    OTHER
                   MOVE    SPACE       TO  WRK-PRT-GOKEI
           END-EVALUATE
      *    合計行セット
           PERFORM 3203-BODY-END-SET-SEC
      *
           MOVE    SPACE               TO  WRK-ZAIKBN
           MOVE    SPACE               TO  WRK-HKNKBN
           MOVE    SPACE               TO  WRK-SYUBETUKBN
           MOVE    SPACE               TO  WRK-KBN
           MOVE    ZERO                TO  SUM-AREA
      *
           .
       3203-ZAIKBN-BODY-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    帳票印刷処理
      *****************************************************************
       390-PRINT-OUT-SEC           SECTION.
      *
      *    頁セット
           ADD     1               TO  CNT-PAGE
           MOVE    CNT-PAGE        TO  WRK-Z4
           MOVE    WRK-Z4          TO  WRK-PRT1-PAGE
      *    見出しセット
           MOVE    WRK-PRT1-AREA   TO  HCMSL55-MOJI(2)
           MOVE    WRK-PRT2-AREA   TO  HCMSL55-MOJI(4)
      *
      *    印刷処理
           INITIALIZE                              ORCSPRTAREA
           MOVE    "INS"                       TO  SPRT-MODE
           MOVE    LNK-PRTKANRI-RENNUM         TO  SPRT-RENNUM
           MOVE    LNK-PRTKANRI-TBL-KEY        TO  SPRT-TBL-KEY
           MOVE    LNK-PRTKANRI-TBL-GROUP      TO  SPRT-TBL-GROUP
           MOVE    WRK-PARA-DENPPRTYM          TO  SPRT-SRYYM
           MOVE    LNK-PRTKANRI-SKYYMD         TO  SPRT-SKYYMD
           MOVE    LNK-PRTKANRI-SHELLID        TO  SPRT-SHELLID
           MOVE    LNK-PRTKANRI-SHORI-RENNUM   TO  SPRT-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-PRIORITY       TO  SPRT-PRIORITY
           MOVE    "HCMSL55.red"               TO  SPRT-PRTID
           MOVE    "保険請求確認リスト"        TO  SPRT-TITLE
           MOVE    HCMSL55                     TO  SPRT-PRTDATA
           MOVE    LNK-PRTKANRI-TERMID         TO  SPRT-TERMID
           MOVE    LNK-PRTKANRI-OPID           TO  SPRT-OPID
           MOVE    LNK-PRTKANRI-PRTNM          TO  SPRT-PRTNM
           MOVE    "1"                         TO  SPRT-SITEKBN
           CALL    "ORCSPRT"           USING
                                       ORCSPRTAREA
                                       SPA-AREA
           IF      SPRT-RETURN         =   ZERO
               CONTINUE
           ELSE
               MOVE    "印刷ＤＢに更新できませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
           .
       390-PRINT-OUT-EXT.
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
           MOVE    LNK-DAY2-EDTYMD3    TO  WRK-HENYMDG
           INSPECT WRK-HENYMDG    REPLACING  ALL "  "  BY  "　"
           .
       31012-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   帳票Ｚ編集処理
      *****************************************************************
       400-ZHEN-SURYO-SET-SEC      SECTION.
      *
           MOVE    SPACE           TO  WRK-HENSURYO
           MOVE    ZERO            TO  CNT-KETA
           PERFORM VARYING CNT-KETA2   FROM    1   BY  1
                   UNTIL   CNT-KETA2   >   10
               IF      WRK-SURYO(CNT-KETA2:1)    NOT =   SPACE
                   COMPUTE CNT-KETA    =   CNT-KETA    +   1
                   MOVE    WRK-SURYO(CNT-KETA2:1)
                                           TO  WRK-HENSURYO(CNT-KETA:1)
               END-IF
           END-PERFORM
      *
           .
       400-ZHEN-SURYO-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付チェック・編集処理
      *****************************************************************
       5002-HIZUKE-CHK-SEC         SECTION.
      *
      *    日付画面チェックサブ
           ACCEPT  WRK-SYMD2       FROM    DATE
           MOVE    ZERO            TO  WRK-SYMD
           MOVE    WRK-SYMD2       TO  WRK-SYMD(3:)
           ADD     2000            TO  WRK-SYY
      *
           INITIALIZE                  ORCSGDAYAREA
           MOVE    WRK-SYMD        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           MOVE    SGDAY-OTDAY     TO  WRK-PRTYMD
      *
           ACCEPT  WRK-PRTTIME     FROM    TIME
           MOVE    SPACE               TO  WRK-HENTIME
           MOVE    WRK-PRTTIME(1:2)    TO  WRK-HH
           MOVE    ":"                 TO  WRK-FILLER1
           MOVE    WRK-PRTTIME(3:2)    TO  WRK-MM
           MOVE    ":"                 TO  WRK-FILLER2
           MOVE    WRK-PRTTIME(5:2)    TO  WRK-SS
           MOVE    SPACE               TO  WRK-PRTTIME
           MOVE    WRK-HENTIME         TO  WRK-PRTTIME
           .
       5002-HIZUKE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラー出力処理
      *****************************************************************
       500-ERR-HENSYU-SEC          SECTION.
      *
           OPEN    INPUT   RECEERR-FILE
           IF      STS-RECEERR         =   ZERO
               CONTINUE
           ELSE
               OPEN    OUTPUT                  RECEERR-FILE
      *
               MOVE    WRK-RECEERR         TO  RECEERR-REC
               WRITE   RECEERR-REC
               CLOSE   RECEERR-FILE
      *
      *        ジョブ管理開始処理
               MOVE    "JBE"               TO  SJOBKANRI-MODE
               INITIALIZE                      JOBKANRI-REC
               MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
               MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
               MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
               MOVE    WRK-RECEERR         TO  JOB-YOBI
               MOVE    "9999"              TO  JOB-ERRCD
               CALL    "ORCSJOB"               USING
                                               ORCSJOBKANRIAREA
                                               JOBKANRI-REC
                                               SPA-AREA
           END-IF
      *
           MOVE    1               TO  FLG-END
                                       FLG-ERR
      *
           .
       500-ERR-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    統計ＣＳＶ出力処理
      *****************************************************************
       500-TOUKEICSV-SEC           SECTION.
      *
           INITIALIZE                  ORCSTOUKEICSVAREA
           MOVE    "INS"           TO  STOUKEICSV-MODE
           MOVE    LNK-PRTKANRI-TBL-KEY
                                   TO  STOUKEICSV-TBL-KEY
           MOVE    LNK-PRTKANRI-RENNUM
                                   TO  STOUKEICSV-RENNUM
           MOVE    LNK-PRTKANRI-TBL-GROUP
                                   TO  STOUKEICSV-TBL-GROUP
           MOVE    LNK-PRTKANRI-SHORI-RENNUM
                                   TO  STOUKEICSV-SHORI-RENNUM
           MOVE    LNK-PRTKANRI-SHELLID
                                   TO  STOUKEICSV-SHELLID
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  STOUKEICSV-SRYYM
           MOVE    WRK-PARA-NYUGAIKBN
                                   TO  STOUKEICSV-NYUGAIKBN
           MOVE    ZERO            TO  STOUKEICSV-PTID
           MOVE    "/var/tmp/"     TO  STOUKEICSV-TO-FOLDER
           MOVE    WRK-TO-DATA     TO  STOUKEICSV-TO-DATA
           MOVE    "2"             TO  STOUKEICSV-CODE-TYPE
           MOVE    WRK-REC         TO  STOUKEICSV-CSVDATA
           MOVE    "保険請求確認リストＣＳＶ作成"
                                   TO  STOUKEICSV-TITLE
      *
           CALL    "ORCSTOUKEICSV"     USING
                                       ORCSTOUKEICSVAREA
                                       SPA-AREA
           IF      STOUKEICSV-RETURN   =   ZERO
               CONTINUE
           ELSE
               MOVE    "統計ＣＳＶＤＢに更新できませんでした"
                                      TO  WRK-TOUKEIERR
               PERFORM 500-ERR-HENSYU-SEC
               PERFORM 500-COBABORT-SEC
           END-IF
           .
      *
       500-TOUKEICSV-EXT.
           EXIT.
      *
      *****************************************************************
      *    異常終了処理
      *****************************************************************
       500-COBABORT-SEC          SECTION.
      *
           MOVE    SPACE           TO  WRK-RECEERR
           STRING  "ORCBG014"      DELIMITED  BY  SIZE
                   WRK-TOUKEIERR   DELIMITED  BY  SIZE
                   LOW-VALUE       DELIMITED  BY  SIZE
                                   INTO    WRK-RECEERR
           END-STRING
           CALL    "cobabort"      USING   WRK-RECEERR
           .
       500-COBABORT-EXT.
           EXIT.
      *
      *****************************************************************
      *    終了　　処理
      *****************************************************************
       300-END-SEC                 SECTION.
      *
           IF          CNT-PAGE    =   ZERO 
               MOVE    "処理対象のデータがありませんでした"
                                       TO  WRK-RECEERR
               PERFORM 500-ERR-HENSYU-SEC
           END-IF
      *
           DISPLAY "*** ORCBG014 END ***"
      *     
      *    ステップ管理終了処理
           MOVE    "STE"               TO  SJOBKANRI-MODE
           INITIALIZE                      JOBKANRI-REC
           MOVE    WRK-PARA-JOBID      TO  JOB-JOBID
           MOVE    WRK-PARA-SHELLID    TO  JOB-SHELLID
           MOVE    SPA-HOSPNUM         TO  JOB-HOSPNUM
           MOVE    CNT-PAGE            TO  JOB-UPDCNT
           CALL    "ORCSJOB"               USING
                                           ORCSJOBKANRIAREA
                                           JOBKANRI-REC
                                           SPA-AREA
      *
           PERFORM 900-DBDISCONNECT-SEC
           .
       300-END-EXT.
           EXIT.
      *
      *****************************************************************
      *    医療機関基本情報読み込み
      *****************************************************************
       900-SYS1001-READ-SEC        SECTION.
      *
           INITIALIZE                  SYS-1001-REC
           INITIALIZE                  ORCSADDSIGNAREA
           MOVE    SPA-HOSPNUM     TO  SYS-1001-HOSPNUM
           MOVE    "1001"          TO  SYS-1001-KANRICD
           MOVE    "*"             TO  SADDSIGN-STR
           CALL    "ORCSADDSIGN"       USING
                                       ORCSADDSIGNAREA
           MOVE    SADDSIGN-STR    TO  SYS-1001-KBNCD
           MOVE    WRK-SYSYMD      TO  SYS-1001-STYUKYMD
                                       SYS-1001-EDYUKYMD
           MOVE    SYS-1001-REC    TO  MCPDATA-REC
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF        ( MCP-RC      =   ZERO )
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syskanri"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF        ( MCP-RC      =   ZERO )
                   MOVE    ZERO            TO  FLG-SYSKANRI
                   MOVE    MCPDATA-REC     TO  SYS-1001-REC
               ELSE
                   MOVE    1               TO  FLG-SYSKANRI
                   INITIALIZE                  SYS-1001-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syskanri"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
            .
       900-SYS1001-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科読込
      *****************************************************************
       900-SYUKA-SELECT-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-SYUKA
           MOVE    SPACE               TO  WRK-KEY
      *
      *    請求管理ＤＢ検索
           MOVE    SPACE               TO  SYUKA-REC
           INITIALIZE                      SYUKA-REC
           MOVE    SPA-HOSPNUM         TO  SYUKA-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYM  TO  SYUKA-SRYYM
           MOVE    "YYYY"              TO  SYUKA-RECESYUBETU
           IF      WRK-PARA-NYUGAIKBN  =   "1"
               OR  WRK-PARA-NYUGAIKBN  =   "2"
               MOVE    WRK-PARA-NYUGAIKBN  TO  SYUKA-NYUGAIKBN
           ELSE
               MOVE    "%"                 TO  SYUKA-NYUGAIKBN
           END-IF
           MOVE    WRK-MISYUFLG        TO  SYUKA-MISEIKYU-FLG
           MOVE    SYUKA-REC           TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    "tbl_syuka"         TO  MCP-TABLE
           MOVE    "key16"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-SYUKA-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUKA
           END-IF
           .
       900-SYUKA-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科読込
      *****************************************************************
       900-SYUKA-READ-SEC          SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM UNTIL   FLG-OK  =   1
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_syuka"     TO  MCP-TABLE
               MOVE    "key16"         TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-SYUKA
                   MOVE    MCPDATA-REC     TO  SYUKA-REC
                   PERFORM 900-SYUKA-CHEK-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUKA
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           .
       900-SYUKA-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科チェック
      *****************************************************************
       900-SYUKA-CHEK-SEC          SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SYUKA-PTID          TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
           IF        ( FLG-PTINF           =   ZERO )
               AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               GO  TO  900-SYUKA-CHEK-EXT
           END-IF
      *
      *    患者番号読み込み
           PERFORM 900-PTNUM-READ-SEC
           IF        ( FLG-PTNUM   NOT =   ZERO )
               GO  TO  900-SYUKA-CHEK-EXT
           END-IF
      *
      *    入外区分
           EVALUATE     SYUKA-NYUGAIKBN
               WHEN    "1"
                   MOVE    "2"         TO  WRK-NYUGAIKBN
               WHEN    "2"
                   MOVE    "1"         TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    診療年月
           MOVE    SYUKA-SRYYM     TO  WRK-SRYYM
      *
           MOVE    ZERO            TO  WRK-HENSRYYM-G
                                       WRK-SRYYMFLG
           MOVE    SYUKA-SRYYM     TO  WRK-HENSRYYM
           MOVE    WRK-PARA-DENPPRTYM
                                   TO  WRK-HENDENPYM
           COMPUTE WRK-HENSYY      =   WRK-HENDENP-YY
                                   -   WRK-HENSRY-YY
           COMPUTE WRK-HENSMM      =   WRK-HENDENP-MM
                                   -   WRK-HENSRY-MM
           IF      WRK-HENSYY      <=  0
               MOVE    WRK-HENSMM  TO  WRK-SRYYMFLG
           ELSE
               COMPUTE WRK-SRYYMFLG    =   WRK-HENSYY * 12
                                       +   WRK-HENSMM
           END-IF
      *
           EVALUATE    SYUKA-MISEIKYU-FLG
               WHEN    "01"
                   MOVE    "4"         TO  WRK-TEISYUTU
               WHEN    "02"
                   MOVE    "5"         TO  WRK-TEISYUTU
           END-EVALUATE
      *
           MOVE    "3"             TO  WRK-SKYKBN
           MOVE    "00000000"      TO  WRK-HKNJANUM
           MOVE    "00000000"      TO  WRK-HKNJANUM2
           MOVE    "4"             TO  WRK-SYOKBN
           MOVE    "1"             TO  WRK-ZAIKBN
           MOVE    SPACE           TO  WRK-HKNKBN
           MOVE    SPACE           TO  WRK-SYUBETUKBN
           MOVE    SPACE           TO  WRK-KBN
      *
           MOVE    1               TO  FLG-OK
           .
       900-SYUKA-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科クローズ
      *****************************************************************
       900-SYUKA-CLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_syuka"     TO  MCP-TABLE
           MOVE    "key16"         TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SYUKA-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号マスタ読込
      *****************************************************************
       900-PTNUM-READ-SEC          SECTION.
      *
           INITIALIZE              PTNUM-REC
           MOVE    SPA-HOSPNUM     TO  PTNUM-HOSPNUM
           MOVE    SYUKA-PTID      TO  PTNUM-PTID
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptnum"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTNUM
                   MOVE    MCPDATA-REC     TO  PTNUM-REC
               ELSE
                   MOVE    1               TO  FLG-PTNUM
                   INITIALIZE                  PTNUM-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTNUM
               INITIALIZE                  PTNUM-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptnum"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTNUM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理読込
      *****************************************************************
       900-SEIKYU-SELECT-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-RECE10
           MOVE    SPACE               TO  WRK-KEY
      *
      *    請求管理ＤＢ検索
           MOVE    SPACE                   TO  RECE10-REC
           INITIALIZE                          RECE10-REC
           MOVE    SPA-HOSPNUM             TO  RECE10-HOSPNUM
           MOVE    WRK-PARA-DENPPRTYM      TO  RECE10-SKYYM
      *    保険者が指定されている場合は条件にセット
           IF          WRK-PARA-HKNJANUM   NOT =   SPACE
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXSTHKNJANUM
               MOVE    WRK-PARA-HKNJANUM   TO  RECE10-XXEDHKNJANUM
           ELSE
               MOVE    "00000000"          TO  RECE10-XXSTHKNJANUM
               MOVE    "99999999"          TO  RECE10-XXEDHKNJANUM
           END-IF
           IF    ( WRK-PATH-PATHNAME   =   "key60" )   OR
                 ( WRK-PATH-PATHNAME   =   "key77" )
               MOVE    WRK-TEISYUTUSAKI-KBN    TO  RECE10-TEISYUTUSAKI
           END-IF
           MOVE    RECE10-REC          TO  MCPDATA-REC
           MOVE    "DBSELECT"          TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-SEIKYU-READ-SEC
           ELSE
               MOVE    1               TO  FLG-RECE10
           END-IF
           .
       900-SEIKYU-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスター読込
      *****************************************************************
       900-SEIKYU-READ-SEC         SECTION.
      *
           MOVE    ZERO            TO  FLG-OK
           PERFORM UNTIL   FLG-OK  =   1
               MOVE    "DBFETCH"           TO  MCP-FUNC
               MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
               MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-RECE10
                   MOVE    MCPDATA-REC     TO  RECE10-REC
                   PERFORM 900-SEIKYU-CHEK-SEC
               ELSE
                   MOVE    1               TO  FLG-RECE10
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           .
       900-SEIKYU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求マスターチェック
      *****************************************************************
       900-SEIKYU-CHEK-SEC         SECTION.
      *
      *    テスト患者のときは読み飛ばす
           MOVE    SPACE               TO  PTINF-REC
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    RECE10-PTID         TO  PTINF-PTID
           MOVE    PTINF-REC           TO  MCPDATA-REC
           PERFORM 900-PTINF-READ-SEC
           IF        ( FLG-PTINF           =   ZERO )
               AND   ( PTINF-TSTPTNUMKBN   =   "1"  )
               GO  TO  900-SEIKYU-CHEK-EXT
           END-IF
      *
      *    提出先チェック
           EVALUATE    RECE10-HKNNUM-S
      *        国保
               WHEN    "060"
               WHEN    "067"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "2"             TO  WRK-TEISYUTU
      *        特別療養費
               WHEN    "068"
               WHEN    "069"
               WHEN    "040"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "3"             TO  WRK-TEISYUTU
      *        広域連合
               WHEN    "039"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "6"             TO  WRK-TEISYUTU
      *        労災
               WHEN    "971"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   EVALUATE    RECE10-HKNKBN
      *                傷病、短期
                       WHEN    "1"
                       WHEN    "2"
                           IF        ( WRK-PARA-TEISYUTU   =   "6" )
                               GO  TO  900-SEIKYU-CHEK-EXT
                           ELSE
                               MOVE    "7"             TO  WRK-TEISYUTU
                           END-IF
      *                アフターケア
                       WHEN    "3"
                           IF        ( WRK-PARA-TEISYUTU   =   "5" )
                               GO  TO  900-SEIKYU-CHEK-EXT
                           ELSE
                               MOVE    "8"             TO  WRK-TEISYUTU
                           END-IF
                       WHEN    OTHER
                           GO  TO  900-SEIKYU-CHEK-EXT
                   END-EVALUATE
      *----(04.08.03)--ADD-START---
      *        自賠責
               WHEN    "970"
               WHEN    "973"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "10"             TO  WRK-TEISYUTU
      *----(04.08.03)--ADD-END-----
      *        公害
               WHEN    "975"
                   IF        ( WRK-PARA-TEISYUTU   =   "1" )
                       OR    ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "9"             TO  WRK-TEISYUTU
               WHEN    OTHER
                   IF        ( WRK-PARA-TEISYUTU   =   "2" )
                       OR    ( WRK-PARA-TEISYUTU   =   "3" )
                       OR    ( WRK-PARA-TEISYUTU   =   "4" )
                       OR    ( WRK-PARA-TEISYUTU   =   "5" )
                       OR    ( WRK-PARA-TEISYUTU   =   "6" )
      *----(04.08.03)--ADD-START---
                       OR    ( WRK-PARA-TEISYUTU   =   "8" )
      *----(04.08.03)--ADD-END-----
                       OR    ( WRK-PARA-TEISYUTU   =   "9" )
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "1"             TO  WRK-TEISYUTU
           END-EVALUATE
      *
      *    保険者チェック
           IF        ( WRK-PARA-HKNJANUM   NOT =   SPACE )
               AND   ( WRK-PARA-HKNJANUM   NOT =   RECE10-HKNJANUM-S )
               GO  TO  900-SEIKYU-CHEK-EXT
           END-IF
      *
      *    入外区分チェック
           EVALUATE    RECE10-NYUGAIKBN
               WHEN    "1"
                   IF      WRK-PARA-NYUGAIKBN  =   "2"
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "2"         TO  WRK-NYUGAIKBN
               WHEN    "2"
                   IF      WRK-PARA-NYUGAIKBN  =   "1"
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "1"         TO  WRK-NYUGAIKBN
           END-EVALUATE
      *
      *    診療年月
           MOVE    RECE10-SRYYM    TO  WRK-SRYYM
      *
           MOVE    ZERO            TO  WRK-HENSRYYM-G
                                       WRK-SRYYMFLG
           MOVE    RECE10-SRYYM    TO  WRK-HENSRYYM
           MOVE    WRK-PARA-DENPPRTYM  TO  WRK-HENDENPYM
           COMPUTE WRK-HENSYY      =   WRK-HENDENP-YY
                                   -   WRK-HENSRY-YY
           COMPUTE WRK-HENSMM      =   WRK-HENDENP-MM
                                   -   WRK-HENSRY-MM
           IF      WRK-HENSYY      <=  0
               MOVE    WRK-HENSMM  TO  WRK-SRYYMFLG
           ELSE
               COMPUTE WRK-SRYYMFLG    =   WRK-HENSYY * 12
                                       +   WRK-HENSMM
           END-IF
      *----(04.07.02)--UPD-START---------------
      *    在総診チェック
           IF    ( WRK-PARA-ZAIKBN     =   "3" )   AND
                 ( RECE10-SRYYM        >=  "201204" )
               MOVE    "1"             TO  WRK-ZAIKBN
           ELSE
               IF          RECE10-ZAITAKU      =   "1"
                   IF      WRK-PARA-ZAIKBN     =   "1"
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "2"             TO  WRK-ZAIKBN
               ELSE
                   IF      WRK-PARA-ZAIKBN     =   "2"
                       GO  TO  900-SEIKYU-CHEK-EXT
                   END-IF
                   MOVE    "1"             TO  WRK-ZAIKBN
               END-IF
           END-IF
      *----(04.07.02)--UPD-END-----------------
      *
           IF      WRK-PATH-PATHNAME   =   "key5"
      *        請求区分チェック（請求しない分）
               IF      RECE10-SKYKBN   NOT =   "0"
                   GO  TO  900-SEIKYU-CHEK-EXT
               END-IF
               MOVE    "2"             TO  WRK-SKYKBN
               MOVE    "00000000"      TO  WRK-HKNJANUM
               MOVE    "00000000"      TO  WRK-HKNJANUM2
               MOVE    "4"             TO  WRK-SYOKBN
               MOVE    "1"             TO  WRK-ZAIKBN
      *----(04.08.03)--UPD-START---
               EVALUATE    WRK-TEISYUTU
                   WHEN    "7"
      *                初回分
                       IF      RECE10-SYOKAIFLG    =   "1"
                           MOVE    RECE10-SYOCD    TO  WRK-HKNJANUM(1:5)
                           MOVE    "000"           TO  WRK-HKNJANUM(6:3)
      *                継続分
                       ELSE
                           MOVE    "99999"         TO  WRK-HKNJANUM(1:5)
                           MOVE    "000"           TO  WRK-HKNJANUM(6:3)
                       END-IF
      *                短期、傷病区分
                       MOVE    RECE10-HKNKBN   TO  WRK-HKNJANUM2(1:1)
                       MOVE    "0000000"       TO  WRK-HKNJANUM2(2:7)
      *----(04.08.03)--UPD-END---
      *----(04.08.03)--ADD-START---
      *            自賠責
                   WHEN    "10"
      *                新様式
                       IF      RECE10-RECEKBN  =   "4"
                           MOVE    "10000000"  TO  WRK-HKNJANUM
                       END-IF
      *                従来様式
                       IF      RECE10-RECEKBN  =   "5"
                           MOVE    "20000000"  TO  WRK-HKNJANUM
                       END-IF
      *                第三者行為
                       IF      RECE10-RECEKBN  =   "6"
                           MOVE    "30000000"  TO  WRK-HKNJANUM
                       END-IF
                       MOVE    "00000000"      TO  WRK-HKNJANUM2
               END-EVALUATE
      *----(04.08.03)--ADD-END-----
           ELSE
      *        請求区分チェック（請求分）
               MOVE    "1"             TO  WRK-SKYKBN
               EVALUATE    WRK-TEISYUTU
      *            国保、特別療養費
                   WHEN    "2"
                   WHEN    "3"
                       MOVE    RECE10-HKNJANUM-S   TO  WRK-HKNJANUM
                       MOVE    "00000000"          TO  WRK-HKNJANUM2
      *            広域
                   WHEN    "6"
                       MOVE    RECE10-HKNJANUM-S(1:4)
                                                   TO  WRK-HKNJANUM(1:4)
                       MOVE    "0000"              TO  WRK-HKNJANUM(5:4)
                       MOVE    RECE10-HKNJANUM-S   TO  WRK-HKNJANUM2
      *            短期、傷病
                   WHEN    "7"
      *                初回分
                       IF      RECE10-SYOKAIFLG    =   "1"
                           MOVE    RECE10-SYOCD    TO  WRK-HKNJANUM(1:5)
                           MOVE    "000"           TO  WRK-HKNJANUM(6:3)
      *                継続分
                       ELSE
                           MOVE    "99999"         TO  WRK-HKNJANUM(1:5)
                           MOVE    "000"           TO  WRK-HKNJANUM(6:3)
                       END-IF
      *                短期、傷病区分
                       MOVE    RECE10-HKNKBN   TO  WRK-HKNJANUM2(1:1)
                       MOVE    "0000000"       TO  WRK-HKNJANUM2(2:7)
      *----(04.08.03)--ADD-START---
      *            自賠責
                   WHEN    "10"
      *                新様式
                       IF      RECE10-RECEKBN  =   "4"
                           MOVE    "10000000"  TO  WRK-HKNJANUM
                       END-IF
      *                従来様式
                       IF      RECE10-RECEKBN  =   "5"
                           MOVE    "20000000"  TO  WRK-HKNJANUM
                       END-IF
      *                第三者行為
                       IF      RECE10-RECEKBN  =   "6"
                           MOVE    "30000000"  TO  WRK-HKNJANUM
                       END-IF
                       MOVE    "00000000"      TO  WRK-HKNJANUM2
      *----(04.08.03)--ADD-END-----
      *            社保
                   WHEN    "1"
      *                直接請求分
                       IF      RECE10-DIRECT-FLG   =   "1"
                           MOVE    1               TO  FLG-DIRECT
                           MOVE    RECE10-HKNJANUM-S
                                                   TO  WRK-HKNJANUM
                           MOVE    "00000000"      TO  WRK-HKNJANUM2
                       ELSE
                           MOVE    "00000000"      TO  WRK-HKNJANUM
                           MOVE    "00000000"      TO  WRK-HKNJANUM2
                       END-IF
                   WHEN    OTHER
                       MOVE    "00000000"      TO  WRK-HKNJANUM
                       MOVE    "00000000"      TO  WRK-HKNJANUM2
               END-EVALUATE
      *        返戻分チェック
               IF      RECE10-HENREIYM     =   "000000"
                   IF      WRK-SRYYM       =   WRK-PARA-DENPPRTYM
                       MOVE    "1"             TO  WRK-SYOKBN
                   ELSE
                       MOVE    "4"             TO  WRK-SYOKBN
                   END-IF
               ELSE
                   IF      RECE10-HENREI-SIJI-FLG  =   "1"
                       MOVE    "3"             TO  WRK-SYOKBN
                   ELSE
                       MOVE    "2"             TO  WRK-SYOKBN
                   END-IF
               END-IF
               IF    ( WRK-PARA-SYOKBN =   "1" ) AND
                     ( WRK-SYOKBN  NOT =   "1" )
                   GO  TO  900-SEIKYU-CHEK-EXT
               END-IF
               IF    ( WRK-PARA-SYOKBN =   "2" ) AND
                     ( WRK-SYOKBN  NOT =   "2" ) AND
                     ( WRK-SYOKBN  NOT =   "3" )
                   GO  TO  900-SEIKYU-CHEK-EXT
               END-IF
               IF    ( WRK-PARA-SYOKBN =   "3" ) AND
                     ( WRK-SYOKBN  NOT =   "4" )
                   GO  TO  900-SEIKYU-CHEK-EXT
               END-IF
           END-IF
      *
           MOVE    1               TO  FLG-OK
           .
       900-SEIKYU-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    請求管理マスタークローズ
      *****************************************************************
       900-SEIKYU-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-SEIKYU-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-SELECT-SEC       SECTION.
      *
           MOVE   "DBSELECT"           TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               PERFORM 900-HKNNUM-FETCH-SEC
           ELSE
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-SELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-FETCH-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    MCPDATA-REC     TO  HKNNUM-REC
               MOVE    ZERO            TO  FLG-HKNNUM
           ELSE
               INITIALIZE                  HKNNUM-REC
               MOVE    1               TO  FLG-HKNNUM
           END-IF
      *
           .
       900-HKNNUM-FETCH-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号マスター読込
      *****************************************************************
       900-HKNNUM-CLOSE-SEC        SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           MOVE    WRK-PATH-TABLE      TO  MCP-TABLE
           MOVE    WRK-PATH-PATHNAME   TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNNUM-CLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者情報マスタ読込
      *****************************************************************
       900-PTINF-READ-SEC          SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC          =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_ptinf"     TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC          =   ZERO
                   MOVE    ZERO            TO  FLG-PTINF
                   MOVE    MCPDATA-REC     TO  PTINF-REC
               ELSE
                   MOVE    1               TO  FLG-PTINF
                   INITIALIZE                  PTINF-REC
               END-IF
           ELSE
               MOVE    1               TO  FLG-PTINF
               INITIALIZE                  PTINF-REC
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_ptinf"     TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険者情報マスター読込（キー）
      *****************************************************************
       900-HKNJAINF-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_hknjainf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_hknjainf"  TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-HKNJAINF
                   MOVE    MCPDATA-REC     TO  HKNJAINF-REC
               ELSE
                   MOVE    1               TO  FLG-HKNJAINF
               END-IF
           ELSE
               MOVE    1               TO  FLG-HKNJAINF
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_hknjainf"  TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-HKNJAINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    労働基準監督署コード読込（キー）
      *****************************************************************
       900-LABOR-SIO-READ-SEC       SECTION.
      *
           MOVE    "DBSELECT"      TO  MCP-FUNC
           MOVE    "tbl_labor_sio" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           IF      MCP-RC              =   ZERO
               MOVE    "DBFETCH"       TO  MCP-FUNC
               MOVE    "tbl_labor_sio" TO  MCP-TABLE
               MOVE    "key"           TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"     USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
               IF      MCP-RC              =   ZERO
                   MOVE    ZERO            TO  FLG-LABOR-SIO
                   MOVE    MCPDATA-REC     TO  LABOR-SIO-REC
               ELSE
                   MOVE    1               TO  FLG-LABOR-SIO
               END-IF
           ELSE
               MOVE    1               TO  FLG-LABOR-SIO
           END-IF
      *
           MOVE    "DBCLOSECURSOR" TO  MCP-FUNC
           MOVE    "tbl_labor_sio" TO  MCP-TABLE
           MOVE    "key"           TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           .
       900-LABOR-SIO-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別ファイル読込
      *****************************************************************
       900-TOKEI-READ-SEC          SECTION.
      *
           READ    TOKEI-FILE
               INVALID
                   MOVE    1           TO  FLG-TOKEI
               NOT INVALID
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト種別ファイル順読込
      *****************************************************************
       900-TOKEI-NEXT-SEC         SECTION.
      *
           READ    TOKEI-FILE     NEXT
               AT  END
                   MOVE    1           TO  FLG-TOKEI
               NOT AT  END
                   MOVE    ZERO        TO  FLG-TOKEI
           END-READ
           .
       900-TOKEI-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　オープン処理
      *****************************************************************
       100-DBOPEN-SEC              SECTION.
      *
           MOVE    "DBOPEN"        TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBSTART"       TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       100-DBOPEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢ　クローズ処理
      *****************************************************************
       900-DBDISCONNECT-SEC        SECTION.
      *
           MOVE    "DBCOMMIT"      TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
      *
           MOVE    "DBDISCONNECT"  TO  MCP-FUNC.
           MOVE    LOW-VALUE       TO  MCP-TABLE.
           MOVE    LOW-VALUE       TO  MCP-PATHNAME.
           CALL    "ORCDBMAIN"     USING   MCPAREA
                                           MCPDATA-REC
                                           SPA-AREA
           .
       900-DBDISCONNECT-EXT.
           EXIT.
