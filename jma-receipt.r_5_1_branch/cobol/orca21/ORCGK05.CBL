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
       IDENTIFICATION              DIVISION.
       PROGRAM-ID.                 ORCGK05.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為セット入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01/04/20  更新前チェック追加
      *  02.00.00    MCC−多々納  01/05/15  約束セット、セット入力の漢字追加
      *  02.00.00    MCC−多々納  01/05/21  入力コードをセットコードを表示へ
      *  02.00.01    MCC−多々納  01/09/07  画面変更に伴う修正
      *  02.00.02    MCC−多々納  02/07/16  インターフェースの変更
      *  02.00.03    NACL−多々納 02/09/24  検索修正
      *  02.00.04    NACL−多々納 03/08/01  画像診断の撮影回数変更
      *  02.00.05    NACL−多々納 03/12/09  入外区分、老人区分追加
      *  02.00.06    NACL−多々納 04/01/16  数量ゼロ追加
      *  02.00.07    NACL-多々納  05/11/17  MONFUNC 対応
      *  02.00.08    NACL-多々納  06/03/02  セット期間追加対応
      *  03.03.00    NACL-多々納  06/10/03  約束セット期間追加対応
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.00.00    NACL-多々納  07/09/XX  テーブル４００対応
      *  04.01.00    NACL-多々納  07/10/XX  悪性腫瘍検査対応
      *  04.01.00    NACL-多々納  07/11/21  器材商品名表示対応
      *  04.03.00    NACL-多々納  08/07/24  前回ＣＤ追加
      *  04.05.00    NACL-多々納  09/06/XX  撮影料等の数量ゼロ対応
      *                                     数量小数５桁対応
      *  04.05.00    NACL-多々納  09/09/XX  '0086'コメントコード対応
      *  04.06.00    NACL-多々納  10/07/XX  エラー内容変更他
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.08.00    NACL-多々納  15/09/XX  セットコード入力ＣＤ対応
      *  04.08.00    NACL-多々納  15/11/XX  残量廃棄システム設定対応
      *  04.08.00    NACL-多々納  15/12/XX  ＡＰＩ対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０１年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    パラメタデータ
      *    SELECT  PARA-FILE       ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDANUM          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(1500).
      *
       WORKING-STORAGE             SECTION.
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K05SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *01  WRK-DATA-REC.
      *    03  WRK-DATA            PIC X(60000)
      *                            OCCURS  10.
      *    ＳＰＡパラメタ
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "K02SCR-SPA".
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
      *    03  FLG-KNJBYO          PIC 9(01).
      *    03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
      *    03  FLG-SRYACT          PIC 9(01).
      *    03  FLG-SRYACCT         PIC 9(01).
      *    03  FLG-CHKMST          PIC 9(01).
           03  FLG-INPUTSET        PIC 9(01).
      *    03  FLG-JYURRK          PIC 9(01).
      *    03  FLG-SANTEI          PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
      *
           03  FLG-GMN             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
      *
           03  FLG-SST             PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *    時間外加算用
           03  FLG-DELFLG          PIC 9(01).
           03  FLG-ZAIEND          PIC 9(01).
      *
           03  FLG-ZAI             PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
      *    更新前フラグ
           03  FLG-KOUSIN          PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    漢字短縮
      *    03  FLG-TANSYUKU        PIC 9(01).
      *
      *    03  FLG-MAE             PIC 9(01).
      *    自費入力
           03  FLG-JIHI            PIC 9(01).
      *
           03  FLG-KAISU           PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    入力有無
           03  FLG-SYORINASI       PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-CHK2            PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-NYU-ARI         PIC 9(01).
      *
           03  FLG-IN1             PIC 9(01).
      *
      *    入力有無
           03  FLG-IN-SETCODE      PIC 9(01).
           03  FLG-IN-ATAI         PIC 9(01).
      *
           03  FLG-TAISYO          PIC 9(01).
      *
           03  FLG-KUHAKU          PIC 9(01).
      *
           03  FLG-FIRUMU          PIC 9(01).
           03  FLG-JYUFKU          PIC 9(01).
      *
           03  FLG-AUTOKBN2        PIC 9(01).
      *
           03  FLG-SAIHEN          PIC 9(01).
           03  FLG-ZEROOK          PIC 9(01).
      *    ＤＢエラー
           03  FLG-DBERR           PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX5                PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDXS                PIC 9(04).
           03  IDX-Z               PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SA              PIC 9(04).
           03  IDX-S               PIC 9(04).
      *
           03  IDX-SIN             PIC 9(04).
           03  IDX-ERR             PIC 9(04).
           03  IDX-SET             PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-KAI2            PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
      *
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-YAKUSOKU        PIC 9(04).
           03  IDX-YAK             PIC 9(04).
           03  IDX-1               PIC 9(04).
      *
           03  IDX-K98             PIC 9(04).
      *
           03  IDX-H               PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-YMDS1.
               05  WRK-YYS1        PIC 9(04).
               05  WRK-MMS1        PIC 9(02).
               05  WRK-DDS1        PIC 9(02).
           03  WRK-YMDS2.
               05  WRK-YYS2        PIC 9(04).
               05  WRK-MMS2        PIC 9(02).
               05  WRK-DDS2        PIC 9(02).
           03  WRK-SRYYM.
               05  WRK-SRYYY       PIC 9(04).
               05  WRK-SRYMM       PIC 9(02).
      *
           03  WRK-MOJISU          PIC 9(04).
           03  WRK-FREE            PIC 9(04).
      *
      **   03  WRK-KEI             PIC 9(08).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(02).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(02).
      *Ｋ９８カウント+
           03  WRK-CNT-ENT         PIC 9(06).
      *Ｋ９８、Ｋ９９戻りＩＤＸ保存
           03  WRK-HZN-IDX         PIC 9(04).
      *
           03  WRK-KEI-ERRCD       PIC X(04).
           03  WRK-ERR-ERRCD       PIC X(04).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-ERRMSG          PIC X(80).
      *
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-KAISU-H         PIC X(04).
           03  WRK-KAISU           PIC 9(04).
           03  WRK-LAST-IDX        PIC 9(04).
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-GMN-STR         PIC 9(04).
      *
           03  WRK-INPUTCD         PIC X(10).
           03  WRK-SURYO           PIC 9(07)V9(05).
           03  WRK-SURYO-R         REDEFINES   WRK-SURYO.
               05  WRK-SURYO-S1    PIC 9(07).
               05  WRK-SURYO-S2    PIC 9(05).
           03  WRK-BUNGSU          PIC 9(08).
           03  WRK-SRYSYUKBN       PIC X(03).
           03  WRK-SRYSYUKBNMEI    PIC X(40).
           03  WRK-SRYKBN          PIC X(02).
      *
           03  WRK-MOJI            PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *
           03  WRK-KIDMSG              PIC X(70).
      *
           03  WRK-MOTOPG              PIC X(08).
      *    入力時表示用
           03  WRK-ERR-IDX             PIC 9(04).
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(54).
      *
      *    入力位置判定用
           03  CNT-IN              PIC 9(02).
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
      *
           03  WRK-STR             PIC 9(04).
      *
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
      *R02     05  WRK-MEISYO              PIC X(78).
               05  WRK-MEISYO              PIC X(138).
           03  IDX-MEI                 PIC 9(04).
      *
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
      *    時間外フラグ
           03  WRK-TIMEFLG         PIC X(01).
      *
           03  WRK-YUKOSTYMD       PIC X(08).
           03  WRK-YUKOEDYMD       PIC X(08).
           03  WRK-SELNUM          PIC 9(03).
           03  WRK-SETCODE         PIC X(06).
      *
           03  WRK-KANSURYO        PIC 9(05)V9(05).
      *
           03  WRK-HZN-SELNUM      PIC 9(03).
      *
       01  WRK-GMNGYO-AREA.
           03  WRK-MAP-MAX         PIC 9(04).
           03  WRK-MAP-CUR         PIC 9(04).
           03  WRK-MAP-CUR2        PIC 9(04).
           03  WRK-MAP-CUR3        PIC 9(04).
           03  WRK-PAGE            PIC 9(04).
           03  WRK-MAP-PAGE        PIC 9(04).
           03  WRK-MAP-PAGE2       PIC 9(04).
           03  WRK-MAP-PAGE3       PIC 9(04).
           03  WRK-MAP-MAXPAGE     PIC 9(04).
      *
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
               05  WRK-HZN-ATAI5           PIC X(10).
      *
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
               05  WRK-H-SURYO        PIC X(11).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(01).
               05  WRK-H-ZAIKEI       PIC X(06).
      *
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-KAKERU          PIC X(03).
      *
           03  WRK-DSPSEQ          PIC 9(02).
      *
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *画面最大行
       01  MAX-GMN                 PIC 9(04)   VALUE   23.
      *画面ページ数（処理最大行÷画面最大行）
       01  MAX-PAGE                PIC 9(04)   VALUE   18.
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
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
           03  FLG-JKNKSN          PIC 9(02).
           03  WRK-IDX-FT          PIC 9(02).
           03  WRK-CD-MCNT         PIC 9(02).
           03  WRK-ACCT-MCNT       PIC 9(02).
           03  WRK-SUR-MCNT        PIC 9(02).
           03  WRK-MCNT1           PIC 9(02).
           03  WRK-MCNT2           PIC 9(02).
           03  WRK-MCNT3           PIC 9(02).
           03  WRK-MCNT4           PIC 9(02).
           03  WRK-MCNT5           PIC 9(02).
           03  WRK-CD              PIC X(54).
           03  WRK-KAI             PIC X(20).
           03  WRK-SUR             PIC X(20).
           03  WRK-MOJI1           PIC X(10).
           03  WRK-MOJI2           PIC X(10).
           03  WRK-MOJI3           PIC X(10).
           03  WRK-MOJI4           PIC X(10).
           03  WRK-MOJI5           PIC X(10).
           03  IDM                 PIC 9(04).
           03  IDMS                PIC 9(04).
      *
           03  WRK-HENKAN-MAE      PIC X(20).
           03  WRK-HENKAN-ATO      PIC X(20).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
           03  WRK-LEN             PIC 9(04).
      *
           03  WRK-SANTEI-SYORI    PIC X(01).
      *
           03  WRK-SETCOD          PIC X(07).
           03  WRK-ICD-INPUTCD     PIC X(20).
           03  WRK-CDSYU           PIC X(01).
      *
      *    約束セット用
           03  WRK-IDX-YAKUSOKU.
               05  WRK-IDX-YAK         PIC 9(04)    OCCURS  20.
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
        01  WRK-HOZ-IDX                PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *D   COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK1005.INC".
      *
           COPY    "CPSK1006.INC".
      *
           COPY    "CPSK1010.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC".
      *
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
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *   患者番号変換サブ
      *D   COPY    "CPORCSPTID.INC".
      *
      *    負担金額計算用パラメタ
      *D   COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *    COPY    "CPORCSC91.INC".
      *
      *    点数編集チェックパラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
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
           COPY    "ORCA21SCRAREA.INC".
      *!!!     COPY    "K01.INC".
      *!!!     COPY    "K02.INC".
      *!!!     COPY    "K02N.INC".
      *!!!     COPY    "K021.INC".
      *!!!     COPY    "K023.INC".
      *!!!     COPY    "K03.INC".
      *!!!     COPY    "K04.INC".
      *!!!     COPY    "K05.INC".
      *!!!     COPY    "K051.INC".
      *!!!     COPY    "K052.INC".
      *!!!     COPY    "K06.INC".
      *!!!     COPY    "K061.INC".
      *!!!     COPY    "K062.INC".
      *!!!     COPY    "K07.INC".
      *!!!     COPY    "K08.INC".
      *!!!     COPY    "K081.INC".
      *!!!     COPY    "K09.INC".
      *!!!     COPY    "K10.INC".
      *!!!     COPY    "K97.INC".
      *!!!     COPY    "K98.INC".
      *!!!     COPY    "K99.INC".
      *!!!     COPY    "K03X.INC".
      *!!!     COPY    "KERR.INC".
      *!!!     COPY    "KERR2.INC".
      *!!!     COPY    "KID1.INC".
      *!!!     COPY    "KID2.INC".
      *!!!     COPY    "KHELP.INC".
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
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  WRK-MOJI-AREA
           INITIALIZE                  WRK-GMNGYO-AREA
           INITIALIZE                  WRK-GMN-SURYO-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  FLG-SYORINASI
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K05SPA-SET-SEC
      *
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *API
               WHEN     "API"            ALSO   SPACE
                   PERFORM 300-API-SYORI-SEC
      *
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
      *    画面ＳＰＡ書込み
           IF      FLG-END2             =   ZERO
            AND    FLG-DBERR            =   ZERO
               PERFORM 1003-K05SPA-OUT-SEC
           END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *API
      *****************************************************************
      *    ＡＰＩ　処理
      *****************************************************************
       300-API-SYORI-SEC                SECTION.
      *
      *    初期設定
           PERFORM 310-SPA-INITSET-SEC
      *    画面パラ登録
           PERFORM 1003-K05SPA-OUT-SEC
           .
       300-API-SYORI-EXT.
           EXIT.
      *API
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    DISPLAY "*** ORCSNPL    START  ***"
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KERR"
                                       OR  "KERR2"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
           ELSE
      *
      *        初期画面セット
               PERFORM 300-SCREEN-SEC
      *
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "K05"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
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
           .
      *
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡデータ読み込み処理
      *****************************************************************
       1002-K05SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *    IF     SPA-HOSPNUM  NOT NUMERIC
      *        GO   TO  1002-K05SPA-SET-EXT
      *    END-IF  
      *
      *    TBL_PARA を使用する
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K05"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1 )
                 OR   (IDX                 >=  SPA-MAX-LINE)
               ADD    1                    TO  IDX
               MOVE   PARA-DATA-REC       TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    行位置とカーソル位置の決定
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           .
       1002-K05SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-K05SPA-OUT-SEC             SECTION.
      *
      *    PARA削除
           PERFORM 10031-PARA-DELETE-SEC
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K05"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )
               IF      (SPA-COM-INPUTCD(IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(IDX)   =   SPACE )
               AND     (IDX           >   SPA-GMN-MAX  )
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-TBL (IDX)   TO  PARA-DATA-REC
                   MOVE    IDX                 TO  PARA-EDANUM
                   MOVE    PARA-REC            TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_para"          TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K05 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
                       MOVE    "5005"          TO  SPA-ERRCD
                       MOVE    1               TO  FLG-DBERR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       1003-K05SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡパラメターＤＢ削除処理
      *****************************************************************
       10031-PARA-DELETE-SEC         SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K05"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       10031-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-GMN
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *
      *DDDDDD  元画面へ戻す
      *        MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
      *        IF      WRK-MOTOPG(1:3)     =   "Y01"  OR  "U01"
      *            MOVE    LNK-COMMON      TO  SPA-KYOTUKEY
      *            MOVE    LNK-SCRSPA      TO  SPA-K02
      *            MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *            MOVE    1               TO  SPA-GMN5-CUR
      *            GO      TO      300-SCREEN-EXT
      *        END-IF
      *
      *        IF      WRK-MOTOPG          =   "Y01"  OR   "U01"
      *            CONTINUE
      ******** ELSE
                   IF      SPA-MOTOPG2         =   SPACE
                       MOVE    SPA-MOTOPG          TO  SPA-MOTOPG2
                   END-IF
      *******  END-IF
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "K051"
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K052"
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K99"
                   IF      SPA-K99-DATA    NOT =   SPACE
                       MOVE    SPA-K99-DATA    TO  K05-INPUTCD
                                                         (SPA-MAP-IDX)
      *                画面をＳＰＡへ編集
                       PERFORM 2002-GMNSPA-SET-SEC
                       MOVE    ZERO                TO  FLG-KOUSIN
      *                入力チェック処理
                       PERFORM 220-INPUT-CHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "K98"
                   IF      SPA-GMN5-CUR        =   1
      *                セットコード選択
                       PERFORM 30112-K98-SETCODE-SEC
                       MOVE    1                   TO  FLG-GMN
                       IF      SPA-ERRCD       NOT =   SPACE
                            PERFORM 510-ERRSET-SEC
                       END-IF
                   ELSE
      *                明細選択
                       PERFORM 3011-K98-SET-SEC
                   END-IF
               WHEN    "KID1"
                   PERFORM 3001-KID1-SYORI-SEC
                   MOVE    1                   TO  FLG-GMN
      *
               WHEN    OTHER
      *            初期設定
                   PERFORM 310-SPA-INITSET-SEC
           END-EVALUATE
      *
           MOVE    SPACE           TO  SPA-KIDCD
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期設定処理
      *****************************************************************
       310-SPA-INITSET-SEC          SECTION.
      *
           INITIALIZE              SPA-K05-AREA
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    SPA-SRYYMD          TO  SPA-K05-MAE-SRYYMD
      *    入外区分、老人区分初期表示
           PERFORM 3101-NYUGAI-SET-SEC
      *
      *    パラメタファイルより見出し表示
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    OPEN    INPUT               PARA-FILE
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    2               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO
      *        IF      PARA-FILEMEI    =   "WKSRYACT"
      *            MOVE    PARA-DATA-REC    TO  LNK-WKSRYACT-REC
      *                                                (PARA-EDANUM)
      *            MOVE    PARA-EDANUM      TO  LNK-WKSRYACT-MAX
      *        END-IF
      *
      *        IF      PARA-FILEMEI     =   "K01PARA"
      *            MOVE    PARA-DATA-REC    TO  SPA-K01KYOUTU
      *        END-IF
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      *    IF      FLG-PARA            =   1
      *        CLOSE                       PARA-FILE
      *****END-IF
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                  OR  (SPA-ERRCD       NOT =   SPACE )
               ADD     1                   TO  LNK-WKSRYACT-MAX
               IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
                   DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
                   MOVE    "9000"              TO  SPA-ERRCD
               ELSE
                   MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                      (LNK-WKSRYACT-MAX)
               END-IF
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    PARA削除
           IF      LNK-WKSRYACT-MAX    >  ZERO
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    PARA-REC            TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "del3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
      *
           MOVE    SPACE           TO  SPA-KIDCD
      *
      *    画面用スパ編集処理
           MOVE    "1"             TO  SPA-K05-SYORI
           PERFORM                 310-SPA-SET-SEC
      *
           INITIALIZE              SPA-K051-AREA
           INITIALIZE              SPA-K051-TBL
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN5-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       310-SPA-INITSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面よりＯＫ処理
      *****************************************************************
       3101-NYUGAI-SET-SEC          SECTION.
      *
      *    入外区分、老人区分を保存する
           MOVE    SPA-NAI-ROUJIN      TO  SPA-K05-MAE-ROUJIN
           MOVE    SPA-NYUGAIKBN       TO  SPA-K05-MAE-NYUGAIKBN
      *
           MOVE    "0 一般"            TO  SPA-K05-ROUJINLST(1)
           MOVE    "1 老人"            TO  SPA-K05-ROUJINLST(2)
           IF      SPA-NAI-ROUJIN      =   "1"
               MOVE    SPA-K05-ROUJINLST(2)    TO  SPA-K05-ROUJIN-G
           ELSE
               MOVE    SPA-K05-ROUJINLST(1)    TO  SPA-K05-ROUJIN-G
           END-IF
      *
           MOVE    SPA-NYUGAIKBN       TO  SPA-K05-NYUGAIKBN
           MOVE    "1 入院"            TO  SPA-K05-NYUGAIKBNLST(1)
           MOVE    "2 入院外"          TO  SPA-K05-NYUGAIKBNLST(2)
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-K05-NYUGAIKBNLST(1) TO  SPA-K05-NYUGAIKBN-G
           ELSE
               MOVE    SPA-K05-NYUGAIKBNLST(2) TO  SPA-K05-NYUGAIKBN-G
           END-IF
      *
           .
       3101-NYUGAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面よりＯＫ処理
      *****************************************************************
       3001-KID1-SYORI-SEC          SECTION.
      *
            EVALUATE    SPA-KIDCD
      *         削除確認ＯＫ
                WHEN    "0001"
                    IF      SPA-KID1-FLG        =   "OK"
                        MOVE    "3"                 TO  SPA-K05-SYORI
                        PERFORM 41002-KOUSIN-SYORI-SEC
                    END-IF
      *         戻るの確認
                WHEN    "0002"
                    IF      SPA-KID1-FLG        =   "OK"
                        MOVE    1                   TO  SPA-GMN5-CUR
                        MOVE    SPACE               TO  SPA-UPDFLG
                        PERFORM 210-BACK-OK-SEC
                    END-IF
      *         修正の決定
                WHEN    "0003"
                    IF      SPA-KID1-FLG        =   "OK"
                        PERFORM 30011-SYUSEI-HEN-SEC
                    ELSE
      *H27.2            入力コード、期間のみ編集
                        PERFORM 30013-SYUSEI-NASI-SEC
                    END-IF
      *         期間変更
                WHEN    "0004"
                    IF      SPA-KID1-FLG        =   "OK"
                        MOVE    "5"             TO  SPA-K05-SYORI
                        PERFORM 41002-KOUSIN-SYORI-SEC
                    END-IF
                    IF      SPA-ERRCD       NOT =   SPACE
                        PERFORM 510-ERRSET-SEC
                    END-IF
           END-EVALUATE
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           MOVE    SPACE               TO  SPA-KIDCD
           .
       3001-KID1-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-K98-SET-SEC          SECTION.
      *
      *    悪性腫瘍の検査
           IF     SPA-AKUSEI-FLG       =   1
               PERFORM 30112-AKUSEI-COMMENT-SEC
               MOVE    1                   TO  FLG-GMN
           ELSE
      *    入力コード決定
               IF     (SPA-K98-SRYCD-G   NOT =   SPACE )
                   PERFORM 3011-K98-SET2-SEC
                   MOVE    1                   TO  FLG-GMN
               ELSE
                   MOVE    2                   TO  FLG-GMN
                   MOVE    3                   TO  SPA-GMN5-CUR
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
           .
      *
       3011-K98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット確定処理
      *****************************************************************
       30112-K98-SETCODE-SEC          SECTION.
      *
           IF      SPA-K98-SRYCD(01)   =   SPACE
               MOVE    1                   TO  SPA-GMN5-CUR
               MOVE    SPACE               TO  SPA-GMN-SETCODE
               MOVE    "1"                 TO  SPA-K05-SYORI
               GO      TO      30112-K98-SETCODE-EXT
           END-IF
      *
           MOVE    SPA-K98-SRYCD(01)   TO  K05-SETCODE
           MOVE    SPA-K98-SRYCD(01)   TO  SPA-GMN-SETCODE
           IF      SPA-GMN-SETCODE(1:1)    =   "S"  OR  "P"
      *         修正の決定
               PERFORM 30011-SYUSEI-HEN-SEC
               MOVE    SPA-GMN-SETCODE     TO  SPA-MAE-SETCODE
           ELSE
               MOVE    1                   TO  SPA-GMN5-CUR
               MOVE    "5001"              TO  SPA-ERRCD
           END-IF
      *
           .
       30112-K98-SETCODE-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-K98-SET2-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-K98 FROM    1   BY  1
                   UNTIL      (IDX-K98             >   10  )  OR
                              (SPA-K98-SRYCD(IDX-K98)  =   SPACE )
      *
               IF      IDX-K98             =   1
                   INITIALIZE                  SPA-COM-TBLREC
                                                       (SPA-GMN-IDX)
                   INITIALIZE                  SPA-GMN-TBLREC
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
               END-IF
      *
               MOVE    "1"                 TO  SPA-COM-IN (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-IN2(SPA-GMN-IDX)
      *
           END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-K98-SET2-EXT.
           EXIT.
      *@@
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       5101-AKUSEI-K98-SEC       SECTION.
      *
           MOVE    SPA-AKUSEI-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-K98-AREA
           MOVE    "//*D009S"          TO  SPA-K98-INPUTCD
           MOVE    "A"                 TO  SPA-K98-CDSYUBETU
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-AKUSEI-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-K98-SYORI-SEC
           .
       5101-AKUSEI-K98-EXT.
           EXIT.
      *****************************************************************
      *    悪性腫瘍検査一覧へ処理
      *****************************************************************
       30112-AKUSEI-COMMENT-SEC       SECTION.
      *
           ADD     1                   TO  SPA-GMN-IDX 
           MOVE    SPA-GMN-IDX         TO  IDX-S
           IF      SPA-K98-SRYCD-G   NOT =   SPACE
               PERFORM 3011-K98-SET2-SEC
           ELSE
      *    コメントコード追加
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "830000015"     TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
      *           入力コード・名称チェック処理
                  PERFORM 410112-KOUI-SPACHK-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-AKUSEI-FLG
           MOVE    ZERO                TO  SPA-AKUSEI-IDX
      *
           .
       30112-AKUSEI-COMMENT-EXT.
           EXIT.
      *@@
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *    患者が選択されてない時、年齢を一般とする
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    50                  TO  SPA-NENREI-YY
           END-IF
      *
      *    画面編集処理
           PERFORM 3103-SAIHEN-HEN-SEC
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-SET
      *    診療室入力あり編集
           PERFORM     VARYING   IDX   FROM    1   BY  1
                       UNTIL    (IDX   >   LNK-WKSRYACT-MAX )
                             OR (IDX2  >   SPA-MAX-LINE  )
      *
      *        自動作成分、診療、保険外は飛ばす
               IF      LNK-WKSRY-SRYKBN (IDX)   =   "11"  OR
                                                    "12"  OR
                                                    "13"
      ******************************************OR  "14"
                   MOVE    6               TO  IDX1
               ELSE
                   MOVE    1               TO  IDX1
               END-IF
      *
               MOVE    ZERO                TO  IDXS
               PERFORM     VARYING   IDY   FROM    IDX1   BY  1
                           UNTIL     IDY   >   5
      *            自動作成分は編集しない
                   IF      LNK-WKSRY-SRYCD (IDX IDY) =   SPACE
                       CONTINUE
                   ELSE
                       PERFORM 31011-SPA-HEN-SEC
                   END-IF
      *
      *            約束セットの時、以下を編集しない
                   IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
                       MOVE    5               TO  IDY
                       MOVE    1               TO  FLG-SET
                   END-IF
               END-PERFORM
      *
      *        剤の最後に回数入力をセット
               IF     (IDXS                >   ZERO  )  OR
                      (FLG-SET             =   1     ) 
                   IF     (IDX                 =   LNK-WKSRYACT-MAX ) OR
                          (LNK-WKSRY-ZAINUM (IDX + 1)
                                           NOT =   LNK-WKSRY-ZAINUM
                                                                (IDX))
                       MOVE    "1"             TO  SPA-COM-KAIFLG
                                                               (IDX2)
                       MOVE    ZERO                TO  FLG-SET
                   END-IF
               END-IF
           END-PERFORM
      *
      *    約束コード以外の画面再編集
           PERFORM VARYING     IDX   FROM    1   BY  1
                   UNTIL      (IDX       >   SPA-MAX-LINE )  OR
                              (IDX       >   IDX2   )
               IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" OR  "." ) OR
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "S"         )
                   CONTINUE
               ELSE
      *
      *            入力コードの画面用再編集
      ************ PERFORM 3101-INPUTCD-HEN-SEC
      *
      *            点数マスタ編集
                   INITIALIZE                  ORCSC92AREA
                   MOVE    "K05"               TO  LNK-SC92-PG
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *            残量廃棄の判定
                   IF     (SPA-COM-SRYKBN (IDX)(1:1)  =   "3" )  AND
                          (SPA-COM-INPUTCD(IDX)(1:1)  =   "6" )
                       PERFORM 31034-HAIKI-CHKI-SEC
                   END-IF
      *H27.11
      *            注射以下
                   IF     (SPA-HAIKIKBN-FLG    =   "2"   )   AND
                          (SPA-COM-INPUTCD(IDX)(1:1)   =   "6" ) AND
                          (SPA-COM-SRYKBN (IDX)(1:1)   =   "4"
                                                        OR  "5"
                                                        OR  "6"
                                                        OR  "7"
                                                        OR  "8" )
                       PERFORM 31034-HAIKI-CHKI-SEC
                   END-IF
      *
      *            換算値×換算値数量＝数量へ変更
                   IF      SPA-COM-KANSURYO (IDX)  >   ZERO
                       COMPUTE WRK-KANSURYO    =   SPA-COM-KANZANCHI
                                                             (IDX)
                                               *   SPA-COM-KANSURYO
                                                             (IDX)
                       MOVE    WRK-KANSURYO    TO  SPA-COM-SURYO (IDX)
                       MOVE    WRK-KANSURYO    TO  SPA-COM-SURYO2(IDX)
                   END-IF
      *
      *            回数の入力
                   IF      SPA-COM-KAIFLG (IDX)  =   "1"
                       IF     (SPA-COM-KAISU(IDX)    >   1  )  OR
                              (SPA-COM-SRYKBN(IDX)(1:1)
                                                   =   "2"    )  OR
                              (IDX                 =   SPA-MAX-LINE )
                           CONTINUE
                       ELSE
                           IF     (SPA-COM-INPUTCD(IDX + 1)
                                                   =   SPACE   )  OR
                                  (SPA-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )
                               MOVE    SPACE       TO  SPA-COM-KAIFLG
                                                         (IDX)
                           ELSE
                               IF    ((SPA-COM-SRYKBN (IDX + 1)
                                               NOT =  SPA-COM-SRYKBN
                                                       (IDX) )  AND
                                      (SPA-COM-INPUTCD(IDX + 1)(1:1)
                                                   =   "6"     )) 
                                   CONTINUE
                               ELSE
                                   MOVE    SPACE   TO  SPA-COM-KAIFLG
                                                         (IDX)
                               END-IF
                           END-IF
                       END-IF
                   END-IF
      *
      *            入力コードの画面用再編集
                   INITIALIZE                  WRK-MOJI-AREA
                   PERFORM 3101-INPUTCD-HEN-SEC
      *
               END-IF
           END-PERFORM
      *
           MOVE    ZERO              TO  FLG-FIRUMU
           PERFORM VARYING     IDX-1 FROM    IDX2   BY  -1
                   UNTIL       IDX-1     <   1
               IF      SPA-COM-ZAIEND (IDX-1)  =   "1"
                   MOVE    ZERO            TO  FLG-FIRUMU
               END-IF
      *        画像診断のフィルム判定
               IF      SPA-COM-SRYKBN (IDX-1)  =   "70"
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "7" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "3" )
                       MOVE    1               TO  FLG-FIRUMU
                   END-IF
                   IF     (SPA-COM-INPUTCD(IDX-1)(1:1)   =   "1" )  AND
                          (SPA-COM-DATAKBN (IDX-1)       =   "1" )  AND
                          (SPA-COM-KZMCOMPSIKIBETU(IDX-1)  NOT =  ZERO)
                     AND  (SPA-COM-SURYO (IDX-1)         >   1   )
                       IF      FLG-FIRUMU                =   ZERO
                           MOVE    "1"             TO  SPA-COM-SURFLG
                                                               (IDX-1)
                       ELSE
                           IF      SPA-COM-SURFLG (IDX-1)
                                                       NOT =   "1"
                               MOVE    1           TO  SPA-COM-SURYO
                                                               (IDX-1)
      *                        入力コードの画面用再編集
                               MOVE    IDX-1               TO  IDX
                               PERFORM 3101-INPUTCD-HEN-SEC
                           END-IF
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
      *    約束セットの画面用再編集
           PERFORM VARYING     IDX-1   FROM    1   BY  1
                   UNTIL      (IDX-1   >   SPA-MAX-LINE )  OR
                              (SPA-COM-INPUTCD(IDX-1)    =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX-1)    =   SPACE )
               IF      SPA-COM-INPUTCD(IDX-1)(1:1)   =   "S"
                   INITIALIZE                  WRK-MOJI-AREA
                   MOVE    IDX-1               TO  IDX
      *            入力コードの画面用再編集
                   PERFORM 3101-INPUTCD-HEN-SEC
                   MOVE    IDX-1               TO  SPA-GMN-IDX
      **********   約束名称編集
      *            MOVE    6                   TO  WRK-CD-MCNT
      *            MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
      *                                            TO  WRK-INPUTCD
      ************ PERFORM 41011921-INPSET-MEI-SEC
      *
                   MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                             (IDX-1)
                   MOVE    SPACE               TO  SPA-COM-KAIFLG
                                                             (IDX-1)
      *
                   MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
                   MOVE    ZERO                TO  IDX-YAKUSOKU
      *
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                               TO  WRK-SETCD
                   INITIALIZE                      WRK-MOJI-AREA
      *
                   INITIALIZE                  ORCSC97AREA
                   MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
                   MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  LNK-SC97-INPUTCD
                   CALL    "ORCSC97"           USING
                                               SPA-AREA
                                               ORCSC97AREA
                                               SPA-SCR-REC
      *
                   MOVE    LNK-SC97-INCD       TO  WRK-CD
                   MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *            変換文字列編集
                   PERFORM 4101121-MOJI-SET-SEC
      *
                   MOVE    SPA-SRYYMD          TO  WRK-YUKOSTYMD
                   MOVE    SPA-SRYYMD          TO  WRK-YUKOEDYMD
                   PERFORM 4101191-INPUTSET-SYORI-SEC
               END-IF
           END-PERFORM
      *    約束セットないのエラー
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
           MOVE    WRK-ERRMSG          TO  SPA-ERRMSG2
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    残量廃棄の判定処理
      *****************************************************************
       31034-HAIKI-CHKI-SEC              SECTION.
      *
           IF      (SPA-COM-YKZKBN (IDX)  =   "4"
      *H27.11  外用薬追加
                                          OR  "6"  )  AND
                   (SPA-COM-TANICD (IDX)  =   "014"
                                            OR  "022"
                                            OR  "046" )  AND
                   (SPA-COM-SURYO(IDX)(6:3)  >   ZERO)
               IF     (IDX               <   SPA-MAX-LINE  )  AND
                      (SPA-COM-INPUTCD(IDX + 1)  =   "099309901")
                   CONTINUE
               ELSE
                   MOVE    "1"             TO  SPA-COM-AUTOFLG2(IDX)
               END-IF
           END-IF
      *
           .
       31034-HAIKI-CHKI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       31011-SPA-HEN-SEC              SECTION.
      *
      *    診療種別セット
           IF     (IDY                     =   1   )  AND
                  (LNK-WKSRY-RENNUM (IDX)  =   1   )
               IF      LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   SPACE
                   ADD     1                   TO  IDX2
                   IF      IDX2                >   SPA-MAX-LINE
                       MOVE    "7004"          TO  SPA-ERRCD
                       GO      TO      31011-SPA-HEN-EXT
                   END-IF
                   MOVE    "."                 TO  SPA-GMN-INPUTCD
                                                            (IDX2)(1:1)
                   MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-GMN-INPUTCD
                                                           (IDX2)(2:3)
                   MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                               TO  SPA-COM-SRYSYUKBN
                                                              (IDX2)
                   MOVE    LNK-WKSRY-SRYKBN (IDX)
                                               TO  SPA-COM-SRYKBN(IDX2)
               END-IF
           END-IF
      *
      *    明細編集
           ADD     1                   TO  IDX2
           IF      IDX2                >   SPA-MAX-LINE
                MOVE    "7004"         TO  SPA-ERRCD
                GO      TO      31011-SPA-HEN-EXT
           END-IF
           MOVE    LNK-WKSRY-SRYKBN(IDX)
                                       TO  SPA-COM-SRYKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                       TO  SPA-COM-SRYSYUKBN  (IDX2)
           MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                       TO  SPA-COM-INPUTCD (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO   (IDX2)
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDY)
                                       TO  SPA-COM-SURYO2  (IDX2)
           MOVE    LNK-WKSRY-AUTOKBN (IDX IDY)
                                       TO  SPA-COM-AUTOKBN  (IDX2)
      *    時間外区分は空白
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "A" OR "B" OR "C"  OR  "D"
                                       OR  "E" OR "F" OR "G"  OR  "H"
               MOVE    SPACE           TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *    数量入力時
           IF      LNK-WKSRY-AUTOKBN (IDX IDY)
                                       =   "S"
               MOVE    "1"                 TO  SPA-COM-SURFLG  (IDX2)
               MOVE    SPACE               TO  SPA-COM-AUTOKBN (IDX2)
           END-IF
      *
           MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDY)
                                       TO  SPA-COM-INPUTCHI-G  (IDX2)
      *
      *    回数を編集
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU(IDX2)
           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX) TO  SPA-COM-KAISU2(IDX2)
           MOVE    IDX2                TO  IDXS
      *
      *    分画数を編集
           MOVE    LNK-WKSRY-SRYKAISU (IDX IDY)
                                       TO  SPA-COM-BUNGSU (IDX2)
      *
           MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2)
      *!!
      *ver.4.5.0
      *    内服１種類区分
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-INPUTKBN(IDX IDY) =   "2"  )
                MOVE    "1"                TO  SPA-COM-INPUTNAIF (IDX2)
           END-IF
      *    コメントの継続
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "8"  )  OR
                  (SPA-COM-INPUTCD (IDX2)(1:3) =   "008")
               IF     (LNK-WKSRY-INPUTKBN (IDX IDY)    =   "1" )
                   MOVE    "1"             TO  SPA-COM-INPUTCONT (IDX2)
               END-IF
           END-IF
      *    換算値入力
           IF     (SPA-COM-INPUTCD (IDX2)(1:1) =   "6"  )  AND
                  (LNK-WKSRY-KANSURYO (IDX IDY) >   ZERO )
               MOVE    LNK-WKSRY-KANSURYO (IDX IDY)
                                           TO  SPA-COM-KANSURYO (IDX2)
            END-IF
      *!!
      *
      *    約束セットの時、以下処理なし
           IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =   "S"
               GO      TO      31011-SPA-HEN-EXT
           END-IF
      *
      **** MOVE    SPA-COM-INPUTCD (IDX2)  TO  SPA-GMN-INPUTCD  (IDX2)
      *
      *    INITIALIZE                  ORCSC92AREA
      *    MOVE    SPACE               TO  LNK-SC92-KBN
      *    MOVE    IDX2                TO  LNK-SC92-GMN-IDX
      *    MOVE    SPA-COM-INPUTCD(IDX2)
      *                                TO  LNK-SC92-SRYCD
      *    MOVE    SPA-NENREI      TO  LNK-SC92-NENREI
      *    MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
      *    MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
      *    MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
      *    CALL    "ORCSC92"           USING
      *                                MCPAREA
      *                                ORCSC92AREA
      *                                SPA-SCR-REC
      *                                SPA-AREA
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        MOVE    SPACE           TO  SPA-ERRCD
      **** END-IF
      *
      *    コメントの時、コメント内容編集
           IF      SPA-COM-INPUTCD (IDX2)(1:1) =   "0"  OR  "8"
               IF      LNK-WKSRY-INPUTCOMENT (IDX IDY)  NOT =   SPACE
                   IF      SPA-COM-INPUTCD (IDX2)       =   "810000001"
                                                        OR  "008500000"
                                                        OR  "008600000"
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO-G (IDX2)
                       MOVE    "1"         TO  SPA-COM-MEIFLG   (IDX2)
                   ELSE
                       MOVE    LNK-WKSRY-INPUTCOMENT (IDX IDY)
                                           TO  SPA-GMN-MEISYO   (IDX2)
                   END-IF
               END-IF
           END-IF
      *
      *    金額入力時、金額へ
           IF      SPA-COM-INPUTCD(IDX2)(1:3)  =   "095"  OR  "096"
               IF      SPA-COM-KISOTEN (IDX2)      =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-JIHIMONEY
                                                                 (IDX2)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    SPA-COM-KISOTEN (IDX2)
                                               TO  SPA-GMN-KEI   (IDX2)
               END-IF
      *
      *        自費の時、金額編集
           ELSE
               IF      SPA-COM-SRYKBN (IDX2)   =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG(IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-COM-KEI   (IDX2)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX IDY)
                                               TO  SPA-GMN-KEI   (IDX2)
               END-IF
           END-IF
      *
           .
       31011-SPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       3101-INPUTCD-HEN-SEC                  SECTION.
      *
      *    画面再編集・基本チェック
           INITIALIZE                  ORCSC94AREA
           MOVE    "1"                 TO  LNK-SC94-KBN
           MOVE    "K05"               TO  LNK-SC94-PG
           MOVE    IDX                 TO  LNK-SC94-IDX
           CALL    "ORCSC94"           USING
                                       ORCSC94AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           .
       3101-INPUTCD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    画面遷移処理
      *****************************************************************
       200-GMNSENI                   SECTION.
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        戻る
               WHEN    "CLICKED"       ALSO    "B01"
                   PERFORM 210-BACK
      *    クリア
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 420-CLERA-SEC
      *    削除
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 430-SETDEL-SEC
      *    前回ＣＤ
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 440-ZEN-INPUTCD-SEC
      *    入力コード
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 440-INPUTCD-SEC
      *    前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *    次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *D   予約登録へ
      *D       WHEN    "CLICKED"       ALSO    "B10"
      *D           PERFORM 480-YOYAKU-SEC
      *D   受付一覧へ
      *D       WHEN    "CLICKED"       ALSO    "B11"
      *D           PERFORM 490-UKEICHI-SEC
      **    印刷
               WHEN    "CLICKED"       ALSO    "B09"
                    PERFORM 4110-PRINT-SEC
      *     登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 4100-TOROKU-SEC
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
           MOVE    "1"                 TO  SPA-UPDFLG
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        入外区分
               WHEN    "ACTIVATE"      ALSO    "NYUGAIKBN"
                   PERFORM 410-NYUGAIKBN-SEC
      *        老人区分
               WHEN    "ACTIVATE"      ALSO    "ROUJIN"
                   PERFORM 420-ROUJIN-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM"
      *!!
                   MOVE    SPA-K05-SELNUM      TO  WRK-HZN-SELNUM
      *!!
                   PERFORM 430-SELNUM-SEC
      *        履歴選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 440-RRKLIST-SEC
      *        他入力
               WHEN    OTHER
      *             入力個所のセット
                    PERFORM 210-GMN-INPUT-SEC
                    MOVE    ZERO                TO  FLG-KOUSIN
      *             入力チェック処理
                    PERFORM 220-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分入力処理
      *****************************************************************
       410-NYUGAIKBN-SEC               SECTION.
      *
           MOVE    K05-NYUGAIKBN       TO  SPA-K05-NYUGAIKBN-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
              IF      SPA-K05-NYUGAIKBN    =   SPA-K05-NYUGAIKBNLST
                                                     (IDX)(1:1)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-K05-NYUGAIKBNLST(IDX)
                                           TO  SPA-K05-NYUGAIKBN-G
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO 
               MOVE    "5020"              TO  SPA-ERRCD
               MOVE    7                   TO  SPA-GMN5-CUR
               GO      TO      410-NYUGAIKBN-EXT
           END-IF
      *
           MOVE    SPA-K05-NYUGAIKBN   TO  SPA-NYUGAIKBN
           IF      SPA-GMN-MAX         >   ZERO
               MOVE    3                   TO  SPA-GMN5-CUR
      *        区分変更時内容チェック
               PERFORM 4101-CHENGE-CHK-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN5-CUR
           END-IF
      *
           .
       410-NYUGAIKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    区分変更時内容チェック処理
      *****************************************************************
       4101-CHENGE-CHK-SEC               SECTION.
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
           PERFORM VARYING IDX         FROM    WRK-STR  BY  1
                   UNTIL   IDX         >   SPA-MAX-LINE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD (IDX)
           END-PERFORM
      *
           MOVE    ZERO                   TO  WRK-STR
      *    画面ＳＰＡチェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       4101-CHENGE-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入外区分入力処理
      *****************************************************************
       420-ROUJIN-SEC               SECTION.
      *
           MOVE    K05-ROUJIN       TO  SPA-K05-ROUJIN-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
              IF      SPA-K05-ROUJIN       =   SPA-K05-ROUJINLST
                                                     (IDX)(1:1)
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-K05-ROUJINLST(IDX)
                                           TO  SPA-K05-ROUJIN-G
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO 
               MOVE    "5021"              TO  SPA-ERRCD
               MOVE    8                   TO  SPA-GMN5-CUR
               GO      TO      420-ROUJIN-EXT
           END-IF
      *
           MOVE    SPA-K05-ROUJIN      TO  SPA-NAI-ROUJIN
           IF      SPA-GMN-MAX         >   ZERO
               MOVE    3                   TO  SPA-GMN5-CUR
      *        区分変更時内容チェック
               PERFORM 4101-CHENGE-CHK-SEC
           ELSE
               MOVE    1                   TO  SPA-GMN5-CUR
           END-IF
      *
           .
       420-ROUJIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号入力処理
      *****************************************************************
       430-SELNUM-SEC               SECTION.
      *
           MOVE    K05-SELNUM          TO  SPA-K05-SELNUM
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >  SPA-K05-RRK-MAX )
                           OR (FLG-CHK    =   1    )
              IF      SPA-K05-TRENNUM (IDX) =   SPA-K05-SELNUM
                   MOVE    1               TO  FLG-CHK
                   MOVE    SPA-K05-TYUKOSTYMD (IDX)
                                           TO  WRK-YUKOSTYMD
                   MOVE    SPA-K05-TYUKOEDYMD (IDX)
                                           TO  WRK-YUKOEDYMD
               END-IF
           END-PERFORM
           IF      SPA-K05-SELNUM      =   ZERO
               MOVE    3               TO  SPA-GMN5-CUR
           ELSE
               IF      FLG-CHK             =   1
      *!!
      *            名称編集
                   IF      WRK-HZN-SELNUM      =   ZERO
                       MOVE    1               TO  FLG-KOUSIN
                       PERFORM 30011-SYUSEI-HEN-SEC
                       MOVE    ZERO            TO  FLG-KOUSIN
                   END-IF
      *!!
      *            セット展開
                   INITIALIZE                  SPA-SCR-REC
                   MOVE    1                   TO  SPA-GMN-IDX
                   MOVE    SPA-GMN-SETCODE     TO  WRK-SETCD
                   MOVE    1                   TO  FLG-SYOKI
                   PERFORM 300113-YUKOKIKAN-HEN-SEC
                   PERFORM 4101191-INPSET-HEN-SEC
                   PERFORM 510-TBLHEN-SEC
      *            入力コードセット
                   PERFORM 410111-INPUTCD-SET-SEC
                   MOVE    3                   TO  SPA-GMN5-CUR
               ELSE
                   MOVE    "5025"              TO  SPA-ERRCD
                   MOVE    13                  TO  SPA-GMN5-CUR
               END-IF
           END-IF
      *
           .
       430-SELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    履歴選択処理
      *****************************************************************
       440-RRKLIST-SEC               SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-K05-RRK-MAX
              IF      K05-RRKLIST-SELECT (IDX) =  "T"
                  IF      IDX                  =   SPA-K05-SELNUM
                      CONTINUE
                  ELSE
                      MOVE    IDX              TO  WRK-SELNUM
                  END-IF
              END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
      *!!
               MOVE    SPA-K05-SELNUM      TO  WRK-HZN-SELNUM
      *!!
               MOVE    WRK-SELNUM          TO  SPA-K05-SELNUM
               MOVE    SPA-K05-SELNUM      TO  K05-SELNUM
               PERFORM 430-SELNUM-SEC
           END-IF
           .
       440-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       210-GMN-INPUT-SEC               SECTION.
      *
           MOVE    ZERO                TO  SPA-GMN5-CUR
      *    入力項目の決定
           PERFORM 2001-INPUT-SET-SEC
      *    画面をＳＰＡへ編集
           PERFORM 2002-GMNSPA-SET-SEC
      *
           .
       210-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET(1:07)    =   "INPUTCD"
               MOVE    MCP-WIDGET(8:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:7)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:6)     =   "MEISYO"
               MOVE    MCP-WIDGET(7:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:6)     TO  WRK-WIDGET
           END-IF
           IF      MCP-WIDGET(1:3)     =   "KEI"
               MOVE    MCP-WIDGET(4:2)     TO  WRK-IDX2
               MOVE    MCP-WIDGET(1:3)     TO  WRK-WIDGET
           END-IF
           IF      WRK-WIDGET          =   SPACE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
               IF      SPA-GMN-IDX     >   SPA-MAX-LINE
                   MOVE    SPA-MAX-LINE    TO  SPA-GMN-IDX
                   MOVE    "7001"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面項目→ＳＰＡ編集処理
      *****************************************************************
       2002-GMNSPA-SET-SEC               SECTION.
      *
           IF      K05-SETCODE         =   SPA-MAE-SETCODE
           AND    (SPA-K05-SYORI   NOT =   SPACE )
               MOVE    ZERO                TO  FLG-IN-SETCODE
           ELSE
               MOVE    1                   TO  FLG-IN-SETCODE
           END-IF
           MOVE    K05-SETCODE         TO  SPA-GMN-SETCODE
      *
           MOVE    K05-NAME            TO  SPA-GMN-DSPNAME
      *
           MOVE    K05-YUKOSTYMD       TO  SPA-K05-GMN-YUKOSTYMD
           MOVE    K05-YUKOEDYMD       TO  SPA-K05-GMN-YUKOEDYMD
      *
           PERFORM 20021-INPUT-GMNSET-SEC
           .
       2002-GMNSPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードのＳＰＡ編集処理
      *****************************************************************
       20021-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   MAX-GMN
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (K05-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (K05-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
               END-IF
      *
               MOVE    K05-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    K05-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   IF     (K05-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      K05-INPUTCD (WRK-IDX2)  NOT =   SPACE
                   IF      WRK-STR             =   ZERO
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       20021-INPUT-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力チェック処理
      *****************************************************************
       220-INPUT-CHK-SEC               SECTION.
      *
      *    セットコード
           PERFORM 41011-SETCODE-HEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END         NOT =   ZERO  )
               GO      TO      220-INPUT-CHK-EXT
           END-IF
      *    登録時、セットコードに変更がある時、再編集へ
           IF      FLG-KOUSIN          =   1
               IF      FLG-IN-SETCODE      =   1
                   GO      TO      220-INPUT-CHK-EXT
               END-IF
           END-IF
      *    開始日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41013-YUKOSTYMD-HEN-SEC
           END-IF
      *    終了日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41014-YUKOEDYMD-HEN-SEC
           END-IF
      *
      *    セット名
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41012-NAME-HEN-SEC
           END-IF
      *
      *    入力コード・名称
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41011-KOUI-ALLHEN-SEC
           END-IF
      *
           .
       220-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットコード名入力
      *****************************************************************
       41011-SETCODE-HEN-SEC               SECTION.
      *
           IF      SPA-GMN-SETCODE(1:1)    =   "P"  OR  "S"
               CONTINUE
           ELSE
               MOVE    "5001"          TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN5-CUR
               IF     (SPA-K05-SYORI   NOT =   "4"   )
                  AND (SPA-GMN-SETCODE NOT =   SPACE )
                   PERFORM 410112-K98HYOJI02-SEC
               END-IF
               GO      TO      41011-SETCODE-HEN-EXT
           END-IF
      *
      *    桁数チェック
           MOVE    ZERO                TO  WRK-MOJISU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   6   )   OR
                              (SPA-GMN-SETCODE(IDX:1)  =   SPACE )
               MOVE    IDX             TO  WRK-MOJISU
           END-PERFORM
      *    ６桁以外の時、一覧表を表示（複写以外の時）
           IF     (WRK-MOJISU      NOT =   6 )  AND
                  (SPA-K05-SYORI   NOT =   "4" )
               MOVE    1               TO  SPA-GMN5-CUR
               PERFORM 410112-K98HYOJI-SEC
               GO      TO      41011-SETCODE-HEN-EXT
           END-IF
      *
      *----(01.00.01) LINE ADD END   ----------------------------------
           IF     (SPA-GMN-SETCODE(2:2)    NOT =   SPACE )  AND
                  (SPA-GMN-SETCODE(4:3)        NUMERIC   )
               CONTINUE
           ELSE
               MOVE    "5001"          TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN5-CUR
               GO      TO      41011-SETCODE-HEN-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *    入力なしの時、以下の処理は行わない
           IF      FLG-IN-SETCODE      =   ZERO
               GO      TO      41011-SETCODE-HEN-EXT
           END-IF
      *
      *    入力セット存在チェック
           INITIALIZE                      INPUTCD-REC
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    SPA-GMN-SETCODE     TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               IF      SPA-K05-SYORI       =   "4"
                   MOVE    "5002"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN5-CUR
               ELSE
      *            修正メッセージへ
                   MOVE    "0003"              TO  SPA-KIDCD
                   MOVE    "2"                 TO  SPA-K05-SYORI
               END-IF
           ELSE
      *        追加処理へ
               MOVE    "1"                 TO  SPA-K05-SYORI
               INITIALIZE                      SPA-K051-TBL
               INITIALIZE                      SPA-K05-KIKAN-AREA
               INITIALIZE                      SPA-K051-AREA
      *
               MOVE    2                   TO  SPA-GMN5-CUR
               MOVE    "00000000"          TO  SPA-K05-GMN-YUKOSTYMD
               MOVE    "00000000"          TO  SPA-K05-NAI-YUKOSTYMD
               MOVE    "99999999"          TO  SPA-K05-GMN-YUKOEDYMD
               MOVE    "99999999"          TO  SPA-K05-NAI-YUKOEDYMD
               MOVE    SPA-K05-MAE-SRYYMD  TO  SPA-SRYYMD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-SETCODE     TO  SPA-MAE-SETCODE
           END-IF
      *
           .
       41011-SETCODE-HEN-EXT.
           EXIT.
      *****************************************************************
      *    修正の入力コードセット処理
      *****************************************************************
       30011-SYUSEI-HEN-SEC               SECTION.
      *
      *    入力セット存在チェック
           INITIALIZE                      INPUTCD-REC
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    SPA-GMN-SETCODE     TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                 920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "5003"              TO  SPA-ERRCD
               GO      TO      30011-SYUSEI-HEN-EXT
           END-IF
      *
           MOVE    "2"                 TO  SPA-K05-SYORI
           MOVE    ICD-DSPNAME         TO  SPA-GMN-DSPNAME
      *
           IF      FLG-KOUSIN          =   1
      *        更新前のコード存在チェック
               CONTINUE
           ELSE
      *        セット内容展開
               PERFORM 300111-SETTENKAI-SEC
           END-IF
           .
       30011-SYUSEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    セット内容展開処理
      *****************************************************************
       300111-SETTENKAI-SEC               SECTION.
      *
           INITIALIZE                  SPA-K05-KIKAN-AREA
           INITIALIZE                  SPA-K051-AREA
           INITIALIZE                  SPA-SCR-REC
      *
           MOVE    1                   TO  SPA-GMN-IDX
           MOVE    SPA-GMN-SETCODE     TO  WRK-SETCD
           MOVE    1                   TO  FLG-SYOKI
      *    期間決定
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    FLG-INPUTSET    =   1
               ADD     1               TO  IDX
               IF      IDX             >   10
                   CONTINUE
               ELSE
                   PERFORM 301112-RRKYMD-SET-SEC
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (SPA-K05-RRK-MAX     =   1 ) 
      ****     OR (SPA-GMN-SETCODE(1:1)    =   "P" )
               MOVE    1                   TO  SPA-K05-SELNUM
               MOVE    SPA-K05-TYUKOSTYMD(1)
                                           TO  WRK-YUKOSTYMD
               MOVE    SPA-K05-TYUKOEDYMD(1)
                                           TO  WRK-YUKOEDYMD
               MOVE    1                   TO  FLG-SYOKI
               PERFORM 300113-YUKOKIKAN-HEN-SEC
               PERFORM 4101191-INPSET-HEN-SEC
               PERFORM 510-TBLHEN-SEC
      *        入力コードセット
               PERFORM 410111-INPUTCD-SET-SEC
               MOVE    SPACE               TO  SPA-ERRCD
      *
               MOVE    2                   TO  SPA-GMN5-CUR
           ELSE
      *        入力コードセット
               PERFORM 410111-INPUTCD-SET-SEC
      *
               MOVE    13                  TO  SPA-GMN5-CUR
           END-IF
      *
           .
       300111-SETTENKAI-EXT.
           EXIT.
      *****************************************************************
      *    期間編集処理
      *****************************************************************
       301112-RRKYMD-SET-SEC               SECTION.
      *
           ADD     1                   TO  SPA-K05-RRK-MAX
           MOVE    SPA-K05-RRK-MAX     TO  SPA-K05-TRENNUM    (IDX)
           MOVE    ISET-YUKOSTYMD      TO  SPA-K05-TYUKOSTYMD (IDX)
           MOVE    ISET-YUKOEDYMD      TO  SPA-K05-TYUKOEDYMD (IDX)
      *
           MOVE    ISET-YUKOSTYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K05-TYUKOSTYMDG(IDX)
           MOVE    ISET-YUKOEDYMD      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-K05-TYUKOEDYMDG(IDX)
      *
           .
       301112-RRKYMD-SET-EXT.
           EXIT.
      *****************************************************************
      *    有効期間編集処理
      *****************************************************************
       300113-YUKOKIKAN-HEN-SEC               SECTION.
      *
           MOVE    SPA-K05-TYUKOSTYMDG(SPA-K05-SELNUM)
                                       TO  SPA-K05-GMN-YUKOSTYMD
           MOVE    SPA-K05-TYUKOEDYMDG(SPA-K05-SELNUM)
                                       TO  SPA-K05-GMN-YUKOEDYMD
           MOVE    SPA-K05-TYUKOSTYMD (SPA-K05-SELNUM)
                                       TO  SPA-K05-NAI-YUKOSTYMD
           MOVE    SPA-K05-TYUKOEDYMD (SPA-K05-SELNUM)
                                       TO  SPA-K05-NAI-YUKOEDYMD
      *
           IF     SPA-K05-NAI-YUKOEDYMD   <   "99999999"
               MOVE    SPA-K05-NAI-YUKOEDYMD   TO  SPA-SRYYMD
           ELSE
               IF      SPA-K05-NAI-YUKOSTYMD   =   "00000000"
                   MOVE    SPA-K05-MAE-SRYYMD      TO  SPA-SRYYMD
               ELSE
                   MOVE    SPA-K05-NAI-YUKOSTYMD   TO  SPA-SRYYMD
               END-IF
           END-IF
           .
       300113-YUKOKIKAN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードセット処理
      *****************************************************************
       410111-INPUTCD-SET-SEC               SECTION.
      *
           INITIALIZE                      SPA-K051-TBL
           MOVE    ZERO                TO  IDX
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-SETCODE     TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           PERFORM
               UNTIL    (FLG-INPUTCD       =   1    )   OR
                        (IDX               >   5    )
               IF      ICD-INPUTCD         =   SPA-GMN-SETCODE
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX
                   IF      IDX                 >   5
                       MOVE    "5000"          TO  SPA-ERRCD
                   ELSE
                       MOVE    ICD-INPUTCD     TO  SPA-K051-INPUTCD
                                                                (IDX)
                       MOVE    ICD-CDSYU       TO  SPA-K051-CDSYU (IDX)
                   END-IF
               END-IF
      *
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       410111-INPUTCD-SET-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
      *H27.2
      *****************************************************************
      *    修正の入力コードセット処理（ＮＯ選択）
      *****************************************************************
       30013-SYUSEI-NASI-SEC               SECTION.
      *
           INITIALIZE                  SPA-K05-KIKAN-AREA
           INITIALIZE                  SPA-K051-AREA
      *
           MOVE    SPA-GMN-SETCODE     TO  WRK-SETCD
      **** MOVE    1                   TO  FLG-SYOKI
      *    期間決定
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM UNTIL    FLG-INPUTSET    =   1
               ADD     1               TO  IDX
               IF      IDX             >   10
                   CONTINUE
               ELSE
                   PERFORM 301112-RRKYMD-SET-SEC
               END-IF
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           END-PERFORM
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入力コードセット
           PERFORM 410111-INPUTCD-SET-SEC
      *
           MOVE    1                   TO  SPA-GMN5-CUR
      *
           MOVE    "2"                 TO  SPA-K05-SYORI
           .
       30013-SYUSEI-NASI-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名入力処理
      *****************************************************************
       410112-K98HYOJI-SEC               SECTION.
      *
           INITIALIZE                      SPA-K98-AREA
           MOVE    "6"                 TO  SPA-K98-CDSYUBETU
           MOVE    SPA-GMN-SETCODE     TO  SPA-K98-INPUTCD
           MOVE    "1"                 TO  SPA-K98-TYPE
      *
      *    画面編集
           INITIALIZE                  SPA-K98GMN-AREA
           PERFORM 4101-K98-SET-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    画面カーソルセット処理
           IF      SPA-K98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
      *    診療行為一覧へ
           MOVE    "K98"               TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K05"               TO  SPA-K98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410112-K98HYOJI-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名入力処理
      *****************************************************************
       410112-K98HYOJI02-SEC               SECTION.
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "1"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-SETCODE     TO  KANACHK-MAE-INPUT
           MOVE    6                   TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-MAE-INPUT   TO  SPA-GMN-SETCODE
           IF      KANACHK-RC2         =   7
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-SETCODE
           END-IF
           IF      KANACHK-RC      NOT =   ZERO
               GO      TO      410112-K98HYOJI02-EXT
           END-IF
           MOVE    KANACHK-MAX     TO  WRK-MOJISU
      *
           INITIALIZE                      SPA-K98-AREA
      *    全角
           IF      KANACHK-SYUBETU     =   2
      *        全角入力
               MOVE    "J"                 TO  SPA-K98-CDSYUBETU
           ELSE
               IF     (SPA-GMN-SETCODE(1:WRK-MOJISU)   NUMERIC)
                   EVALUATE    WRK-MOJISU
                       WHEN    4
                           MOVE    "4"         TO  SPA-K98-CDSYUBETU
                       WHEN    5
                           MOVE    "5"         TO  SPA-K98-CDSYUBETU
                       WHEN    3
                       WHEN    6
                           MOVE    "6"         TO  SPA-K98-CDSYUBETU
                       WHEN    OTHER
                           MOVE    "A"         TO  SPA-K98-CDSYUBETU
                   END-EVALUATE
               ELSE
                   MOVE    "A"             TO  SPA-K98-CDSYUBETU
               END-IF
           END-IF
           MOVE    SPA-GMN-SETCODE     TO  SPA-K98-INPUTCD
           MOVE    "1"                 TO  SPA-K98-TYPE
      *
           MOVE    SPACE               TO  SPA-ERRCD
      *    画面編集
           INITIALIZE                  SPA-K98GMN-AREA
      *    セットのみ検索
           MOVE    "T"                 TO  SPA-K98-GMN-SYORI
           PERFORM 4101-K98-SET-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    画面カーソルセット処理
           IF      SPA-K98-GMN-MAX     =   ZERO
               MOVE    "5100"          TO  SPA-ERRCD
               GO      TO      410112-K98HYOJI02-EXT
           END-IF
           MOVE    "SELNUM01"          TO  MCP-WIDGET
      *
      *    診療行為一覧へ
           MOVE    "K98"               TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K05"               TO  SPA-K98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410112-K98HYOJI02-EXT.
           EXIT.
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
      *****************************************************************
      *    開始日処理
      *****************************************************************
       41013-YUKOSTYMD-HEN-SEC               SECTION.
      *
           IF      SPA-K05-GMN-YUKOSTYMD   =   SPACE
                                           OR  "00000000"
               MOVE    "00000000"          TO  SPA-K05-GMN-YUKOSTYMD
               MOVE    "00000000"          TO  SPA-K05-NAI-YUKOSTYMD
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-K05-GMN-YUKOSTYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-K05-GMN-YUKOSTYMD
                   MOVE    SGDAY-SDAY      TO  SPA-K05-NAI-YUKOSTYMD
      *D           IF      SPA-GMN-SETCODE(1:1)    =   "P"
      *                MOVE    "5026"          TO  SPA-ERRCD
      *                MOVE    11              TO  SPA-GMN5-CUR
      *D           END-IF
               ELSE
                   MOVE    "5022"          TO  SPA-ERRCD
                   MOVE    11              TO  SPA-GMN5-CUR
               END-IF
           END-IF
      *
           .
       41013-YUKOSTYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    終了日処理
      *****************************************************************
       41014-YUKOEDYMD-HEN-SEC               SECTION.
      *
           IF      SPA-K05-GMN-YUKOEDYMD   =   SPACE
                                           OR  "99999999"
               MOVE    "99999999"          TO  SPA-K05-GMN-YUKOEDYMD
               MOVE    "99999999"          TO  SPA-K05-NAI-YUKOEDYMD
      *        IF     (SPA-K05-MAE-SRYYMD
      *                                NOT =   SPA-SRYYMD )
      ***        AND  (SPA-K05-NAI-YUKOSTYMD   <   SPA-SRYYMD)
      *            MOVE    SPA-K05-MAE-SRYYMD  TO  SPA-SRYYMD
      *            PERFORM 410121-SAIHEN-SEC
      *        ELSE
      *            IF     (SPA-K05-NAI-YUKOSTYMD   >   "00000000") AND
      *                   (SPA-K05-NAI-YUKOSTYMD
      *                                    NOT =   SPA-SRYYMD )
      *                MOVE    SPA-K05-NAI-YUKOSTYMD  TO  SPA-SRYYMD
      *                PERFORM 410121-SAIHEN-SEC
      *            END-IF
      ***      END-IF
      *        開始日＞システム日付は開始日を診療日
      ******** IF      (SPA-K05-NAI-YUKOSTYMD  >   SPA-K05-MAE-SRYYMD)
               IF      (SPA-K05-NAI-YUKOSTYMD  NOT =   "00000000" )
                    IF      SPA-K05-NAI-YUKOSTYMD  NOT =   SPA-SRYYMD
                        MOVE    SPA-K05-NAI-YUKOSTYMD  TO  SPA-SRYYMD
                        PERFORM 410121-SAIHEN-SEC
                    END-IF
               ELSE
                    IF      SPA-K05-MAE-SRYYMD     NOT =   SPA-SRYYMD
                        MOVE    SPA-K05-MAE-SRYYMD     TO  SPA-SRYYMD
                        PERFORM 410121-SAIHEN-SEC
                    END-IF
               END-IF
           ELSE
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-K05-GMN-YUKOEDYMD   TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY     TO  SPA-K05-GMN-YUKOEDYMD
                   MOVE    SGDAY-SDAY      TO  SPA-K05-NAI-YUKOEDYMD
      *D           IF      SPA-GMN-SETCODE(1:1)    =   "P"
      *                MOVE    "5026"          TO  SPA-ERRCD
      *D               MOVE    12              TO  SPA-GMN5-CUR
      *D           ELSE
                       IF      SPA-K05-NAI-YUKOEDYMD
                                           NOT =   SPA-SRYYMD
                           MOVE    SPA-K05-NAI-YUKOEDYMD
                                               TO  SPA-SRYYMD
                           PERFORM 410121-SAIHEN-SEC
                       END-IF
      *d           END-IF
               ELSE
                   MOVE    "5023"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN5-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K05-NAI-YUKOSTYMD
                                       >=   SPA-K05-NAI-YUKOEDYMD
                   MOVE    "5024"          TO  SPA-ERRCD
                   MOVE    12              TO  SPA-GMN5-CUR
               END-IF
           END-IF
           .
       41014-YUKOEDYMD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    点数名称再表示処理
      *****************************************************************
       410121-SAIHEN-SEC               SECTION.
      *
           PERFORM VARYING IDX         FROM    1  BY  1
                   UNTIL   IDX         >   SPA-MAX-LINE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD (IDX)
               MOVE    SPACE           TO  SPA-COM-KZMFLG5 (IDX)
           END-PERFORM
      *
           MOVE    1                      TO  FLG-SAIHEN
      **   MOVE    ZERO                   TO  WRK-STR
      *    画面ＳＰＡチェック処理
      *    PERFORM 410112-KOUI-SPACHK-SEC
      ***  MOVE    ZERO                   TO  FLG-SAIHEN
           .
       410121-SAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット名入力処理
      *****************************************************************
       41012-NAME-HEN-SEC               SECTION.
      *
           IF      SPA-GMN-DSPNAME     =   SPACE
               IF      FLG-KOUSIN      NOT =   ZERO
                   MOVE    "5004"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN5-CUR
               END-IF
           ELSE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "1"                 TO  KANACHK-SYORI
               MOVE    SPA-GMN-DSPNAME     TO  KANACHK-MAE-INPUT
               MOVE    80                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-DSPNAME
           END-IF
      *
           .
       41012-NAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC           SECTION.
      *
      *    画面ＳＰＡチェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       41011-KOUI-ALLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC          SECTION.
      *
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
           MOVE    ZERO                TO  FLG-INS
           PERFORM VARYING SPA-GMN-IDX     FROM    WRK-STR  BY  1
                   UNTIL   SPA-GMN-IDX     >   SPA-MAX-LINE
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HENDEL-SEC
               END-IF
           END-PERFORM
           IF      FLG-INS             =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
           END-IF
      *
           IF      WRK-ERRCD       NOT =   SPACE
               MOVE    WRK-ERRCD           TO  SPA-ERRCD
               MOVE    WRK-ERRMSG          TO  SPA-ERRMSG2
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )  AND
                     ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "81" OR "83" OR "84") OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "008" ) ) AND
                      (SPA-GMN-MEISYO-G(SPA-GMN-IDX) 
                                       NOT =   SPACE  )
                   PERFORM 401131-MEISYO-HENKAN-SEC
               END-IF
      *
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               IF      FLG-END         =   1
      *            ＳＰＡの内容を画面へ編集する。カーソル設定なし
                   PERFORM 500-GMNHEN-SEC
               END-IF
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *    行挿入、警告あり
           IF     (FLG-INS             =   1     )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           IF     (SPA-ERRCD       =   SPACE
                                   OR  "0021" )  AND
                  (FLG-END         =   ZERO   )
               CONTINUE
           ELSE
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    診療コード入力
           PERFORM 410119-SAIINPUT-SEC
      *
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC              SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
               IF      SPA-GMN-IDX     >   SPA-MAX-LINE
                   MOVE    SPA-MAX-LINE    TO  SPA-GMN-IDX
                   MOVE    "7001"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
      *****MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    1                   TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  SPA-GMN-INCNT  (SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-GMNFLG (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR    (SPA-GMN-IDX)
               IF      WRK-ERRCD           =   SPACE
                   MOVE    SPA-ERRCD           TO  WRK-ERRCD
               END-IF
           END-IF
      *
      *    削除時
           IF      LNK-SC97-INPUTCD(1:1)   =   SPACE
      *
      *        セットが削除の時セット内容を削除する
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
      *
      *    剤削除時
           IF      LNK-SC97-INPUTCD(1:1)   =   "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HENDEL-EXT
           END-IF
      *
           .
       41011-KOUI-HENDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-HEN2-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
      *****MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
           MOVE    1                   TO  LNK-SC97-JIKANTOKU
           MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
           MOVE    LNK-SC97-NYURYOKU   TO  FLG-NYURYOKU
           MOVE    LNK-SC97-INCD       TO  WRK-CD
           MOVE    LNK-SC97-INCDCNT    TO  WRK-CD-MCNT
      *    '_'を削除した時、クリアする
           IF      LNK-SC97-JODOU      >   ZERO
               MOVE    "3"             TO  SPA-COM-IN  (SPA-GMN-IDX)
           END-IF
      *
      *    行挿入
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "+"
               MOVE    SPACE           TO  WRK-HEN-INPUTCD
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)
                                       TO  WRK-HEN-INPUTCD
               MOVE    WRK-HEN-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41014-GYOINSN-SEC
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               ADD     1               TO  SPA-GMN-IDX
               MOVE    1               TO  FLG-INS
               GO  TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    一覧へ
           IF      LNK-SC97-INPUTCD(1:2)
                                       =  "//"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INPUTCD    TO  SPA-K98-INPUTCD
               PERFORM 410-K98-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-K99-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *        セットが変更削除の時セット内容を削除する
               IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
                   PERFORM 41016-SETDEL-SEC
               END-IF
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
           ELSE
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
           END-IF
      *
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *    診療行為一覧へ
           IF      LNK-SC97-KENSAKU    =   "1"
               INITIALIZE                      SPA-K98-AREA
               MOVE    LNK-SC97-INCD       TO  SPA-K98-INPUTCD
               MOVE    "1"                 TO  SPA-K98-TYPE
               MOVE    LNK-SC97-CDSYU      TO  SPA-K98-CDSYUBETU
               MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-K98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
           ELSE
      *        文字変換・編集処理
               PERFORM 410112-INPUTCD-HENKAN-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX
           MOVE    ZERO                TO  WRK-IN-IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
      *        薬評・器評の数量なしとする
               IF     (SPA-COM-IN2     (IDX)   =   "2" )
                  AND (SPA-COM-YAKUHYO (IDX)   =   "1" )
                   IF    (SPA-COM-INPUTCD (IDX)(1:1)  =   "6"
                                                      OR  "7" )
                     OR  (SPA-COM-INPUTCD (IDX)(1:3)  =   "059" )
                       MOVE    SPACE           TO  SPA-COM-IN2 (IDX)
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      WRK-IN-IDX          =   ZERO
               MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *
      *    数量・回数入力なし
           IF     (SPA-COM-SURFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (SPA-COM-IN2    (SPA-GMN-IDX)  =   "2"   ) AND
                  (SPA-COM-KAIFLG (SPA-GMN-IDX)  =   SPACE ) AND
                  (WRK-IN-IDX2                   =   ZERO  )
               MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                            TO  WRK-INPUTCD
               MOVE    "1"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001" )
                 AND  (SPA-COM-YCOMKBN (SPA-GMN-IDX)   =   0    )
      *            投薬の時のみ、回数入力へ
                   IF      SPA-COM-SRYKBN(SPA-GMN-IDX)     =   "21" OR
                                                               "22" OR
                                                               "23"
                       STRING  WRK-INPUTCD      DELIMITED  BY  SPACE
                               "*"              DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                       END-STRING
                   ELSE
                       MOVE    SPACE            TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                   END-IF
               ELSE
                   STRING  WRK-INPUTCD          DELIMITED  BY  SPACE
                           "_"                  DELIMITED  BY  SIZE
                                                INTO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
                   END-STRING
               END-IF
               MOVE    SPACE                TO  SPA-ERRCD
           END-IF
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "81"  OR  "83" ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"   )
                   OR (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0085"   )
                   OR (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0086"   )
                   IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                               =   "831"
                       CONTINUE
                   ELSE
                       MOVE    "3"         TO  SPA-COM-IN2(SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF      FLG-KUHAKU          =   1
               IF     (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                               NOT =   "."  )  AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)    =   SPACE )
      *            回数入力へ
                   MOVE    ZERO                TO  WRK-IN-IDX2
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   54
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                                   =   "*"
                           MOVE    1               TO  FLG-ARI
                       END-IF
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                               NOT =  SPACE
                           MOVE    IDX             TO  WRK-IN-IDX2
                       END-IF
                   END-PERFORM
                   IF     (FLG-ARI                 =   ZERO )  AND
                          (WRK-IN-IDX2             <   54   )
                       ADD     1                   TO  WRK-IN-IDX2
                       IF     (SPA-COM-IN3 (SPA-GMN-IDX)  =   "1" ) AND
                             ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                      NOT =   "001") OR
                              (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                          =   "001" AND
                               SPA-COM-YCOMKBN (SPA-GMN-IDX)
                                                          =   1    )
      *H30.4             分割調剤など
                         OR   (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992081"  )
      *R02.4
      *        新コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85"   ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                       =   "831" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE )))
      *
                           MOVE    "_"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       ELSE
                           MOVE    "*"             TO  SPA-GMN-INPUTCD
                                                     (SPA-GMN-IDX)
                                                     (WRK-IN-IDX2:)
                       END-IF
                   END-IF
               END-IF
      *        空白行入力時、最終行へ
               MOVE    "1"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF
      *
           .
       410119-SAIINPUT-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    文字列変換・編集処理
      *****************************************************************
       410112-INPUTCD-HENKAN-SEC      SECTION.
      *
      *    変換文字列編集
           PERFORM 4101121-MOJI-SET-SEC
      *
      *    セット入力処理へ
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  OR  "P"
               PERFORM 410119-INPSET-SEC
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *!!
      *    約束セットは器材商品名は入力不可とする
           IF      (SPA-GMN-SETCODE(1:1)   =   "S"    )  AND
                   (LNK-SC97-INCD (1:3)    =   "058"  )
               MOVE    "5028"              TO  SPA-ERRCD
               GO      TO      410112-INPUTCD-HENKAN-EXT
           END-IF
      *
      *    入力内容チェック、編集
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    LNK-SC97-INCD   TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF
           PERFORM 4101113-INPUTCD-CHK-SEC
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC
      *
           .
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC         SECTION.
      *
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
               CONTINUE
           ELSE
      *        名称を戻す
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "1"  )
                 AND  (SPA-COM-AUTOKBN2 (SPA-GMN-IDX)
                                           =   SPACE )
                   MOVE    SPA-COM-NAME(SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                        (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  SPA-COM-IN3 (SPA-GMN-IDX)
      *    入力値の入力判定用
           MOVE    SPA-COM-ATAI1 (SPA-GMN-IDX) TO  WRK-HZN-ATAI1
           MOVE    SPA-COM-ATAI2 (SPA-GMN-IDX) TO  WRK-HZN-ATAI2
           MOVE    SPA-COM-ATAI3 (SPA-GMN-IDX) TO  WRK-HZN-ATAI3
           MOVE    SPA-COM-ATAI4 (SPA-GMN-IDX) TO  WRK-HZN-ATAI4
           MOVE    SPA-COM-ATAI5 (SPA-GMN-IDX) TO  WRK-HZN-ATAI5
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    入力コード変更ありの時
           IF      LNK-SC97-NYURYOKU   =   1
             OR   (FLG-SAIHEN          =   1    )
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
      *
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K05"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
      *
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
           MOVE    LNK-SC92-TIMEFLG    TO  SPA-NAI-TIMEFLG
      *    数量ゼロフラグ
           IF      SPA-SURYOZERO-FLG   =   1
               IF     (LNK-SC97-SURYOCNT   >   ZERO  )  AND
                      (LNK-SC97-SURYO      =   ZERO  )
                   MOVE    ZERO                TO  FLG-ZEROOK
                   IF    ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                               =   "6"  OR  "7") OR
                         ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "059"       )  OR
                         ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                               =   "1"     ) AND
                          (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                           NOT =   0        )  )
                       MOVE    1                   TO  FLG-ZEROOK
                    END-IF
                    IF    (FLG-ZEROOK              =   1   )
                    AND   (SPA-GMN-SETCODE(1:1)    =   "P" )
                       CONTINUE
                   ELSE
                       MOVE    "A002"              TO  SPA-ERRCD
                       GO      TO      4101113-INPUTCD-CHK-EXT
                   END-IF
               END-IF
           END-IF
      *    カーソル位置設定
           PERFORM 41011139-INFLG-HEN-SEC
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  AND
                  (LNK-SC92-ERRMSG NOT =   SPACE )
               MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
           END-IF
      *
           PERFORM VARYING     IDX-ERR FROM    1  BY  1
                   UNTIL      (IDX-ERR     >   10     )  OR
                              (SPA-ERRCD   NOT =   SPACE)
               IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                   MOVE    LNK-SC92-ERRCD(IDX-ERR)     TO  SPA-ERRCD
      *            算定履歴,併用算定チェックははずす
                   IF      IDX-ERR             =   3  OR  6
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
               END-IF
           END-PERFORM
      *    その他値の入力の有無
           IF     (WRK-HZN-ATAI1   =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI2   =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI3   =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI4   =   SPA-COM-ATAI4 (SPA-GMN-IDX)) AND
                  (WRK-HZN-ATAI5   =   SPA-COM-ATAI5 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
      *
      *    警告判定
           IF      SPA-ERRCD(1:1)      =   "K"
             AND  (SPA-ERRCD       NOT =   "K033" )
               IF      SPA-COM-KZMFLG (SPA-GMN-IDX)  =   SPACE
                   MOVE    "1"             TO  SPA-COM-KZMFLG
                                                          (SPA-GMN-IDX)
                   MOVE    "2"             TO  SPA-COM-CUR(SPA-GMN-IDX)
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-COM-CUR(SPA-GMN-IDX)
               END-IF
           END-IF
      *---(01.00.01) LINE ADD  END    ----
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPACE           TO  SPA-COM-CHKFLG  (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *!!  保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    1               TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-KEI(SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-COM-JIHIMONEY
                                                          (SPA-GMN-IDX)
                   MOVE    WRK-SURYO       TO  SPA-GMN-KEI(SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"             TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    カーソル位置設定処理
      *****************************************************************
       41011139-INFLG-HEN-SEC       SECTION.
      *
      *    数量以降入力なし（’＿’編集用）
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )  AND
                  (WRK-MCNT5           =   ZERO )  AND
                  (FLG-KAISU           =   ZERO )
               MOVE    ZERO                TO  FLG-IN1
      *        薬剤と数量入力必須の診療行為、薬剤コメント
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
                   MOVE    "7"                     TO  WRK-MOJI
               ELSE
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1)
                                                   TO  WRK-MOJI
               END-IF
      *        数量の入力の有無
               IF     (WRK-MOJI                    =   "6" OR
                                                       "7" )  OR
                     ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001" )
      *                  用量コメント
                 OR  ( SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" )
      *            薬剤・器材・用法
      *            点数ゼロで金額はなし
                   IF     (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO ) AND
                          (SPA-COM-TENSIKIBETU (SPA-GMN-IDX)
                                                       =   1    )
                       CONTINUE
                   ELSE
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
               IF      WRK-MOJI                =   "1"
                   IF  (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)(1:1)
                                                    NOT =   "7" ) AND
                       (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                                    NOT =   0   )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" )  OR
                      (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084")
      *            コメント
                   MOVE    1                   TO  FLG-IN1
               END-IF
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:5)
                                                   =   "09500" OR
                                                       "09600" )
      *            自費
                   IF     (SPA-COM-TEN (SPA-GMN-IDX)   =   ZERO )
                       MOVE    1                   TO  FLG-IN1
                   END-IF
               END-IF
      *        用法コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "001" )
                 AND  (SPA-COM-YCOMKBN(SPA-GMN-IDX) =   1 )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *H30.4   分割調剤など
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                               =   "0992081" )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *R02.4
      *        新コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                               =   "85"  )
                  OR  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "831"  )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *
               IF      FLG-IN1             =   1
                   MOVE    "2"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN3 (SPA-GMN-IDX)
               END-IF
           END-IF
      *
           .
       41011139-INFLG-HEN-EXT.
           EXIT.
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-K99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-K99GMN-AREA
           MOVE    SPACE               TO  K99
      *
      *    画面カーソルセット処理
           INITIALIZE                      K99
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-K99-DATA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K99"               TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K05"               TO  SPA-K99-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K99"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-K99-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為一覧へ
      *****************************************************************
       410-K98-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-KOUI
      *
      *    内服を編集する
           INITIALIZE                  SPA-K98GMN-AREA
      *****IF      SPA-K98-INPUTCD         =   SPACE
           IF      SPA-K98-INPUTCD(1:2)    =   "//"
               EVALUATE    SPA-K98-INPUTCD(3:1)
      *        コメント一覧
                   WHEN    "C"
                   WHEN    "c"
                       MOVE    "U"             TO  SPA-K98-GMN-SYORI
                       MOVE    "U4"            TO  SPA-K98-GMN-SYORI3
                       MOVE    SPACE           TO  SPA-K98-INPUTCD
      *        用法
                   WHEN    "Y"
                   WHEN    "y"
                       MOVE    "U"             TO  SPA-K98-GMN-SYORI
                       MOVE    "U1"            TO  SPA-K98-GMN-SYORI3
                       MOVE    SPACE           TO  SPA-K98-INPUTCD
      *        診療種別一覧
                   WHEN    "."
                       MOVE    "."             TO  SPA-K98-INPUTCD
      *            区分番号別
                   WHEN    "*"
                       MOVE    SPA-K98-INPUTCD(3:) TO  SPA-K98-INPUTCD
                   WHEN    "/"
                       MOVE    SPA-K98-INPUTCD(3:) TO  SPA-K98-INPUTCD
      *H25.6       労災一覧
                   WHEN    "R"
                   WHEN    "r"
                       MOVE    SPA-K98-INPUTCD     TO  SPA-K98-GMN-INPUT
                   WHEN    OTHER
                       MOVE    SPACE               TO  SPA-K98-INPUTCD
               END-EVALUATE
      *        画面編集
               MOVE    SPACE               TO  K98
               INITIALIZE                      K98
               PERFORM 4101-K98-SET-SEC
           ELSE
               INITIALIZE                  SPA-K98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)   =  "S")
      *            内服薬剤
                   MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               ELSE
                   EVALUATE    SPA-COM-SRYKBN(SPA-GMN-IDX - 1)
                       WHEN    SPACE
      *                    診療区分が決定してない時
                           PERFORM  4101-K98-SYRKBN-SEC
                       WHEN    "31"
                       WHEN    "32"
                       WHEN    "33"
      *                    注射
                           MOVE    "5"         TO  SPA-K98-GMN-SYORI
                       WHEN    "23"
      *                    外薬
                           MOVE    "4"         TO  SPA-K98-GMN-SYORI
                       WHEN    "21"
                       WHEN    "22"
      *                    外薬
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                       WHEN    OTHER
      *                    診療行為
      *D                   MOVE    "8"         TO  SPA-K98-GMN-SYORI
                           MOVE    "3"         TO  SPA-K98-GMN-SYORI
                           MOVE    1           TO  FLG-KOUI
                   END-EVALUATE
               END-IF
      *        画面編集
               PERFORM 4101-K98-SET-SEC
      *
      *        点数マスタ編集で、対象データなしの時、診療行為で検索する
               IF     (SPA-K98-GMN-MAX     =   ZERO )  AND
                      (SPA-K98-CDSYUBETU   =   "J"  )  AND
                      (FLG-KOUI            =   1    )
                   INITIALIZE                  SPA-K98GMN-AREA
                   MOVE    "8"                 TO  SPA-K98-GMN-SYORI
      *            画面編集
                   PERFORM 4101-K98-SET-SEC
               END-IF
           END-IF
      *    対象データなし
      *????IF      SPA-K98-GMN-MAX     =   ZERO
      *        MOVE    "0001"              TO  SPA-ERRCD
      *        MOVE    3                   TO  SPA-GMN5-CUR
      *        MOVE    "1"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *        GO      TO      410-K98-SYORI-EXT
      *????END-IF
      *
      *    画面カーソルセット処理
           IF      SPA-K98-GMN-MAX     =   ZERO
               MOVE    "INPUT"             TO  MCP-WIDGET
           ELSE
               MOVE    "SELNUM01"          TO  MCP-WIDGET
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K98"               TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K05"               TO  SPA-K98-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K98"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
      *
       410-K98-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療区分が決定してない時診療種別処理
      *****************************************************************
       4101-K98-SYRKBN-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-IDX
               EVALUATE    SPA-GMN-INPUTCD(IDX)(1:3)
                   WHEN    ".21"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".22"
                       MOVE    "3"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".23"
                       MOVE    "4"                 TO  SPA-K98-GMN-SYORI
                   WHEN    ".31"
                   WHEN    ".32"
                   WHEN    ".33"
                       MOVE    "5"                 TO  SPA-K98-GMN-SYORI
               END-EVALUATE
           END-PERFORM
      *
           .
      *
       4101-K98-SYRKBN-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-K98-SET-SEC       SECTION.
      *
           MOVE    "K05"               TO  SPA-K98-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK98"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA.
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
           .
      *
       4101-K98-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力文字を数字変換処理
      *****************************************************************
       4101121-MOJI-SET-SEC        SECTION.
      *
           MOVE    LNK-SC97-SURYOCNT   TO  WRK-SUR-MCNT
           MOVE    LNK-SC97-KAISUCNT   TO  WRK-ACCT-MCNT
           MOVE    LNK-SC97-SURYO      TO  WRK-SURYO
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    LNK-SC97-KAISUFLG   TO  FLG-KAISU
           MOVE    LNK-SC97-BUNGSU     TO  WRK-BUNGSU
           MOVE    LNK-SC97-MCNT1      TO  WRK-MCNT1
           MOVE    LNK-SC97-MCNT2      TO  WRK-MCNT2
           MOVE    LNK-SC97-MCNT3      TO  WRK-MCNT3
           MOVE    LNK-SC97-MCNT4      TO  WRK-MCNT4
           MOVE    LNK-SC97-MCNT5      TO  WRK-MCNT5
           MOVE    LNK-SC97-MOJI1      TO  WRK-MOJI1
           MOVE    LNK-SC97-MOJI2      TO  WRK-MOJI2
           MOVE    LNK-SC97-MOJI3      TO  WRK-MOJI3
           MOVE    LNK-SC97-MOJI4      TO  WRK-MOJI4
           MOVE    LNK-SC97-MOJI5      TO  WRK-MOJI5
      *
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療コード入力チェック処理
      *****************************************************************
       4101121-INPUTCD-CHECK-SEC    SECTION.
      *
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                           TO  WRK-MOJI
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "059"
               MOVE    "7"                 TO  WRK-MOJI
           END-IF
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)   =   "008"
               MOVE    "8"                 TO  WRK-MOJI
           END-IF
      *
           EVALUATE    WRK-MOJI
      *
      *        診療行為
               WHEN    "1"
      *            きざみ値計算識別判定がない時、数量入力エラー(29)
                   IF      TNS-KZMCOMPSIKIBETU     =   0
                       IF      WRK-SUR-MCNT    NOT =   ZERO
                           MOVE    "0002"          TO  SPA-ERRCD
                       END-IF
                   END-IF
      *        特定器材
               WHEN    "7"
      *            1行目入力不可
                   IF      SPA-GMN-IDX     =   1
                       MOVE    "0013"          TO  SPA-ERRCD
                   END-IF
      *        コメント
               WHEN    "8"
      *            数量をゼロとする
                   MOVE    ZERO            TO  WRK-SUR-MCNT
               WHEN    OTHER
                   CONTINUE
           END-EVALUATE
      *
      *    時間加算チェック
           IF     (SPA-ERRCD           =   SPACE   )  AND
                  (FLG-JKNKSN      NOT =   ZERO    )
      *        診療で、手技料の時
               IF     (FLG-JKNKSN          =   1   OR  2  OR  3  ) AND
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                           =   "111"   OR
                                               "112"    )  AND
                      (TNS-DATAKBN         =   1        )
                   CONTINUE
               ELSE
                   MOVE    "0030"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4101121-INPUTCD-CHECK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    数量
           MOVE    SPACE              TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
           IF      WRK-SUR-MCNT        =   ZERO
               MOVE    1               TO  SPA-COM-SURYO(SPA-GMN-IDX)
               MOVE    SPACE           TO  SPA-COM-SURFLG(SPA-GMN-IDX)
           ELSE
               MOVE    WRK-SURYO       TO  SPA-COM-SURYO(SPA-GMN-IDX)
               IF      SPA-COM-SURYO(SPA-GMN-IDX)  =  ZERO
      *            数量ゼロフラグ
                   IF      SPA-SURYOZERO-FLG   =   ZERO
                       MOVE    1               TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                   END-IF
               END-IF
               MOVE    "1"             TO  SPA-COM-SURFLG(SPA-GMN-IDX)
      *!!
               IF      LNK-SC97-AUTOFLG2   =   "1"
                   MOVE    "1"         TO  SPA-COM-AUTOFLG2(SPA-GMN-IDX)
               END-IF
      *!!
           END-IF
           MOVE    SPA-COM-SURYO(SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2 (SPA-GMN-IDX)
      *
      *    回数（未入力時、１回）
           IF      WRK-ACCT-MCNT       =   ZERO
               MOVE    SPACE           TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    1               TO  SPA-COM-KAISU (SPA-GMN-IDX)
           ELSE
               MOVE    "1"             TO  SPA-COM-KAIFLG(SPA-GMN-IDX)
               MOVE    WRK-KAISU       TO  SPA-COM-KAISU (SPA-GMN-IDX)
               IF      SPA-COM-KAISU(SPA-GMN-IDX)
                                       =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU (SPA-GMN-IDX)
               END-IF
           END-IF
           MOVE    SPA-COM-KAISU(SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2 (SPA-GMN-IDX)
      *
      *    分画数（未入力時、１回）
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX) (1:1)  =   "7" ) AND
                  (WRK-BUNGSU          =   ZERO   )  AND
                  (LNK-SC97-MOJI2  NOT =   SPACE  )
      *        分画数変換
               PERFORM 41011221-BUNGSU-HENSYU-SEC
               IF      WRK-BUNGSU          =   ZERO
                   MOVE    1               TO  WRK-BUNGSU
               END-IF
           END-IF
           MOVE    WRK-BUNGSU          TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
      *    IF      WRK-BUNGSU          =   ZERO
      *        MOVE    1               TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
      *    END-IF
      *
      *    その他値
           IF     (WRK-MOJI1       =   SPA-COM-ATAI1 (SPA-GMN-IDX)) AND
                  (WRK-MOJI2       =   SPA-COM-ATAI2 (SPA-GMN-IDX)) AND
                  (WRK-MOJI3       =   SPA-COM-ATAI3 (SPA-GMN-IDX)) AND
                  (WRK-MOJI4       =   SPA-COM-ATAI4 (SPA-GMN-IDX)) AND
                  (WRK-MOJI5       =   SPA-COM-ATAI5 (SPA-GMN-IDX))
               MOVE    ZERO                TO  FLG-IN-ATAI
           ELSE
               MOVE    1                   TO  FLG-IN-ATAI
           END-IF
           MOVE    WRK-MOJI1           TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
           MOVE    WRK-MOJI2           TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
           MOVE    WRK-MOJI3           TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
           MOVE    WRK-MOJI4           TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
           MOVE    WRK-MOJI5           TO  SPA-COM-ATAI5 (SPA-GMN-IDX)
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    分画数変換処理
      *****************************************************************
       41011221-BUNGSU-HENSYU-SEC           SECTION.
      *
           MOVE    ZERO                TO  WRK-BUNGSU
           INITIALIZE                      ORCSNUMAREA
           MOVE    LNK-SC97-MOJI2(1:LNK-SC97-MCNT2)
                                       TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                       ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )
               MOVE    "0012"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM         TO  WRK-BUNGSU
           END-IF
           MOVE    SPACE              TO  LNK-SC97-MOJI2
           MOVE    SPACE              TO  WRK-MOJI2
           MOVE    ZERO               TO  LNK-SC97-MCNT2
      *
           .
       41011221-BUNGSU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    時間外がないこと
           IF      FLG-JKNKSN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
      *    セットコードが約束の時、セットは入力不可とする
           IF      SPA-GMN-SETCODE(1:1)    =   "S"
               MOVE    "5009"              TO  SPA-ERRCD
               GO      TO      410119-INPSET-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-SETSYORI
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF     (SPA-GMN-IDX         =   1   )   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)
                                                   NOT =   "." )
                   MOVE    "0048"              TO  SPA-ERRCD
                   GO      TO      410119-INPSET-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-SETCD
      *
           MOVE    ZERO                TO  FLG-SYOKI
           MOVE    SPA-SRYYMD          TO  WRK-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  WRK-YUKOEDYMD
           PERFORM 4101191-INPSET-HEN-SEC
           .
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPSET-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  WRK-ERRCD
           MOVE    SPACE               TO  WRK-ERRMSG
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-STR
      *
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
      *    約束セットがなくなるまで以下の処理を行う
           MOVE    ZERO                TO  IDX-IP
           PERFORM VARYING     IDX-YAK FROM    1   BY  1
                   UNTIL      (IDX-YAK     >   20 )  OR
                              (WRK-IDX-YAK(IDX-YAK)  =   ZERO  )  OR
                              (SPA-ERRCD     NOT =   SPACE )
               COMPUTE SPA-GMN-IDX         =   WRK-IDX-YAK (IDX-YAK)
                                           +   IDX-IP
               MOVE    ZERO                TO  IDX-IP
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   PERFORM 4101191-INPUTSET-SYORI2-SEC
               END-IF
           END-PERFORM
      *
      *    残量廃棄の有無判定
           PERFORM VARYING     IDX-H   FROM    WRK-GMN-STR  BY 1
                   UNTIL       IDX-H       >   SPA-GMN-IDX
               IF    ((SPA-COM-SRYKBN (IDX-H)(1:1)  =   "3" )  AND
                      (SPA-COM-INPUTCD(IDX-H)(1:1)  =   "6" ))
      *H27.11
      *            注射以下
               OR    ((SPA-HAIKIKBN-FLG    =   "2"   )   AND
                      (SPA-COM-INPUTCD(IDX-H)(1:1)  =   "6" ) AND
                      (SPA-COM-SRYKBN (IDX-H)(1:1)  =   "4"
                                                    OR  "5"
                                                    OR  "6"
                                                    OR  "7"
                                                    OR  "8" ))
      *
      *        注射薬でアンプル、少数以下がある時、次に残量廃棄がある
                   IF      (SPA-COM-YKZKBN (IDX-H)  =   "4"
                                                    OR  "6"  )  AND
                           (SPA-COM-TANICD (IDX-H)  =   "014"
                                                    OR  "022"
                                                    OR  "046" )  AND
                           (SPA-COM-SURYO(IDX-H)(6:3)  >   ZERO)
                       IF     (IDX-H               <   SPA-MAX-LINE) AND
                              (SPA-COM-INPUTCD(IDX-H + 1)
                                                   =   "099309901")
                           CONTINUE
                       ELSE
                           MOVE    "1"         TO  SPA-COM-AUTOFLG2
                                                            (IDX-H)
                           MOVE    IDX-H       TO  IDX
                           PERFORM 3101-INPUTCD-HEN-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101191-INPSET-HEN-EXT
           END-IF
      *
      **** PERFORM 510-TBLHEN-SEC
           MOVE    3                   TO  SPA-GMN5-CUR
      *
           .
       4101191-INPSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPUTSET-SYORI-SEC      SECTION.
      *
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  IDX-YAKUSOKU
           MOVE    ZERO                TO  WRK-IDX-YAKUSOKU
      *
      *    約束の時、１行前に診療種別があること
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF     (SPA-GMN-IDX         =   1   )   OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)
                                        NOT =   "." )
                   MOVE    "0048"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
      *    入力セットマスタ検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
           MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
           MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
      *    約束処方の初期処理
           IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" )  AND
                  (FLG-INPUTSET        =   ZERO  )
      *
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
           END-IF
      *
           PERFORM
                   UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )  OR
                             (SPA-GMN-IDX      >   SPA-MAX-LINE )
              ADD     1               TO  IDX-IP
      *!!
      *        器材商品コード対応
      *        1行前が器材コードの自動発生の時、置換え
               MOVE    ZERO            TO  FLG-AUTOKBN2
               IF     (SPA-GMN-IDX         >   1    )    AND
                      (ISET-INPUTCD (1:1)  =   "7"  )    AND
                      (SPA-COM-AUTOKBN2(SPA-GMN-IDX - 1)
                                           =   "1"   )   AND
                      (ISET-INPUTCD        =   SPA-COM-INPUTCD
                                                  (SPA-GMN-IDX - 1 ))
                   MOVE    1               TO  FLG-AUTOKBN2
                   COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX   -  1
               END-IF
      *!!
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *
      *        約束セットの時
               IF      ISET-INPUTCD(1:1)       =   "S"
                   MOVE    ISET-INPUTCD(1:6)   TO  SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX)
               END-IF
      *
               IF      ISET-SURYO1     =   ZERO
      *            数量ゼロフラグ
                   IF     (SPA-SURYOZERO-FLG   =   1      )  AND
                         ((ISET-INPUTCD(1:1)   =   "7"  OR  "6")  OR
                          (ISET-INPUTCD(1:1)   =   "1"         )  OR
                          (ISET-INPUTCD(1:3)   =   "059"     )) AND
                          (ISET-INPUTCD    NOT =   "799990070"
                                               AND "770020070" )
                       MOVE    ZERO            TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                       MOVE    "1"             TO  SPA-COM-SURFLG
                                                          (SPA-GMN-IDX)
                   ELSE
                       MOVE    1               TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                   END-IF
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX)
      *            １以上の時、数量入力とする
                   IF     (ISET-INPUTCD(1:1)   =   "1"
                                               OR  "6"
                                               OR  "7" )   AND
                         (ISET-SURYO1          >   1   )
                      MOVE    "1"              TO  SPA-COM-SURFLG
                                                          (SPA-GMN-IDX)
                   END-IF
               END-IF
      *        換算値
               MOVE    ISET-KANSURYO       TO  SPA-COM-KANSURYO
                                                           (SPA-GMN-IDX)
      *        関係コメント指示
               IF      ISET-INPUTKBN       =   "1"
                    MOVE    "1"            TO  SPA-COM-INPUTCONT
                                                           (SPA-GMN-IDX)
               END-IF
      *        内服１種類区分
               IF      ISET-INPUTKBN       =   "2"
                    MOVE    "1"            TO  SPA-COM-INPUTNAIF
                                                           (SPA-GMN-IDX)
               END-IF
      *
               MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
               IF      ISET-SURYO2     =   ZERO
                   MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               END-IF
               IF      ISET-KAISU      =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
                   MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *********MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *
               IF      ISET-INPUTCD        =   "810000001"
                                           OR  "008500000"
                                           OR  "008600000"
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (05)  TO  SPA-COM-ATAI5 (SPA-GMN-IDX)
      *
      *        約束処方の時、画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *            画面表示なし
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "." 
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                                 TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
      *
      *            約束処方の時、数量を計算する
                   IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *
      *                数量変更
                       COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                               =   SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                                               *   SPA-COM-SURYO
                                                          (WRK-GMN-IDX)
                       MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                         (SPA-GMN-IDX)
                       MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
                   END-IF
      *
      *            セット内に約束セットがある時
                   IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "S"
                       INITIALIZE                  WRK-MOJI-AREA
                       MOVE    SPA-GMN-IDX         TO  IDX
      *                入力コードの画面用再編集
                       PERFORM 3101-INPUTCD-HEN-SEC
      *                約束名称編集
                       MOVE    "6"                 TO  LNK-SC97-CDSYU
                       MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                                   TO  WRK-INPUTCD
                       PERFORM 41011921-INPSET-MEI-SEC
      *
                       MOVE    SPACE               TO  SPA-COM-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    1                   TO  FLG-YAKUSOKU
                       ADD     1                   TO  IDX-YAKUSOKU
                       IF      IDX-YAKUSOKU        >   20
                           MOVE    "0007"          TO  SPA-ERRCD
                       ELSE
                           MOVE    SPA-GMN-IDX     TO  WRK-IDX-YAK
                                                        (IDX-YAKUSOKU)
                       END-IF
                   ELSE
      *                基本情報編集チェック
                       PERFORM 41011910-INPUTCD-HEN-SEC
                   END-IF
               END-IF
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *!!
               IF      FLG-AUTOKBN2        =   1
                   MOVE    "1"             TO  SPA-COM-AUTOKBN2 
                                                      (SPA-GMN-IDX)
               END-IF
      *!!
      *
               IF     (SPA-ERRCD        NOT =   SPACE )  OR
                      (LNK-SC92-ERRAREA NOT =   SPACE )
                 OR   (FLG-ERR              =   1     )
                   MOVE    SPACE               TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               END-IF
      *
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
      *
               IF      (FLG-INPUTSET     =   ZERO  )  AND
                       (SPA-ERRCD        =   SPACE )
      *
      *            行を挿入して追加する
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
      *
           END-PERFORM
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    4101191-INPUTSET-SYORI-EXT
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    対象なし
           IF      IDX-IP              =   ZERO
               MOVE    "0001"              TO  SPA-ERRCD
               GO      TO    4101191-INPUTSET-SYORI-EXT
           END-IF
      *    最終行を回数入力とする
           IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
               MOVE    "1"             TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *        セットの時前に編集
               IF      WRK-ERRCD       =   SPACE
                   MOVE    SPA-GMN-INPUTCD(WRK-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(WRK-GMN-IDX)
               ELSE
                   MOVE    "1"         TO  SPA-COM-CUR (WRK-GMN-IDX)
               END-IF
           END-IF
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    基本情報編集チェック処理
      *****************************************************************
       41011910-INPUTCD-HEN-SEC     SECTION.
      *
      *    点数マスタ編集・基本チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "K05"               TO  LNK-SC92-PG
           MOVE    SPACE               TO  LNK-SC92-KBN
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-COM-INPUTCD (SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           CALL    "ORCSC92"           USING
                                           MCPAREA
                                           ORCSC92AREA
                                           SPA-SCR-REC
                                           SPA-AREA
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *
      *ver.4.5
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                       =   "6"     ) AND
                  (ISET-KANSURYO       >   ZERO    )
               IF      ISET-SURYO1         =  (ISET-KANSURYO
                                           *   SPA-COM-KANZANCHI
                                                     (SPA-GMN-IDX) )
                  CONTINUE
               ELSE
                   MOVE    ZERO            TO  SPA-COM-KANSURYO
                                                          (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1     TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    点数マスタなしのエラーは表示する
           IF      SPA-ERRCD           =   "0001"
               IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                   IF      WRK-ERRCD       =   SPACE
                       MOVE    LNK-SC92-ERRMSG     TO  WRK-ERRMSG
                   END-IF
                   MOVE    "S001"          TO  WRK-ERRCD
               END-IF
               MOVE    1                   TO  FLG-ERR
               MOVE    SPACE               TO  SPA-ERRCD
           ELSE
               MOVE    ZERO                TO  FLG-ERR
           END-IF
      *    警告
           IF      SPA-ERRCD(1:1)      =   "K"
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *    診療コードの数量ゼロフラグ判定
           IF     (SPA-SURYOZERO-FLG   =   1      )  AND
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                       =   "1"    )  AND
                  (SPA-COM-SURYO (SPA-GMN-IDX)
                                       =   ZERO   )
               IF      (SPA-COM-KZMCOMPSIKIBETU(SPA-GMN-IDX)
                                           =   0   ) 
                   MOVE    1               TO  SPA-COM-SURYO
                                                         (SPA-GMN-IDX)
                   MOVE    SPACE           TO  SPA-COM-SURFLG
                                                          (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    エラー時、追加しない（点数マスタなしは表示）
           IF     (SPA-ERRCD           NOT =   SPACE )  OR
                  (LNK-SC92-ERRAREA    NOT =   SPACE )
      *        訂正の展開時はすべて
               IF      FLG-SYOKI           =   1
                   CONTINUE
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
                   INITIALIZE              SPA-GMN-TBLREC (SPA-GMN-IDX)
                   INITIALIZE              SPA-COM-TBLREC (SPA-GMN-IDX)
                   IF      WRK-GMN-IDX     =   SPA-GMN-IDX
                       CONTINUE
                   ELSE
                       COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                               -   1
                   END-IF
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-ERRCD
      *        保険外金額判定
               PERFORM 4101191-JIHI-HEN-SEC
      *        入力コードの画面用再編集
               MOVE    SPA-GMN-IDX         TO  IDX
               PERFORM 3101-INPUTCD-HEN-SEC
           END-IF
      *
           .
       41011910-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束セット展開処理
      *****************************************************************
       4101191-INPUTSET-SYORI2-SEC     SECTION.
      *
      *    約束の時、１行前に診療種別があること
           IF     (SPA-GMN-IDX         =   1   )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1) NOT =   "." )
               MOVE    "0048"              TO  SPA-ERRCD
               MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
               GO      TO      4101191-INPUTSET-SYORI2-EXT
           END-IF
      *
      *    行オーバーの為、ＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *
           MOVE    ZERO                TO  IDX-IP
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:6)
                                       TO  WRK-SETCD
      *    入力セットマスタ検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    WRK-SETCD           TO  ISET-SETCD
           MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
           MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           IF      FLG-INPUTSET        =   ZERO
      *        行を挿入して追加する
               ADD     1               TO  SPA-GMN-IDX
               PERFORM 41014-GYOINSN-SEC
           END-IF
           PERFORM
                  UNTIL     (FLG-INPUTSET     =   1 )  OR
                             (SPA-ERRCD    NOT =   SPACE )  OR
                             (SPA-GMN-IDX      >=  SPA-MAX-LINE )
      *
      ******** 行を挿入して追加する
      ***      ADD     1               TO  SPA-GMN-IDX
      ******** PERFORM 41014-GYOINSN-SEC
      *
               ADD     1               TO  IDX-IP
      *
               MOVE    ISET-INPUTCD    TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      ISET-SURYO1     =   ZERO
                   MOVE    1           TO  SPA-COM-SURYO (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO1 TO  SPA-COM-SURYO (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
               IF      ISET-SURYO2     =   ZERO
                   MOVE    1           TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-SURYO2 TO  SPA-COM-BUNGSU(SPA-GMN-IDX)
               END-IF
               IF      ISET-KAISU      =   ZERO
                   MOVE    1           TO  SPA-COM-KAISU  (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-KAISU  TO  SPA-COM-KAISU  (SPA-GMN-IDX)
                   MOVE    "1"         TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
               END-IF
               MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
      *
      *********MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *
               IF      ISET-INPUTCD        =   "810000001"
                                           OR  "008500000"
                                           OR  "008600000"
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO-G
                                                          (SPA-GMN-IDX)
               ELSE
                   MOVE    ISET-COMMENT    TO  SPA-GMN-MEISYO
                                                          (SPA-GMN-IDX)
               END-IF
      *
               MOVE    ISET-ATAI (01)  TO  SPA-COM-ATAI1 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (02)  TO  SPA-COM-ATAI2 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (03)  TO  SPA-COM-ATAI3 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (04)  TO  SPA-COM-ATAI4 (SPA-GMN-IDX)
               MOVE    ISET-ATAI (05)  TO  SPA-COM-ATAI5 (SPA-GMN-IDX)
      *        換算値
               MOVE    ISET-KANSURYO       TO  SPA-COM-KANSURYO
                                                           (SPA-GMN-IDX)
      *        関係コメント指示
               IF      ISET-INPUTKBN       =   "1"
                    MOVE    "1"            TO  SPA-COM-INPUTCONT
                                                           (SPA-GMN-IDX)
               END-IF
      *        内服１種類区分
               IF      ISET-INPUTKBN       =   "2"
                    MOVE    "1"            TO  SPA-COM-INPUTNAIF
                                                           (SPA-GMN-IDX)
               END-IF
      *
      *        約束処方の時、画面表示なし
               IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *            画面表示なし
                   MOVE    "1"             TO  SPA-COM-SET(SPA-GMN-IDX)
               END-IF
      *
               IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "." 
      *            診療種別区分名称チェック
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
                   PERFORM 510-SRYKBN-MEI-SEC
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:9)
                                               TO  SPA-COM-INPUTCD
                                                     (SPA-GMN-IDX)
      *
      *            約束処方の時、数量を計算する
                   IF      SPA-GMN-INPUTCD(WRK-GMN-IDX)(1:1)   =   "S"
      *
      *                数量変更
                       COMPUTE SPA-COM-SURYO (SPA-GMN-IDX)
                                               =   SPA-COM-SURYO
                                                        (SPA-GMN-IDX)
                                               *   SPA-COM-SURYO
                                                        (WRK-GMN-IDX)
                       MOVE    SPA-COM-SURYO (SPA-GMN-IDX)
                                       TO  SPA-COM-SURYO2(SPA-GMN-IDX)
      *                回数変更
                       MOVE    SPA-COM-KAISU (WRK-GMN-IDX)
                                               TO  SPA-COM-KAISU
                                                       (SPA-GMN-IDX)
                       MOVE    SPA-COM-KAISU (SPA-GMN-IDX)
                                       TO  SPA-COM-KAISU2(SPA-GMN-IDX)
                   END-IF
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    "K05"               TO  LNK-SC92-PG
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *
                   MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *ver.4.5
                   IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                               =   "6"     ) AND
                          (ISET-KANSURYO       >   ZERO    )
                       IF      ISET-SURYO1     =  (ISET-KANSURYO
                                               *   SPA-COM-KANZANCHI
                                                     (SPA-GMN-IDX) )
                           CONTINUE
                       ELSE
                           MOVE    ZERO            TO  SPA-COM-KANSURYO
                                                          (SPA-GMN-IDX)
                           MOVE    ISET-SURYO1     TO  SPA-COM-SURYO
                                                          (SPA-GMN-IDX)
                       END-IF
                   END-IF
      *
      *            点数マスタなしのエラーは表示する
                   IF      SPA-ERRCD           =   "0001"
                       IF      SPA-COM-SET (SPA-GMN-IDX)   =  "1"
                           IF      WRK-ERRCD       =   SPACE
                               MOVE    LNK-SC92-ERRMSG
                                                       TO  WRK-ERRMSG
                           END-IF
                           MOVE    "S001"          TO  WRK-ERRCD
                       END-IF
                       MOVE    SPACE           TO  SPA-ERRCD
                   END-IF
      *           エラー時、追加しない（点数マスタなしは表示）
                  IF     (SPA-ERRCD           NOT =   SPACE )  OR
                         (LNK-SC92-ERRAREA    NOT =   SPACE )
                      MOVE    SPACE           TO  SPA-ERRCD
                      INITIALIZE              SPA-GMN-TBLREC
                                                          (SPA-GMN-IDX)
                      INITIALIZE              SPA-COM-TBLREC
                                                         (SPA-GMN-IDX)
                       IF      WRK-GMN-IDX     =   SPA-GMN-IDX
                           CONTINUE
                       ELSE
                           COMPUTE SPA-GMN-IDX     =   SPA-GMN-IDX
                                                   -   1
                       END-IF
                   ELSE
                       MOVE    SPACE           TO  SPA-ERRCD
      *                保険外金額判定
                       PERFORM 4101191-JIHI-HEN-SEC
      *                入力コードの画面用再編集
                       MOVE    SPA-GMN-IDX         TO  IDX
                       PERFORM 3101-INPUTCD-HEN-SEC
                   END-IF
               END-IF
      *
               IF      SPA-COM-KAIFLG (SPA-GMN-IDX)    =   "1"
                   MOVE    SPACE               TO  WRK-SRYKBN
               END-IF
      *
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 975-INPUTSET-READ-SEC
      *
      *
               IF     (FLG-INPUTSET        =   ZERO )  AND
                      (SPA-ERRCD           =   SPACE )
      *            行を挿入して追加する
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
          IF      (SPA-ERRCD       NOT =   SPACE )  AND
                  (FLG-INPUTSET        =   ZERO  )
               MOVE    WRK-SCR-REC        TO  SPA-SCR-REC
               MOVE    WRK-GMN-IDX        TO  SPA-GMN-IDX
               MOVE    ZERO               TO  IDX-IP
               GO      TO    4101191-INPUTSET-SYORI2-EXT
           END-IF
           MOVE    SPACE               TO  SPA-ERRCD
      *
      *    最終行を回数入力とする
           MOVE    "1"                 TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       4101191-INPUTSET-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険外金額判定処理
      *****************************************************************
       4101191-JIHI-HEN-SEC           SECTION.
      *
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)  =   "095"  OR
                                                          "096"
               IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
                   MOVE    "1"                 TO  SPA-COM-KEIFLG 
                                                          (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-KEI
                                                          (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
                                                          (SPA-GMN-IDX)
                   MOVE    ISET-SURYO1         TO  SPA-COM-JIHIMONEY
                                                          (SPA-GMN-IDX)
                   MOVE    1                   TO  SPA-COM-SURYO
                                                        (SPA-GMN-IDX)
               ELSE
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
      *****        MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-COM-KEI
      *                                                   (SPA-GMN-IDX)
      *            MOVE    SPA-COM-KISOTEN (SPA-GMN-IDX)
      *                                        TO  SPA-GMN-KEI
      ****                                               (SPA-GMN-IDX)
               END-IF
      ****     MOVE    1                   TO  SPA-COM-SURYO
      *****                                             (SPA-GMN-IDX)
           ELSE
               IF      WRK-SRYKBN          =   "95"  OR  "96"
                   MOVE    "2"                 TO  SPA-COM-KEIFLG
                                                         (SPA-GMN-IDX)
      ********     MOVE    ISET-SURYO1         TO  SPA-COM-KEI
      *                                                  (SPA-GMN-IDX)
      *            MOVE    ISET-SURYO1         TO  SPA-GMN-KEI
      *                                                  (SPA-GMN-IDX)
      *            MOVE    1                   TO  SPA-COM-SURYO
      ********                                          (SPA-GMN-IDX)
               END-IF
           END-IF
      *    残量廃棄の為、診療区分設定
           IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:1) =   "1" ) AND
                  (SPA-COM-DATAKBN(SPA-GMN-IDX) =   "1" ) AND
                  (WRK-SRYKBN (1:1)    NOT =   "9"         )
               MOVE    SPA-COM-TNSSRYKBN (SPA-GMN-IDX) TO  WRK-SRYKBN
           END-IF
           MOVE    WRK-SRYKBN          TO  SPA-COM-SRYKBN (SPA-GMN-IDX)
      *
           .
       4101191-JIHI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力文字を数字へ変換
           MOVE    1                   TO  WRK-IDX-FT
           PERFORM 4101121-MOJI-SET-SEC
      *
      *    入力位置のセット
           MOVE    ZERO                TO  WRK-IN-SURYO
           MOVE    SPA-GMN-IDX         TO  WRK-IN-IDX
      *    数量以降入力なし
           IF     (WRK-SUR-MCNT        =   ZERO )  AND
                  (WRK-ACCT-MCNT       =   ZERO )  AND
                  (WRK-MCNT1           =   ZERO )  AND
                  (WRK-MCNT2           =   ZERO )  AND
                  (WRK-MCNT3           =   ZERO )  AND
                  (WRK-MCNT4           =   ZERO )
               IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)  =   "S"
                   MOVE    1                   TO  WRK-IN-SURYO
                   MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
               END-IF
           END-IF
      *
      *    数量・回数編集
           PERFORM 4101122-KOMOKU-HENSYU-SEC
           MOVE    WRK-CD(1:WRK-CD-MCNT)   TO  WRK-INPUTCD
      *
      *    約束名称編集
           PERFORM 41011921-INPSET-MEI-SEC
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       41011921-INPSET-MEI-SEC           SECTION.
      *
      *    約束名称編集
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
      *
           MOVE    LNK-SC97-CDSYU      TO  ICD-CDSYU
           MOVE    WRK-INPUTCD         TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD     NOT =   ZERO
               MOVE    "0050"          TO  SPA-ERRCD
               GO  TO                  41011921-INPSET-MEI-EXT
           END-IF
           MOVE    ICD-DSPNAME         TO  SPA-GMN-MEISYO(SPA-GMN-IDX)
      *
      *    回数入力としない
           MOVE    SPACE               TO  SPA-COM-KAIFLG (SPA-GMN-IDX)
      *
           .
       41011921-INPSET-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
      *    空白行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    剤分離処理
           INITIALIZE                  ORCSC93AREA
           MOVE    SPA-GMN-SETCODE     TO  LNK-SC93-SETCODE
           MOVE    "K05"               TO  LNK-SC93-PG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-SC93-HAIKIKBN-FLG
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *    約束セットは薬評・減点を入力不可とする
           IF      (SPA-GMN-SETCODE(1:1)   =   "S"    ) 
               PERFORM 5009-SETCHEK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *
      *    剤別初回編集
           INITIALIZE                  ORCSC00AREA
      *    診療種別件数クリア
           INITIALIZE                  SPA-SRYKBN-AREA
      *    剤ごとの処理
           MOVE    ZERO                TO  IDX-Z
           MOVE    ZERO                TO  WRK-KAISU
           MOVE    ZERO                TO  WRK-LAST-IDX
           MOVE    SPACE               TO  WRK-KEI-ERRCD
           MOVE    SPACE               TO  WRK-ERR-ERRCD
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  WRK-HZN-SCR-AREA
           INITIALIZE                  WRK-HOZ-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )   OR
                              (SPA-GMN-INPUTCD (IDX)
                                                   =   SPACE )
      *        回数の入力があるかのチェック
               PERFORM 51011-KAISU-CHK-SEC
      *        剤ごとの算定処理で変更される加算データは対象としない
      *          但し、コメントの自動追加分は、そのまま
      *              （注射の廃棄コメントははずす）
               MOVE    ZERO                TO  FLG-TAISYO
               IF      SPA-COM-AUTOKBN (IDX)    =   "2"  OR  "3"
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)
                                               NOT =   "8"   )   OR
                          ((SPA-COM-SRYKBN  (IDX)  =   "31" OR
                                                       "32" OR
                                                       "33")   AND
                           (SPA-COM-INPUTCD (IDX)  =   "099309901" ))
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            薬剤の自動追加コメントは対象外
                   IF     ( SPA-COM-INPUTCD (IDX)(1:1)   =  "8" ) AND
                          ( SPA-COM-SRYKBN  (IDX)    =   "21"
                                                     OR  "22"
                                                     OR  "33"  )
                        MOVE    1               TO  FLG-TAISYO
                   END-IF
      *            自動追加コメントで入力値ありの時対象外
                   IF      (SPA-COM-INPUTCD (IDX)(1:2)   =  "84") OR
                           (SPA-COM-INPUTCD (IDX)(1:4)   =  "0084")
                        MOVE    ZERO            TO  FLG-TAISYO
                   END-IF
               END-IF
      *
               IF      FLG-TAISYO              =   ZERO
                   ADD     1               TO  IDX-Z
                   MOVE    SPA-COM-TBLREC (IDX)
                                           TO  WRK-COM-TBLREC(IDX-Z)
                   MOVE    SPA-GMN-TBLREC (IDX)
                                           TO  WRK-GMN-TBLREC(IDX-Z)
               END-IF
      *        回数編集
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."
                   CONTINUE
               ELSE
                   IF      SPA-COM-KAIFLG (IDX)    =   "1"
                        MOVE    SPA-COM-KAISU (IDX) TO  WRK-KAISU
      *                 外用薬の時、入力回数を
                        IF     (SPA-COM-SRYKBN(IDX) =  "23"  )  AND
                               (SPA-COM-KAISU2(IDX) >   ZERO ) AND
                               (SPA-COM-KAISU (IDX) =   1    )
                            MOVE    SPA-COM-KAISU2 (IDX) TO  WRK-KAISU
                        END-IF
                   END-IF
               END-IF
      *
               IF     (IDX                 =   SPA-GMN-MAX   )  OR
                      (SPA-COM-ZAIEND (IDX)
                                           =   "1"           )
      *            剤ごとの処理
                   PERFORM 5102-ZAIBETU-SEC
                   INITIALIZE                  WRK-SCR-REC
                   MOVE    ZERO                TO  IDX-Z
                   MOVE    ZERO                TO  WRK-KAISU
               END-IF
      *        エラーコードを保存する
               IF      SPA-ERRCD       NOT =   SPACE
                   IF      SPA-ERRCD(1:1)      =   "K"
                       MOVE    SPA-ERRCD       TO  WRK-KEI-ERRCD
                   ELSE
                       IF      WRK-ERR-ERRCD       =   SPACE
                           MOVE    SPA-ERRCD       TO  WRK-ERR-ERRCD
                       END-IF
                   END-IF
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-PERFORM
      *@@
      *検査一覧表示へ
           IF      WRK-ERR-ERRCD       =   "AK98"
               MOVE    SPACE               TO  WRK-ERR-ERRCD
               MOVE    1                   TO  SPA-AKUSEI-FLG
           ELSE
               MOVE    ZERO                TO  SPA-AKUSEI-FLG
           END-IF
      *@@
      *
      *    エラーの再セットを行う
           IF      WRK-ERR-ERRCD   NOT =   SPACE
               MOVE    WRK-ERR-ERRCD       TO  SPA-ERRCD
           END-IF
      *    警告メッセージの時、ＳＰＡの再セットを行う
           IF     (SPA-ERRCD           =   SPACE)  AND
                  (WRK-KEI-ERRCD   NOT =   SPACE)
               MOVE    WRK-KEI-ERRCD       TO  SPA-ERRCD
           END-IF
      *
      *    スパ領域へ戻す
           MOVE    ZERO                TO  SPA-GMN-MAX
           INITIALIZE                  SPA-GMN-REC
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (WRK-HZN-GMN-INPUTCD (IDX)   =   SPACE)  AND
                      (WRK-HZN-COM-INPUTCD (IDX)   =   SPACE)
                   CONTINUE
               ELSE
                   ADD     1                   TO  SPA-GMN-MAX
                   MOVE    WRK-HZN-GMN-TBLREC (IDX)
                                               TO  SPA-GMN-TBLREC
                                                         (SPA-GMN-MAX)
                   MOVE    WRK-HZN-COM-TBLREC (IDX)
                                               TO  SPA-COM-TBLREC
                                                         (SPA-GMN-MAX)
      *@
      *            検査一覧表示へ
                   IF      SPA-AKUSEI-FLG      =   1
                       IF    SPA-COM-CUR (SPA-GMN-MAX)  =  "A"
                           MOVE    SPA-GMN-MAX     TO  SPA-AKUSEI-IDX
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                       (SPA-GMN-MAX)
                       END-IF
                    END-IF
      *@
               END-IF
           END-PERFORM
      *@@
      *悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
               PERFORM 5101-AKUSEI-K98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *@@
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *
      *    登録前の全体のチェックを行う
           IF      FLG-TOROKU      NOT =   "2"
               INITIALIZE                  ORCSC00AREA
               MOVE    "2"                 TO  LNK-ORCSC-KBN
               PERFORM 520-ALL-CHEK-SEC
           END-IF
      *
      *
           .
       510-TBLHEN-EXT.
           EXIT.
      *****************************************************************
      *    約束セットの薬評・減点チェック処理
      *****************************************************************
       5009-SETCHEK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE  )   OR
                              (SPA-GMN-INPUTCD (IDX)
                                                   =   SPACE ) OR
                              (SPA-ERRCD       NOT =   SPACE )
               IF      SPA-COM-GENTEN (IDX)    =   "1"
                   MOVE    "5029"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
               IF      SPA-COM-YAKUHYO(IDX)    =   "1"
                   MOVE    "5030"              TO  SPA-ERRCD
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
           END-PERFORM
           .
       5009-SETCHEK-EXT.
           EXIT.
      *****************************************************************
      *    最終行に回数の入力があるかのチェック処理
      *****************************************************************
       51011-KAISU-CHK-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-KAISU
           PERFORM VARYING     IDX-KAI FROM    1   BY  1
                   UNTIL       IDX-KAI     >   54
               IF      FLG-OK              =   1
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1) NUMERIC
                       MOVE    1               TO  FLG-KAISU
                   END-IF
               ELSE
                   IF      SPA-GMN-INPUTCD(IDX)(IDX-KAI:1)   =   "*"
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *    ＊ 以降に入力のない時、＊を削除
           IF      FLG-KAISU           =   ZERO
               INSPECT SPA-GMN-INPUTCD(IDX)
                       REPLACING   FIRST "*"  BY  " "
           END-IF
      *
           .
       51011-KAISU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤ごとの処理
      *****************************************************************
       5102-ZAIBETU-SEC             SECTION.
      *
           IF      WRK-KAISU           =   ZERO
               MOVE    1               TO  WRK-KAISU
           END-IF
      *
      *****INITIALIZE                  ORCSC00AREA
           INITIALIZE                  LNK-ORCSZAI-AREA
           MOVE    "1"                 TO  LNK-ORCSC-KBN
           MOVE    "K05"               TO  LNK-ORCSC-PG
           MOVE    WRK-KAISU           TO  LNK-ORCSC-KAISU
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-ORCSC-JIKANTOKU
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-ORCSC-HAIKIKBN-FLG
      *
           EVALUATE    WRK-COM-SRYKBN (01)
      *        診療
               WHEN    "11"
               WHEN    "12"
               WHEN    "13"
               WHEN    "14"
                   CALL    "ORCSC10"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (01)
      *            薬剤
               WHEN    "21"
               WHEN    "22"
               WHEN    "23"
                   CALL    "ORCSC20"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (02)
      *            注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   CALL    "ORCSC30"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (03)
      *            処置
               WHEN    "40"
                   CALL    "ORCSC40"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (04)
      *            手術
               WHEN    "50"
                   CALL    "ORCSC50"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (05)
      *            麻酔
               WHEN    "54"
                   CALL    "ORCSC54"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (06)
      *            検査
               WHEN    "60"
               WHEN    "64"
                   CALL    "ORCSC60"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (07)
      *            画像診断
               WHEN    "70"
                   CALL    "ORCSC70"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (08)
      *            その他
               WHEN    "80"
                   CALL    "ORCSC80"       USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (09)
      *            コメント等
      *********WHEN    "90"
               WHEN    "93"
               WHEN    "99"
                   PERFORM 510211-SRYKBN-90-SEC
      *            自費
               WHEN    "95"
               WHEN    "96"
                   CALL    "ORCSCJIH"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *************PERFORM 510212-SRYKBN-95-SEC
      *            入院（食事）
               WHEN    "97"
                   CALL    "ORCSCN97"      USING
                                           MCPAREA
                                           ORCSC00AREA
                                           WRK-SCR-REC
                                           SPA-AREA
                   ADD     1               TO  SPA-SRYKBN-CNT (10)
           END-EVALUATE
      *
      *    編集後内容セット
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
      *        診区
               IF      IDX-Z               =   1
                   MOVE    WRK-COM-SRYKBN (IDX-Z)
                                           TO  WRK-GMN-SRYKBN(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-GMN-SRYKBN(IDX-Z)
                   MOVE    WRK-COM-SRYSYUKBN (01)
                                           TO  WRK-COM-SRYSYUKBN(IDX-Z)
               END-IF
      *        名称（最初の行に’＊’）
               IF     (WRK-COM-INPUTCD(IDX-Z)(1:1) =   "8" )  OR
                      (WRK-COM-INPUTCD(IDX-Z)(1:3) =   "008")
                   CONTINUE
               ELSE
                   IF      IDX-Z               =   1
                       MOVE    "* "            TO  WRK-GMN-MEIH (IDX-Z)
                   ELSE
                       MOVE    SPACE           TO  WRK-GMN-MEIH (IDX-Z)
                   END-IF
               END-IF
      *
      *        最終行を剤終了とする
               IF      (IDX-Z              =   SPA-MAX-LINE )  OR
                       (WRK-COM-INPUTCD(IDX-Z + 1) =   SPACE  AND
                        WRK-GMN-INPUTCD(IDX-Z + 1) =   SPACE )
                   MOVE    "1"             TO  WRK-COM-ZAIEND(IDX-Z)
               ELSE
                   MOVE    SPACE           TO  WRK-COM-ZAIEND(IDX-Z)
               END-IF
      *        回数
               IF      LNK-ORCSC-KAISU NOT =   WRK-COM-KAISU (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU     TO  WRK-COM-KAISU(IDX-Z)
               END-IF
      *        最終行に回数入力（＊）が有った時、最終行を回数入力
               IF      FLG-KAISU           NOT =   ZERO
                   IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   "1"
                           MOVE    SPACE           TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                           PERFORM 51021-KAISU-SAIHEN-SEC
                       END-IF
                   ELSE
      *                最終行
                       IF      WRK-COM-KAIFLG (IDX-Z)  =   SPACE
                           PERFORM 51021-KAISU-SAIHEN-SEC
                           MOVE    "1"             TO  WRK-COM-KAIFLG
                                                               (IDX-Z)
                       END-IF
                   END-IF
               END-IF
      *
      *        約束セット行の編集
               IF      WRK-GMN-INPUTCD(IDX-Z)(1:1) =   "S"
                   MOVE    LNK-ORCSC-ZAITEN        TO  WRK-COM-TENSU
                                                                (IDX-Z)
                   MOVE    LNK-ORCSC-ZAIKEI        TO  WRK-COM-KEI
                                                               (IDX-Z)
                   MOVE    LNK-ORCSC-KAISU         TO  WRK-COM-KAISU
                                                               (IDX-Z)
               END-IF
      *        画面再編集・基本チェック
               INITIALIZE                  ORCSC94AREA
               MOVE    "2"                 TO  LNK-SC94-KBN
               MOVE    "K05"               TO  LNK-SC94-PG
               MOVE    IDX-Z               TO  LNK-SC94-IDX
               CALL    "ORCSC94"           USING
                                           ORCSC94AREA
                                           WRK-SCR-REC
                                           SPA-AREA
      *
               ADD     1                   TO  WRK-HOZ-IDX
               MOVE    WRK-COM-TBLREC (IDX-Z)
                                           TO  WRK-HZN-COM-TBLREC
                                                       (WRK-HOZ-IDX)
               MOVE    WRK-GMN-TBLREC (IDX-Z)
                                           TO  WRK-HZN-GMN-TBLREC
                                                       (WRK-HOZ-IDX)
      *
           END-PERFORM
      *
           .
       5102-ZAIBETU-EXT.
           EXIT.
      *
      *****************************************************************
      *    回数の表示変更処理
      *****************************************************************
       51021-KAISU-SAIHEN-SEC          SECTION.
      *
      *
           IF      WRK-COM-ZAIEND (IDX-Z)  =   SPACE
      *        *　以降を削除
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX-KAI FROM    1   BY  1
                       UNTIL      (IDX-KAI     >   54  )   OR
                                  (FLG-OK      =   1   )
                   IF      WRK-GMN-INPUTCD(IDX-Z)(IDX-KAI:1)   =   "*"
                       MOVE    IDX-KAI         TO  IDX-KAI2
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK          =   1
                   MOVE    SPACE           TO  WRK-GMN-INPUTCD(IDX-Z)
                                                   (IDX-KAI2:)
               END-IF
           ELSE
      *        * を追加
      *
               MOVE    WRK-COM-KAISU(IDX-Z)    TO  WRK-KAISU-Z
               MOVE    ZERO                TO  IDK2
               MOVE    SPACE               TO  WRK-KAISU-H 
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   4
                   IF      WRK-KAISU-X(IDK1:1)  NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X(IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
      *
               MOVE    WRK-GMN-INPUTCD (IDX-Z) TO  WRK-HEN-INPUTCD
               MOVE    SPACE                   TO  WRK-GMN-INPUTCD
                                                            (IDX-Z)
      *
               STRING  WRK-HEN-INPUTCD         DELIMITED   BY  SPACE
                       "*"                     DELIMITED   BY  SIZE
                       WRK-KAISU-H             DELIMITED   BY  SPACE
                       INTO                    WRK-GMN-INPUTCD(IDX-Z)
               END-STRING
      *
           END-IF
           .
       51021-KAISU-SAIHEN-EXT.
           EXIT.
      *****************************************************************
      *     コメント等　編集処理
      *****************************************************************
       510211-SRYKBN-90-SEC            SECTION.
      *
      *
           PERFORM VARYING     IDX-Z   FROM    1   BY  1
                   UNTIL      (IDX-Z       >   SPA-MAX-LINE  )  OR
                              (WRK-GMN-INPUTCD (IDX-Z) =   SPACE )
               IF      WRK-COM-KISOTEN (IDX-Z) =   ZERO
                   MOVE    ZERO            TO  WRK-COM-KEI (IDX-Z)
                   MOVE    1               TO  WRK-COM-KAISU(IDX-Z)
                   MOVE    1               TO  WRK-COM-SURYO(IDX-Z)
                   MOVE    ZERO            TO  WRK-COM-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-TENSU(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-SURYO(IDX-Z)
                   MOVE    SPACE           TO  WRK-GMN-KAISU(IDX-Z)
                   MOVE    ZERO            TO  WRK-GMN-KEI (IDX-Z)
               END-IF
           END-PERFORM
      *
           .
       510211-SRYKBN-90-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療種別区分名称、診療行為区分セット
      *****************************************************************
       510-SRYKBN-MEI-SEC            SECTION.
      *
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYKBN
                       MOVE    SPACE           TO  WRK-SRYSYUKBNMEI
                       MOVE    "0006"          TO  SPA-ERRCD
                   WHEN    WRK-SRYSYUKBN       =   MSYU-SRYSYUKBN 
                                                           (MSYU-IDX)
                       MOVE    MSYU-SRYKBN  (MSYU-IDX)
                                                TO  WRK-SRYKBN
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUKBNMEI
           END-SEARCH 
      *
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
      *****IF      SPA-GMN-MEISYO-G (SPA-GMN-IDX)
      *                                =   SPACE
      *        GO      TO      40113-MEISYOU-CHK-EXT
      *****END-IF
      *
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84" )  OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084")
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0085")
              OR  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0086")
               IF     (FLG-NYURYOKU        =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
      *
           IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =  "008" )) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
      *        半角の空白を全角に置きかえる
               PERFORM 401131-MEISYO-HENKAN-SEC
           END-IF
      *
           .
       40113-MEISYOU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    名称チェック・全角置換処理
      *****************************************************************
       401131-MEISYO-HENKAN-SEC        SECTION.
      *
      *    フリーコメントは１桁目から
           IF      SPA-COM-INPUTCD(SPA-GMN-IDX)   =   "810000001"
                                                  OR  "008500000"
                                                  OR  "008600000"
               MOVE    1                   TO  IDX-STR
      *        MOVE    80                  TO  WRK-LEN
      *R02.4
               MOVE    100                 TO  WRK-LEN
           ELSE
               MOVE    3                   TO  IDX-STR
      *        MOVE    78                  TO  WRK-LEN
      *R02.4
               MOVE    100                 TO  WRK-LEN
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "831"
                   MOVE    138                 TO  WRK-LEN
               END-IF
               IF     (SPA-GMN-MEISYO-G(SPA-GMN-IDX)(1:2)
                                           =   "* "
                                           OR  SPACE    )
                   CONTINUE
               ELSE
      *            開始位置より前に入力があれば元に戻してエラー
                   MOVE    "0100"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   MOVE    4               TO  SPA-GMN5-CUR
                   MOVE    SPACE           TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
                   MOVE    SPA-COM-NAME (SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                           (SPA-GMN-IDX)
               END-IF
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    4               TO  SPA-GMN5-CUR
           ELSE
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                      (SPA-GMN-IDX)
                                                            (IDX-STR:)
           END-IF
      *
           .
       401131-MEISYO-HENKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行挿入処理
      *****************************************************************
       41014-GYOINSN-SEC         SECTION.
      *
           IF      SPA-GMN-MAX         >=   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  SPA-GMN-MAX
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           IF      SPA-GMN-IDX         >=   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE        TO  SPA-GMN-IDX
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      IDX             =   SPA-GMN-IDX
                   CONTINUE
               ELSE
                   ADD     1               TO  IDY
                   MOVE    WRK-COM-TBLREC (IDY) 
                                           TO  SPA-COM-TBLREC(IDX)
                   MOVE    WRK-GMN-TBLREC (IDY) 
                                           TO  SPA-GMN-TBLREC(IDX)
               END-IF
               IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                   MOVE    IDX                 TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   MAX-GMN     +    1
           END-IF
      *
           MOVE    3                   TO  SPA-GMN5-CUR
      *
           .
       41014-GYOINSN-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       41015-ZAIDEL-SEC              SECTION.
      *
      *****IF      (SPA-GMN-IDX        =   1   )  OR
      *            (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
      *                                =   "1" )
      *        CONTINUE
      *    ELSE
      *        MOVE    "0023"              TO  SPA-ERRCD
      *        MOVE    "1"                 TO  SPA-COM-CUR(SPA-GMN-IDX)
      *        GO      TO      41015-ZAIDEL-EXT
      **** END-IF
      *
      *    ’−’は剤内のどの位置でも削除とする
      *    対象より、前
           IF      (SPA-GMN-IDX        =   1   )  OR
                   (SPA-COM-ZAIEND(SPA-GMN-IDX - 1 )
                                       =   "1" )
               CONTINUE
           ELSE
               PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                       UNTIL       IDX     <   1
                   IF      IDX             <   SPA-GMN-IDX
                       IF      SPA-COM-ZAIEND(IDX)     =   "1"
                          MOVE    1               TO  IDX
                       ELSE
                           MOVE    SPACE       TO  SPA-GMN-INPUTCD(IDX)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    後ろ
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF     (SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE) OR
                      (SPA-COM-ZAIEND(IDX)     =   "1"   )
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
               IF      SPA-GMN-SRYKBN (IDX + 1)    NOT =   SPACE
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理
      *****************************************************************
       41016-SETDEL-SEC              SECTION.
      *
           COMPUTE IDX2                =   SPA-GMN-IDX   +   1
      *
           PERFORM VARYING     IDX     FROM    IDX2    BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
               IF      SPA-COM-SET     (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD(IDX)
               ELSE
                   MOVE    SPA-MAX-LINE        TO  IDX
               END-IF
           END-PERFORM
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
               END-IF
           END-PERFORM
      *
           .
       41016-SETDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行削除処理
      *****************************************************************
       420-SYOKYO-SEC              SECTION.
      *
           MOVE    SPA-GMN-REC         TO  WRK-GMN-REC
           INITIALIZE                      SPA-GMN-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    SPACE               TO  FLG-SYORI
           MOVE    ZERO                TO  FLG-ZAIEND
           MOVE    ZERO                TO  FLG-DELFLG
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   IF      WRK-COM-TIMEFLG (IDX)   NOT =   ZERO
                       MOVE    1               TO  FLG-DELFLG
                       IF      WRK-COM-SRYKBN (IDX)    =   "11"  OR
                                                           "12"
                           MOVE    2               TO  FLG-DELFLG
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    NOT =   "1" )  AND
                          (WRK-COM-DATAKBN(IDX)        =   "1")
                       MOVE    1               TO  FLG-ZAIEND
                   END-IF
      *            器材商品名
                   IF     (WRK-COM-INPUTCD(IDX)(1:3)   =   "058")
                       MOVE    2               TO  FLG-ZAIEND
                   END-IF
      *            前も空白の時対象行のみ
                   IF     (WRK-COM-INPUTCD(IDX)        =   SPACE )  AND
                          (WRK-MAE-INPUTCD(IDX)        =   SPACE )
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *            前が器材商品名
                   IF     (WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE )
                      AND (WRK-COM-INPUTCD(IDX - 1)(1:3)
                                                       =   "058")
                       MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                             (IDX - 1)
                   END-IF
               ELSE
                   IF      FLG-ZAIEND          =   1
                       IF      WRK-COM-AUTOKBN(IDX)    NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                               (IDX)
                       END-IF
                   END-IF
      *            前が器材商品名
                   IF      FLG-ZAIEND          =   2
                       IF      WRK-COM-AUTOKBN2(IDX)   NOT =   SPACE
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                           MOVE    ZERO            TO  FLG-ZAIEND
                       END-IF
                   END-IF
      *            診療の時間加算削除の時、自動追加した時間加算を削除
                   IF      FLG-DELFLG          =   2
                       IF     (WRK-COM-AUTOKBN(IDX)  NOT =  SPACE) AND
                              (WRK-COM-TIMEKSNKBN(IDX) NOT =  ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                 (IDX)
                       END-IF
                   END-IF
                   IF      WRK-COM-ZAIEND (IDX)    =   "1"
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-DELFLG      NOT =   ZERO
               MOVE    ZERO                TO  SPA-NAI-TIMEFLG
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      WRK-GMN-INPUTCD(IDX)    =   SPACE
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-COM-TBLREC (IDX) 
                                           TO  SPA-COM-TBLREC
                                                        (SPA-GMN-MAX)
                   MOVE    WRK-GMN-TBLREC (IDX) 
                                           TO  SPA-GMN-TBLREC
                                                       (SPA-GMN-MAX)
                   IF      SPA-GMN-MAX     NOT =   IDX
                       MOVE    "1"             TO  FLG-SYORI
                   END-IF
               END-IF
           END-PERFORM
      *
      *
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    1               TO  SPA-MAX-PAGE
           ELSE
               COMPUTE SPA-MAX-PAGE    =  (SPA-GMN-MAX    -  1  )
                                       /   MAX-GMN     +    1
           END-IF
      *
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    各診療行為チェックへ
      *****************************************************************
       520-ALL-CHEK-SEC                SECTION.
      *
           MOVE    SPA-NAI-ROUJIN      TO  LNK-ORCSC-ROUJIN 
           MOVE    SPA-NENREI          TO  LNK-ORCSC-NENREI 
           MOVE    FLG-TOROKU          TO  LNK-ORCSC-TOROKU
      *    皮下筋肉注射料変換選択
           MOVE    SPA-CHG310FLG       TO  LNK-ORCSC-CHG310FLG
      *    腫瘍マーカー一覧展開選択
           MOVE    SPA-AKUSEIHYOFLG    TO  LNK-ORCSC-AKUSEIHYOFLG
      *    検査の重複チェックのみ行う
           IF      SPA-SRYKBN-CNT (07) >   ZERO
               CALL    "ORCSC60"       USING
                                       MCPAREA
                                       ORCSC00AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD           =   "0060"  OR  "0064"
               CONTINUE
           ELSE
               IF      SPA-ERRCD       NOT =   SPACE
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   SPA-MAX-LINE
                       MOVE    SPACE           TO  SPA-COM-CUR (IDX)
                   END-PERFORM
               END-IF
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
      *
           .
       520-ALL-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
      *----(01.00.01) LINE ADD START ----------------------------------
      *    更新前のチェック
           IF     (SPA-K05-SYORI   NOT =   "3" )  AND
                  (SPA-UPDFLG          =   "1" )
               MOVE    "0002"          TO  SPA-KIDCD
               MOVE    99              TO  SPA-GMN5-CUR
               GO      TO      210-BACK-EXT
           END-IF
      *----(01.00.01) LINE ADD END   ----------------------------------
      *    患者が選択されてない時、年齢を戻す
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-NENREI-YY
           END-IF
      *
      *    入外区分、老人区分を戻す
           MOVE    SPA-K05-MAE-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-K05-MAE-ROUJIN      TO  SPA-NAI-ROUJIN
           IF      SPA-K05-MAE-SRYYMD  NOT =   SPACE
               MOVE    SPA-K05-MAE-SRYYMD      TO  SPA-SRYYMD
           END-IF
      *
      *    中間ファイルのクリア
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ***                              ORCSFDELAREA
      *    PARA削除
           PERFORM 10031-PARA-DELETE-SEC
      *
           MOVE    1                   TO  FLG-END2
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
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
      *    戻る　処理
      *****************************************************************
       210-BACK-OK-SEC               SECTION.
      *
      *    入外区分、老人区分を戻す
           MOVE    SPA-K05-MAE-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-K05-MAE-ROUJIN      TO  SPA-NAI-ROUJIN
           IF      SPA-K05-MAE-SRYYMD  NOT =   SPACE
               MOVE    SPA-K05-MAE-SRYYMD      TO  SPA-SRYYMD
           END-IF
      *
      *    患者が選択されてない時、年齢を戻す
           IF      SPA-GMN-PTNUM       =   SPACE
               MOVE    ZERO                TO  SPA-NENREI-YY
           END-IF
      *
      *    中間ファイルのクリア
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    PARA削除
           PERFORM 10031-PARA-DELETE-SEC
      *
           MOVE    1                   TO  FLG-END2
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       210-BACK-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    セットクリア　処理
      *****************************************************************
       420-CLERA-SEC                  SECTION.
      *
      *    入外区分、老人区分を戻す
           MOVE    SPA-K05-MAE-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-K05-MAE-ROUJIN      TO  SPA-NAI-ROUJIN
           IF      SPA-K05-MAE-SRYYMD  NOT =   SPACE
               MOVE    SPA-K05-MAE-SRYYMD      TO  SPA-SRYYMD
           END-IF
           MOVE    SPA-GMN-SETCODE     TO  WRK-SETCODE
      *
           INITIALIZE              SPA-SCR-REC
           INITIALIZE              SPA-K05-AREA
           INITIALIZE              SPA-K051-AREA
      *
           MOVE    SPA-SRYYMD          TO  SPA-K05-MAE-SRYYMD
      *    入外区分、老人区分初期表示
           PERFORM 3101-NYUGAI-SET-SEC
      *    前回のセットコード
           MOVE    WRK-SETCODE         TO  SPA-MAE-SETCODE
      *
           MOVE    SPACE               TO  SPA-K05-SYORI
           MOVE    1                   TO  SPA-GMN5-CUR
      *
           .
       420-CLERA-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除　処理
      *****************************************************************
       430-SETDEL-SEC                  SECTION.
      *
           IF     (SPA-GMN-SETCODE     NOT =   SPACE )  AND
                  (SPA-K05-SYORI       =   "2"       )
      *H28.9 
      ****    MOVE    "0001"               TO  SPA-KIDCD
              CONTINUE
           ELSE
               MOVE    "5007"               TO  SPA-ERRCD
               MOVE    1                    TO  SPA-GMN5-CUR
           END-IF
      *
      *H28.9 
      *    選択番号があること
           IF     SPA-K05-SELNUM       =   ZERO
               MOVE    "5025"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN5-CUR
           END-IF
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >  SPA-K05-RRK-MAX )
                           OR (FLG-CHK        =   1    )
                           OR (SPA-ERRCD  NOT =   SPACE )
               IF      SPA-K05-TRENNUM (IDX) =   SPA-K05-SELNUM
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "5025"              TO  SPA-ERRCD
               MOVE    13                  TO  SPA-GMN5-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0001"              TO  SPA-KIDCD
           END-IF
           .
       430-SETDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回コード画面へ　処理
      *****************************************************************
       440-ZEN-INPUTCD-SEC             SECTION.
      *
           MOVE    SPA-MAE-SETCODE     TO  SPA-GMN-SETCODE
           MOVE    1                   TO  FLG-IN-SETCODE
      *    セットコード
           PERFORM 41011-SETCODE-HEN-SEC
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-KIDCD           =   "0003" )
      *        セット内容を展開する
               PERFORM 30011-SYUSEI-HEN-SEC
               MOVE    SPACE               TO  SPA-KID1-FLG
               MOVE    SPACE               TO  SPA-KIDCD
           END-IF
           .
       440-ZEN-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード画面へ　処理
      *****************************************************************
       440-INPUTCD-SEC             SECTION.
      *----(01.00.01) LINE ADD START ----------------------------------
      *    確定あり
           MOVE    "1"                 TO  SPA-UPDFLG
      *----(01.00.01) LINE ADD END   ----------------------------------
      *
           MOVE    SPACE               TO  K051
      *
      *    画面編集
           INITIALIZE                      K051
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   5
               MOVE    SPA-K051-INPUTCD   (IDX)
                                          TO  SPA-GMN-K051-INPUTCD(IDX)
               MOVE    SPA-GMN-K051-INPUTCD(IDX)
                                          TO  K051-INPUTCD (IDX)
           END-PERFORM
      *
           MOVE    "INPUTCD1"          TO  MCP-WIDGET
      *
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K051"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "K051"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       440-INPUTCD-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
           IF      SPA-GMN-PAGE        =   1
               MOVE    1               TO  SPA-GMN-PAGE
           ELSE
               COMPUTE SPA-GMN-PAGE    =   SPA-GMN-PAGE   -   1
           END-IF
      *
           MOVE    1                   TO  SPA-MAP-IDX
           MOVE    99                  TO  SPA-GMN5-CUR
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
           COMPUTE WRK-FREE            =   SPA-GMN-PAGE  *  MAX-GMN
           IF      SPA-GMN-INPUTCD(WRK-FREE)   =   SPACE
               GO  TO                 460-ATOGMN-EXT
           END-IF
      *
           IF      SPA-GMN-PAGE        =   MAX-PAGE
               MOVE    3                   TO  SPA-GMN-CUR
               MOVE    "7002"              TO  SPA-ERRCD
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
           ADD     1                   TO  SPA-GMN-PAGE
           IF      SPA-GMN-PAGE        =   MAX-PAGE
               MOVE    "7003"              TO  SPA-ERRCD
           END-IF
      *
           MOVE    3                   TO  SPA-GMN5-CUR
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-KOUSIN
      *    入力チェック処理
           PERFORM 220-INPUT-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END         NOT =   ZERO  )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *    明細がない時、エラー
           IF     (SPA-GMN-MAX         =   ZERO )
               MOVE    "9301"          TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    登録、更新の時、更新前チェックを行う
           IF      SPA-K05-SYORI       =   "1"  OR  "2"
      *        更新前の基本チェック
               PERFORM 41001-KIHON-CHK-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4100-TOROKU-EXT
               END-IF
      *
      *        更新前のチェック
      *        MOVE    "1"                 TO  FLG-TOROKU
      *        PERFORM 510-TBLHEN-SEC
      *        IF      SPA-ERRCD       NOT =   SPACE
      *            GO      TO      4100-TOROKU-EXT
      *        END-IF
           END-IF
      *
           IF      (SPA-GMN-SETCODE    =   SPACE )  OR
                   (SPA-GMN-DSPNAME    =   SPACE )
               MOVE    "5004"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-GMN5-CUR
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *****IF     (SPA-GMN-SETCODE(1:1)    =   "S" ) AND
           IF     (SPA-K05-SYORI           =   "2" )
      *        約束処方で期間変更があった時
               IF      (SPA-K05-SELNUM     =   ZERO  )  OR
                       (SPA-K05-NAI-YUKOSTYMD
                                       NOT =   SPA-K05-TYUKOSTYMD
                                                   (SPA-K05-SELNUM))
                   OR  (SPA-K05-NAI-YUKOEDYMD
                                       NOT =   SPA-K05-TYUKOEDYMD
                                                     (SPA-K05-SELNUM))
                   PERFORM 41009-KIKANCHK-SEC
               END-IF
           END-IF
      *H27.2
      *    期間は１０件とする
           IF     (SPA-K05-SYORI       =   "2"  )
             AND  (SPA-K05-SELNUM      =   ZERO )
             AND  (SPA-K05-RRK-MAX     =   10   )
               MOVE    "5031"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-GMN5-CUR
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               IF      SPA-K05-SYORI       =   "3"
                   MOVE    "0001"          TO  SPA-KIDCD
               ELSE
      *        更新処理
                   PERFORM 41002-KOUSIN-SYORI-SEC
               END-IF
           END-IF
           .
       4100-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間チェック処理
      *****************************************************************
       41009-KIKANCHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-JYUFKU
      *    期間重複チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-K05-RRK-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      (SPA-K05-NAI-YUKOSTYMD  >=  SPA-K05-TYUKOSTYMD
                                                               (IDX))
                 AND   (SPA-K05-NAI-YUKOSTYMD  <=  SPA-K05-TYUKOEDYMD
                                                               (IDX))
                   MOVE    "5027"              TO  SPA-ERRCD
                   MOVE    11                  TO  SPA-GMN5-CUR
               END-IF
               IF      (SPA-K05-NAI-YUKOEDYMD  >=  SPA-K05-TYUKOSTYMD
                                                               (IDX))
                  AND  (SPA-K05-NAI-YUKOEDYMD  <=  SPA-K05-TYUKOEDYMD
                                                               (IDX))
                  MOVE    "5027"           TO  SPA-ERRCD
                  MOVE    12               TO  SPA-GMN5-CUR
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE) AND
                      (SPA-K05-SELNUM      >   ZERO ) AND
                      (IDX                 =   SPA-K05-SELNUM)
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    1               TO  FLG-JYUFKU
               END-IF
           END-PERFORM
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-K05-SELNUM      >   ZERO
                   IF      FLG-JYUFKU      =   ZERO
                       MOVE    "0004"          TO  SPA-KIDCD
      *H27.2
      *                期間は１０件とする
                       IF      (SPA-K05-RRK-MAX     =   10   )
                           MOVE    "5031"              TO  SPA-ERRCD
                           MOVE    1                   TO  SPA-GMN5-CUR
                           MOVE    SPACE               TO  SPA-KIDCD
                       END-IF
                   END-IF
               END-IF
           END-IF
           .
       41009-KIKANCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K05SPA-OUT-SEC
      *
           MOVE    SPACE               TO  SPA-UPDFLG
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
      *    入力コード更新処理 
           PERFORM 410021-INPUTCD-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *    入力セット更新処理 
           PERFORM 410022-INPUTSET-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *    入外区分、老人区分を戻す
           MOVE    SPA-K05-MAE-NYUGAIKBN   TO  SPA-NYUGAIKBN
           MOVE    SPA-K05-MAE-ROUJIN      TO  SPA-NAI-ROUJIN
           IF      SPA-K05-MAE-SRYYMD  NOT =   SPACE
               MOVE    SPA-K05-MAE-SRYYMD      TO  SPA-SRYYMD
           END-IF
      *
      *    患者が選択されてない時、年齢を戻す
           IF      SPA-GMN-PTNUM       =   SPACE
      *        初期設定
               MOVE    SPA-GMN-SETCODE     TO  WRK-SETCODE
               PERFORM 310-SPA-INITSET-SEC
               MOVE    WRK-SETCODE         TO  SPA-MAE-SETCODE
               MOVE    SPACE               TO  SPA-K05-SYORI
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
      *    中間ファイルのクリア
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      ***                              ORCSFDELAREA
      *    PARA削除
           PERFORM 10031-PARA-DELETE-SEC
      *
      *
           MOVE    1                   TO  FLG-END2
      *
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K05"               TO  SPA-MOTOPG
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41002-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前の基本チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   SPA-MAX-LINE) OR
                              (SPA-ERRCD        NOT =   SPACE )   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-CHKFLG (IDX)    =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)
                                           NOT =   "."  AND "S")
      *
                   INITIALIZE                  LNK-SC97-INPUT-G
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    "K05"               TO  LNK-SC92-PG
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD (IDX)
                                               TO  LNK-SC92-SRYCD
                   MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
                   MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
                   MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
                   MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
                   MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
                   MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
                   CALL    "ORCSC92"           USING
                                               MCPAREA
                                               ORCSC92AREA
                                               SPA-SCR-REC
                                               SPA-AREA
      *
      *            算定履歴チェックははずす
                   MOVE    SPACE               TO  LNK-SC92-ERRCD3
                   PERFORM VARYING     IDX-ERR FROM    1  BY  1
                           UNTIL      (IDX-ERR     >   10     )  OR
                                      (SPA-ERRCD   NOT =   SPACE)
                       IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                           MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                       TO  SPA-ERRCD
                       END-IF
                   END-PERFORM
                   IF      SPA-ERRCD(1:1)      =   "K"
                       IF      SPA-COM-KZMFLG (IDX)
                                                   =   SPACE
                            MOVE    "1"        TO  SPA-COM-KZMFLG
                                                         (IDX)
                            MOVE    "2"        TO  SPA-COM-CUR
                                                        (IDX)
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                           MOVE    SPACE           TO  SPA-COM-CUR
                                                      (IDX)
                      END-IF
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       NOT =   SPACE
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
               END-IF
           END-PERFORM
      *
      *    点数マスタ有効期間チェック
           IF      SPA-K05-NAI-YUKOEDYMD   =   "99999999"
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX              >   SPA-MAX-LINE) OR
                                  (SPA-ERRCD        NOT =   SPACE )   OR
                                  (SPA-GMN-INPUTCD(IDX) =   SPACE )
                   IF     (SPA-COM-KZMFLG5 (IDX)    =   SPACE )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)
                                                 NOT =   "."  AND "S")
                       PERFORM 410011-TENSU-RRK-CHK-SEC
                       MOVE    "1"             TO  SPA-COM-KZMFLG5 (IDX)
                   END-IF
      *
                   IF      SPA-ERRCD       NOT =   SPACE
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   END-IF
               END-PERFORM
           END-IF
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ期間チェック処理
      *****************************************************************
       410011-TENSU-RRK-CHK-SEC      SECTION.
      *
           MOVE    SPACE               TO  SPA-ERRMSG2
      *
           INITIALIZE                  TNS-TENSU-REC
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    SPA-COM-INPUTCD(IDX)    TO  TNS-SRYCD
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key24"             TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
               ELSE
                   INITIALIZE                  TNS-TENSU-REC
                   MOVE    1                   TO  FLG-TENSU
               END-IF
           ELSE
               INITIALIZE                  TNS-TENSU-REC
               MOVE    1                   TO  FLG-TENSU
           END-IF
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key24"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-TENSU           =   ZERO
               IF     (TNS-YUKOEDYMD   NOT =   "99999999" )
                   INITIALIZE                      STS-AREA-DAY
                   INITIALIZE                      LNK-DAY2-AREA
                   MOVE    "21"                TO  LNK-DAY2-IRAI
                   MOVE    TNS-YUKOEDYMD       TO  LNK-DAY2-YMD
                   CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                                       LNK-DAY2-AREA
                   STRING  LNK-DAY2-EDTYMD3        DELIMITED  BY  SIZE
                           "終了です。【"          DELIMITED  BY  SIZE
                           TNS-NAME                DELIMITED  BY  SPACE
                           "】"                    DELIMITED  BY  SIZE
                          INTO                     SPA-ERRMSG2
                   END-STRING
                   MOVE    "K050"                  TO  SPA-ERRCD
               END-IF
           END-IF
           .
        410011-TENSU-RRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード更新処理
      *****************************************************************
       410021-INPUTCD-SYORI-SEC          SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    SPA-GMN-SETCODE     TO  ICD-INPUTCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM                     920-INPUTCD-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           EVALUATE    SPA-K05-SYORI
      *            登録
               WHEN    "1"
                   IF      FLG-INPUTCD         =   ZERO
                       MOVE    "5002"          TO  SPA-ERRCD
                   ELSE
                       MOVE    SPA-GMN-SETCODE     TO  WRK-SETCOD
                       MOVE    SPA-GMN-SETCODE     TO  WRK-ICD-INPUTCD
                       MOVE    SPACE               TO  WRK-CDSYU
                       MOVE    ZERO                TO  WRK-DSPSEQ
                       PERFORM 410022-INPUTCD-INS-SEC
      *                入力コード追加処理
                       PERFORM 410023-INPUTCD-TBLINS-SEC
                   END-IF
      *            更新
               WHEN    "2"
      *            期間変更
               WHEN    "5"
                   IF      FLG-INPUTCD         =   1
                       MOVE    "5003"          TO  SPA-ERRCD
                   ELSE
      *                入力コード削除処理
                       PERFORM 410025-INPUTCD-TBLDEL-SEC
      *                入力コード追加処理
                       MOVE    SPA-GMN-SETCODE     TO  WRK-SETCOD
                       MOVE    SPA-GMN-SETCODE     TO  WRK-ICD-INPUTCD
                       MOVE    SPACE               TO  WRK-CDSYU
                       MOVE    ZERO                TO  WRK-DSPSEQ
                       PERFORM 410022-INPUTCD-INS-SEC
      *                入力コード追加処理
                       IF      SPA-ERRCD           =   SPACE
                           PERFORM 410023-INPUTCD-TBLINS-SEC
                       END-IF
                   END-IF
      *            削除
               WHEN    "3"
                   IF      FLG-INPUTCD         =   1
                       MOVE    "5003"          TO  SPA-ERRCD
                   ELSE
      *            複数ある時、削除なし
                     IF      SPA-K05-RRK-MAX   =   1
      *
                       MOVE    INPUTCD-REC         TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_inputcd"       TO  MCP-TABLE
                       MOVE    "del12"             TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "K05 INPUTCD DEL ERR:"  MCP-RC
                           MOVE    "5007"          TO  SPA-ERRCD
                       END-IF
      *                入力コード削除処理
                       IF      SPA-ERRCD           =   SPACE
                           PERFORM 410025-INPUTCD-TBLDEL-SEC
                       END-IF
                     END-IF
                   END-IF
           END-EVALUATE
      *
           .
       410021-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード追加処理
      *****************************************************************
       410022-INPUTCD-INS-SEC             SECTION.
      *
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           IF      WRK-CDSYU           =   SPACE
               EVALUATE    TRUE
                   WHEN    WRK-ICD-INPUTCD(5:)      =   SPACE
                       MOVE    "4"                 TO  ICD-CDSYU
                   WHEN    WRK-ICD-INPUTCD(6:)      =   SPACE
                       MOVE    "5"                 TO  ICD-CDSYU
                   WHEN    WRK-ICD-INPUTCD(7:)      =   SPACE
                       MOVE    "6"                 TO  ICD-CDSYU
               END-EVALUATE
           ELSE
               MOVE    WRK-CDSYU           TO  ICD-CDSYU
           END-IF
           MOVE    WRK-DSPSEQ          TO  ICD-DSPSEQ
           MOVE    WRK-ICD-INPUTCD     TO  ICD-INPUTCD
           MOVE    SPA-GMN-DSPNAME     TO  ICD-DSPNAME
           MOVE    SPA-GMN-SETCODE     TO  ICD-SRYCD
      *
           MOVE    SMCNDATE-YMD        TO  ICD-CREYMD
           MOVE    SMCNDATE-YMD        TO  ICD-UPYMD
           MOVE    SMCNDATE-HMS        TO  ICD-UPHMS
      *
           MOVE    SPA-OPID            TO  ICD-OPID
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K05 INPUTCD INS ERR:"  MCP-RC
               MOVE    "5005"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-DBERR
           END-IF
           .
       410022-INPUTCD-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード追加処理
      *****************************************************************
       410023-INPUTCD-TBLINS-SEC             SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )  OR
                              (SPA-K051-INPUTCD (IDX)  =   SPACE )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               MOVE    SPA-K051-INPUTCD (IDX)  TO  WRK-ICD-INPUTCD
               MOVE    SPA-K051-CDSYU   (IDX)  TO  WRK-CDSYU
               MOVE    IDX                     TO  WRK-DSPSEQ
      *
               PERFORM 410022-INPUTCD-INS-SEC
           END-PERFORM
           .
       410023-INPUTCD-TBLINS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード削除処理
      *****************************************************************
       410025-INPUTCD-TBLDEL-SEC       SECTION.
      *
      *    入力コード削除
           INITIALIZE                      INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
           MOVE    SPA-GMN-SETCODE     TO  ICD-SRYCD
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "del22"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       410025-INPUTCD-TBLDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット作成処理
      *****************************************************************
       410022-INPUTSET-SYORI-SEC             SECTION.
      *
           EVALUATE    SPA-K05-SYORI
      *            登録
               WHEN    "1"
                   PERFORM 4100221-INPUTSET-INS-SEC
      *            更新
               WHEN    "2"
                   PERFORM 4100221-INPUTSET-UPD-SEC
      *            削除
               WHEN    "3"
                   PERFORM 4100223-INPUTSET-DEL-SEC
      *            期間変更
               WHEN    "5"
                   PERFORM 4100224-INPUTSET-KIKAN-SEC
           END-EVALUATE
           .
       410022-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット追加作成処理
      *****************************************************************
       4100221-INPUTSET-INS-SEC        SECTION.
      *
           IF      SPA-K05-NAI-YUKOSTYMD   =   SPACE
               MOVE    "00000000"          TO  SPA-K05-NAI-YUKOSTYMD
           END-IF
           IF      SPA-K05-NAI-YUKOEDYMD   =   SPACE
               MOVE    "99999999"          TO  SPA-K05-NAI-YUKOEDYMD
           END-IF
      *
           MOVE    ZERO                TO  IDX-SET
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX   )  OR
                              (SPA-ERRCD   NOT =   SPACE )
      *      画面表示なしの時、出力しない
             IF      SPA-COM-SET(IDX)      =   SPACE
      *
               ADD     1                   TO  IDX-SET
               INITIALIZE                      INPUTSET-REC
               MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
               MOVE    SPA-GMN-SETCODE     TO  ISET-SETCD
               MOVE    IDX-SET             TO  ISET-SETSEQ
               MOVE    SPA-K05-NAI-YUKOSTYMD
                                           TO  ISET-YUKOSTYMD
               MOVE    SPA-K05-NAI-YUKOEDYMD
                                           TO  ISET-YUKOEDYMD
      *
      *        入力セット内容編集処理
               PERFORM 4100221-INPUTSET-HEN-SEC
      *
               MOVE    SMCNDATE-YMD        TO  ISET-CREYMD
               MOVE    SMCNDATE-YMD        TO  ISET-UPYMD
               MOVE    SMCNDATE-HMS        TO  ISET-UPHMS
      *
               MOVE    SPA-OPID            TO  ISET-OPID
      *
               MOVE    INPUTSET-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_inputset"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K05 INPUTSET   INS ERR:"  MCP-RC
                   MOVE    "5005"          TO  SPA-ERRCD
                   MOVE    1               TO  FLG-DBERR
               END-IF
             END-IF
           END-PERFORM
      *
           .
       4100221-INPUTSET-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット内容編集処理
      *****************************************************************
       4100221-INPUTSET-HEN-SEC              SECTION.
      *
           IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "."  OR  "S"
               MOVE    SPA-GMN-INPUTCD(IDX)(1:6)   TO  ISET-INPUTCD
           ELSE
               MOVE    SPA-COM-INPUTCD(IDX)    TO  ISET-INPUTCD
           END-IF
      *    数量
           IF      SPA-COM-KEIFLG (IDX)    =   SPACE  OR  "2"
      *        数量
               MOVE    SPA-COM-SURYO  (IDX)    TO  ISET-SURYO1
               IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )  AND
                      (SPA-COM-SURFLG (IDX)    =   SPACE   )  AND
                      (SPA-COM-SURYO (IDX)     >   1      )
                   MOVE    1                   TO  ISET-SURYO1
               END-IF
           ELSE
      *        保険外金額
      *********MOVE    SPA-COM-KEI (IDX)       TO  ISET-SURYO1
               MOVE    SPA-COM-JIHIMONEY (IDX)     TO  ISET-SURYO1
           END-IF
           MOVE    SPA-COM-BUNGSU (IDX)    TO  ISET-SURYO2
      *
      *    回数
      *?   IF     (SPA-COM-ZAIEND (IDX)    =   "1"    )   OR
      *?          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S")
           IF     (SPA-COM-KAIFLG (IDX)    =   "1"    )
           OR     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S")
               MOVE    SPA-COM-KAISU  (IDX)    TO  ISET-KAISU
           END-IF
      *    入力コメント番号
           IF     (SPA-COM-INPUTCD(IDX)(1:1)   =   "8" )  OR
                  (SPA-COM-INPUTCD(IDX)(1:3)   =   "008")
             OR   (SPA-COM-INPUTCD(IDX)(1:3)   =   "001")
      *H30.4
             OR   (SPA-COM-INPUTCD (IDX)(1:7)  =   "0992081")
      *        入力コメント
               IF      SPA-COM-INPUTCD (IDX)    =   "810000001"
                                                OR  "008500000"
                                                OR  "008600000"
                   MOVE    SPA-GMN-MEISYO-G (IDX)
                                               TO  ISET-COMMENT
               ELSE
                   IF    ((SPA-COM-INPUTCD (IDX)(1:1)  =   "8"  ) AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
                   OR    ((SPA-COM-INPUTCD (IDX)(1:4)  =   "0084") AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
                   OR     (SPA-COM-INPUTCD (IDX)(1:2)  =   "83" )
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0083")
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0085")
                   OR     (SPA-COM-INPUTCD (IDX)(1:4)  =   "0086")
                   OR    ((SPA-COM-INPUTCD (IDX)(1:3)  =   "001" ) AND
                          (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE))
      *H30.4
                   OR    ((SPA-COM-INPUTCD (IDX)(1:7)  =   "0992081")
                      AND (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE ))
      *R02.4
                   OR    ((SPA-COM-INPUTCD (IDX)(1:2)  =   "85"  )
                      AND (SPA-COM-INPUTCHI-G(IDX) NOT =   SPACE ))
                       MOVE    SPA-GMN-MEISYO(IDX)
                                               TO  ISET-COMMENT
                   END-IF
               END-IF
      *        入力値
               MOVE    SPA-COM-ATAI1(IDX)  TO  ISET-ATAI1
               MOVE    SPA-COM-ATAI2(IDX)  TO  ISET-ATAI2
               MOVE    SPA-COM-ATAI3(IDX)  TO  ISET-ATAI3
               MOVE    SPA-COM-ATAI4(IDX)  TO  ISET-ATAI4
               MOVE    SPA-COM-ATAI5(IDX)  TO  ISET-ATAI5
           END-IF
      *    換算値
           MOVE    SPA-COM-KANSURYO (IDX)  TO  ISET-KANSURYO
      *    関係コメント指示
           IF      SPA-COM-INPUTCONT (IDX) =   "1"
               MOVE    "1"                 TO  ISET-INPUTKBN
           END-IF
      *    内服１種類区分
           IF      SPA-COM-INPUTNAIF (IDX)     =   "1"
               MOVE    "2"                 TO  ISET-INPUTKBN
           END-IF
           .
       4100221-INPUTSET-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット更新作成処理
      *****************************************************************
       4100221-INPUTSET-UPD-SEC        SECTION.
      *
      *    全件削除
           PERFORM 4100223-INPUTSET-DEL-SEC
      *
      *    全件追加
           PERFORM 4100221-INPUTSET-INS-SEC
           .
       4100221-INPUTSET-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セット削除処理
      *****************************************************************
       4100223-INPUTSET-DEL-SEC        SECTION.
      *
      *****IF      (SPA-GMN-SETCODE(1:1)   =   "S" ) AND
           IF      (SPA-K05-SELNUM         >   ZERO )
               MOVE    SPA-K05-TYUKOSTYMD(SPA-K05-SELNUM)
                                           TO  WRK-YUKOSTYMD
               MOVE    SPA-K05-TYUKOEDYMD(SPA-K05-SELNUM)
                                           TO  WRK-YUKOEDYMD
           ELSE
               MOVE    SPA-K05-NAI-YUKOSTYMD
                                           TO  WRK-YUKOSTYMD
               MOVE    SPA-K05-NAI-YUKOEDYMD
                                           TO  WRK-YUKOEDYMD
           END-IF
      *    入力セットマスタ検索
           INITIALIZE                      INPUTSET-REC
           MOVE    SPA-HOSPNUM         TO  ISET-HOSPNUM
           MOVE    SPA-GMN-SETCODE     TO  ISET-SETCD
           MOVE    WRK-YUKOSTYMD       TO  ISET-YUKOSTYMD
           MOVE    WRK-YUKOEDYMD       TO  ISET-YUKOEDYMD
      *
           MOVE    INPUTSET-REC        TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_inputset"      TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K05 INPUTSET   DEL ERR:"  MCP-RC
               MOVE    "5007"          TO  SPA-ERRCD
           END-IF
           .
       4100223-INPUTSET-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    期間変更処理
      *****************************************************************
       4100224-INPUTSET-KIKAN-SEC             SECTION.
      *
      *    全件追加
           PERFORM 4100221-INPUTSET-INS-SEC
           .
       4100224-INPUTSET-KIKAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧表印刷処理
      *****************************************************************
       4110-PRINT-SEC             SECTION.
      *
           INITIALIZE                  SPA-K052-AREA
           MOVE    SPACE               TO  SPA-K052-INPUTCD1
           MOVE    ALL "9"             TO  SPA-K052-INPUTCD2
      *
           MOVE    SPACE               TO  K052
           INITIALIZE                      K052
           MOVE    SPA-K052-INPUTCD1   TO  K052-INPUTCD1
           MOVE    SPA-K052-INPUTCD2   TO  K052-INPUTCD2
      *
           MOVE    "INPUTCD1"          TO  MCP-WIDGET
      *
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "K052"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "K052"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *****CALL    "ORCHCSET"          USING   SPA-AREA
      *
           .
       4110-PRINT-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
                                                    OR   ZERO
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
      ******** MOVE    WRK-HENYMD          TO  WRK-HENYMDG
               MOVE    WRK-SYMD            TO  WRK-HENYMDG
           ELSE
               INITIALIZE                      STS-AREA-DAY
               INITIALIZE                      LNK-DAY2-AREA
               MOVE    "21"                TO  LNK-DAY2-IRAI
               MOVE    WRK-SYMD            TO  LNK-DAY2-YMD
               CALL    "ORCSDAY"            USING   STS-AREA-DAY
                                                   LNK-DAY2-AREA
               MOVE    LNK-DAY2-EDTYMD1    TO  WRK-HENYMDG
           END-IF
           .
       800-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *TEST
      *    コメント入力位置エラーはカーソル移動のみ
           IF      SPA-ERRCD           =   "0100"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *TEST
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KIDCD       NOT =   SPACE
               PERFORM 520-KIDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K05    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *
           MOVE    SPACE               TO  K05
           INITIALIZE                      K05
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE )  OR
                              (IDX         >=  MAX-GMN   )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1               TO  IDX
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  K05-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  K05-INPUTCD(IDX)
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  K05-MEISYO(IDX)
                   PERFORM 5002-SURYO-HEN-SEC
               END-IF
      *
           END-PERFORM
      *
      *    前頁表示の時、１行目へ
           IF      SPA-GMN5-CUR         =   99  OR  88
               MOVE    3                   TO  SPA-GMN5-CUR
               MOVE    1                   TO  SPA-MAP-IDX
           END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN5-CUR         =   4
               IF      K05-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    3               TO  SPA-GMN5-CUR
               END-IF
           END-IF
      *
      *
           MOVE    SPA-GMN-SETCODE     TO  K05-SETCODE
           MOVE    SPA-GMN-DSPNAME     TO  K05-NAME
      *
           EVALUATE    SPA-K05-SYORI
               WHEN    "1"
                   MOVE    "black"         TO  K05-SYORINAME-STYLE
                   MOVE    "［  追加  ］"  TO  K05-SYORINAME-VALUE
               WHEN    "2"
                   MOVE    "blue"          TO  K05-SYORINAME-STYLE
                   MOVE    "［  修正  ］"  TO  K05-SYORINAME-VALUE
               WHEN    "3"
                   MOVE    "red"           TO  K05-SYORINAME-STYLE
                   MOVE    "［  削除  ］"  TO  K05-SYORINAME-VALUE
           END-EVALUATE
      *
           MOVE    SPA-K05-NYUGAIKBN-G     TO  K05-NYUGAIKBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
                MOVE    SPA-K05-NYUGAIKBNLST (IDX)
                                           TO  K05-NYUGAIKBN-ITEM (IDX)
                MOVE    IDX                TO  K05-NYUGAIKBN-COUNT
           END-PERFORM
      *
           MOVE    SPA-K05-ROUJIN-G    TO  K05-ROUJIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   2
                MOVE    SPA-K05-ROUJINLST (IDX)
                                           TO  K05-ROUJIN-ITEM (IDX)
                MOVE    IDX                TO  K05-ROUJIN-COUNT
           END-PERFORM
      *    有効期間
           MOVE    SPA-K05-GMN-YUKOSTYMD   TO  K05-YUKOSTYMD
           MOVE    SPA-K05-GMN-YUKOEDYMD   TO  K05-YUKOEDYMD
      *
           MOVE    SPA-K05-SELNUM      TO  K05-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K05-RRK-MAX
               MOVE    SPA-K05-TRENNUM (IDX)
                                           TO  K05-TRENNUM (IDX)
               MOVE    SPA-K05-TYUKOSTYMDG (IDX)
                                           TO  K05-TYUKOSTYMD (IDX)
               MOVE    SPA-K05-TYUKOEDYMDG (IDX)
                                           TO  K05-TYUKOEDYMD (IDX)
               IF      SPA-K05-TRENNUM (IDX)   =   SPA-K05-SELNUM
                   MOVE    "T"             TO  K05-RRKLIST-SELECT  (IDX)
               ELSE
                   MOVE    "F"             TO  K05-RRKLIST-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K05-RRK-MAX     TO  K05-RRKLIST-COUNT
      *
           IF      FLG-END             =   ZERO
                PERFORM 5001-MAPCUR-SEC
            END-IF
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       5002-GMNSPA-HEN-SEC                  SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  WRK-MAP-MAX
           MOVE    ZERO                TO  WRK-MAP-CUR
           MOVE    ZERO                TO  WRK-MAP-CUR2
           MOVE    ZERO                TO  WRK-MAP-CUR3
           MOVE    ZERO                TO  WRK-MAP-MAXPAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE
           MOVE    ZERO                TO  WRK-MAP-PAGE2
           MOVE    ZERO                TO  WRK-MAP-PAGE3
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *        画面表示外のものに頁、行位置を設定する
               IF      SPA-COM-SET    (WRK-IDX)  =   SPACE
                   ADD     1                 TO  IDX
                   IF      IDX               >   MAX-GMN
                       ADD     1             TO  WRK-PAGE
                       MOVE    1             TO  IDX
                   END-IF
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE
                           END-IF
                           MOVE    "1"                TO  SPA-COM-IN2
                                                              (WRK-IDX)
                       END-IF
      *                警告位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "2"
                           IF      WRK-MAP-CUR2        =   ZERO
                               MOVE    IDX             TO  WRK-MAP-CUR2
                               MOVE    WRK-PAGE        TO  WRK-MAP-PAGE2
                           END-IF
                       END-IF
      *                フリーコメントで未入力の行
                       IF     (SPA-COM-IN2 (WRK-IDX)   =   "3" ) AND
                              (SPA-COM-IN  (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR3
                           MOVE    WRK-PAGE        TO  WRK-MAP-PAGE3
                       END-IF
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   ADD     1               TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
      *
      *    画面カーソル位置決定（警告はエラーの次に）
           MOVE    ZERO                TO  SPA-MAP-IDX
           IF      WRK-MAP-CUR         =   ZERO
               IF      WRK-MAP-CUR2    NOT =   ZERO
                   MOVE    WRK-MAP-CUR2        TO  SPA-MAP-IDX
                   MOVE    WRK-MAP-PAGE2       TO  SPA-GMN-PAGE
               END-IF
           ELSE
               MOVE    WRK-MAP-CUR         TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE        TO  SPA-GMN-PAGE
           END-IF
      *
           IF      SPA-MAP-IDX     NOT =   ZERO
               IF      SPA-GMN5-CUR    =   ZERO
                   MOVE    3               TO  SPA-GMN5-CUR
               END-IF
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    4                   TO  SPA-GMN5-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
      *    後画面へ
           IF      SPA-GMN5-CUR        =   88  OR  99
               IF      WRK-MAP-MAXPAGE     =   SPA-GMN-PAGE
                   COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                               +   1
               ELSE
                   MOVE    1                   TO  SPA-MAP-IDX
               END-IF
           ELSE
               COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                           +   1
               IF      SPA-MAP-IDX         >   MAX-GMN
                   OR  SPA-GMN-PAGE        <   WRK-MAP-MAXPAGE
                   MOVE    99                  TO  SPA-MAP-IDX
               ELSE
                   IF      SPA-GMN-PAGE    >   WRK-MAP-MAXPAGE
                       MOVE    WRK-MAP-MAXPAGE     TO  SPA-GMN-PAGE
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-PAGE        =   ZERO
               MOVE    1               TO  SPA-GMN-PAGE
           END-IF
      *
           .
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面数量項目編集処理
      *****************************************************************
       5002-SURYO-HEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           MOVE    SPACE               TO  WRK-KAISU-H 
      *
      *    回数
           MOVE    ZERO                TO  IDK2
      *    外用薬の時、１
           IF     (SPA-COM-SRYKBN (WRK-IDX)    =   "23" )  AND
                  (SPA-GMN-KAISU  (WRK-IDX)    NOT =   SPACE )
               MOVE    1                   TO  WRK-KAISU-Z
           ELSE
               MOVE    SPA-GMN-KAISU  (WRK-IDX)    TO  WRK-KAISU-Z
           END-IF
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   4
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-ZAIKEI-H
           IF      SPA-GMN-KAISU (WRK-IDX) =   SPACE
               MOVE    SPACE               TO  WRK-KAKERU
           ELSE
               IF      SPA-GMN-KEI   (WRK-IDX) =   ZERO
      *H26.10
      *           診療人数合計
                  AND (SPA-COM-INPUTCD (WRK-IDX)
                                               NOT =   "099140012" )
      *
                   MOVE    SPACE               TO  WRK-KAISU-H
                   MOVE    SPACE               TO  WRK-KAKERU
               ELSE
                   MOVE    " X "               TO  WRK-KAKERU
               END-IF
           END-IF
           STRING  WRK-KAKERU              DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI-H
           END-STRING
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
           MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
           MOVE    WRK-ZAIKEI-H                TO  WRK-H-ZAIKEI
      *
           MOVE    WRK-SURYO-HENSYU    TO  K05-SURYO (IDX)
           MOVE    "user-font"         TO  K05-SURYO-STYLE(IDX)
      *
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC                 SECTION.
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-GMN5-CUR        =   ZERO  )
               EVALUATE    WRK-WIDGET
                   WHEN    "SETCODE"
                       MOVE    2               TO  SPA-GMN5-CUR
                   WHEN    "YUKOSTYMD"
                       MOVE    12              TO  SPA-GMN5-CUR
                   WHEN    OTHER
                       MOVE    3               TO  SPA-GMN5-CUR
               END-EVALUATE
           END-IF
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX         TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF
      *
           IF      WRK-IDX2            >   MAX-GMN
               IF      SPA-GMN5-CUR    =   3  OR  4
                   MOVE    06              TO  SPA-GMN5-CUR
               END-IF
           END-IF
      *
      *
           EVALUATE    SPA-GMN5-CUR
               WHEN    01
                   MOVE    "SETCODE"       TO  MCP-WIDGET
               WHEN    02
                   MOVE    "NAME"          TO  MCP-WIDGET
               WHEN    03
                   MOVE    "INPUTCD"       TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
               WHEN    04
                   MOVE    "MEISYO"        TO  MCP-WIDGET
                   MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
               WHEN    05
                   MOVE    "B12"           TO  MCP-WIDGET
               WHEN    06
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    07
                   MOVE    "NYUGAIKBN"     TO  MCP-WIDGET
               WHEN    08
                   MOVE    "ROUJINKBN"     TO  MCP-WIDGET
               WHEN    11
                   MOVE    "YUKOSTYMD"     TO  MCP-WIDGET
               WHEN    12
                   MOVE    "YUKOEDYMD"     TO  MCP-WIDGET
               WHEN    13
                   MOVE    "SELNUM"        TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       510-ERRSET-SEC              SECTION.
      *
           MOVE    "K05"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
      *
           IF      SPA-ERRMSG2     NOT =   SPACE
      *        エラー画面２
               PERFORM 5101-KERR2-SET-SEC
               GO      TO      510-ERRSET-EXT
           END-IF
      *
           MOVE    SPACE               TO  KERR
           MOVE    SPA-ERRCD           TO  KERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG(1:80)
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG2
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "KERR2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KERR2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       5101-KERR2-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "0001"
                   MOVE    "セットを削除してもよろしいですか？"
                                       TO  WRK-KIDMSG
      *----(01.00.01) LINE ADD START ----------------------------------
               WHEN    "0002"
                   MOVE
                   "ＯＫで修正分は反映されずに終了します"
                                       TO  WRK-KIDMSG
      *----(01.00.01) LINE ADD END   ----------------------------------
               WHEN    "0003"
                   MOVE
                   "セットコードが存在します。修正をしますか？"
                                       TO  WRK-KIDMSG
               WHEN    "0004"
                   STRING  "ＯＫでセットの追加登録をします。"
                                       DELIMITED   BY  SIZE
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID1
           INITIALIZE                      KID1
           MOVE    SPA-KIDCD           TO  KID1-ID1CODE
           MOVE    WRK-KIDMSG          TO  KID1-ID1MSG
      *
           MOVE    "戻る"              TO  KID1-B01
           MOVE    "ＯＫ"              TO  KID1-B12
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K05"               TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KID1"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KIDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensu"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TNS-TENSU-REC
                   MOVE    ZERO                TO  FLG-TENSU
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター次読込
      *****************************************************************
       920-INPUTCD-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTCD-REC
               MOVE    ZERO                TO  FLG-INPUTCD
           ELSE
               INITIALIZE                      INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           .
       920-INPUTCD-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  INPUTCD-REC
                   MOVE    ZERO                TO  FLG-INPUTCD
               ELSE
                   INITIALIZE                  INPUTCD-REC
                   MOVE    1                   TO  FLG-INPUTCD
               END-IF
           ELSE
               INITIALIZE                  INPUTCD-REC
               MOVE    1                   TO  FLG-INPUTCD
           END-IF
      *
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力セットデータ次読込
      *****************************************************************
       975-INPUTSET-READ-SEC         SECTION.
      *
           MOVE    "tbl_inputset"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  INPUTSET-REC
               MOVE    ZERO                TO  FLG-INPUTSET
           ELSE
               INITIALIZE                      INPUTSET-REC
               MOVE    1                   TO  FLG-INPUTSET
           END-IF
      *
           .
       975-INPSET-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE 
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA-READ-EXT.
      *    EXIT.
      *****************************************************************
      *    パラメタ情報読み込み
      *****************************************************************
       990-PARA-READ-SEC        SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PARA-REC
               MOVE    ZERO            TO  FLG-PARA
           ELSE
               INITIALIZE                  PARA-REC
               MOVE    1               TO  FLG-PARA
           END-IF
      *
           .
       990-PARA-READ-EXT.
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
      *
      *****************************************************************
      *    ＤＢクローズ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＰＵＴ　処理
      *****************************************************************
       900-PUT-WINDOW              SECTION.
      *
           MOVE   "PUTWINDOW"       TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
