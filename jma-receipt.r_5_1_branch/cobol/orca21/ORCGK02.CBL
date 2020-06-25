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
       PROGRAM-ID.                 ORCGK02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為入力
      *  管理者            : 
      *  作成日付    作業者        記述
      *  00/12/01    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC−多々納  01/04/25  セット入力修正
      *  02.00.00    MCC−多々納  01/05/15  約束セット、セットコード漢字
      *                                     追加
      *  02.00.03    MCC−多々納  01/05/17  老人慢性疾患外来総合診療科の
      *                                     追加
      *  02.00.00    MCC−多々納  01/05/21  入力コードをセットコードを
      *                                     表示へ
      *  02.00.01    MCC-多々納   01/05/24  Ｋ０８追加
      *  02.00.02    MCC-多々納   01/05/30  薬剤一部負担金修正
      *  02.00.03    MCC-多々納   02/03/01  初期自動算定をサブプロへ
      *  02.00.04    MCC-多々納   02/03/20  Ｈ１４．４改正対応他
      *  02.00.05    MCC-多々納   02/04/04  訂正時の中途終了エラー追加
      *  02.00.06    MCC-多々納   02/04/12  自費の修正
      *  02.00.07    MCC-多々納   02/04/12  労災追加
      *  02.00.08    MCC-多々納   02/04/19  生活習慣病指導算定時の追加
      *  03.00.00    MCC-多々納   02/07/01  インターフェースの変更
      *  03.00.01    NACL-多々納  02/09/03  インターフェースの変更その２
      *                                     剤削除の入力をどこでもＯＫとした
      *  03.00.02    NACL-多々納  02/09/11  自賠責追加
      *  03.00.03    NACL−多々納 02/09/24  検索修正
      *  03.00.04    NACL−多々納 02/10/08  処方せんに在宅薬剤院外追加
      *                                     フリーコメントの修正など
      *  03.00.05    NACL−多々納 02/10/10  外来管理加算チェックの有無
      *                                     訂正時の薬情表示等
      *  03.00.06    NACL−多々納 02/10/11  老人判定の修正
      *  03.00.07    NACL−多々納 02/10/31  外来管理加算自動算定なしの追加
      *  03.00.08    NACL-多々納  02/11/11  ＤＵＭＭＹ診察料追加
      *  03.00.09    NACL-多々納  02/11/20  自賠責器材追加
      *  03.00.10    NACL-多々納  02/11/28  中途終了再表示修正
      *  03.00.11    NACL-多々納  02/12/26  前回処方のドクター変更
      *  03.00.12    NACL-多々納  03/02/19  約束セット名称編集修正
      *  03.00.13    NACL-多々納  03/03/05  排他制御処理追加
      *  03.00.14    NACL-多々納  03/04/07  入院日のチェック追加
      *  03.00.15    NACL-多々納  03/05/26  最終退院日編集追加
      *  03.00.16    NACL-多々納  03/06/27  カスタマイズ帳票名取得サブ追加
      *  03.00.17    NACL-多々納  03/07/17  入力修正他
      *  03.00.18    NACL-多々納  03/08/01  数量入力判定追加
      *  03.00.19    NACL-多々納  03/09/12  同一検査削除追加
      *  03.00.20    NACL-多々納  03/09/26  ＣＴ、ＭＲＩ同時入力追加
      *  03.00.21    NACL-多々納  04/02/20  セット展開変更
      *  03.00.22    NACL-多々納  04/03/XX  Ｈ１６年４月改正対応他
      *  03.00.23    NACL-多々納  04/07/XX  複数科・保険組合せ一括入力
      *  03.00.24    NACL-多々納  05/11/10  前回処方印刷変更
      *  03.00.25    NACL-多々納  05/11/18  MONFUNC 対応 他
      *  03.02.01    NACL-多々納  06/08/29  平成１８年１０月改正対応
      *  03.04.00    NACL-多々納  06/12/01  自賠責健保準拠追加
      *  03.04.01    NACL-多々納  06/12/14  薬剤附加チェック追加
      *  03.04.02    NACL-多々納  07/01/30  排他中処理追加
      *  03.04.03    NACL-多々納  07/02/XX  包括診療処理追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  03.05.01    NACL-多々納  07/07/XX  同日再診・訂正選択追加他
      *  03.05.01    NACL-多々納  07/07/19  "0085XXXXX"コメント対応
      *  03.05.01    NACL-多々納  07/08/10  メモ登録追加
      *  04.01.00    NACL-多々納  07/10/XX  テーブル４００対応
      *  04.01.00    NACL-多々納  07/10/22  公務災害対応
      *  04.01.00    NACL-多々納  07/10/XX  悪性腫瘍検査対応
      *  04.01.00    NACL-多々納  07/11/21  器材商品名表示対応
      *  04.01.00    NACL-多々納  07/12/XX  患者禁忌薬剤対応
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      *  04.04.00    NACL-多々納  08/11/28  処理共通対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/07/30  リハビリ発症日編集対応
      *  04.05.00    NACL-多々納  09/08/11  退職国保年齢チェック対応
      *  04.05.00    NACL-多々納  09/09/15  同日会計の上限エラー対応
      *  04.05.00    NACL-多々納  09/09/16  "0086XXXXX"コメント対応
      *  04.05.00    NACL-多々納  09/10/26  他医院退院反映対応
      *  04.06.00    NACL-多々納  10/07/XX  エラー内容変更
      *  04.06.00    NACL-多々納  10/08/12  診療科変更対応
      *  04.06.00    NACL-多々納  10/09/06  自費保険入院中入力対応
      *  04.06.00    NACL-多々納  11/02/16  用法コメント対応
      *  04.07.00    NACL-多々納  11/09/XX  院外処方点数表示対応
      *  04.07.00    NACL-多々納  12/01/05  クライアント印刷対応
      *  04.07.00    NACL-多々納  12/08/XX  ドクター選択対応
      *  04.07.00    NACL-多々納  12/10/10  照会からの遷移対応
      *  04.08.00    NACL-多々納  12/12/17  患者情報通知対応
      *  04.08.00    NACL-多々納  13/03/XX  Ｈ２５年４月改正対応
      *  04.08.00    NACL-多々納  13/06/28  ドクターカーソル移動対応
      *  04.08.00    NACL-多々納  14/02/XX  行挿入・カーソル移動位置変更
      *  04.08.00    NACL-多々納  14/03/19  .123 処理対応他
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.08.00    NACL-多々納  14/08/11  クライアント印刷変更
      *  04.07.00    NACL-多々納  14/10/09  患者登録戻り処理変更
      *  04.08.00    NACL-多々納  14/10/23  連続挿入対応
      *  04.08.00    NACL-多々納  15/03/XX  Ｈ２７年４月改正対応
      *  04.08.00    NACL-多々納  16/03/XX  Ｈ２８年４月改正対応
      *  04.08.00    NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  16/06/XX  中心静脈（植入型カテーテル）対応
      *  04.08.00    NACL-多々納  16/12/XX  電子お薬手帳連携対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年４月改正対応
      *  04.08.00    NACL-多々納  18/09/XX  選択式コメント対応
      *  05.00.00    NACL-多々納  20/01/XX  訂正時の患者登録更新後対応
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
      *    パラメタデータ
      *    SELECT  PARAK02-FILE    ASSIGN  FILE-NAME22
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA2.
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
      *    画面スパ領域パラメタデータ
      *FD  PARAK02-FILE.
      *    画面スパ領域
      *    COPY    "K02SCR-SPA"      REPLACING
      *                              //SPA-// BY //PARA-SPA-//.
      *
      *    ＳＰＡ画面パラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(1500).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME22.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(08)   VALUE   "K01PARA2".
      *    03  FILE-HOSPNUM22      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID22       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
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
           03  STS-PARA2           PIC X(02).
           03  STS-SPASCR          PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-KNJBYO          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTBYOMEI        PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
           03  FLG-PTMEMOINF       PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-GMN             PIC 9(01).
           03  FLG-KINKI           PIC 9(01).
           03  FLG-KINKI-ARI       PIC 9(01).
           03  FLG-KINKI-ARI2      PIC 9(01).
           03  FLG-ALLFLG          PIC 9(01).
           03  FLG-KANA            PIC 9(01).
           03  FLG-SUJI            PIC 9(01).
           03  FLG-TOROKU          PIC X(01).
           03  FLG-SYORI           PIC X(01).
      *
           03  FLG-SST             PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-OK2             PIC 9(01).
           03  FLG-ADD             PIC 9(01).
      *    初期画面かの判定
           03  FLG-SYOKI           PIC 9(01).
      *
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-SYOSIN          PIC 9(01).
      *    診察料算定済み
      **** 03  FLG-SINSATU          PIC 9(01).
      *    小児科外来診療科算定
           03  FLG-SYONIKA         PIC 9(01).
      *    算定履歴有無
           03  FLG-JYURR-ARI       PIC 9(01).
      *    時間外加算用
           03  FLG-DELFLG          PIC 9(01).
           03  FLG-ZAIEND          PIC 9(01).
      *----(01.00.01) LINE ADD START ----------------------------------
      *    セット処理有無
           03  FLG-SETSYORI        PIC 9(01).
           03  FLG-SET             PIC 9(01).
           03  FLG-SET-HEN         PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
           03  FLG-MAE             PIC 9(01).
      *    剤チェック対象外
           03  FLG-TAISYO          PIC 9(01).
      *    回数入力なし
           03  FLG-KAISU          PIC 9(01).
      *    自費入力
           03  FLG-JIHI           PIC 9(01).
      *
      *    処方せんフラグ
           03  FLG-SYOHOU         PIC 9(01).
           03  FLG-SYOHOU-ARI     PIC 9(01).
      *
           03  FLG-PARASET         PIC 9(01).
      *    検索種別
           03  FLG-KOUI            PIC 9(01).
      *    入力有無
      **** 03  FLG-SYORINASI       PIC 9(01).
      *    治癒有無
           03  FLG-KEIZOKU         PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-INS             PIC 9(01).
      *
           03  FLG-KUHAKU          PIC 9(01).
      *
           03  FLG-CHK2            PIC 9(01).
           03  FLG-CHK3            PIC 9(01).
           03  FLG-ARI2            PIC 9(01).
           03  FLG-NYU-ARI         PIC 9(01).
      *    入院中
           03  FLG-NYUIN           PIC 9(01).
      *
           03  FLG-SYUBETU         PIC 9(01).
      *
           03  FLG-K09TUIKA           PIC 9(01).
      *    入力なし
           03  FLG-INPUT-NASI      PIC 9(01).
      *
           03  FLG-HEN-CHK         PIC 9(01).
      *
           03  FLG-IN1             PIC 9(01).
      *
           03  FLG-FIRUMU           PIC 9(01).
      *
           03  FLG-SONOTA           PIC 9(01).
           03  FLG-NAIFUTAN         PIC 9(01).
      *
           03  FLG-SYOKAI           PIC 9(01).
      *
           03  FLG-RIHTEI           PIC 9(01).
      *    院内・院外処方判定
           03  FLG-INGAIKBN         PIC 9(01).
      *
           03  FLG-TEISEI           PIC 9(01).
      *    薬剤有無
           03  FLG-YAKUZAI-ARI       PIC 9(01).
      *
           03  FLG-SRYKBN            PIC 9(01).
           03  FLG-FUKU-END          PIC 9(01).
           03  FLG-FUKU              PIC 9(01).
           03  FLG-HKN-ARI           PIC 9(01).
           03  FLG-MOD-FLG           PIC 9(01).
      *
      *    画面変更
           03  FLG-HENKOU            PIC 9(01).
      *    初期
           03  FLG-INIT              PIC 9(01).
           03  FLG-MAE-TENSU         PIC 9(01).
      *
           03  FLG-TOUGETU            PIC 9(01).
      *
           03  FLG-WKSRYACT-ARI       PIC 9(01).
      *
           03  FLG-HKNCHK             PIC 9(01).
      *
           03  FLG-KENSADEL            PIC 9(01).
      * 
           03  FLG-CHKSET              PIC 9(01).
      *    包括診療用
           03  FLG-HKTCHK              PIC 9(01).
           03  FLG-HOUKATU-ZAI         PIC 9(01).
      *
           03  FLG-TEI-ARI             PIC 9(01).
           03  FLG-SANTEI              PIC 9(01).
      *    クライアント印刷有
           03  FLG-CLIENT-OK           PIC 9(01).
      *    スクロール背景
           03  FLG-BGCOLORS            PIC 9(01).
      *    会計照会戻りチェック
           03  FLG-CHK-ERR             PIC 9(01).
           03  FLG-CHKRRK              PIC 9(01).
      *ver.4.8
           03  FLG-POPINI              PIC 9(01).
      *    悪性区分前
           03  FLG-MAE-AKUSEI          PIC 9(01).
      *
      *!!!!!!!!!!!!!!!!!!
      *    連続行挿入
           03  FLG-INPUT-NASI2         PIC 9(01).
           03  FLG-F6                  PIC 9(01).
           03  FLG-F7                  PIC 9(01).
           03  FLG-INSEND              PIC 9(01).
      *!!!!!!!!!!!!!!!!!!
      *H28.4
           03  FLG-CHUEND              PIC 9(01).
      *
           03  FLG-INGAI-ARI           PIC 9(01).
      *H31.2
           03  FLG-NAI-NYUIN           PIC 9(01).
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
           03  IDX-Z2              PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-SA              PIC 9(04).
      *
           03  IDX-1               PIC 9(04).
      *
           03  IDX-ERR             PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-SIN2            PIC 9(04).
      *
           03  IDX-STR             PIC 9(04).
           03  IDX-STR2            PIC 9(04).
           03  IDX-STR3            PIC 9(04).
      *
           03  IDX-P               PIC 9(04).
           03  IDX-K               PIC 9(04).
           03  IDX-KAI             PIC 9(04).
           03  IDX-KAI2            PIC 9(04).
           03  IDX-K1              PIC 9(04).
           03  IDX-K2              PIC 9(04).
           03  IDX-K3              PIC 9(04).
           03  IDX-K4              PIC 9(04).
           03  IDX-C               PIC 9(04).
           03  IDX-X               PIC 9(04).
           03  IDX-S2              PIC 9(04).
           03  IDX-IP              PIC 9(04).
           03  IDX-SST             PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDX-K98             PIC 9(04).
      *
           03  IDX-HKN             PIC 9(04).
           03  IDX-HKN2            PIC 9(04).
           03  IDX-KA              PIC 9(04).
           03  IDX-KA2             PIC 9(04).
           03  IDX-HZN              PIC 9(04).
      *
           03  IDX-GG              PIC 9(04).
      *****03  WRK-MAX             PIC 9(04).
           03  WRK-STR             PIC 9(04).
      *
           03  IDX-SET             PIC 9(04).
           03  IDX-TEI             PIC 9(04).
      *
           03  IDH                 PIC 9(04).
           03  IDH3                PIC 9(04).
      *
           03  IDX-F               PIC 9(04).
           03  IDX-F2              PIC 9(04).
           03  IDX-F3              PIC 9(04).
      *
           03  IDX-NUM             PIC 9(06).
           03  IDX-H1              PIC 9(04).
           03  IDX-H2              PIC 9(04).
      *
           03  IDY2                PIC 9(04).
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
           03  WRK-YMD.
               05  WRK-YM          PIC X(06).
               05  WRK-DD          PIC X(02).
           03  WRK-DD-1            PIC 9(02).
      *
           03  WRK-TOUYMD.
               05  WRK-TOUYY       PIC 9(04).
               05  WRK-TOUMM       PIC 9(02).
               05  WRK-TOUDD       PIC 9(02).
      *
           03  WRK-FREE            PIC 9(04).
           03  WRK-IDX             PIC 9(03).
      *    入力項目名編集用 
           03  WRK-IDX2            PIC 9(03).
           03  WRK-IDX3            PIC 9(03).
           03  WRK-WIDGET          PIC X(20).
           03  WRK-IN-MAP-IDX      PIC 9(03).
      *    年齢判定用
           03  WRK-BIRTHDAY.
               05  WRK-BIRTH-YY    PIC 9(04).
               05  WRK-BIRTH-MM    PIC 9(02).
               05  WRK-BIRTH-DD    PIC 9(02).
           03  WRK-NYUYOJI         PIC 9(04).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU-Z     PIC --------.
           03  WRK-KAISU-X.
               05  WRK-KAISU-Z     PIC ZZZZ.
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZZZZZZ.
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
           03  WRK-NOZ             PIC ZZ9.
      *ver.4.7
           03  WRK-TENSU-GX.
               05  WRK-TENSU-G1    PIC X(01).
               05  WRK-TENSU-GZ    PIC ZZZZZZ.
               05  WRK-TENSU-G2    PIC X(01).
      *
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(04).
      *
           03  WRK-KAISU           PIC 9(04).
           03  WRK-GMN-IDX         PIC 9(04).
      *
           03  WRK-DOSELNUM            PIC 9(04).
           03  WRK-DOSELNUM2           PIC 9(04).
           03  WRK-SELNUM              PIC 9(04).
           03  WRK-SELNUM2             PIC 9(04).
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
           03  WRK-GOKEI-9         PIC 9(10).
      *
           03  WRK-MEINUM          PIC 9(04).
           03  WRK-RENNUM          PIC 9(04).
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-PARAEDABAN      PIC 9(04).
      *
      *    退院日
           03  WRK-TAIINYMD        PIC X(08).
           03  WRK-LASTTAIINYMD    PIC X(08).
           03  WRK-GMN-TAIINYMD    PIC X(09).
      *    入院期間
           03  WRK-STDD            PIC 9(02).
           03  WRK-EDDD            PIC 9(02).
           03  IDX-D2              PIC 9(02).
      *
           03  WRK-MOJI            PIC X(01).
      *
           03  WRK-SRYCD           PIC X(09).
           03  IDX-S               PIC 9(04).
      *    時間外フラグ
           03  WRK-TIMEFLG         PIC X(01).
      *    入力セット処理
           03  WRK-SETCD           PIC X(06).
      *
           03  WRK-KINKIFLG            PIC X(01).
           03  WRK-PTKINKI-ARI         PIC 9(01).
           03  WRK-TUKIJGNFLG          PIC X(01).
           03  WRK-TUKIJGNFLG1         PIC X(01).
      *    算定回数
           03  WRK-SANTEI-CNT          PIC 9(04).
      *    入力位置
           03  WRK-FLG-IN              PIC X(01).
           03  WRK-FLG-IN4             PIC X(01).
      *    同日初診フラグ
           03  WRK-DOUSIN              PIC 9(01).
      *
      *    診療行為チェック用編集
           03  WRK-TDD                 PIC 9(02).
           03  FLG-ACCTSYORI           PIC 9(01).
      *
           03  WRK-KIDMSG          PIC X(100).
           03  WRK-KID-SRYCD       PIC X(09).
      *H28.4
           03  WRK-KIDCD-MSG       PIC X(20).
      *
           03  WRK-MOTOPG          PIC X(08).
      *
      *    労災保険用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-EN              PIC 9(08).
      *
           03  WRK-KEI-ERRCD           PIC X(04).
           03  WRK-KEI-ERRMSG2         PIC X(80).
           03  WRK-ERR-ERRCD           PIC X(04).
           03  WRK-ERR-ERRMSG2         PIC X(80).
           03  WRK-KEI-IDX             PIC 9(04).
      *
      *    老人年齢計算領域
           03  WRK-NENREI.
               05  WRK-NENREI-YY         PIC 9(03).
               05  WRK-NENREI-MM         PIC 9(02).
               05  WRK-NENREI-DD         PIC 9(02).
      *    老人フラグ
           03  WRK-ROUJIN              PIC 9(01).
      *
      *    入力時表示用
           03  IDX-INCNT               PIC 9(04).
           03  FLG-NYURYOKU            PIC 9(01).
           03  WRK-HEN-INPUTCD         PIC X(54).
           03  FLG-IN-ATAI             PIC 9(01).
      *
      *    入力位置判定用
           03  WRK-IN-SURYO        PIC 9(02).
           03  WRK-IN-IDX          PIC 9(04).
           03  WRK-IN-IDX2         PIC 9(04).
           03  WRK-MAX-IDX         PIC 9(04).
      *
           03  WRK-WKSRY-HKNCOMBI      PIC 9(04).
      *
      *    院外、院内変更
           03  WRK-IDX-IN          PIC 9(04).
      *
      *    来院回数
           03  WRK-SINSATU-CNT     PIC 9(04).
      *    ワーク処理フラグ
           03  WRK-SYORIFLG        PIC 9(01).
      *
           03  WRK-ERRCD           PIC X(04).
           03  WRK-KIDCD           PIC X(04).
           03  WRK-KIDCD2          PIC X(04).
      *    パス名
           03  WRK-PATH            PIC X(64).
      *
           03  WRK-SAKIPG          PIC X(08).
      *    画面制御
           03  WRK-PUTTYPE         PIC X(08).
      *    ＤＯ選択編集
           03  WRK-ZZ              PIC ZZZZ.
      *    複数保険
           03  WRK-HKNCOMBI        PIC X(04).
           03  WRK-SEL-HKNNUM          PIC X(03).
      *
      *    保険組合せ保存
           03  WRK-MAE-HKNCOMBI    PIC X(04).
      *
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *
      *    入力名称
           03  WRK-MEISYO-G.
               05  WRK-MEIH                PIC X(02).
      *R02.    05  WRK-MEISYO              PIC X(78).
               05  WRK-MEISYO              PIC X(138).
           03  WRK-MEISYO-GHZN.
               05  WRK-MEIHHZN             PIC X(02).
      *R02.    05  WRK-MEISYOHZN           PIC X(78).
               05  WRK-MEISYOHZN           PIC X(138).
           03  WRK-LEN                 PIC 9(04).
      *    入力値の入力判定用
           03  WRK-HZN-ATAI-AREA.
               05  WRK-HZN-ATAI1           PIC X(10).
               05  WRK-HZN-ATAI2           PIC X(10).
               05  WRK-HZN-ATAI3           PIC X(10).
               05  WRK-HZN-ATAI4           PIC X(10).
               05  WRK-HZN-ATAI5           PIC X(10).
      *
      *    初期業務識別区分
           03  WRK-GYOUMU-KBN          PIC X(04).
      *    ドクターコード
           03  WRK-DRCD.
               05  WRK-DRCD1               PIC X(01).
               05  WRK-DRCD2               PIC X(04).
           03  WRK-DRCD-MEI                PIC X(80).
           03  WRK-SRYKA                   PIC X(02).
           03  WRK-HZN-ERRCD           PIC X(04).
      *
           03  WRK-ERRCD1              PIC X(04).
           03  WRK-ERRCD2              PIC X(04).
           03  WRK-ERRCD3              PIC X(04).
      *
      *    コラムリスト位置
           03  WRK-K02-ROW             PIC S9(09).
           03  WRK-K02-ROWATTR         PIC S9(09).
           03  FLG-GMN-INIT            PIC  9(01).
      *
      *    診察料有無
           03  WRK-SINSATU-ARI          PIC 9(01).
      *
      *    小児科外来診察料加算判定
           03  WRK-SYONIKA-KBN         PIC X(01).
      *    訂正時の点数判定
           03  WRK-MAE-TENSU           PIC 9(08).
      *    急性発生日
           03  WRK-KYUSEI-YMD          PIC X(08).
           03  WRK-KYUSEI-END          PIC X(08).
      *
           03  WRK-SYOKAI-YMD          PIC X(08).
      *
           03  WRK-SPA-GMN-SYOSINYMD   PIC X(24).
      *    P97検索名
           03  WRK-P97-NAME            PIC X(22).
           03  WRK-GMN-PTID            PIC 9(10).
      *!
           03  WRK-HOSPNUM             PIC 9(02).
           03  WRK-GRPHOSPUSERID       PIC X(64).
           03  WRK-MACHIPG             PIC X(08).
      *
      *    同日連番保存
           03  WRK-SC95-LAST-RENNUM    PIC 9(01). 
      *!
      *    今回リハビリ発生日
           03  WRK-KYUSEI-INP-G.
               05  WRK-KYUSEI-IN-G         OCCURS   7.
                   07  WRK-KYUSEI-ST       PIC X(01).
                   07  WRK-KYUSEI-ED       PIC X(01).
               05  WRK-TEIGEN-IN           PIC X(01).
      *
           03  WRK-SYOSINYMD-G         PIC X(09).
      *
      *    ドクター入力
           03  WRK-GMN-DRCD-G.
               05  WRK-GMN-DRCD        PIC X(04).
               05  WRK-GMN-F1          PIC X(01).
               05  WRK-GMN-DRCDNAME    PIC X(24).
      *
      *!!!!!!!!!!!!!!!!!!
      *    挿入行数
           03  WRK-INSCNT              PIC 9(02).
           03  WRK-GYO                 PIC 9(04).
      *改頁行数
      ***  03  WRK-PAGE-LINE           PIC 9(02).
      *!!!!!!!!!!!!!!!!!!
      *
           03  WRK-MAE-BIRTHDAY        PIC X(08).
      *
      *H28.1
           03  WRK-HZN-DRCD             PIC X(05).
      *H30.9
           03  FLG-AUTOERR             PIC 9(01).
           03  IDX-SELCOM              PIC 9(04).
      *
      *    訂正時の内容保存
       01  WRK-ORCSC95AREA             PIC X(150).
      *
      *    小児科外来診察料テーブル
      **** COPY   "CMSYONIKATBL.INC".
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
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
      *ver.4.7
           03  WRK-MAP-CUR4        PIC 9(04).
           03  FLG-MAP-CUR4        PIC 9(01).
      *    画面数量編集用
       01  WRK-GMN-SURYO-AREA.
           03  WRK-SURYO-HENSYU.
      **       05  WRK-H-SURYO        PIC X(09).
               05  WRK-H-SURYO        PIC X(11).
               05  WRK-H-TANINAME     PIC X(04).
               05  WRK-H-F1           PIC X(01).
      *        05  WRK-H-ZAIKEI       PIC X(15).
               05  WRK-H-ZAIKEI       PIC X(14).
               05  WRK-H-KEI          PIC X(08).
      *
           03  WRK-KEI-X.
               05  WRK-KEI-Z       PIC Z(08).
           03  WRK-KEI-X2.
               05  WRK-KEI-Z2      PIC -(08).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI-H        PIC X(40).
           03  WRK-KAKERU          PIC X(03).
      *
        01  WRK-HENSYU-AREA.
      *    ページ表示
           03  WRK-HEN-PAGE.
               05  WRK-HEN1               PIC X(02).
               05  WRK-HEN-PAGE1          PIC ZZZ.
               05  WRK-HEN2               PIC X(01).
               05  WRK-HEN-PAGE2          PIC ZZ.
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
      *処理最大行
       01  MAX-LINE-VALUE          PIC 9(04)   VALUE   400.
      *画面最大行
      *01  MAX-GMN                 PIC 9(04)   VALUE   20.
       01  MAX-GMN                 PIC 9(04)   VALUE   400.
      *画面ページ数（処理最大行÷画面最大行）
      *01  MAX-PAGE                PIC 9(04)   VALUE   20.
       01  MAX-PAGE                PIC 9(04)   VALUE   1.
      *TEST
      *改頁行数（１ページ行数）
       01  PAGE-LINE               PIC 9(02)   VALUE   24.
      *
      *
      *PandaTable色
      *青
       01  CL-BLUE                 PIC X(07)   VALUE   "#0000EE".
      *赤
       01  CL-RED                  PIC X(07)   VALUE   "#FF0000".
      *クロ
       01  CL-BLACK                PIC X(07)   VALUE   "#000000".
      *白
       01  CL-WHITE                PIC X(07)   VALUE   "#FFFFFF".
      *緑
       01  CL-GREEN                PIC X(07)   VALUE   "#00B000".
      *
       01  WRK-MOJI-AREA.
           03  FLG-SP              PIC 9(02).
           03  FLG-AST             PIC 9(02).
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
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDX-D               PIC 9(04).
      *
           03  WRK-SANTEI-SYORI    PIC X(01).
      *
      *    継続の時、自動発生のチェック
      *01  WRK-CONTCHK-TBL.
      *    03  WRK-IDX-Y               PIC 9(04).
      *
      *    自動加算テーブル
       01  WRK-KASAN-AREA.
           03  WRK-KASAN-TBL               OCCURS   60.
               05  WRK-KSN-HKNCOMBI        PIC X(04).
               05  WRK-KSN-SRYKA           PIC X(02).
               05  WRK-KSN-SRYSYUKBN       PIC X(03).
               05  WRK-KSN-SRYKBN          PIC X(02).
               05  WRK-KSN-SRYCD           PIC X(09).
               05  WRK-KSN-ATAI-G.
                   07  WRK-KSN-ATAI        PIC X(08)   OCCURS  4.
               05  WRK-KSN-TENSU           PIC 9(08).
               05  WRK-KSN-SURYO           PIC 9(05)V9(05).
               05  WRK-KSN-ZAINUM          PIC 9(08).
               05  WRK-KSN-AUTOKBN         PIC X(01).
               05  WRK-KSN-INPUTCD         PIC X(54).
               05  WRK-KSN-ZAIEND          PIC X(01).
               05  WRK-KSN-HKTKBN          PIC 9(01).
           03  WRK-KSN-IDX             PIC 9(04).
      *
      *    画面ＳＰＡ領域ワーク
       01  WRK-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  MAE-SCR-REC.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //MAE-//.
      *
      *    保存ＳＰＡ領域ワーク
       01  WRK-HZN-SCR-AREA.
           COPY    "CPK02SPASCR.INC"  REPLACING
                                       //SPA-// BY //WRK-HZN-//.
      *
       01  WRK-HOZ-IDX             PIC 9(04).
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *    スパ領域 ワーク
           COPY    "COMMON-SPA"      REPLACING
                                     //SPA-// BY //WRK-SPA-//.
      *
      *    画面スパ領域ワーク
           COPY    "K02SCR-SPA"      REPLACING
                                     //SPA-// BY //HZN-SPA-//.
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA"   REPLACING 
                                     //SPA-// BY //HZN-SPA-//.
      *
       01  WRK-SYSTIME               PIC 9(08).
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
      *D   COPY    "CPSYSKANRI.INC".
      *
      *    COPY    "CPSK1005.INC".
      *
      *    COPY    "CPSK1006.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    患者番号変換
      *01  PTNUM-REC.
      *    COPY    "CPPTNUM.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    チェックマスタ−
       01  CHK-REC.
           COPY    "CPCHK.INC".
      *
      *    入力セットコード
       01  INPUTSET-REC.
           COPY    "CPINPUTSET.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC          OCCURS   400.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    収納マスタワーク
      *01  HZN-SYUNOU-REC.
      *    COPY    "CPSYUNOU.INC"  REPLACING
      *                              //SYU-// BY //HZN-SYU-//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    患者病名
       01  PTBYOMEI-REC.
           COPY    "CPPTBYOMEI.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *    患者メモ
       01  PTMEMOINF-REC.
           COPY    "CPPTMEMOINF.INC".
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
      *
      *    ＳＰＡ一時テーブル
       01  SPATMP-REC.
           COPY    "CPSPA-TMP.INC".
      *
           COPY    "MCPDATA2.INC".
      *
      *
      *****************************************************************
      *    サブプロ用　領域
      *****************************************************************
      *
      *   日付変換サブ
           COPY    "CPORCSDAY.INC".
           COPY    "CPORCSLNK.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    数字変換領域
           COPY    "CPORCSNUM.INC".
      *   患者番号変換サブ
      *D   COPY    "CPORCSPTID.INC".
      *   患者番号変換サブ
           COPY    "CPORCSPTNUM.INC".
      *
      *    負担金額計算用パラメタ
      *D   COPY    "CPORCS01.INC".
      *
      *    診療行為算定用共通パラメタ
           COPY    "CPORCSC00.INC".
      *
      *    算定履歴チェックパラメタ
      *D   COPY    "CPORCSC91.INC".
      *
      *    点数マスタ編集パラメタ
           COPY    "CPORCSC92.INC".
      *
      *    剤分離パラメタ
           COPY    "CPORCSC93.INC".
      *
      *    画面再編集パラメタ
           COPY    "CPORCSC94.INC".
      *
      *    初期ＳＰＡ画面編集パラメタ
           COPY    "CPORCSC95.INC".
      *
      *    エラーメッセージ編集パラメタ
           COPY    "CPORCSC96.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    投与量等チェックパラメタ
      *D   COPY    "CPORCSC98.INC".
      *
      *    包括診療チェックパラメタ
           COPY    "CPORCSCHKTCHK.INC".
      *
      *    診察料自動発生パラメタ
           COPY    "CPORCSC10S.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    薬情報印刷パラメタ
           COPY    "CPORCSCH30.INC".
      *
      *    お薬手帳発行サブパラメタ
           COPY    "CPORCSCH62.INC".
      *
      *    全角チェックパラメタ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    マシン日付取得サブ
      *D   COPY    "CPORCSMCNDATE.INC".
      *
      *    セット展開サブ
           COPY    "CPORCSCSETHEN.INC".
      *
      *    ワーク診療行為展開・作成
           COPY    "CPORCSCWKSRY.INC".
      *    内分泌負荷試験更新パラメタ
      *D   COPY    "CPORCSSANFUKA.INC".
      *
      *    禁忌薬剤チェックパラメタ
           COPY    "CPORCSCKNKCHK.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    入院履歴編集サブパラメタ
           COPY    "CPORCSCNYUHEN.INC".
      *
      *    診療行為内容編集・計算
           COPY    "CPORCSC100.INC".
      *
           COPY    "CPORCSCOMMON.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ドクター編集処理パラメタ
           COPY    "CPORCSDRSET.INC".
      *
      *    患者情報通知パラメタ
           COPY    "CPORCSPOPUPSET.INC".
      *
      *    ＤＢ検索
      *01  MCPDATA-REC                 PIC X(5000).
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
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
      *TEST
      *   ACCEPT  WRK-SYSTIME         FROM    TIME
      *    DISPLAY "K02 START:" WRK-SYSTIME (1:2) ":" 
      *                         WRK-SYSTIME (3:2) ":" 
      *                         WRK-SYSTIME (5:2) ":" 
      *                         WRK-SYSTIME (7:2) "/" 
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  MAE-SCR-REC
           INITIALIZE                  WRK-SCR-REC
           INITIALIZE                  LNK-WKSRYACT-MAX
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    "K02"               TO  SPA-HZN-MOTOPG
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-ERRMSG2
           MOVE    ZERO                TO  FLG-END
           MOVE    ZERO                TO  FLG-END
      *    テーブル最大行
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *v4.7
      *    頁行数
           IF     (SPA-GMLINCNT(1:2)   NOT NUMERIC )
             OR   (SPA-GMLINCNT        =    ZERO)
               MOVE    PAGE-LINE            TO  SPA-GMLINCNT
           END-IF
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-K02SPA-SET-SEC
      *    画面制御
           MOVE    SPACE           TO  WRK-PUTTYPE
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
      *    画面ＳＰＡ書込み
      *    IF     (FLG-END2            =   ZERO  )
      *       AND (SPA-ERRCD       NOT =   "1999")
      *        PERFORM 1003-K02SPA-OUT-SEC
      *    END-IF
      *
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    "K02"           TO  SPA-HZN-MOTOPG
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K02         TO  SPA-FREE
      *TEST
      *    ACCEPT  WRK-SYSTIME         FROM    TIME
      *    DISPLAY "K02 END  :" WRK-SYSTIME (1:2) ":" 
      *                         WRK-SYSTIME (3:2) ":" 
      *                         WRK-SYSTIME (5:2) ":" 
      *                         WRK-SYSTIME (7:2) "/" 
      *
           .
           EXIT    PROGRAM
           .
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
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
               IF      FLG-END             =   ZERO
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE    SPACE               TO  LINKAREA
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "K02"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
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
      *
           .
       1001-SRYKBN-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-K02SPA-SET-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                      SPA-SCR-REC
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           IF     SPA-HOSPNUM  NOT NUMERIC
               GO   TO  1002-K02SPA-SET-EXT
           END-IF
      *
      *    TBL_PARA を使用する
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SPAGMN"            TO  PARA-FILEMEI
           MOVE    PARA-REC           TO  MCPDATA-REC
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
               MOVE   PARA-DATA-REC        TO  SPA-GMN-TBL (IDX)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      IDX                 =   ZERO
               MOVE    ZERO                TO  SPA-GMN-MAX
           ELSE
      *
      *        行位置とカーソル位置の決定
               PERFORM 10021-K02SPA-INIT-SET-SEC
           END-IF
      *
           .
       1002-K02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    行位置とカーソル位置の決定処理
      *****************************************************************
       10021-K02SPA-INIT-SET-SEC             SECTION.
      *
      *    行位置とカーソル位置の決定
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  WRK-MAP-MAX
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
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *            画面表示の最終行（次頁用）
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-INPUTCD(WRK-IDX)  NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
                   MOVE    WRK-IDX         TO  SPA-GMN-MAX
               END-IF
           END-PERFORM
           .
       10021-K02SPA-INIT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-K02SPA-OUT-SEC             SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
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
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
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
                       DISPLAY "K02 PARA INSERT ERR:"  MCP-RC
                           ",KEY2:" PARA-KEY
                           ",EDANUM:" PARA-EDANUM
                   END-IF
               END-IF
           END-PERFORM
           .
       1003-K02SPA-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    ZERO                TO  FLG-GMN
           MOVE    SPACE               TO  WRK-KID-SRYCD
      *
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
      *H28.1
               IF      SPA-MOTOPG          =   "UDR"
                   MOVE    SPA-DRCD            TO  WRK-HZN-DRCD
               END-IF
      *
      *        画面ＳＰＡ読み込み
               PERFORM 1002-K02SPA-SET-SEC
      *        元画面へ戻す
               MOVE    SPA-MOTOPG          TO  WRK-MOTOPG
               IF      WRK-MOTOPG          =   "Y01"  OR  "U01"
                                                      OR  "P97"
                                                      OR  "KA01"
                                                      OR  "XF01"
                                                      OR  "UDR"
                   MOVE    LNK-COMMON      TO  SPA-KYOUTU
                   MOVE    LNK-SCRSPA      TO  SPA-K02
                   MOVE    SPA-WORK        TO  SPA-K01KYOUTU
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
      *        テーブル最大行
               MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *!
               CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *        予約の時、そのまま画面表示へ
               IF      WRK-MOTOPG          =   "Y01"
                                           OR  "KA01"
                   IF      SPA-PTID            =   ZERO
                       INITIALIZE              SPA-K01SET-AREA
                       MOVE    SPA-SYOHOKBN    TO  SPA-INGAIKBN
                       MOVE    ZERO            TO  SPA-GMN-CUR
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
               IF      WRK-MOTOPG          =   "XF01"
      *            メモ登録有無
                   PERFORM 41021-PTMEMOINF-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
      *        外部より選択がある時
               IF      WRK-MOTOPG(1:3)     =   "U01"  OR
                                               "P97"
                   PERFORM 3101-P97SPASET-SEC
                   GO      TO      300-SCREEN-EXT
               END-IF
      *H28.1
               IF      WRK-MOTOPG          =   "UDR"
                   IF      SPA-PTID    NOT =   ZERO
      *                ドクター一覧再編集
                       PERFORM 30091-UDR-SETSYORI-SEC
                   END-IF
                   GO      TO      300-SCREEN-EXT
               END-IF
      *
           END-IF
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *
           MOVE    MAX-LINE-VALUE      TO  SPA-MAX-LINE
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    入院機能の判定
           IF     (SPA-MOTOPG(1:3)     =   "M01" )  OR
                 ((SPA-MACHIPG         =   "U02" ) AND
                  (SPA-MOTOPG          =   "U02" ) AND
                  (SPA-PTNUM           =   SPACE ))
               IF      (SPA-BEDFLG         >   ZERO )
                   PERFORM 3009-NYUINCHK-SEC
                   IF      FLG-END             =   1
                       GO      TO      300-SCREEN-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "K99"
                   IF      SPA-K99-DATA    NOT =   SPACE
                        MOVE    SPA-COM-GYOU(SPA-GMN-IDX)
                                                   TO  WRK-IN-MAP-IDX
                       MOVE    SPA-GMN-IDX         TO  WRK-STR
                       MOVE    SPA-K99-DATA        TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                       MOVE    "1"                 TO  SPA-COM-IN
                                                         (SPA-GMN-IDX)
      *                入力コード・名称チェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *                画面ＳＰＡチェック処理
                       PERFORM 410112-KOUI-SPACHK-SEC
                       MOVE    2                   TO  FLG-GMN
                   END-IF
               WHEN    "K98"
      *@
      *            悪性腫瘍の検査
                   IF     SPA-AKUSEI-FLG       =   1
                       PERFORM 30112-AKUSEI-COMMENT-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
      *TEST
      *H30.9
      *            選択式コメント
                   IF      SPA-SELCOM-FLG      =   1
                       PERFORM 30113-SELCOMMNT-COMMENT-SEC
                   ELSE
      *TEST
      *
                   IF      SPA-K98-SRYCD-G   NOT =   SPACE
                       PERFORM 3011-K98-SET-SEC
                       MOVE    1                   TO  FLG-GMN
                   ELSE
                       MOVE    "1"                 TO  SPA-COM-CUR
                                                         (SPA-GMN-IDX)
                       MOVE    2                   TO  FLG-GMN
                   END-IF
      *
                   END-IF
      *
                   END-IF
               WHEN    "K04"
      *            算定処理より
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K021"
      *             禁忌一覧 ＯＫの時、更新処理へ
                   IF     (SPA-K021-SYORI      =   2  )  AND
                          (SPA-K021-CHK        =   1  )
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       EVALUATE    SPA-K021-SAKI
                           WHEN    "K03"
      *                        更新処理
                               PERFORM 41002-KOUSIN-SYORI-SEC
                           WHEN    "C02"
      *                        病名登録遷移処理
                               PERFORM 4401-C02SET-SEC
                       END-EVALUATE
                   END-IF
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K023"
      *            入力コード設定
                   PERFORM 30014-K023-SET-SEC
                   MOVE    2                   TO  FLG-GMN
      *H28.12
      *        電子お薬手帳画面
               WHEN    "K024"
                   MOVE    2                   TO  FLG-GMN
      *!!!
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KID2"
      *            確認画面２より
                   PERFORM 30011-KID2-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K09"
      *            ＤＯ選択画面より
                   PERFORM 3013-K09-SET-SEC
                   MOVE    2                   TO  FLG-GMN
               WHEN    "K97"
      *            診療科・保険選択画面より
                   IF      SPA-K97-SYORI       =   1
                       PERFORM 3014-K97-SET-SEC
                   ELSE
                       IF      (SPA-GMN-IDX    NOT =   ZERO ) AND
                               (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                                   =   "#"
                                                   OR  "$"   )  AND
                               (SPA-GMN-INPUTCD (SPA-GMN-IDX)(2:)
                                                   =   SPACE )
                           MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (SPA-GMN-IDX)
                       END-IF
                   END-IF
                   MOVE    2                   TO  FLG-GMN
               WHEN    "KHELP"
      *            ヘルプ画面より
                   MOVE    2                   TO  FLG-GMN
      *
               WHEN    "M01"
      *            業務選択画面より
                   PERFORM 30010-INIT-GMN-SEC
               WHEN    "XD01"
      *             ユーザプログラム実行画面より
                   PERFORM 30010-INIT-GMN-SEC
      *ver.4.7
               WHEN    "Q02"
               WHEN    "Q02A"
               WHEN    "Q02B"
      *            照会より
                   MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
                   PERFORM 30010-INIT-GMN-SEC
                   MOVE    WRK-SPA-PTNUM       TO  K02-PTNUM
                   PERFORM 41010-PTNUM-SYORI-SEC
                   IF      SPA-KIDCD           =   "0121"
      *                入院中の時、入院画面へ
                       MOVE    SPACE               TO  WRK-PUTTYPE
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 450-K02N-SYORI-SEC
                   END-IF
      *
               WHEN    OTHER
      *            外部・患者決定
                   PERFORM 3000-SCREEN-SYORI-SEC
           END-EVALUATE
      *
           IF      FLG-GMN         NOT =   ZERO
               IF      SPA-ERRCD       NOT =   SPACE
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
      *            エラー編集
      *            数量ゼロの場合、エラー表示しない
                   IF      SPA-ERRCD           =   "A001"
                       MOVE    SPACE               TO  SPA-ERRCD
                   ELSE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               END-IF
               IF     (SPA-KIDCD       NOT =   SPACE ) AND
                      (FLG-END             =   ZERO  )
      *            画面編集
                   PERFORM 500-GMNHEN-SEC
                   PERFORM 520-KIDSET-SEC
               END-IF
      *!!
           ELSE
               IF     (FLG-END2        =   1      )
                  OR  (SPA-ERRCD       =   "1999" )
                   MOVE    "1999"          TO  SPA-ERRCD
                   MOVE    "E"             TO  SPA-K01-YOBI(1:1)
               END-IF
      *!!
           END-IF
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者設定初期画面処理
      *****************************************************************
       3000-SCREEN-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
           MOVE    ZERO                TO  FLG-SYOKAI
           EVALUATE    SPA-MOTOPG
               WHEN    "K01"
      *            診療選択より
                   IF     (SPA-ERRMSG(1:2)    =   "NO"  )  AND
                          (SPA-CONTFLG        =   ZERO  )
      *                戻る
                       MOVE    ZERO               TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       PERFORM 3003-GMN-HEN-SEC
                   ELSE
                       MOVE    ZERO               TO  SPA-SAISIN-CHK
                       MOVE    2                  TO  FLG-PARASET
                       PERFORM 3002-PARA-SET-SEC
                       IF     (SPA-SYORIFLG        =   ZERO )
                          AND (SPA-WKSRY-HKNCOMBI  =   ZERO )
                          PERFORM 3009-SAIHEN-SEC
                       END-IF
                   END-IF
               WHEN    "P02"
      *            患者登録より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF      SPA-ERRMSG(1:2)    =   "NO"
      *                戻る
                       IF      SPA-PTNUM(1:1)     =   "*"
                           MOVE    SPACE          TO  SPA-PTNUM
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           PERFORM 3003-GMN-HEN-SEC
                       END-IF
                   ELSE
                       IF      SPA-PTNUM       =   SPACE
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           IF      SPA-SYORIFLG        =   ZERO
                               IF      SPA-GMN-PTNUM(1:1)  =   "*"
                                   PERFORM 3009-SAIHEN-SEC
                               ELSE
                                   PERFORM 30011-INIT-HEN-SEC
                               END-IF
                           ELSE
                               PERFORM 3003-GMN-HEN-SEC
      *!!!
      *R02.1                   患者登録変更チェック
                               PERFORM 30044-PTINF-P02CHEK-SEC
                           END-IF
                       END-IF
                   END-IF
               WHEN    "K02N"
      *            入院
                   INITIALIZE                  SPA-K02
                   IF      SPA-PTID        NOT =   ZERO
                       PERFORM 3009-SAIHEN-SEC
                   ELSE
                       PERFORM 30010-INIT-GMN-SEC
                   END-IF
               WHEN    "U02"
      *            受付より再編集
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   IF     (SPA-PTNUM       NOT =   SPACE    ) AND
                          (SPA-GMN-PTNUM   NOT =   SPA-PTNUM)
                       PERFORM 3009-SAIHEN-SEC
                       IF      SPA-KIDCD           =   "0121"
      *                     入院中の時、入院画面へ
                           MOVE    SPACE               TO  WRK-PUTTYPE
                           PERFORM 450-K02N-SYORI-SEC
                       END-IF
                   ELSE
                       IF     (SPA-PTNUM       =   SPACE )  AND
                              (SPA-GMN-PTNUM   =   SPACE )
                           PERFORM 30010-INIT-GMN-SEC
                       ELSE
                           PERFORM 3003-GMN-HEN-SEC
                       END-IF
                   END-IF
               WHEN    "K08"
               WHEN    "K03"
      *            登録後クリア画面の表示
                   IF      SPA-PTNUM          =   SPACE
                       PERFORM 30010-INIT-GMN-SEC
                   ELSE
      *
      *            前回の表示
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
      *            特定疾患判定のクリア
                   MOVE    ZERO            TO  SPA-TOKUTEIFLG
                   MOVE    ZERO            TO  SPA-NAIFUKU-CHK
                   MOVE    ZERO            TO  SPA-NAIFUKU-OK
      *H26.10      向精神薬多剤
                   MOVE    ZERO            TO  SPA-PSYCHO-OK
                   MOVE    ZERO            TO  SPA-PSYCHO-CHK
      *H27.4
                   MOVE    ZERO            TO  SPA-TEIGEN30-CHK
                   MOVE    ZERO            TO  SPA-TEIGEN30-OK
      *H30.4
                   MOVE    ZERO            TO  SPA-FUANCHK-CHK
                   MOVE    ZERO            TO  SPA-FUANCHK-OK
      *
                   MOVE    ZERO            TO  SPA-CHOKITOFLG-CHK
                   MOVE    ZERO            TO  SPA-CHOZAI-CHK
                   MOVE    ZERO            TO  SPA-SYOHOU-CHK
                   MOVE    SPACE           TO  SPA-TOKUTEI-SRYKA 
                   MOVE    ZERO            TO  SPA-HKTKEI-FLG
      *            生活習慣病の警告
                   MOVE    ZERO            TO  SPA-SEIKATU-FLG
      *!!          手帳加算確認
                   MOVE    ZERO            TO  SPA-TECHOKBN-CHK
      *ver.4.7     緊急時間外確認
                   MOVE    ZERO            TO  SPA-KENJIKAN-CHK
                   MOVE    ZERO            TO  SPA-GAZJIKAN-CHK
      *H28.9       認知症地域包括投薬確認
                   MOVE    ZERO            TO  SPA-NINCHI-CHK
      *H29.4       投薬３０日超（未使用）
                   MOVE    ZERO            TO  SPA-SHOH30-CHK
      *            診療行為、計計算処理、編集
                   MOVE    SPACE           TO  FLG-TOROKU
                   PERFORM 510-TBLHEN-SEC
                   MOVE    SPACE           TO  FLG-TOROKU
      *
                   END-IF
      *
               WHEN    "K10"
                   IF      SPA-K10-SELNUM      =   ZERO
                       MOVE    2                   TO  FLG-SYOKAI
                   ELSE
                       IF      SPA-K10-SYORI       =   ZERO
      *                変更
                           PERFORM 30091-SAIHEN2-SEC
                       ELSE
      *                追加
                           PERFORM 30092-SAIHEN3-SEC
                       END-IF
      *!!!
      *                ４００行以上はエラーメッセージ表示
                       IF      SPA-ERRCD       =   "0007"
                                               OR  "7004"
                           MOVE    01              TO  SPA-GMN-CUR
                           PERFORM 500-GMNHEN-SEC
                           PERFORM 510-ERRSET-SEC
                       END-IF
      *!!!
      *                ＤＢエラー表示
                       IF      SPA-ERRCD       =   "1999"
                                               OR  "9999"
                           MOVE    01              TO  SPA-GMN-CUR
                           PERFORM 500-GMNHEN-SEC
                           PERFORM 510-ERRSET-SEC
                       END-IF
                   END-IF
      *
               WHEN    OTHER
                   INITIALIZE                  SPA-K02
                   MOVE    ZERO            TO  FLG-PARASET
                   PERFORM 3002-PARA-SET-SEC
                   PERFORM 3003-GMN-HEN-SEC
                   IF      SPA-MOTOPG      =   "S02"  OR  "J02" OR "C02"
                       IF      SPA-PTID    NOT =   ZERO
                           PERFORM 3004-SAI-SET-SEC
                       END-IF
                   END-IF
           END-EVALUATE
      *
           IF      FLG-SYOKAI          =   2
               CONTINUE
           ELSE
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    1               TO  SPA-MAP-IDX
           END-IF
      *
           IF     (SPA-ERRCD       =   SPACE  )  AND
                  (FLG-SYOKAI      =   1      )
      *        システム管理よりカーソル指定
               EVALUATE    SPA-GAI-CURSOR-SET
                   WHEN    "1"
      *                保険組合せ
                       MOVE    05              TO  SPA-GMN-CUR
                   WHEN    "2"
                   WHEN    "3"
      *                受付一覧からの時
                       IF      SPA-MOTOPG          =   "U02"
                           MOVE    1               TO  SPA-GMN-CUR
                       ELSE
                           MOVE    06              TO  SPA-GMN-CUR
                       END-IF
               END-EVALUATE
               IF      LNK-WKSRYACT-MAX        >   ZERO
                   MOVE    01              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       3000-SCREEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院機能の判定処理
      *****************************************************************
       3009-NYUINCHK-SEC              SECTION.
      *
      *    職員情報より業務識別の判定
           MOVE    SPACE               TO  SYS-1010-REC
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           STRING    SPA-OPID          DELIMITED  BY  SPACE
                     "%"               DELIMITED  BY  SIZE
                                       INTO    SYS-1010-TBL
           END-STRING
           MOVE    SPA-SYSYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-1010-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
      *    システム管理検索
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    SPACE               TO  WRK-GYOUMU-KBN
           PERFORM UNTIL   FLG-SYSKANRI        =   1
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               IF      SYS-1010-USERID     =   SPA-OPID
                   IF      SYS-1010-ORCA21FLG  =   SPACE
      *            初期画面設定が無い時、入退院登録の権限がある時
                       IF      SYS-1010-GSRAUTH(36:1)   =  "1"
                           MOVE    "1"         TO  SYS-1010-ORCA21FLG
                       END-IF
                   END-IF
                   MOVE    1               TO  FLG-SYSKANRI
               ELSE
                   MOVE    "key3"              TO  MCP-PATHNAME
                   PERFORM 960-KANRIMST-READ-SEC
               END-IF
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    入院業務を表示する
           IF      (SPA-BEDFLG         >   ZERO )  AND
                   (SYS-1010-ORCA21FLG =   "1"  )
               INITIALIZE              SPA-KYOTUKEY
               INITIALIZE              SPA-K02
               INITIALIZE              SPA-SCR-REC
               INITIALIZE              SPA-K01KYOUTU
               PERFORM 450-K02N-SYORI-SEC
           END-IF
      *
           .
       3009-NYUINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２初期画面表示
      *****************************************************************
       30010-INIT-GMN-SEC              SECTION.
      *
      *    一時データ削除
           PERFORM 210-PARA-DEL-SEC
           MOVE    ZERO                TO  FLG-END2
      *
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-SCR-REC
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-KIDCD2
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    0               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *
           .
       30010-INIT-GMNN-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡのＫ０２を共通ＳＰＡへ
      *****************************************************************
       3003-GMN-HEN-SEC              SECTION.
      *
           IF      SPA-GMN-PTID        =   ZERO
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
               MOVE    SPA-GMN-PTID        TO  SPA-PTID
               GO      TO      3003-GMN-HEN-EXT
           END-IF
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
      *    前回患者保存
           MOVE    SPA-PTID            TO  SPA-LAST-PTID
           MOVE    SPA-PTNUM           TO  SPA-LAST-PTNUM
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *    ドクター
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-DRCD
           ELSE
               MOVE    "1"                 TO  SPA-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:4)
           END-IF
      **   MOVE    SPA-GMN-DRCD        TO  SPA-DRCD
      *    IF     (SPA-DRCD-NAME       NOT =   SPACE )  AND
      *           (SPA-DRCD-NAME(1:4)  NOT =   SPA-DRCD(2:4))
      *        MOVE    "1"                 TO  SPA-DRCD(1:1)
      *        MOVE    SPA-DRCD-NAME(1:4)  TO  SPA-DRCD(2:4)
      **** END-IF
      *
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *
      *    排他制御処理
           PERFORM 990-LOCK-IN-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      3003-GMN-HEN-EXT
           END-IF
      *
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           .
       3003-GMN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       3004-SAI-SET-SEC              SECTION.
      *
      *    収納からの戻りの時、未収額変更
           IF      SPA-MOTOPG      =   "S02"
               INITIALIZE                  ORCSC95AREA
               MOVE    "6"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *
      *        未収金
               MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
           END-IF
      *    病名からの戻りの時、病名再編集
           IF      SPA-MOTOPG          =   "C02"
               INITIALIZE                  ORCSC95AREA
               MOVE    "B"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           END-IF
      *    会計照会からの戻りの時、受診履歴判定
           IF      SPA-MOTOPG          =   "J02"
               PERFORM 30041-JYURRK-J02CHK-SEC
           END-IF
      *
           .
       3004-SAI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    会計照会変更チェック処理
      *****************************************************************
       30041-JYURRK-J02CHK-SEC              SECTION.
      *
           MOVE    SPA-K02             TO  HZN-SPA-K02
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "N"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       HZN-SPA-K02
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX1 )
               IF      (SPA-NAI-RRKYMD  (IDX)  =   HZN-SPA-NAI-RRKYMD
                                                                 (IDX))
               AND    (SPA-NAI-RENNUM   (IDX)  =   HZN-SPA-NAI-RENNUM
                                                                 (IDX))
               AND    (SPA-NAI-RRK-SRYKA(IDX)  =   HZN-SPA-NAI-RRK-SRYKA
                                                                 (IDX))
               AND    (SPA-NAI-RRK-HKNCOMBI(IDX)
                                           =   HZN-SPA-NAI-RRK-HKNCOMBI
                                                                  (IDX))
                   CONTINUE
               ELSE
                   MOVE    "K178"          TO  SPA-ERRCD
               END-IF
               IF      SPA-SYORIFLG        =   1
                   IF      (SPA-SRYYMD     =   HZN-SPA-NAI-RRKYMD(IDX))
                   AND     (SPA-OLD-SRYKA  =   HZN-SPA-NAI-RRK-SRYKA
                                                                 (IDX))
                   AND     (SPA-OLD-HKNCOMBI
                                           =   HZN-SPA-NAI-RRK-HKNCOMBI
                                                                 (IDX))
                   AND     (SPA-RENNUM     =   HZN-SPA-NAI-RENNUM(IDX))
                   AND     (SPA-DOUJI-RENNUM
                                           =   HZN-SPA-NAI-DOUJI-RENNUM
                                                                 (IDX))
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
           END-PERFORM
      *H24.11
      *    訂正で受診履歴の変更があった時のチェック
           IF      (SPA-SYORIFLG       =   1    )
              AND  (SPA-ERRCD      NOT =   SPACE)
              AND  (FLG-OK             =   1    )
              AND  (SPA-DENPNUM    NOT =   ZERO )
               PERFORM 300411-J02CHK-DENPNUM-SEC
           END-IF
      *
           IF      SPA-GMN-MAX1    NOT =   HZN-SPA-GMN-MAX1
               MOVE    "K178"          TO  SPA-ERRCD
           END-IF
           MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    2                   TO  SPA-NAI-JYURRKFLG
           END-IF
           IF     (SPA-SYORIFLG        =   1  )  AND
                  (FLG-OK              =   ZERO)
               MOVE    "0178"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-JYURRKFLG
           END-IF
      *H26.12
      *    警告をエラーにする
           IF      SPA-NAI-JYURRKFLG   =   2
               MOVE    "0178"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-JYURRKFLG
           END-IF
           .
       30041-JYURRK-J02CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正受診履歴変更チェック処理
      *****************************************************************
       300411-J02CHK-DENPNUM-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK-ERR
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX1 )
                          OR  (FLG-CHK-ERR     =   1    )
               IF      SPA-DENPNUM         =   SPA-NAI-GRP-DENPNUM (IDX)
                   MOVE    ZERO                TO  FLG-CHKRRK
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >   SPA-GMN-MAX1 )
                                  OR  (FLG-CHKRRK  =   1        )
                       IF      (SPA-DENPNUM
                                           =   HZN-SPA-NAI-GRP-DENPNUM
                                                                 (IDY))
                       AND     (SPA-SRYYMD
                                           =   HZN-SPA-NAI-RRKYMD(IDY))
                       AND     (SPA-NAI-RENNUM   (IDX)
                                           =   HZN-SPA-NAI-RENNUM(IDY))
                       AND     (SPA-NAI-RRK-HKNCOMBI(IDX)
                                           =   HZN-SPA-NAI-RRK-HKNCOMBI
                                                                (IDY))
                       AND     (SPA-NAI-DOUJI-RENNUM (IDX)
                                           =   HZN-SPA-NAI-DOUJI-RENNUM
                                                                 (IDY))
                       AND    (SPA-NAI-RRK-SRYKA(IDX)
                                           =   HZN-SPA-NAI-RRK-SRYKA
                                                                 (IDY))
                           MOVE    1               TO  FLG-CHKRRK
                       END-IF
                   END-PERFORM
      *            今回受診履歴なし
                   IF      FLG-CHKRRK          =   ZERO
                       MOVE    1               TO  FLG-CHK-ERR
                   END-IF
               END-IF
           END-PERFORM
           IF      FLG-CHK-ERR         =   1
               MOVE    ZERO            TO  FLG-OK
           END-IF
      *
           .
       300411-J02CHK-DENPNUM-EXT.
           EXIT.
      *!!!!
      *R02.1
      *****************************************************************
      *    患者登録変更チェック処理
      *****************************************************************
       30044-PTINF-P02CHEK-SEC         SECTION.
      *
           MOVE    SPA-K02             TO  HZN-SPA-K02
           MOVE    SPA-K01KYOUTU       TO  HZN-SPA-K01KYOUTU
           MOVE    SPA-AREA            TO  WRK-SPA-AREA
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       WRK-SPA-AREA
                                       HZN-SPA-K01KYOUTU
                                       HZN-SPA-K02
      *
           MOVE    ZERO                TO  FLG-CHK
           IF     (SPA-NAME        NOT =   WRK-SPA-NAME    )
              OR  (SPA-KANANAME    NOT =   WRK-SPA-KANANAME)
              OR  (SPA-BIRTHDAY    NOT =   WRK-SPA-BIRTHDAY)
              OR  (SPA-SEX         NOT =   WRK-SPA-SEX     )
               MOVE    1                   TO  FLG-CHK
           END-IF
      *    保険組合せ
           MOVE    ZERO                TO  FLG-OK
           IF     (HZN-SPA-HKN-MAX NOT =   SPA-HKN-MAX)
               MOVE    1               TO  FLG-OK
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   HZN-SPA-HKN-MAX )
                         OR   (FLG-CHK     =    1   )
                         OR   (FLG-OK      =    1   )
               IF      SPA-GMN-THKNCOMBINUM (IDX)
                                       NOT =   HZN-SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    1                 TO  FLG-OK
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-NAI-PTINFFLG
           IF     (FLG-CHK             =   1    )
              OR  (FLG-OK              =   1    )
      *        患者情報変更あり
               MOVE    "0179"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-NAI-PTINFFLG
           END-IF
           INITIALIZE                      HZN-SPA-K02
           INITIALIZE                      HZN-SPA-K01KYOUTU
           INITIALIZE                      WRK-SPA-AREA
           .
       30044-PTINF-P02CHEK-EXT.
           EXIT.
      *!!!
      *
      *****************************************************************
      *    診療行為再編集処理（患者登録遷移後）
      *****************************************************************
       30011-INIT-HEN-SEC              SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
           MOVE    SPA-GMN-HKNCOMBINUM TO  WRK-MAE-HKNCOMBI
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
      *H28.10      患者登録の診療科が同じとき、ドクターの変更
           IF      SPA-GMN-SRYKA       =   SPA-SRYKA
               MOVE    SPA-DRCD            TO  WRK-DRCD
           ELSE
               MOVE    "1"                 TO  WRK-DRCD1
               MOVE    SPA-GMN-DRCD        TO  WRK-DRCD2
           END-IF
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      **** MOVE    SPA-DRCD            TO  WRK-DRCD
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
      *
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *H26.10
           MOVE    SPA-BIRTHDAY        TO  WRK-MAE-BIRTHDAY
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
           MOVE    WRK-DRCD            TO  SPA-DRCD
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-GMN-HKNCOMBINUM
           MOVE    WRK-MAE-HKNCOMBI    TO  SPA-HKNCOMBINUM
      *H28.10
           IF      WRK-DRCD2       NOT =   SPA-GMN-DRCD
      *        ドクター編集
               MOVE    WRK-DRCD            TO  WRK-HZN-DRCD
               PERFORM 30091-UDR-SETSYORI-SEC
           END-IF
      *
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
      *!!
      *    受診履歴が９まで登録済みの時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               PERFORM 41022-JYURRK-NUM-CHK-SEC
           END-IF
      *!!
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      30011-INIT-HEN-EXT
           END-IF
      *
      *****診察料のみ変換する
      *H26.10
      *    診療算定回数編集
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "3"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    年齢チェックの為、前項目の再チェック
           IF      SPA-BIRTHDAY    NOT =   WRK-MAE-BIRTHDAY
               PERFORM VARYING IDX     FROM    1   BY  1
                       UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   MOVE    SPACE               TO  SPA-COM-CHKFLG
                                                           (IDX)
               END-PERFORM
           END-IF
      *!!!
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
           .
       30011-INIT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理
      *****************************************************************
       3009-SAIHEN-SEC              SECTION.
      *
      *    患者番号変更、Ｋ０１、Ｐ０２よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *    患者情報通知判定
           IF      SPA-PTID            =   SPA-GMN-PTID
               MOVE    ZERO            TO  FLG-POPINI
           ELSE
               MOVE    1               TO  FLG-POPINI
           END-IF
      *
      *    初期ＳＰＡ編集処理
           IF     (SPA-MOTOPG          =   "P02"  OR  "U02" 
                                                  OR  "K02N" )  AND
                  (SPA-PTID        NOT =   SPA-GMN-PTID )
      ***      IF      SPA-MOTOPG          =   "P02"  OR  "U02"
      *            MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
      *            MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *        END-IF
               IF      SPA-SRYYMD          =   SPACE
                   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
                   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               END-IF
      *        患者変更時は、診療日＝システム日付とする
               IF      SPA-MOTOPG          =   "P02"  OR  "U02"
                   MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
                   MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               END-IF
      *
               MOVE    1                   TO  FLG-SYOKAI
      *
               IF      SPA-MOTOPG          =   "K01"
                   MOVE    SPA-DRCD            TO  WRK-DRCD
               END-IF
      *        初期ＳＰＡ編集処理
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "1"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
               IF      SPA-MOTOPG          =   "K01"
                   MOVE    WRK-DRCD            TO  SPA-DRCD
               END-IF
           ELSE
               IF      SPA-MOTOPG          =   "K01"
                   MOVE    SPA-DRCD            TO  WRK-DRCD
                   MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
                   MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
                   MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
                   MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
               ELSE
                   IF     (SPA-PTID            =   SPA-GMN-PTID )  AND
                          (SPA-NAI-SRYYMD  NOT =   SPA-SRYYMD   )
                       MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
                       MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
                   END-IF
               END-IF
           END-IF
           IF      SPA-PTID            =   ZERO
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *
           INITIALIZE                  ORCSC95AREA
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           IF     (SPA-MOTOPG          =   "K01"        )
               MOVE    "K01"               TO  LNK-SC95-PG
           ELSE
               IF      SPA-MOTOPG          =   "U02"
                   MOVE    "U02"               TO  LNK-SC95-PG
               ELSE
                   MOVE    "K02"               TO  LNK-SC95-PG
                   MOVE    SPACE               TO  SPA-SRYKA
               END-IF
           END-IF
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    LNK-SC95-LAST-RENNUM    TO  WRK-SC95-LAST-RENNUM
      *H27.9
           IF      SPA-ERRCD           =   "1999"
                                       OR  "9999"
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
      *        中途終了エラー
               MOVE    "1999"          TO  SPA-ERRCD
               MOVE    1               TO  FLG-END2
               MOVE    90              TO  SPA-GMN-CUR
               GO      TO      3009-SAIHEN-EXT
            END-IF
      *
      *    メモ登録有無
           PERFORM 41021-PTMEMOINF-SEC
      *    排他制御処理
           PERFORM 990-LOCK-IN2-SEC
           IF      SPA-ERRCD           =   "9999"
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *ver.4.8
           IF      FLG-POPINI          =   1
      *        患者情報通知
               PERFORM 41023-POPUP-SYORI-SEC
           END-IF
      *
           IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
               PERFORM                 310-SPA-SET-SEC
               MOVE   "1006"           TO  SPA-ERRCD 
               MOVE    3               TO  SPA-HKN-ERR
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    00              TO  SPA-GMN-CUR
               GO      TO      3009-SAIHEN-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *!!
      *    受診履歴が９まで登録済みの時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               PERFORM 41022-JYURRK-NUM-CHK-SEC
           END-IF
      *    受診履歴が３で、通常の時、警告
      *ver.4.8 から解除
      *    IF     (SPA-ERRCD           =   SPACE )
      *       AND (SPA-KIDCD           =   SPACE )
      *        IF     (SPA-RRK-LSTYMD       =   SPA-SRYYMD )  AND
      *               (WRK-SC95-LAST-RENNUM    >=  3      )
      *            MOVE    "0106"          TO  SPA-KIDCD
      *        END-IF
      ***  END-IF
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
           MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *
      *
           .
       3009-SAIHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為再編集処理（Ｋ１０より）
      *****************************************************************
       30091-SAIHEN2-SEC              SECTION.
      *
      *    中途終了分よりの時　初画面を編集
           INITIALIZE                  SPA-SCR-REC
      *
      *    ワーク診療行為解除
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    患者情報通知判定
           IF      SPA-PTID            =   SPA-GMN-PTID
               MOVE    ZERO            TO  FLG-POPINI
           ELSE
               MOVE    1               TO  FLG-POPINI
           END-IF
      *    初期ＳＰＡ編集処理
           IF     (SPA-SRYYMD      NOT =   SPA-GMN-SRYYMD )  OR
                  (SPA-PTID        NOT =   SPA-GMN-PTID   )
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "1"                 TO  LNK-SC95-KBN
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    "2"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           ELSE
      *        中途終了分のみ表示
               INITIALIZE                  ORCSC95AREA
               MOVE    "3"                 TO  LNK-SC95-KBN
               MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
               MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
               MOVE    "K10"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           END-IF
           IF      SPA-ERRCD           =   "1999"
               MOVE    1                   TO  FLG-END2
           END-IF
      *
      *!   IF     SPA-HKNCOMBINUM      =   SPACE  OR  ZERO
      *        スパ共通編集処理
      *        PERFORM                 310-SPA-SET-SEC
      *        MOVE    "1006"          TO  SPA-ERRCD 
      *        MOVE    3               TO  SPA-HKN-ERR
      *        MOVE    1               TO  SPA-GMN-PAGE
      *        MOVE    00              TO  SPA-GMN-CUR
      *        GO      TO      30091-SAIHEN2-EXT
      *!   END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM                 3002-PARA-SET-SEC
      *
      *    チェック用月内診療行為セット
           PERFORM                 320-SPA-CHKSET-SEC
      *
      *    スパ共通編集処理
           PERFORM                 310-SPA-SET-SEC
      *!!
           IF      SPA-KIDCD           =   "0121"
      *        入院中自費保険確認
               IF      SPA-HKNNUM (1:2)    =   "98"
      *H31.2
                                           OR  SPACE
                   MOVE    SPACE               TO  SPA-KIDCD
      *            入院中自費保険へ
                   MOVE    1                   TO  SPA-NAI-NYUIN
                   MOVE    ZERO                TO  FLG-NYUIN
      *            入院日編集
                   MOVE    "入院日"            TO  SPA-GMN-LASTMID
                   MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
                   PERFORM 520-SEIWA-HEN-SEC
                   MOVE    WRK-HENYMDG         TO  SPA-GMN-LASTYMD-G
               ELSE
                   GO      TO  30091-SAIHEN2-EXT
               END-IF
           END-IF
      *
           IF      FLG-POPINI          =   1
      *        患者情報通知
               PERFORM 41023-POPUP-SYORI-SEC
           END-IF
      *
      *    保険組合せのエラーをクリアする
           IF      SPA-HKN-ERR     NOT =   ZERO
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
           IF      SPA-RJN-ERR     NOT =   ZERO
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
      *    画面用スパ編集処理
           PERFORM                 310-SPA-GMNSET-SEC
      *
           .
       30091-SAIHEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為追加編集処理（Ｋ１０より）
      *****************************************************************
       30092-SAIHEN3-SEC              SECTION.
      *
      *    中途終了分のみ表示
           INITIALIZE                  ORCSC95AREA
           MOVE    "A"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    SPA-K10-SRYKA(SPA-K10-SELNUM)
                                       TO  LNK-SC95-SRYKA
           MOVE    SPA-K10-HKNCOMBI(SPA-K10-SELNUM)
                                       TO  LNK-SC95-HKNCOMBI
           MOVE    "K10"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           IF      SPA-ERRCD           =   "1999"
                                       OR  "9999"
               MOVE    1                   TO  FLG-END2
               GO      TO      30092-SAIHEN3-EXT
           END-IF
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
           MOVE    SPA-GMN-MAX         TO  IDX2
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           MOVE    SPA-GMN-MAX         TO  IDX-STR2
           MOVE    SPACE               TO  WRK-ERRCD1
           MOVE    SPACE               TO  WRK-ERRCD3
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    3               TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
      ***      MOVE    SPACE           TO  FLG-TOROKU
      ***      PERFORM 510-TBLHEN-SEC
           END-IF
      *H27.12
           COMPUTE IDX-STR3            =   IDX-STR2  +   1
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    IDX-STR3   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD(IDX)
                                           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG (IDX)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG2(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG4(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG5(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG6(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG7(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX)
           END-PERFORM
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    1                TO  FLG-INIT
               PERFORM 510-TBLHEN-SEC
           END-IF
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD(IDX)
                                           TO  SPA-MAE-INPUTCD(IDX)
      *H29.10
      *   追加前の警告表示対応
               ELSE
      *            エラーがあれば再チェック
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX)
                   IF     (IDX             <=  IDX-STR2)
                     AND  (SPA-ERRCD(1:1)  =   "K"     )
      *                追加前の警告フラグクリアする
                       MOVE    "C"         TO  SPA-COM-KZMFLG (IDX)
                   END-IF
               END-IF
      *!!!
      *        追加時にエラーがあれば、再読込みを行う
               IF    ((WRK-ERRCD1      NOT =   SPACE )  OR
                      (WRK-ERRCD3      NOT =   SPACE ))
                  AND (IDX                 >   IDX-STR2 )
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
      *        保険組合せエラーの時、保険の行へ
               IF     (IDX                 >   IDX-STR2 )
                   IF     (WRK-ERRCD3          =   "1051"  )  AND
                          (SPA-GMN-INPUTCD(IDX)(1:1)   =   "#" )
                       MOVE    "1"             TO  SPA-COM-CUR (IDX)
                       MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX)
                   END-IF
               END-IF
               IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "P" )
                   MOVE    "1"             TO  SPA-COM-CUR (IDX)
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
           END-PERFORM
      *H27.12
      *    警告フラグクリア
      ***  PERFORM VARYING IDX     FROM    IDX-STR3   BY  1
      *H29.10
           PERFORM VARYING IDX     FROM    1          BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *H29.10
            IF   (IDX              >=  IDX-STR3 )
             OR  (SPA-COM-KZMFLG (IDX) =   "C"  )
               MOVE    SPACE           TO  SPA-COM-KZMFLGAREA (IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG (IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1(IDX)
      *        併用算定が警告の時、クリア
               IF      SPA-COM-HKEIFLG (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-COM-HKEIFLG (IDX)
                   MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
               END-IF
              END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *
           IF      WRK-ERRCD3      NOT =   SPACE
               MOVE    WRK-ERRCD3          TO  SPA-ERRCD
           END-IF
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
      *
           .
       30092-SAIHEN3-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＬＤ外の項目入力編集処理
      *****************************************************************
       3101-P97SPASET-SEC              SECTION.
      *
           MOVE    LNK-KYOUTU          TO  WRK-SPA-KYOUTU
      *    受付、患者検索より
           IF      WRK-SPA-PTNUM       =   SPA-PTNUM
               CONTINUE
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      WRK-SPA-PTNUM       NOT =   SPACE
      *            初期表示へ
                   MOVE    WRK-SPA-PTNUM   TO  K02-PTNUM
      *            受付一覧からの時、診療科を編集する
                   IF      WRK-MOTOPG          =   "U01"
                       MOVE    WRK-SPA-SRYKA       TO  SPA-SRYKA
                       MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
                   END-IF
                   PERFORM 41010-PTNUM-SYORI-SEC
      *!!          ＤＢエラー
                   IF     (FLG-END2        =   1      )
                      OR  (SPA-ERRCD       =   "1999" )
                       MOVE    "E"             TO  SPA-K01-YOBI(1:1)
                   END-IF
      *!!
                   MOVE    SPACE           TO  SPA-ERRCD
                   MOVE    ZERO            TO  SPA-NAI-ROUSTR
      *            システム管理よりカーソル指定
                   EVALUATE    SPA-GAI-CURSOR-SET
                       WHEN    "1"
      *                保険組合せ
                           MOVE    05              TO  SPA-GMN-CUR
                       WHEN    "2"
                       WHEN    "3"
      *                診療科
      *                受付一覧からの時
                           IF      WRK-MOTOPG          =   "U01"
                               MOVE    01              TO  SPA-GMN-CUR
                           ELSE
                               MOVE    06              TO  SPA-GMN-CUR
                           END-IF
                       WHEN    OTHER
                           MOVE    01              TO  SPA-GMN-CUR
                   END-EVALUATE
                   IF      LNK-WKSRYACT-MAX        >   ZERO
                       MOVE    01              TO  SPA-GMN-CUR
                   END-IF
      *            入院中の時、カーソルを患者番号へ
                   IF      SPA-KIDCD           =   "0121"
                        MOVE    ZERO           TO  SPA-GMN-CUR
                   END-IF
      *!!
                   IF     (FLG-END2        =   1      )
                       MOVE    90              TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
      *
           IF     (SPA-PTID            =   ZERO  )  OR
                  (SPA-ERRCD       NOT =   SPACE )
               MOVE    ZERO                TO  SPA-GMN-CUR
           END-IF
           .
       3101-P97SPASET-EXT.
           EXIT.
      *
      *****************************************************************
      *    一覧画面（Ｋ９８）ＯＫ処理
      *****************************************************************
       3011-K98-SET-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-IN-MAP-IDX
           MOVE    SPA-GMN-IDX         TO  WRK-STR
      *
           PERFORM VARYING     IDX-K98 FROM    1   BY  1
                   UNTIL      (IDX-K98             >   10  )  OR
                              (SPA-K98-SRYCD(IDX-K98)  =   SPACE )  OR
                              (SPA-ERRCD           NOT =   SPACE )
      *
               IF      IDX-K98             =   1
                   INITIALIZE                  SPA-COM-TBLREC
                                                       (SPA-GMN-IDX)
                   INITIALIZE                  SPA-GMN-TBLREC
                                                       (SPA-GMN-IDX)
                   MOVE    SPA-K98-SRYCD(IDX-K98)
                                           TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
      *!!!!!!!!
      *            挿入行へ移動
                   IF     SPA-GMN-INPUTCD (SPA-GMN-IDX + 1)
                                               NOT =   SPACE
                       MOVE    "1"             TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
                   END-IF
      *!!!!!!!!
               ELSE
                   ADD     1               TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
                   IF      SPA-ERRCD       =   SPACE
                       MOVE    SPA-K98-SRYCD(IDX-K98)
                                               TO  SPA-GMN-INPUTCD
                                                       (SPA-GMN-IDX)
      *!!!!!!!!
      *            挿入行へ移動
                       IF     SPA-GMN-INPUTCD (SPA-GMN-IDX + 1)
                                               NOT =   SPACE
                           MOVE    "1"             TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
                       END-IF
      *!!!!!!!!
                   END-IF
               END-IF
      *
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "1"             TO  SPA-COM-IN (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-IN2(SPA-GMN-IDX)
               END-IF
      *
            END-PERFORM
      *
      *    入力コード・名称チェック処理
           PERFORM 410112-KOUI-SPACHK-SEC
      *
           .
       3011-K98-SET-EXT.
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
           IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
               PERFORM 41014-GYOINSN-SEC
           END-IF
           MOVE    SPA-GMN-IDX         TO  IDX-S
           IF      SPA-K98-SRYCD-G   NOT =   SPACE
               PERFORM 3011-K98-SET-SEC
           ELSE
      *    コメントコード追加
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
      *
      *H30.9
      *****************************************************************
      *    選択式コメントへ処理
      *****************************************************************
       30113-SELCOMMNT-COMMENT-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-AUTOERR
           MOVE    ZERO                TO  IDX-SELCOM
      *
           IF     (SPA-SELCOM-IDX      >   ZERO  )
             AND  (SPA-COM-INPUTCD(SPA-SELCOM-IDX)(1:1)  =   "1"  )
             AND  (SPA-COM-RECEKISAI (SPA-SELCOM-IDX)    =   "1"
                                                         OR  "2"  )
      *        選択済み
               MOVE    "1"             TO  SPA-COM-RECEAUTO
                                                    (SPA-SELCOM-IDX)
      *!
      *        回数入力の下に追加
               IF     (SPA-COM-KAIFLG (SPA-SELCOM-IDX)  =   "1"  )
                 AND  (SPA-COM-ZAIEND (SPA-SELCOM-IDX)  =   "1"  )
                 AND  (SPA-K98-SRYCD-G     NOT =   SPACE  )
                   MOVE    1                   TO  FLG-AUTOERR
                   MOVE    SPA-SELCOM-IDX      TO  IDX-SELCOM
               END-IF
           END-IF 
      *
      *    対象行の下に挿入
           IF      SPA-COM-INPUTCD (SPA-GMN-IDX) NOT =  SPACE
               ADD     1                   TO  SPA-GMN-IDX
               IF      SPA-GMN-IDX         <=  SPA-GMN-MAX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-SELCOM-FLG
           MOVE    ZERO                TO  SPA-SELCOM-IDX
      *
           PERFORM 3011-K98-SET-SEC
      *
      *    コメント挿入行前で剤終了は警告
           IF      FLG-AUTOERR         =   1
               MOVE    "1"              TO  SPA-COM-CUR
                                                    (IDX-SELCOM)
               MOVE    "K117"           TO  SPA-ERRCD
           END-IF
      *
           MOVE    1                   TO  FLG-GMN
           .
       30113-SELCOMMNT-COMMENT-EXT.
           EXIT.
      *
      *H28.1
      *****************************************************************
      *    医師選択より処理
      *****************************************************************
       30091-UDR-SETSYORI-SEC              SECTION.
      *
      *    ドクターを再編集する
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "1"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    WRK-HZN-DRCD(2:4)   TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           IF      ORCSDR-DRCD-IDX     >   ZERO
               MOVE    SPA-GMN-DRCDLST  (ORCSDR-DRCD-IDX)
                                           TO  SPA-GMN-DRCD-G
           ELSE
               PERFORM VARYING   IDX       FROM    1   BY  1
                   UNTIL     IDX           >   SPA-DRCD-MAX 
                   IF      WRK-HZN-DRCD(2:4)   =   SPA-GMN-DRCDLST(IDX)
                                                                 (1:4)
                       MOVE    SPA-GMN-DRCDLST(IDX)
                                               TO  SPA-GMN-DRCD-G
                       MOVE    SPA-DRCD-MAX    TO  IDX
                   END-IF
               END-PERFORM
           END-IF
           MOVE    SPA-GMN-DRCD-G      TO  SPA-DRCD-NAME
           MOVE    "1"                 TO  WRK-DRCD1
           MOVE    SPA-GMN-DRCD        TO  WRK-DRCD2
           MOVE    WRK-DRCD            TO  SPA-DRCD
      *
           .
       30091-UDR-SETSYORI-EXT.
           EXIT.
      *H28.1
      *@@
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    ZERO                TO  SPA-SYOSIN
                   PERFORM 30019-DOUSAISIN-SEC
      *        同日再診と訂正選択（ＯＫが同日）
               WHEN    "0100"
                   MOVE    SPACE               TO  SPA-KIDCD
      *******      MOVE    ZERO                TO  SPA-SYOSIN
                   IF      SPA-KID1-FLG        =   "OK"
      *                同日再診
                       PERFORM 300100-DOUSAISIN-SEC
                   ELSE
      *                訂正
                       MOVE    "T"             TO  SPA-GMN-TEISEI
                       MOVE    1               TO  SPA-NAI-SYORI
                       MOVE    "01"            TO  K02-DOSELNUM
                       PERFORM 41014-DOSELNUM-SEC
                   END-IF
      *
      *        同日再診と訂正選択（ＯＫが訂正）
               WHEN    "0131"
                   MOVE    SPACE               TO  SPA-KIDCD
      *******      MOVE    ZERO                TO  SPA-SYOSIN
                   IF      SPA-KID1-FLG        =   "OK"
      *                訂正
                       MOVE    "T"             TO  SPA-GMN-TEISEI
                       MOVE    1               TO  SPA-NAI-SYORI
                       MOVE    "01"            TO  K02-DOSELNUM
                       PERFORM 41014-DOSELNUM-SEC
                   ELSE
      *                同日再診
                       PERFORM 300100-DOUSAISIN-SEC
                   END-IF
      *
               WHEN    "0102"
                   MOVE    SPACE               TO  SPA-KIDCD
      *            外来加算料削除
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPA-CHK-IDX     TO  SPA-GMN-IDX
      *                診療料をクリアする
                       MOVE    SPACE           TO  SPA-GMN-INPUTCD
                                                        (SPA-GMN-IDX)
      *
      *                診療行為、計計算処理、編集
                       MOVE    "1"             TO  FLG-TOROKU
                       PERFORM 510-TBLHEN-SEC
                       MOVE    SPACE           TO  FLG-TOROKU
      *
                       PERFORM 500-GMNHEN-SEC
      *
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       PERFORM 4100-TOROKU-SEC
      *
                   END-IF
      *
               WHEN    "0103"
      *************IF      SPA-KID1-FLG        =   "OK"
      *                画面制御
                       MOVE    "JOIN"          TO  WRK-PUTTYPE
                       MOVE    1               TO  SPA-CONTFLG
                       MOVE    SPACE               TO  SPA-KIDCD
      *                Ｋ０１画面へ
                       PERFORM 4001-K01-SYORI-SEC
      *************END-IF
      *
               WHEN    "0104"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                終了処理へ
                       PERFORM 210-BACK-OK-SEC
                   END-IF
      *        保険組合せ変更確認
               WHEN    "0105"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    ZERO            TO  FLG-CHK2
                       PERFORM 41012-HKNCOMBI-CHANGE-SEC
                   ELSE
                       MOVE    SPA-MAE-HKNCOMBI-G
                                           TO  SPA-GMN-HKNCOMBI-G
                       PERFORM 3101-HKNCOMBI-HEN-SEC
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0107"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   END-IF
      *H25.7
                   IF     (SPA-GAI-CURSOR-SET  =   "3"   )
                    AND   (SPA-ERRCD           =   SPACE )
      *                ドクターへ
                       MOVE    10              TO  SPA-GMN-CUR
                   END-IF
      *!!
               WHEN    "0138"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 41013-SRYKA-CHANGE02-SEC
      *H25.7
                   IF     (SPA-GAI-CURSOR-SET  =   "3"   )
                    AND   (SPA-ERRCD           =   SPACE )
      *                ドクターへ
                       MOVE    10              TO  SPA-GMN-CUR
                   END-IF
      *!!
      *
      *        同日再診３回以上の確認
               WHEN    "0106"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE           TO  SPA-KID1-FLG
                       MOVE    SPACE           TO  SPA-KIDCD
      *                パラメタファイルより表示
                       MOVE    3               TO  FLG-PARASET
                       PERFORM                 3002-PARA-SET-SEC
      *                画面用スパ編集処理
                       PERFORM                 310-SPA-GMNSET-SEC
                       MOVE    1               TO  SPA-GMN-PAGE
                       MOVE    1               TO  SPA-GMN-CUR
                       MOVE    1               TO  SPA-MAP-IDX
                   ELSE
                       MOVE    ZERO                TO  SPA-GMN-CUR
                   END-IF
      *
      *        初診料自動履歴算定の確認
               WHEN    "0108"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                パラメタファイルより表示
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM                 300108-SAISET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *???
      *        初回算定日の確認
               WHEN    "0140"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 3001099-SYOKAISET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *???
      *        同日再診料算定の確認
               WHEN    "0109"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 30019-DOUSAISIN-SEC
      *H25.7
                   IF     (SPA-GAI-CURSOR-SET  =   "3"   )
                    AND   (SPA-ERRCD           =   SPACE )
      *                ドクターへ
                       MOVE    10              TO  SPA-GMN-CUR
                   END-IF
      *
      *        再診を初診料へ自動算定の確認
               WHEN    "0110"
      *        再診を初診料へ自動算定の確認（警告表示）
               WHEN    "0112"
      *        初診料の再算定
               WHEN    "0132"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
      *                初診料算定へ
                       PERFORM                 300109-SYOSINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        初診料を再診料へ自動算定の確認
               WHEN    "0117"
               WHEN    "0137"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 300110-SAISINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *H26.4
      *        小児科外来診療料を初診料へ自動算定の確認
               WHEN    "0133"
               WHEN    "0134"
      *                初診料算定へ
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    9               TO  SPA-SYONIKA-ARI
                       MOVE    9               TO  SPA-SYONIKKR-ARI
                       PERFORM 300109-SYOSINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
      *        小児科外来診療料を再診料へ自動算定の確認
               WHEN    "0135"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    9               TO  SPA-SYONIKA-ARI
                       MOVE    9               TO  SPA-SYONIKKR-ARI
                       PERFORM 300110-SAISINSET-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
      *
      *        診療日が未来日の確認
               WHEN    "0111"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                診療日確定後、編集
                       PERFORM 410171-SRYYMD-OK-SEC
                   ELSE
                       MOVE    7               TO  SPA-GMN-CUR
                   END-IF
      *
      *        特定疾患処方管理加算の算定確認
               WHEN    "0113"
               WHEN    "0213"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TOKUTEIFLG
                   ELSE
                       MOVE    1                   TO  SPA-TOKUTEIFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *        長期投与加算の算定確認
               WHEN    "0130"
               WHEN    "0230"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-CHOKITOFLG
                   ELSE
                       MOVE    1                   TO  SPA-CHOKITOFLG
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *!!
      *平成２２年４月から
      *        手帳加算の算定確認
               WHEN    "2003"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    2                   TO  SPA-TECHOKBN-CHK
                   ELSE
                       MOVE    1                   TO  SPA-TECHOKBN-CHK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *!!
      *!!
      *
      *        再診の振替え
               WHEN    "0114"
               WHEN    "0115"
               WHEN    "0120"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 3001010-SAISIN-HEN-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *            再診の振替えチェック済み
                   MOVE    1               TO  SPA-SAISIN-CHK
                   MOVE    SPACE           TO  SPA-KIDCD
                   MOVE    SPACE           TO  SPA-KID1-FLG
      *
      *        クリアの確認
               WHEN    "0116"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 220-CLEAR-SYORI-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *        中途終了データのクリア
               WHEN    "0127"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                ワーク診療行為削除
                       PERFORM 2202-CLEAR-WKSRYACT-SEC
                   ELSE
      *                ワーク診療行為解除
                       MOVE    ZERO            TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
      *
      *        保険変更時、中途終了あり
               WHEN    "0118"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                内容置換え
                       PERFORM 410132-SAIHENSYU-SEC
                   ELSE
      *                診察料の変更のみ
                       PERFORM 410131-SINSATUHEN-SEC
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   END-IF
      *
      *        科変更時、中途終了あり
               WHEN    "0122"
               WHEN    "0124"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                内容置換え
                       PERFORM 410132-SAIHENSYU-SEC
                   ELSE
      *                診察料の変更のみ
                       MOVE    2                  TO  SPA-TAHOKEN-FLG
                       PERFORM 410131-SINSATUHEN-SEC
      *                ワーク診療行為クリア
                       MOVE    2                   TO  FLG-MOD-FLG
                       PERFORM 4900-WKSRYACT-CLEAR-SEC
                   END-IF
      *
      *        診療科変更確認
               WHEN    "0123"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 41013-SRYKA-CHANGE-SEC
                   ELSE
                       MOVE    1                   TO  SPA-GMN-CUR
                       MOVE    SPA-MAE-SRYKA-G     TO  SPA-GMN-SRYKA-G
                   END-IF
      *H25.7
                   IF     (SPA-GAI-CURSOR-SET  =   "3"   )
                    AND   (SPA-ERRCD           =   SPACE )
      *                ドクターへ
                       MOVE    10              TO  SPA-GMN-CUR
                   END-IF
      *
      *        他保険算定あり
               WHEN    "0119"
                   MOVE    SPACE               TO  SPA-KIDCD
                   PERFORM 300110-TAHOKEN-SYORI-SEC
      *
      *        入院画面へ
               WHEN    "0121"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    ZERO                TO  SPA-GMN-CUR
                       MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
                       MOVE    SPA-GMN-PTID        TO  SPA-PTID
                       MOVE    "JOIN"              TO  WRK-PUTTYPE
                       PERFORM 450-K02N-SYORI-SEC
                   ELSE
      *                入院中自費保険確認
                       PERFORM 30018-NYUINJIHI-CHK-SEC
                   END-IF
               WHEN    "0128"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPA-GMN-PTNUM        TO  SPA-PTNUM
                       MOVE    SPA-GMN-PTID         TO  SPA-PTID
                       MOVE    "JOIN"               TO  WRK-PUTTYPE
                       PERFORM 450-K02N-SYORI-SEC
                   ELSE
      *                入院日を診療日にして登録する
                       PERFORM 30017-NYUINHEN-SEC
                   END-IF
      *        入院中自費保険入力
               WHEN    "0129"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
      *                入院中自費保険へ
                       PERFORM 30019-NYUINJIHI-SEC
                   ELSE
      *                入院日を診療日にして登録する
                       PERFORM 30017-NYUINHEN-SEC
                   END-IF
      *
      *        同一検査削除へ
               WHEN    "0160"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 30016-KENSA-DELETE-SEC
                   END-IF
      *        リハビリ逓減確認
               WHEN    "0180"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                        MOVE    1                  TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   ELSE
                        MOVE    ZERO               TO  FLG-RIHTEI
      *                 診療行為、計計算処理、編集
                        MOVE    SPACE              TO  FLG-TOROKU
                        PERFORM 510-TBLHEN-SEC
                        MOVE    SPACE              TO  FLG-TOROKU
                   END-IF
      *
      *        包括分入力変更確認
               WHEN    "0190"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      INITIALIZE                  SPA-SCR-REC
                   END-IF
      *
      *        内服７種類以上逓減確認
               WHEN    "2001"
               WHEN    "2002"
               WHEN    "2004"
               WHEN    "2005"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-NAIFUKU-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-NAIFUKU-OK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *H26.10
      *        向精神薬剤多剤投与
               WHEN    "2006"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-PSYCHO-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-PSYCHO-OK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *H27.4
      *        低紹介率３０日以上投与
               WHEN    "2007"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                      MOVE    1                TO  SPA-TEIGEN30-OK
                   ELSE
                      MOVE    ZERO             TO  SPA-TEIGEN30-OK
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *            画面制御
                   MOVE    "JOIN"              TO  WRK-PUTTYPE
                   PERFORM 41002-KOUSIN-SYORI-SEC
      *
      *        クリア確認チェック
               WHEN    "0125"
                   IF      SPA-KID1-FLG        =   "OK"
                       PERFORM 2221-PTNUMCLEAR-SYORI-SEC
                   ELSE
                        MOVE    1               TO  SPA-GMN-CUR
                   END-IF
                   MOVE    SPACE               TO  SPA-KIDCD
                   MOVE    SPACE               TO  SPA-KID1-FLG
      *@
      *        包括エラーの削除
               WHEN    "0150"
                   MOVE    SPACE               TO  SPA-KIDCD
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    SPACE               TO  SPA-KIDCD
                       PERFORM 51091-HKTSANTEI-DEL-SEC
                   ELSE
                       MOVE    1               TO  SPA-GMN-CUR
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID1-FLG
      *****MOVE    SPACE               TO  SPA-KIDCD
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＩＤ２）ＯＫ処理
      *****************************************************************
       30011-KID2-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD2
      *        中途終了時の印刷確認
               WHEN    "0117"
                   IF      SPA-KID2-FLG        =   SPACE
                       MOVE    1               TO  SPA-GMN-CUR
                   ELSE
                       PERFORM 4510-SNRYO-NYU-SEC
                   END-IF
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KID2-FLG
           MOVE    SPACE               TO  SPA-KIDCD2
           .
       30011-KID2-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料の履歴算定、再診料の再変更処理
      *****************************************************************
       300108-SAISET-SEC     SECTION.
      *
      *    初診算定なし
           MOVE    ZERO                TO  SPA-SYOSIN
           MOVE    SPA-SYOSIN-YMD      TO  SPA-SYOYMD
      *    初診算定日画面編集
           PERFORM 3001081-SYOSIN-HEN-SEC
      *
      *    画面用スパ編集処理
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  SPA-KIDCD
           PERFORM                     310-SPA-GMNSET-SEC
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
           .
       300108-SAISET-EXT.
           EXIT.
      *??
      *****************************************************************
      *    初診算定日画面編集処理
      *****************************************************************
       3001081-SYOSIN-HEN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-GMN-SYOSINYMD
           MOVE    SPACE               TO  WRK-SYOSINYMD-G
           MOVE    SPA-SYOYMD          TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-SYOSINYMD
      *    同時初診
           IF     (SPA-DOUYMD      NOT =   SPACE )
             AND  (SPA-DOUYMD          >=  SPA-SYOYMD)
               MOVE    WRK-HENYMDG         TO  WRK-SYOSINYMD-G
      *
               MOVE    SPA-DOUYMD          TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    SPACE               TO  SPA-GMN-SYOSINYMD
               STRING  WRK-SYOSINYMD-G
                       " (" 
                       WRK-HENYMDG
                       ")"                 DELIMITED  BY  SIZE
                                       INTO    SPA-GMN-SYOSINYMD
               END-STRING
           END-IF
           .
       3001081-SYOSIN-HEN-EXT.
           EXIT.
      *??
      *****************************************************************
      *    初診料の履歴算定、再診料の再変更処理
      *****************************************************************
       3001099-SYOKAISET-SEC     SECTION.
      *
      *!!!
      *    受診履歴の登録されいる診療科で作成する
           MOVE    SPACE               TO  WRK-SRYKA
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL    (IDX         >   SPA-GMN-MAX1)
                          OR  (SPA-NAI-RRK-SRYKA (IDX)(1:1)  =   "A")
               IF      SPA-NAI-RRK-SRYKA (IDX) =   SPA-SRYKA
                   MOVE    SPA-SRYKA       TO  WRK-SRYKA
                   MOVE    SPA-GMN-MAX1    TO  IDX
               ELSE
                   IF      WRK-SRYKA       =   SPACE
                       MOVE    SPA-NAI-RRK-SRYKA (IDX)
                                           TO   WRK-SRYKA
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-SRYKA           =   SPACE
               MOVE    SPA-SRYKA       TO  WRK-SRYKA
           END-IF
      *
      *    算定履歴作成
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    SPA-SYOKAI-SRYCD    TO  SSANTEI-SRYCD
           MOVE    SPA-SYOKAI-YMD      TO  SSANTEI-SRYYMD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           MOVE    1                   TO  SSANTEI-KAISU
           MOVE    WRK-SRYKA           TO  SSANTEI-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  SSANTEI-HKNCOMBINUM
           MOVE    SPA-HKNKBN          TO  SSANTEI-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  SSANTEI-RSI-HKNKBN
           MOVE    SPA-RSI-JUNKYO      TO  SSANTEI-RSI-JUNKYO
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
           IF      SPA-ERRCD           =   "9000"
      *        DB更新エラー
               MOVE    1                   TO  FLG-END2
           END-IF
      *
           IF      SPA-SYOKAI-SRYCD     =   "099110001"
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
               MOVE    WRK-SRYKA           TO  SKARRK-SRYKA
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    1                   TO  FLG-END2
               END-IF
               MOVE    SPA-SYOKAI-YMD      TO  SPA-SYOYMD
               MOVE    SPACE               TO  SPA-SYOSIN-YMD
               MOVE    SPACE               TO  SPA-SYOSIN-SRYCD
               MOVE    ZERO                TO  SPA-SYOSIN
      *        初診算定日
               MOVE    SPA-SYOYMD          TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-SYOSINYMD
      *        画面用スパ編集処理
               INITIALIZE                  SPA-SCR-REC
               MOVE    ZERO                TO  LNK-WKSRYACT-MAX
               MOVE    SPACE               TO  SPA-KIDCD
               MOVE    ZERO                TO  FLG-TEISEI
               PERFORM                     310-SPA-GMNSET-SEC
               MOVE    SPACE               TO  SPA-KID1-FLG
      *
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-MAP-IDX
           ELSE
      *        行削除
               MOVE    SPA-SYOKAI-IDX      TO  SPA-GMN-IDX
               MOVE    SPACE               TO  SPA-GMN-INPUTCD
                                                (SPA-GMN-IDX)
               MOVE    "2"                 TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE               TO  FLG-TOROKU
           END-IF
           MOVE    ZERO                TO  SPA-SYOKAI-IDX
           MOVE    SPACE               TO  SPA-SYOKAI-YMD
           MOVE    SPACE               TO  SPA-SYOKAI-SRYCD
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       3001099-SYOKAISET-EXT.
           EXIT.
      *****************************************************************
      *    同日再診料の設定処理
      *****************************************************************
       30019-DOUSAISIN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  WRK-SRYCD
      *
           IF      SPA-KID1-FLG        =   "OK"
      *
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "2"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    LNK-SC10S-IDX2      TO  IDX2
      *
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30019-DOUSAISIN-EXT.
           EXIT.
      *****************************************************************
      *    同日再診料の設定処理
      *****************************************************************
       300100-DOUSAISIN-SEC     SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  WRK-SRYCD
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "G"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    LNK-SC10S-IDX2      TO  IDX2
      *
           IF     (SPA-ERRCD       NOT =   SPACE ) OR
                  (SPA-KIDCD       NOT =   SPACE )
               CONTINUE
           ELSE
      *
      *       診療行為、計計算処理、編集
               MOVE    "1"             TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE               TO  FLG-TOROKU
           END-IF
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       300100-DOUSAISIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診を初診料へ自動算定より処理
      *****************************************************************
       300109-SYOSINSET-SEC                  SECTION.
      *
      *---- 治癒への変更はやめる  ------------------------------------
      *    患者病名病名を治癒へ
      *    MOVE    SPACE               TO  PTBYOMEI-REC
      *    INITIALIZE                  PTBYOMEI-REC
      *    MOVE    SPA-HOSPNUM          TO  PTBYO-HOSPNUM
      *    MOVE    SPA-PTID            TO  PTBYO-PTID
      *
      *    MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
      *    MOVE    "DBSELECT"          TO  MCP-FUNC
      *    MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *    CALL    "ORCMCPSUB"          USING
      *                                 MCPAREA
      *                                 ORCMCPAREA
      *                                 MCPDATA-REC
      *    IF      MCP-RC              =   ZERO
      *        MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *        PERFORM 980-PTBYOMEI-READ-SEC
      *    ELSE
      *        MOVE    1                   TO  FLG-PTBYOMEI
      *        MOVE    SPACE               TO  PTBYOMEI-REC
      *        INITIALIZE                  PTBYOMEI-REC
      *    END-IF
      *    PERFORM UNTIL       FLG-PTBYOMEI    =   1
      *        IF      PTBYO-SRYYMD        <   SPA-SRYYMD
      *            EVALUATE    PTBYO-TENKIKBN
      *                継続
      *                WHEN    " "
      *                    MOVE    SPA-SRYYMD      TO  PTBYO-TENKIYMD
      *                    MOVE    "1"             TO  PTBYO-TENKIKBN
      *
      *                    MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
      *                    MOVE    "DBUPDATE"          TO  MCP-FUNC
      *                    MOVE    "PTBYOMEI-KEY"      TO  ORC-DBPATH
      *                    CALL    "ORCMCPSUB"         USING
      *                                                MCPAREA
      *                                                ORCMCPAREA
      *                                                MCPDATA-REC
      *            END-EVALUATE
      *        END-IF
      *        MOVE    "PTBYOMEI-KEY3"     TO  ORC-DBPATH
      *        PERFORM 980-PTBYOMEI-READ-SEC
      *    END-PERFORM
      *-----------------------------------------------------------------
      *
      *    初診算定
           MOVE    1                   TO  SPA-SYOSIN
           MOVE    SPACE               TO  SPA-GMN-SYOSINYMD
      *
      *    画面用スパ編集処理
           MOVE    SPACE               TO  SPA-KIDCD
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "S"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    複数科の診察料変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "A"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           .
       300109-SYOSINSET-EXT.
           EXIT.
      *****************************************************************
      *    初診料を再診へ変更処理
      *****************************************************************
       300110-SAISINSET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  SPA-SYOSIN
      *    初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOSIN          =   ZERO  )
      *        初診算定日画面編集
               PERFORM 3001081-SYOSIN-HEN-SEC
           END-IF
      *
      *    画面用スパ編集処理
           MOVE    SPACE               TO  SPA-KIDCD
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "E"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      SPA-KIDCD       NOT =   SPACE
              GO     TO    300110-SAISINSET-EXT
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
           .
       300110-SAISINSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    再診料の振替え（４回目以降）処理
      *****************************************************************
       3001010-SAISIN-HEN-SEC                  SECTION.
      *
      *    再診料の４回目以降の振替え処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "8"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       3001010-SAISIN-HEN-EXT.
           EXIT.
      *****************************************************************
      *    他保険算定あり処理
      *****************************************************************
       300110-TAHOKEN-SYORI-SEC        SECTION.
      *
           IF      SPA-KID1-FLG        =   "OK"
      *        他保険算定済み変更のみ
               MOVE    9                   TO  SPA-TAHOKEN-FLG
               PERFORM 410131-SINSATUHEN-SEC
           ELSE
      *        診察料置換え
               MOVE    8                   TO  SPA-TAHOKEN-FLG
               PERFORM 410131-SINSATUHEN-SEC
           END-IF
      *
           .
       300110-TAHOKEN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同一検査削除処理
      *****************************************************************
       30016-KENSA-DELETE-SEC                  SECTION.
      *
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    ZERO                TO  FLG-KENSADEL
           MOVE    SPACE               TO  FLG-TOROKU
           MOVE    SPACE               TO  SPA-KID1-FLG
           .
       30016-KENSA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院日を診療日に編集処理
      *****************************************************************
       30017-NYUINHEN-SEC                  SECTION.
      *
           MOVE    SPA-K02N-NYUINYMD   TO  SPA-SRYYMD
           MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
           PERFORM 520-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-SRYYMDW
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
      *    診療日確定後、編集
           PERFORM 410171-SRYYMD-OK-SEC
           .
       30017-NYUINHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中自費保険確認処理
      *****************************************************************
       30018-NYUINJIHI-CHK-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF     (SPA-NAI-HKNNUM (IDX)(1:2)   =   "98")
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   1
               MOVE    "0129"              TO  SPA-KIDCD
           ELSE
      *        入院日を診療日にして登録する
               PERFORM 30017-NYUINHEN-SEC
           END-IF
           .
       30018-NYUINJIHI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中自費保険処理
      *****************************************************************
       30019-NYUINJIHI-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF     (SPA-NAI-HKNNUM (IDX)(1:2)   =   "98")
                   MOVE    1                 TO  FLG-CHK
                   MOVE    IDX               TO  IDY
               END-IF
           END-PERFORM
      *
           IF      FLG-CHK             =   1
      *        自費保険へ変更
               MOVE    1                   TO  SPA-NAI-NYUIN
               MOVE    SPA-GMN-THKNCOMBINUM(IDY)
                                           TO  SPA-GMN-HKNCOMBINUM
               PERFORM 3101-HKNCOMBI-HEN-SEC
               MOVE    ZERO                TO  FLG-NYUIN
      *        入院日編集
               MOVE    "入院日"            TO  SPA-GMN-LASTMID
               MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-LASTYMD-G
               MOVE    1                   TO  SPA-GMN-CUR
      *        中途終了データ判定
               PERFORM 410121-HKNCOMBI-SAI-SEC
           ELSE
      *        入院日を診療日にして登録する
               PERFORM 30017-NYUINHEN-SEC
           END-IF
           .
       30019-NYUINJIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択（Ｋ０９）画面より処理
      *****************************************************************
       3013-K09-SET-SEC                  SECTION.
      *
      *    選択がない時、そのまま
           IF      SPA-K09-SELOK       =   ZERO
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      3013-K09-SET-EXT
           END-IF
      *
      *    パラメタファイルより表示
           MOVE    3                   TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    ＤＯ画面（Ｋ０９）より、ＤＯ内容追加編集
           MOVE    SPA-GMN-MAX         TO  IDX2
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
           MOVE    SPA-GMN-MAX         TO  WRK-STR
      *
      *    再表示編集処理
           IF     (SPA-K09-SELOK       =   1    )  AND
                  (LNK-WKSRYACT-MAX    >   ZERO )
               MOVE    ZERO            TO  FLG-SYOSIN
               MOVE    1               TO  FLG-K09TUIKA
               MOVE    SPACE           TO  WRK-ERRCD1
               PERFORM 3103-SAIHEN-HEN-SEC
      *
               IF      WRK-ERRCD1      NOT =   SPACE
                   MOVE    WRK-ERRCD1          TO  SPA-ERRCD
               END-IF
      *H28.4   再チェックを行う
               IF      WRK-STR         =   ZERO
                   MOVE    1               TO  WRK-STR
               ELSE
                   ADD     1               TO  WRK-STR
               END-IF
      *
               PERFORM VARYING IDX     FROM    WRK-STR   BY  1
                       UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                              ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                               (SPA-COM-INPUTCD(IDX)   =   SPACE ))
                   MOVE    SPACE       TO  SPA-MAE-INPUTCD(IDX)
               END-PERFORM
      *
      *        診療行為、計計算処理、編集
               MOVE    ZERO            TO  FLG-INIT
               MOVE    SPACE           TO  FLG-TOROKU
               MOVE    1               TO  FLG-SYOKI
               PERFORM 510-TBLHEN-SEC
               MOVE    SPACE           TO  FLG-TOROKU
           END-IF
      *
           MOVE    1               TO  SPA-GMN-CUR
      *
           .
       3013-K09-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     入力コード設定より編集処理
      *****************************************************************
       30014-K023-SET-SEC                  SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX
               IF     (SPA-GMN-INPUTCD (IDX)(1:1)  =   "." OR "S" )  OR
                      (SPA-COM-INPUTCD (IDX)       =   SPACE     ) 
                   CONTINUE
               ELSE
      *
                   MOVE    SPACE               TO  INPUTCD-REC
                   MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
                   MOVE    SPA-COM-INPUTCD(IDX) 
                                               TO  ICD-SRYCD
      *
                   MOVE    "key4"              TO  WRK-PATH
      *
                   PERFORM 930-INPUTCD-READ-SEC
                   MOVE    SPACE           TO  SPA-COM-GMNFLG (IDX)
                   IF      FLG-INPUTCD         =   ZERO
                       MOVE    ICD-INPUTCD     TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   ELSE
                       MOVE    SPACE           TO  SPA-COM-ICD-INPUTCD
                                                              (IDX)
                   END-IF
                   MOVE    SPACE               TO  WRK-PATH
      *
      *            画面再編集・基本チェック
                   INITIALIZE                  ORCSC94AREA
                   MOVE    "2"                 TO  LNK-SC94-KBN
                   MOVE    "K02"               TO  LNK-SC94-PG
                   MOVE    IDX                 TO  LNK-SC94-IDX
                   CALL    "ORCSC94"           USING
                                               ORCSC94AREA
                                               SPA-SCR-REC
                                               SPA-AREA
               END-IF
           END-PERFORM
      *
           MOVE    1                    TO  SPA-GMN-CUR
           .
       30014-K023-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療科・保険選択画面より編集処理
      *****************************************************************
       3014-K97-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  FLG-FUKU
           MOVE    ZERO                TO  IDX-HZN
           IF      SPA-K97-SYU-DRCD    NOT =   SPACE
                MOVE    SPA-K97-SYU-DRCD       TO  SPA-DRCD
      ********* MOVE    SPA-K97-SYU-DRNAME     TO  SPA-DRCD-NAME
                MOVE    SPA-K97-SYU-DRNAME     TO  SPA-GMN-DRCD-G
           ELSE
               IF     (SPA-GMN-IDX         =   ZERO )  OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                        NOT =   "$"  AND
                                                "#" )
                   IF      SPA-GMN-MAX         <   398
                       MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
                       ADD     1                   TO  SPA-GMN-IDX
                   ELSE
                       MOVE    "7001"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-K97-HKNCOMBI    NOT =   SPACE ) AND
                      (SPA-K97-SRYKA       NOT =   SPACE )
                   PERFORM 30141-SRYKA-HKNCOMBI-INS-SEC
               ELSE
                   PERFORM 30142-KOBETU-INS-SEC
               END-IF
           END-IF
           IF     (FLG-FUKU            =   1 )  AND
                  (FLG-END             =   ZERO)
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "A"                 TO  LNK-SC10S-SYORI
               MOVE    IDX-HZN             TO  LNK-SC10S-STRIDX
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               MOVE    SPACE               TO  SPA-K97-HKNCOMBI
      *
               MOVE    SPACE           TO  FLG-TOROKU
               PERFORM 510-TBLHEN-SEC
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    01                  TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    SPA-GMN-MAX         TO  SPA-GMN-IDX
      *
           .
       3014-K97-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療科・保険入力処理
      *****************************************************************
       30141-SRYKA-HKNCOMBI-INS-SEC                  SECTION.
      *
      *    診療科
           MOVE    SPA-K97-SRYKA       TO  WRK-SRYKA
           MOVE    SPACE               TO  LNK-SC97-INPUTCD
           MOVE    "$"                 TO  LNK-SC97-INPUTCD
           MOVE    WRK-SRYKA           TO  LNK-SC97-INPUTCD(2:2)
           IF      SPA-K97-DRCD        NOT =   SPACE
               MOVE    SPA-K97-DRCD        TO  WRK-DRCD
               MOVE    SPACE               TO  LNK-SC97-INPUTCD (4:1)
               MOVE    WRK-DRCD            TO  LNK-SC97-INPUTCD (5:4)
           END-IF
           MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD
                                                      (SPA-GMN-IDX)
           MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                       (SPA-GMN-IDX)
           MOVE    SPA-GMN-IDX         TO  IDX-KA2
           MOVE    SPA-GMN-IDX         TO  IDX-HZN
           MOVE    1                   TO  FLG-FUKU
           MOVE    SPACE               TO  SPA-K97-SRYKA
           MOVE    SPACE               TO  SPA-K97-DRCD
      *    保険組合せ
      *    科の下へ
           MOVE    SPA-GMN-IDX         TO  IDX-HKN2
           MOVE    IDX-KA2             TO  SPA-GMN-IDX
           ADD     1                   TO  SPA-GMN-IDX
           PERFORM 41014-GYOINSN-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    SPA-K97-HKNCOMBI    TO  WRK-HKNCOMBI
               MOVE    SPACE               TO  LNK-SC97-INPUTCD
               MOVE    "#"                 TO  LNK-SC97-INPUTCD
               MOVE    WRK-HKNCOMBI        TO  LNK-SC97-INPUTCD(2:4)
               MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD
                                                      (SPA-GMN-IDX)
               MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                      (SPA-GMN-IDX)
               MOVE    1                   TO  FLG-FUKU
           END-IF
           .
       30141-SRYKA-HKNCOMBI-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *     単独入力処理
      *****************************************************************
       30142-KOBETU-INS-SEC                  SECTION.
      *
      *    保険組合せ入力
           MOVE    ZERO                TO  IDX-HKN2
           IF      SPA-K97-HKNCOMBI    NOT =   SPACE
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "$") AND
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:2)   NUMERIC)
                   MOVE    SPA-GMN-IDX         TO  IDX-HKN2
                   MOVE    SPACE               TO  SPA-GMN-INPUTCD
                                                    (IDX-HKN2)(4:1)
                   ADD     1                   TO  SPA-GMN-IDX
                   PERFORM 41014-GYOINSN-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-K97-HKNCOMBI    TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  LNK-SC97-INPUTCD
                   MOVE    "#"                 TO  LNK-SC97-INPUTCD
                   MOVE    WRK-HKNCOMBI        TO  LNK-SC97-INPUTCD
                                                                   (2:4)
                   MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                      (SPA-GMN-IDX)
                   MOVE    1                   TO  FLG-FUKU
                   MOVE    SPACE               TO  SPA-K97-HKNCOMBI
                   MOVE    SPA-GMN-IDX         TO  IDX-HZN
               END-IF
           END-IF
      *
      *    診療科、ドクター入力
           MOVE    SPACE               TO  LNK-SC97-INPUTCD
           IF      SPA-K97-SRYKA       NOT =   SPACE
               IF      IDX-HKN2            >   ZERO
                   MOVE    IDX-HKN2            TO  SPA-GMN-IDX
               END-IF
               MOVE    SPA-K97-SRYKA       TO  WRK-SRYKA
               MOVE    "$"                 TO  LNK-SC97-INPUTCD
               MOVE    WRK-SRYKA           TO  LNK-SC97-INPUTCD(2:2)
               IF      SPA-K97-DRCD        NOT =   SPACE
                   MOVE    SPA-K97-DRCD        TO  WRK-DRCD
                   MOVE    SPACE               TO  LNK-SC97-INPUTCD(4:1)
                   MOVE    WRK-DRCD            TO  LNK-SC97-INPUTCD(5:4)
               END-IF
               IF     (SPA-K97-SRYKA       NOT =   SPACE )  OR
                      (SPA-K97-DRCD        NOT =   SPACE )
                   MOVE    LNK-SC97-INPUTCD    TO  SPA-GMN-INPUTCD
                                                      (SPA-GMN-IDX)
                   MOVE    SPACE               TO  SPA-MAE-INPUTCD
                                                       (SPA-GMN-IDX)
                   MOVE    1                   TO  FLG-FUKU
                   MOVE    SPACE               TO  SPA-K97-SRYKA
                   MOVE    SPACE               TO  SPA-K97-DRCD
                   MOVE    SPA-GMN-IDX         TO  IDX-HZN
               END-IF
           END-IF
           .
       30142-KOBETU-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *     パラメタファイルより編集処理
      *****************************************************************
       3002-PARA-SET-SEC                  SECTION.
      *
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    2               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO
      *        IF      PARA-FILEMEI        =   "WKSRYACT"
      *            IF      PARA-DATA-REC   NOT =   SPACE
      *                ADD     1                   TO  LNK-WKSRYACT-MAX
      *                IF      LNK-WKSRYACT-MAX    >   SPA-MAX-LINE
      *                    DISPLAY "K02 LNK-WKSRYACT-MAX 400 OVR"
      *                    MOVE    "9000"          TO  SPA-ERRCD
      *                ELSE
      *                    MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
      *                                               (LNK-WKSRYACT-MAX)
      *                END-IF
      *            END-IF
      *        END-IF
      *
      *        IF      PARA-FILEMEI        =   "K01PARA"
      *            IF      FLG-PARASET     =   ZERO    OR  1
      *                MOVE    PARA-DATA-REC       TO  SPA-K01KYOUTU
      *            END-IF
      **       END-IF
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *
      **** CLOSE                       PARA-FILE
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
      *
      *
           IF      FLG-PARASET         =   ZERO  OR  1
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "K02"               TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "K01KYOUTU"         TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-K01KYOUTU
               END-IF
           END-IF
      *
      **
           IF      FLG-PARASET         =   ZERO  OR  2
      *        MOVE    SPA-TERMID          TO  FILE-TERMID22
      *        MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *        OPEN    INPUT               PARAK02-FILE
      *        IF      STS-PARA2           =  ZERO
      *            READ    PARAK02-FILE
      *                AT  END
      *                    INITIALIZE                  PARA-SPA-K02
      *                NOT AT  END
      *                    MOVE    PARA-SPA-K02        TO  SPA-K02
      *            END-READ
      *        END-IF
      *******  CLOSE                           PARAK02-FILE
               INITIALIZE                      SPATMP-REC
               MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
               MOVE    "K02"               TO  SPATMP-GYOUMUID
               MOVE    SPA-TERMID          TO  SPATMP-TERMID
               MOVE    "K02SPA"            TO  SPATMP-FILEMEI
               MOVE    1                   TO  SPATMP-EDANUM
               PERFORM 9100-SPATMP-KEYREAD-SEC
               IF      FLG-SPATMP          =   ZERO
                   MOVE    SPATMP-DATA-REC     TO  SPA-K02
               END-IF
      *
           END-IF
      *
           IF      FLG-PARASET         =   ZERO  OR  2
      *         クリア
                INITIALIZE                 SPA-K021GMN-AREA
                INITIALIZE                 SPA-K97-AREA
                INITIALIZE                 SPA-K052-AREA
                INITIALIZE                 SPA-K10GMN-AREA
                INITIALIZE                 SPA-K99GMN-AREA
                INITIALIZE                 SPA-K023GMN-AREA
                INITIALIZE                 SPA-K98GMN-AREA
                INITIALIZE                 SPA-K05-AREA
                INITIALIZE                 SPA-K051-AREA
                INITIALIZE                 SPA-K09KYOU-AREA
                INITIALIZE                 SPA-K07-AREA
           END-IF
      *
           .
       3002-PARA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    見出し
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    SPA-PTID            TO  SPA-GMN-PTID
           MOVE    SPA-NAME            TO  SPA-GMN-NAME
           MOVE    SPA-KANANAME        TO  SPA-GMN-KANANAME
           MOVE    SPA-SEX             TO  SPA-GMN-SEX
           MOVE    SPA-BIRTHDAYW       TO  SPA-GMN-BIRTHDAY
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  SPA-NAI-BIRTHDAY
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYKA           TO  SPA-GMN-SRYKA
           MOVE    SPA-FTNRATE         TO  SPA-GMN-FTNRATE
      *
      *    診療科
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
               END-IF
           END-PERFORM
      *
      *    入院判定
           MOVE    SPACE               TO  SPA-KIDCD
           PERFORM 3109-NYUIN-CHK-SEC
      *    IF      SPA-KIDCD       NOT =   SPACE
           IF      SPA-KIDCD           =   SPACE
                                       OR  "0121"
               CONTINUE
           ELSE
               GO      TO      310-SPA-SET-EXT
           END-IF
      *
      *    保険組合
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 3101-HKNCOMBI-HEN-SEC
      *
      *    未収金
           MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
      *    最終来院日
           MOVE    SPACE               TO  SPA-GMN-LASTYMD-G
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTYMD
           END-IF
      *
      *    初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOSIN          =   ZERO  )
      *        初診算定日画面編集
               PERFORM 3001081-SYOSIN-HEN-SEC
           END-IF
      *
      *    年齢計算
           INITIALIZE                  ORCSAGECHKAREA
           MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
           MOVE    SPA-PTID            TO  AGECHK-PTID
           MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
           MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
           MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
           CALL    "ORCSAGECHK"        USING
                                       ORCSAGECHKAREA
                                       SPA-AREA
           MOVE    AGECHK-NENREI2-YY   TO  SPA-NENREI-YY
           MOVE    AGECHK-NENREI2-MM   TO  SPA-NENREI-MM
           MOVE    AGECHK-NENREI2-DD   TO  SPA-NENREI-DD
      *H28.4
      *    未就学前
           MOVE    AGECHK-SYUGAKUKBN   TO  SPA-SYUGAKUKBN
      *
      *    画面表示用
           IF      SPA-NENREI-YY       =   ZERO
               MOVE    SPA-NENREI-MM       TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    SPA-NENREI-YY       TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-NENREI-HEN
      *
      *    ＤＯ編集
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
      *    最終退院日
           IF      SPA-NAI-LASTTAIIN   =   SPACE
               MOVE    SPACE               TO  WRK-GMN-TAIINYMD
           ELSE
               MOVE    SPA-NAI-LASTTAIIN   TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  WRK-GMN-TAIINYMD
               MOVE    SPA-GMN-LASTYMD     TO  WRK-HENYMDG
      *        最終来院日
               MOVE    SPACE               TO  SPA-GMN-LASTYMD-G
               STRING  WRK-HENYMDG         DELIMITED  BY  SIZE
                       " ("                DELIMITED  BY  SIZE
                       WRK-GMN-TAIINYMD    DELIMITED  BY  SIZE
                       ")"                 DELIMITED  BY  SIZE
                                           INTO    SPA-GMN-LASTYMD-G
               END-STRING
           END-IF
           IF     (WRK-GMN-TAIINYMD    =   SPACE )  AND
                  (SPA-BEDFLG          =   ZERO  )
      *        入院なし
               MOVE    "最終来院日"            TO  SPA-GMN-LASTMID
           ELSE
               MOVE    "最終来院日（退院日）"  TO  SPA-GMN-LASTMID
           END-IF
      *
      *    入院中
           IF      SPA-NAI-NYUIN       =   1
               MOVE    "入院日"            TO  SPA-GMN-LASTMID
               MOVE    SPA-K02N-NYUINYMD   TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-LASTYMD-G
           END-IF
           .
       310-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合決定編集処理
      *****************************************************************
       3101-HKNCOMBI-HEN-SEC              SECTION.
      *
           MOVE    SPA-GMN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *
      *    保険組合せ詳細編集
           INITIALIZE                  ORCSC95AREA
           MOVE    "H"                 TO  LNK-SC95-KBN
           MOVE    SPA-SRYYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SRYYMDW         TO  LNK-SC95-SRYYMDG
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    入院中アフターケア以外はエラーとする
           IF     (SPA-NAI-NYUIN       =   1  )
           AND    (SPA-HKN-ERR     NOT =   4  )
               IF    ((SPA-HKNNUM          =   "971" ) AND
                      (SPA-RSI-HKNKBN      =   "3"   ))
      *            自費保険は可能
                  OR  (SPA-HKNNUM (1:2)    =   "98"  )
                   CONTINUE
               ELSE
                   IF     (SPA-SYORIFLG        =   1   )
                       MOVE    "9036"              TO  SPA-ERRCD
                       MOVE    6                   TO  SPA-HKN-ERR
                   ELSE
      *                入院へ
                       MOVE    "0128"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
           .
       3101-HKNCOMBI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面用スパ編集処理
      *****************************************************************
       310-SPA-GMNSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SPA-SCR-REC
           INITIALIZE                  SPA-SCR-REC
           MOVE    ZERO                TO  SPA-GMN-MAX
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  SPA-GMN-IDX
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *    診察料編集なし
           IF      SPA-KIDCD      NOT =   SPACE
               GO      TO      310-SPA-GMNSET-EXT
           END-IF
      *
      *    初診料自動発生処理
           IF    ((LNK-WKSRYACT-MAX        >   ZERO    )  AND
                  (LNK-WKSRY-TERMID (01)   NOT =   SPACE ))   OR
                 ( SPA-DOUJI-DENPNUM   NOT =   ZERO  )   OR
                 ( SPA-SYORIFLG            =   1     )
               MOVE    ZERO                TO  FLG-SYOSIN
               IF     (SPA-SYORIFLG            =   1     )  OR
                      (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
      *            受診回数編集処理
                   MOVE    SPA-K01KYOUTU       TO  SPA-WORK
                   INITIALIZE                      ORCSC10SAREA
                   MOVE    "3"                 TO  LNK-SC10S-SYORI
                   CALL    "ORCSC10S"          USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
                   MOVE    SPA-WORK            TO  SPA-K01KYOUTU
                   MOVE    SPACE               TO  SPA-ERRCD
                   MOVE    SPACE               TO  SPA-KIDCD
               END-IF
      *******  IF     (LNK-WKSRY-TERMID (01)   =   "WKSRYACT")
      *        AND    (FLG-NYUIN       =   ZERO  )
      *            PERFORM 3104-WKSRYACT-HEN-SEC
      ******   END-IF
           ELSE
               MOVE    1               TO  FLG-SYOSIN
               IF     (FLG-NYUIN       =   ZERO  )  OR
                      (SPA-K02N-NYUINYMD   NOT =   SPACE )
                   PERFORM 330-SYOSIN-SET-SEC
               END-IF
               IF     (SPA-ERRCD       NOT =   SPACE ) OR
                      (SPA-KIDCD       NOT =   SPACE )
                   GO      TO      310-SPA-GMNSET-EXT
               END-IF
           END-IF
      *
      *****MOVE    "2"                 TO  FLG-TOROKU
      *
      *    再表示編集処理
           IF      LNK-WKSRYACT-MAX    >   ZERO
               MOVE    ZERO            TO  FLG-K09TUIKA
               PERFORM 3103-SAIHEN-HEN-SEC
               IF      SPA-SYORIFLG            =   1
      *            初診料算定の判定
                   MOVE    SPA-K01KYOUTU       TO  SPA-WORK
                   INITIALIZE                      ORCSC10SAREA
                   MOVE    "C"                 TO  LNK-SC10S-SYORI
                   CALL    "ORCSC10S"          USING
                                               ORCSC10SAREA
                                               SPA-AREA
                                               SPA-SCR-REC
                                               SPA-K02
                   MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               END-IF
      *
               IF     (LNK-WKSRY-TERMID (01)   =   "WKSRYACT")
               AND    (FLG-NYUIN       =   ZERO  )
                   PERFORM 3104-WKSRYACT-HEN-SEC
                   MOVE    SPACE               TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *    入力コード・名称チェック処理
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
      *
               IF      SPA-COM-CUR (IDX)   =   SPACE
                   MOVE    SPA-GMN-INPUTCD(IDX)
                                           TO  SPA-MAE-INPUTCD(IDX)
               END-IF
      *        警告表示をしない
               MOVE    "1"                 TO  SPA-COM-KZMFLG (IDX)
               MOVE    "1"                 TO  SPA-COM-HKEIFLG(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG2(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG3(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG4(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG5(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG6(IDX)
               MOVE    "1"                 TO  SPA-COM-KZMFLG7(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG1(IDX)
               MOVE    "1"                 TO  SPA-COM-TUKIJGNFLG(IDX)
           END-PERFORM
      *    内分泌負荷試験検査情報
           INITIALIZE                  SPA-NAI-FUKATEN-AREA
      *    診療行為、計計算処理、編集
           MOVE    1                   TO  FLG-INIT
           MOVE    SPACE               TO  FLG-TOROKU
      *
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
      *    訂正時点数変更チェック
           MOVE    ZERO                TO  SPA-TEISEI-CHK
           MOVE    ZERO                TO  SPA-TEISEI-ERR
      *    警告フラグクリア
           MOVE    ZERO                TO  WRK-MAE-TENSU
           MOVE    ZERO                TO  FLG-MAE-TENSU
           PERFORM VARYING IDX     FROM    1   BY  1
                   UNTIL   (IDX        >   SPA-MAX-LINE )  OR
                          ((SPA-GMN-INPUTCD(IDX)   =   SPACE ) AND
                           (SPA-COM-INPUTCD(IDX)   =   SPACE ))
               IF     (SPA-COM-KZMFLG (IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG2(IDX)    NOT =   SPACE )  OR
                      (SPA-COM-KZMFLG3(IDX)    NOT =   SPACE )
      *            約束セット
                   IF     (SPA-GMN-INPUTCD(IDX)(1:1)   =   "." )
                      OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "#" )
                      OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "$" )
      *?              OR  (SPA-GMN-INPUTCD(IDX)(1:1)   =   "S" )
                      CONTINUE
                   ELSE
                       MOVE    SPACE       TO  SPA-MAE-INPUTCD (IDX)
                   END-IF
               END-IF
      *        警告フラグクリア
               MOVE    SPACE           TO  SPA-COM-KZMFLGAREA (IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG(IDX)
               MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1(IDX)
      *        併用算定が警告の時、クリア
               IF      SPA-COM-HKEIFLG (IDX)   =   "1"
                   MOVE    SPACE               TO  SPA-COM-HKEIFLG (IDX)
                   MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
               END-IF
      *
      *H30.9
      *        訂正展開時、選択コメント対応済みとする
               IF     (SPA-SYORIFLG        =   1  )
                 AND  (FLG-TEISEI          =   1  )
                   IF     (SPA-COM-INPUTCD (IDX) (1:1) =   "1" )
                      AND (SPA-COM-RECEKISAI(IDX) NOT  =   SPACE)
                       MOVE    "1"         TO  SPA-COM-RECEAUTO
                                                            (IDX)
                   END-IF
               END-IF
      *!!
      *        訂正時の点数判定
               IF      SPA-SYORIFLG        =   1
                   IF      FLG-MAE-TENSU   =   ZERO
                       IF     (SPA-COM-MAE-TENSU(IDX)  >   ZERO  )  OR
                              (SPA-COM-INPUTCD (IDX)   NOT =   SPACE)
                           IF     (SPA-COM-MAE-TENSU(IDX)  =   ZERO) AND
                                  (SPA-COM-INPUTCD (IDX)(1:1)  =  "8"
                                                               OR "0")
                               CONTINUE
                           ELSE
                               MOVE    SPA-COM-MAE-TENSU(IDX)
                                                   TO  WRK-MAE-TENSU
                               MOVE    1           TO  FLG-MAE-TENSU
                           END-IF
                       END-IF
                   END-IF
                   IF     (SPA-COM-ZAIEND(IDX)     =   "1" )  OR
                         ((SPA-COM-SRYKBN(IDX)     =   "95"
                                                   OR  "96" ) AND
                          (SPA-COM-JIHIMONEY(IDX)  >   ZERO ))
                       IF      SPA-COM-SRYKBN(IDX)     =   "95"
                                                       OR  "96"
                           MOVE    SPA-COM-JIHIMONEY(IDX)
                                                   TO  WRK-TENSU
                       ELSE
                           MOVE    SPA-COM-TENSU (IDX)
                                                   TO  WRK-TENSU
                       END-IF
                       IF      WRK-TENSU       NOT =   WRK-MAE-TENSU
                           MOVE    1               TO  SPA-TEISEI-ERR
                           MOVE    "1"             TO  SPA-COM-CUR (IDX)
                       END-IF
                       MOVE    ZERO            TO  WRK-MAE-TENSU
                       MOVE    ZERO            TO  FLG-MAE-TENSU
                   END-IF
                   IF      SPA-COM-ZAIEND(IDX)     =   "1"
                       MOVE    ZERO                TO  WRK-MAE-TENSU
                       MOVE    ZERO                TO  FLG-MAE-TENSU
                   END-IF
               END-IF
           END-PERFORM
      *    画面ＳＰＡ書込み（画面遷移前）
           IF      FLG-END             =   1
               PERFORM 1003-K02SPA-OUT-SEC
           END-IF
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    SPACE               TO  SPA-KIDCD
      *    内分泌負荷試験検査情報
           INITIALIZE              SPA-NAI-FUKATEN-AREA
      *
           IF      WRK-ERRCD1      NOT =   SPACE
               MOVE    WRK-ERRCD1          TO  SPA-ERRCD
           END-IF
      *
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-ERRCD           =   SPACE)
               MOVE    "K999"          TO  SPA-ERRCD
               MOVE    1               TO  SPA-TEISEI-CHK
           END-IF
      *
           .
       310-SPA-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   中途終了よりの画面用再編集処理
      *****************************************************************
       3104-WKSRYACT-HEN-SEC           SECTION.
      *
      *    剤分離処理を行う
           INITIALIZE                  ORCSC93AREA
           MOVE    "K02"               TO  LNK-SC93-PG
      *    小児科外来診療科算定有り
           MOVE    SPA-SYONIKA-ARI2    TO  LNK-SC93-SYONIKA-ARI2
      *    寝たきり在宅科算定有り
           IF      (SPA-NETAKIRI-ARI   NOT =   ZERO )  OR
                   (SPA-NETAKIRI-ARI2  NOT =   ZERO )
               MOVE    1                   TO  LNK-SC93-NETAKIRI-ARI
           END-IF
      *    残量廃棄区分
           MOVE    SPA-HAIKIKBN-FLG    TO  LNK-SC93-HAIKIKBN-FLG
           CALL    "ORCSC93"           USING
                                       MCPAREA
                                       SPA-SCR-REC
                                       SPA-AREA
                                       ORCSC93AREA
                                       MSYU-DATA-REC
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "W"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       3104-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   再表示あり　編集処理
      *****************************************************************
       3103-SAIHEN-HEN-SEC                  SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "1"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
      *    処理区分（０：訂正展開、１：中途終了展開、２：ＤＯ展開）
           IF      FLG-K09TUIKA        =   1
               MOVE    2               TO  ORCSCWKSRY-SYORIFLG
           ELSE
               MOVE    1               TO  ORCSCWKSRY-SYORIFLG
               IF      SPA-SYORIFLG    =   1
                   MOVE    ZERO            TO  ORCSCWKSRY-SYORIFLG
               END-IF
           END-IF
      *    中途終了分追加フラグ
           IF      FLG-K09TUIKA        =   3
              MOVE    1                TO  ORCSCWKSRY-K09TUIKA
           END-IF
      *    追加開始位置
           MOVE    IDX2                TO  ORCSCWKSRY-STRIDX
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           IF      ORCSCWKSRY-ERRCD    NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD    TO  WRK-ERRCD1
           END-IF
           IF      ORCSCWKSRY-ERRCD2   NOT =   SPACE
               MOVE    ORCSCWKSRY-ERRCD2   TO  WRK-ERRCD3
           END-IF
      *
           .
       3103-SAIHEN-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中判定処理
      *****************************************************************
       3109-NYUIN-CHK-SEC              SECTION.
      *
           INITIALIZE                  ORCSCNYUHENAREA
           MOVE    "1"                 TO  ORCSCNYUHEN-KBN
           MOVE    SPA-SRYYMD          TO  ORCSCNYUHEN-SRYYMD
           CALL    "ORCSCNYUHEN"       USING
                                       SPA-AREA
                                       ORCSCNYUHENAREA
           MOVE    ZERO                TO  FLG-NYUIN
      *    入院中
           IF      ORCSCNYUHEN-NYUIN-KBN   =   1
               MOVE    1                   TO  FLG-NYUIN
           ELSE
               IF      ORCSCNYUHEN-NYUIN-DAYG  NOT =   ZERO
      *            入院中でなく当月に入院がある
                   MOVE    1                   TO  SPA-K02N-NYUINFLG
               END-IF
           END-IF
      *    入退院日
           MOVE    ORCSCNYUHEN-NYUINYMD    TO  SPA-K02N-NYUINYMD
           MOVE    ORCSCNYUHEN-TAIINYMD    TO  SPA-K02N-TAIINYMD
      *    入院期間
           MOVE    ORCSCNYUHEN-NYUIN-DAYG  TO  SPA-K02-NYUIN-AREA
      *    最終退院日
           MOVE    ORCSCNYUHEN-LAST-TAIINYMD
                                           TO  SPA-NAI-LASTTAIIN
           MOVE    ORCSCNYUHEN-MAE-TAIINYMD
                                           TO  SPA-K02N-TAIINYMD
      *    他医院退院日
           MOVE    ORCSCNYUHEN-TAI-TAIINYMD
                                           TO  SPA-TAI-TAIINYMD
      *    入院中の時、エラーとする
           MOVE    ZERO                TO  SPA-NAI-NYUIN
           IF      FLG-NYUIN           =   1
      *        保険にアフターケアがあれば対象
               IF     (SPA-NAI-SRYYMD      >=  "20060401" )
                   PERFORM 31091-NYUIN-HKNCHK-SEC
               END-IF
               IF      FLG-NYUIN           =   1
                   MOVE    "0121"              TO  SPA-KIDCD
      ****         MOVE    ZERO                TO  SPA-GMN-CUR
               END-IF
           END-IF
      *!!
      *    他医院退院日の算定履歴編集
           IF      FLG-NYUIN           =   ZERO
               PERFORM 1009-SANTEI-HEN-SEC
           END-IF
      *
           .
       3109-NYUIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院中アフターケア処理
      *****************************************************************
       31091-NYUIN-HKNCHK-SEC              SECTION.
      *
      *    保険組合せ情報検索
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      SPA-GMN-HKNCOMBINUM   =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    IDX               TO  IDY
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               GO      TO      31091-NYUIN-HKNCHK-EXT
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
               IF    ((SPA-NAI-HKNNUM     (IDY)    =   "971" ) AND
                      (SPA-NAI-RSI-HKNKBN (IDY)    =   "3" ) )
                OR   ( SPA-NAI-HKNNUM (IDY)(1:2)   =   "98"  )
                   MOVE    ZERO                TO  FLG-NYUIN
                   MOVE    1                   TO  SPA-NAI-NYUIN
               END-IF
               GO      TO      31091-NYUIN-HKNCHK-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           IF    ((SPA-NAI-HKNNUM     (IDY)    =   "971" ) AND
                  (SPA-NAI-RSI-HKNKBN (IDY)    =   "3"  ))
               MOVE    ZERO                TO  FLG-NYUIN
               MOVE    1                   TO  SPA-NAI-NYUIN
           ELSE
               MOVE    ZERO                TO  FLG-CHK
               MOVE    ZERO                TO  IDY2
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                                  (FLG-CHK     =    1   ) 
                   IF     (SPA-NAI-HKNNUM (IDX)     =   "971" )
                     AND  (SPA-NAI-RSI-HKNKBN(IDX)  =   "3"   )
                       MOVE    IDX               TO  IDY2
                       MOVE    1                 TO  FLG-CHK
                   END-IF
                   IF     (SPA-NAI-HKNNUM (IDX)(1:2) =  "98"  )
                       MOVE    2                 TO  FLG-CHK
                   END-IF
               END-PERFORM
           END-IF
      *
           IF      FLG-CHK             =   1
               MOVE    1                   TO  SPA-NAI-NYUIN
               IF      (IDY                >   ZERO    )   AND
                       (SPA-NAI-HKNNUM(IDY)(1:2)   =   "98" )
                   CONTINUE
               ELSE
                   MOVE    IDY2             TO  IDY
               END-IF
               IF     (SPA-NAI-HKNNUM (IDY)     =   SPA-HKNNUM )
                 AND  (SPA-NAI-RSI-HKNKBN(IDY)  =   SPA-RSI-HKNKBN)
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-THKNCOMBINUM(IDY)
                                                TO  SPA-GMN-HKNCOMBINUM
                   PERFORM 3101-HKNCOMBI-HEN-SEC
               END-IF
               IF     (SPA-HKNNUM(1:2)         =   "98" )
                   MOVE    1                   TO  FLG-NYUIN
               ELSE
                   MOVE    ZERO                TO  FLG-NYUIN
               END-IF
           END-IF
           .
       31091-NYUIN-HKNCHK-EXT.
           EXIT.
      *****************************************************************
      *    算定履歴から他院退院日編集処理
      *****************************************************************
       1009-SANTEI-HEN-SEC             SECTION.
      *
      *    他医院退院日
           MOVE    "099999906"         TO  WRK-SRYCD
      *
           MOVE    ZERO                TO  FLG-TEI-ARI
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING    IDX-T    FROM    1   BY  1
                       UNTIL     (IDX-T    >   SPA-MAX-LINE )  OR
                                 (SPA-K02-TEI-SRYCD(IDX-T) =   SPACE)
                   IF      SPA-K02-TEI-SRYCD(IDX-T)    =   WRK-SRYCD
                       MOVE    1               TO  FLG-TEI-ARI
                       MOVE    SPA-MAX-LINE    TO  IDX-T
                   END-IF
               END-PERFORM
           END-IF
           MOVE    SPACE               TO  WRK-YMD
      *
           INITIALIZE                  SANTEI-REC
           MOVE    SPA-HOSPNUM         TO  SANTEI-HOSPNUM
           MOVE    SPA-PTID            TO  SANTEI-PTID
           MOVE    WRK-SRYCD           TO  SANTEI-SRYCD
           MOVE    SPA-SRYYMD(1:6)     TO  SANTEI-SRYYM
      *
           MOVE    SANTEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key8"              TO  MCP-PATHNAME
               PERFORM 920-SANTEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           PERFORM UNTIL       FLG-SANTEI          =   1
      *        診療日以前の算定日
               IF      SANTEI-SRYYM    =   SPA-SRYYMD(1:6)
                   MOVE    SPA-SRYYMD(7:2) TO  IDX-Y3
      *            訂正で今回算定であれば、前日までを対象
                   IF      FLG-TEI-ARI     =   1
                       COMPUTE IDX-Y3          =    IDX-Y3  -  1
                   END-IF
               ELSE
                   MOVE    31              TO  IDX-Y3
               END-IF
               MOVE    ZERO                TO  WRK-DD-1
               PERFORM VARYING     IDX-Y2  FROM    1  BY  1
                       UNTIL       IDX-Y2      >   IDX-Y3
                   IF      SANTEI-DAY (IDX-Y2) >   ZERO
                       MOVE    IDX-Y2          TO  WRK-DD-1
                   END-IF
               END-PERFORM
               IF      WRK-DD-1            >   ZERO
                   MOVE    SANTEI-SRYYM        TO  WRK-YMD(1:6)
                   MOVE    WRK-DD-1            TO  WRK-DD
                   MOVE    1                   TO  FLG-SANTEI
               END-IF
               IF      FLG-SANTEI          =   ZERO
                   MOVE    "tbl_santei"        TO  MCP-TABLE
                   MOVE    "key8"              TO  MCP-PATHNAME
                   PERFORM 920-SANTEI-READ-SEC
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           MOVE    "key8"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      WRK-YMD         NOT =   SPACE
               IF      WRK-YMD         >   SPA-TAI-TAIINYMD
                   MOVE    WRK-YMD     TO  SPA-TAI-TAIINYMD
               END-IF
           END-IF
           .
       1009-SANTEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       320-SPA-CHKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  SPA-ZAITENTOTAL
           MOVE    SPACE               TO  SPA-ACCT2-SRYCD
           MOVE    ZERO                TO  SPA-ACCT2-ZAINUM
           MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
      *
           MOVE    ZERO                TO  FLG-TOUGETU
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
               IF     SPA-NAI-RRKYMD (IDX)(1:6)
                                       =   SPA-SRYYMD(1:6)
                   MOVE    1               TO  FLG-TOUGETU
                   MOVE    SPA-GMN-MAX1    TO  IDX
               ELSE
                   IF     SPA-NAI-RRKYMD (IDX)(1:6)
                                           <   SPA-SRYYMD(1:6)
                       MOVE    SPA-GMN-MAX1    TO  IDX
                   END-IF
               END-IF
           END-PERFORM
           IF     (FLG-TOUGETU         =   1   ) OR
                  (SPA-SYORIFLG        =   1   )
      *        当月診療行為チェック用編集、当月累計点数計算
               PERFORM 3201-TOUCHKSET-SEC
           END-IF
      *
      *    禁忌薬剤チェック用編集
           IF      SPA-INTERACT-KIKAN  =   ZERO
               INITIALIZE                  SPA-KNKYAK-AREA
           ELSE
               PERFORM 3202-KNKYAKSET-SEC
           END-IF
      *
           .
       320-SPA-CHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェック用月内診療行為セット処理
      *****************************************************************
       3201-TOUCHKSET-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDX-SIN
           MOVE    ZERO                TO  IDX-SIN2
      *    診療年月の診療会計マスタ読み込み
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
      *****MOVE    SPA-SRYKA           TO  ACCT-SRYKA
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    SPA-SRYYMD(7:2)     TO  WRK-TDD
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM UNTIL       (FLG-SRYACCT         =   1       )
      *
      *      包括分入力は対象外
             IF     (ACCT-HKNCOMBI     =   "9999" )
                OR  (ACCT-JIHOKBN      =   "1"    )
               CONTINUE
             ELSE
      *        点数累計
               IF     (ACCT-ZAITEN         =   ZERO )  OR
                      (ACCT-ZAIKAISU       =   ZERO )  OR
                      (ACCT-SRYKBN         =   "95" OR "96")
                   CONTINUE
               ELSE
                   INITIALIZE                  SRYACCTPLUS-REC
                   IF      ACCT-ZAIREQFLG      =   "1"
      *                附加情報あり
                       PERFORM 32012-SRYACCTPLUS-READ-SEC
                   END-IF
                   IF      ACCTP-PLUSKBN        =   1
                       COMPUTE SPA-ZAITENTOTAL  =   SPA-ZAITENTOTAL
                                                -  (ACCT-ZAITEN
                                                *   ACCT-ZAIKAISU )
                   ELSE
                       COMPUTE SPA-ZAITENTOTAL  =   SPA-ZAITENTOTAL
                                                +  (ACCT-ZAITEN
                                                *   ACCT-ZAIKAISU )
                   END-IF
               END-IF
      *病理診断料のみで使用する為、入力時へ
      *TET     検査で回数があるもの
      *        IF     (ACCT-ZAIKAISU   NOT =   ZERO  )  AND
      *               (ACCT-SRYKBN         =   "60"  )
      *            MOVE    1               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
      *            PERFORM 3201-SRYACT-HEN-SEC
      *TES     END-IF
      *（注射のためにチェックしていたが、現在していないのではずす）
      *復活
      *        当日算定分
               IF     (ACCT-DAY(1 WRK-TDD) >   ZERO  )  AND
                      (ACCT-SRYKBN         =   "33"
                                           OR  "32"  )
                   MOVE    2               TO  FLG-ACCTSYORI
      *            診療行為マスター編集
                   PERFORM 3201-SRYACT-HEN-SEC
               END-IF
      *
             END-IF
      *
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-SRYACCT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       3201-TOUCHKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       3201-SRYACT-HEN-SEC            SECTION.
      *
      *    診療会計マスターから診療行為マスターを検索する
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                   IF      SRY-SRYCD (IDX2)(1:1)   =   "8"  OR
                                                       "0"
                       CONTINUE
                   ELSE
                       EVALUATE    FLG-ACCTSYORI
                           WHEN    1
                               PERFORM 32011-SRYACT-HEN1-SEC
                           WHEN    2
                               PERFORM 32012-SRYACT-HEN2-SEC
                       END-EVALUATE
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       3201-SRYACT-HEN-EXT.
           EXIT.
      *DEL
      *****************************************************************
      *    診療行為チェック用編集１処理（検査）
      *****************************************************************
       32011-SRYACT-HEN1-SEC           SECTION.
      *
           ADD     1                   TO  IDX-SIN
           IF      IDX-SIN             >   SPA-MAX-LINE
               MOVE    SPA-MAX-LINE            TO  IDX-SIN
           END-IF
           MOVE    ACCT-SRYKBN         TO  SPA-ACCT-SRYKBN
                                                      (IDX-SIN)
           MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT-SRYCD
                                                       (IDX-SIN)
      *    点数マスタより、区分番号編集
           MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           MOVE    TNS-CDKBN-KBN       TO  SPA-ACCT-CDKBN-KBN
                                                          (IDX-SIN)
           MOVE    TNS-CDKBN-KBNNUM    TO  SPA-ACCT-CDKBN-KBNNUM
                                                          (IDX-SIN)
      **** MOVE    TNS-CDKBN-SYO       TO  SPA-ACCT-CDKBN-SYO
      *                                                   (IDX-SIN)
      *    MOVE    TNS-CDKBN-BU        TO  SPA-ACCT-CDKBN-BU
      *****                                               (IDX-SIN)
           .
       32011-SRYACT-HEN1-EXT.
           EXIT.
      *DEL
      *****************************************************************
      *    診療行為チェック用編集２処理（点滴）
      *****************************************************************
       32012-SRYACT-HEN2-SEC           SECTION.
      *
      *    点滴
           IF      SRY-SRYSYUKBN       =   "330"  OR
                                           "331"
               IF     (IDX2            =   1     ) AND
                      (SRY-SRYCD (IDX2)(1:1)   =   "1" )
                   MOVE    SRY-SRYCD (IDX2)    TO  SPA-ACCT2-SRYCD
                   MOVE    SRY-ZAINUM          TO  SPA-ACCT2-ZAINUM
               END-IF
      *        点数マスタより編集
               IF      SRY-SRYCD(IDX2)(1:1)    =   "6"
                   MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   COMPUTE SPA-ACCT2-CSYYOURYO =   SPA-ACCT2-CSYYOURYO
                                               +  (TNS-CSYYOURYO
                                               *   SRY-SRYSURYO (IDX2))
               END-IF
           END-IF
      *
      *    静脈注射
           IF     (SRY-SRYSYUKBN       =   "320"  OR
                                           "340" )  AND
                  (SRY-SRYCD (IDX2)(1:1)   =   "1" ) AND
                  (SPA-ACCT2-SRYCD-JYO1    =   SPACE ) AND
                  (SPA-ACCT2-SRYCD-JYO2    =   SPACE )
               MOVE    SRY-SRYCD (IDX2)    TO  TNS-SRYCD
               PERFORM 910-TENSU-READ-SEC
               IF     (TNS-DATAKBN         =   "1" )  AND
                      (TNS-CDKBN-KBN       =   "G" )
      *            静脈注射
                   IF    (TNS-CDKBN-KBNNUM =   001 )
                       MOVE    SRY-SRYCD (IDX2)
                                               TO  SPA-ACCT2-SRYCD-JYO1
                   END-IF
      *            中心静脈
                   IF     (TNS-CDKBN-KBNNUM        =   005) AND
                          (TNS-CDKBN-KBNNUM-EDA    =   00 )
                       MOVE    SRY-SRYCD (IDX2)
                                               TO  SPA-ACCT2-SRYCD-JYO2
                   END-IF
      *H28.6       中心静脈注射(植入型カテーテル）
                   IF      SPA-SRYYMD          >=  "20160401"
                       IF     (TNS-CDKBN-KBNNUM        =   006) AND
                              (TNS-CDKBN-KBNNUM-EDA    =   00 )
                           MOVE    SRY-SRYCD (IDX2)
                                               TO  SPA-ACCT2-SRYCD-JYO2
                       END-IF
                   END-IF
               END-IF
           END-IF
      *
           .
       32012-SRYACT-HEN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌薬剤チェック用編集処理
      *****************************************************************
       3202-KNKYAKSET-SEC              SECTION.
      *
           INITIALIZE                  ORCSCKNKCHKAREA
           MOVE    "1"                 TO  ORCSCKNKCHK-KBN 
           CALL    "ORCSCKNKCHK"       USING
                                       ORCSCKNKCHKAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
      *
           .
       3202-KNKYAKSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診料自動発生処理
      *****************************************************************
       330-SYOSIN-SET-SEC                  SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "1"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    LNK-SC10S-IDX2      TO  IDX2
           .
       330-SYOSIN-SET-EXT.
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
                   PERFORM 210-BACK
      *        Ｋ０１へ
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4001-K01-SYORI-SEC
      *        患者取消
               WHEN    "CLICKED"       ALSO    "B02"
                   PERFORM 222-PTNUMCLEAR-SEC
      *        途中終了
               WHEN    "CLICKED"       ALSO    "B12S"
                   PERFORM 4110-KYOSEIEND-SEC
      *        途中終了画面へ（Ｋ１０）
               WHEN    "CLICKED"       ALSO    "B12CS"
                   PERFORM 4110S-K10-SYORI-SEC
      *        セット登録（Ｋ０５）
               WHEN    "CLICKED"       ALSO    "B03S"
                   PERFORM 420-KOUISET-SEC
      *        前回の患者番号
               WHEN    "CLICKED"       ALSO    "B03"
                   PERFORM 230-MAEKPTNUM-SEC
      *        算定履歴更新（Ｋ０６）
               WHEN    "CLICKED"       ALSO    "B10S"
                   PERFORM 430-SANTEIRRK-SEC
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02S"
                   PERFORM 220-CLEAR-SEC
      *        患者登録へ（Ｐ０２）
               WHEN    "CLICKED"       ALSO    "B05S"
                   PERFORM 440-KNJADD-SEC
      *        病名登録　（Ｃ０２）
               WHEN    "CLICKED"       ALSO    "B07S"
                   PERFORM 440-BYOTOROKU-SEC
      *        会計カード（Ｊ０２）
               WHEN    "CLICKED"       ALSO    "B09S"
                   PERFORM 450-KAIKEI-SEC
      *        収納登録（Ｓ０２）
               WHEN    "CLICKED"       ALSO    "B08S"
                   PERFORM 460-SYUNOU-SEC
      *        複数科保険選択
               WHEN    "CLICKED"       ALSO    "B06S"
                   PERFORM 450-MAECUR-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 450-MAEGMN-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 460-ATOGMN-SEC
      *        ＤＯ（Ｋ０９）
               WHEN    "CLICKED"       ALSO    "B08"
                   PERFORM 480-DOSYORI-SEC
      *****    算定（Ｋ０４）
      *        WHEN    "CLICKED"       ALSO    "B23"
      ********     PERFORM 470-SANTEI-SEC
      *        訂正処理
               WHEN    "CLICKED"       ALSO    "B04"
                   PERFORM 4101-TEISEI-SEC
      *        訂正処理（トグルボタン）
               WHEN    "CLICKED"       ALSO    "TEISEI"
                   PERFORM 4101-TEISEI-SEC
      *        氏名検索（Ｐ９７）
               WHEN    "CLICKED"       ALSO    "B09"
                   MOVE    SPACE              TO  WRK-P97-NAME
                   PERFORM 470-P97-KENSAKU-SEC
      *        予約登録へ（Ｙ０１）
               WHEN    "CLICKED"       ALSO    "B10"
                   PERFORM 480-YOYAKU-SEC
      *        受付一覧へ（Ｕ０１）
               WHEN    "CLICKED"       ALSO    "B11"
                   PERFORM 490-UKEICHI-SEC
      *        算定（包括算定チェック）
               WHEN    "CLICKED"       ALSO    "B11S"
                   PERFORM 4911-HKTSANTEI-CHEK-SEC
      *        受付画面へ（Ｕ０２）
               WHEN    "CLICKED"       ALSO    "B04S"
                   PERFORM 4102-UKETUKE-SEC
      **       登録（Ｋ０８）
               WHEN    "CLICKED"       ALSO    "B12"
                   MOVE    SPACE           TO  WRK-PUTTYPE
                   PERFORM 4100-TOROKU-SEC
      *        院外院内
               WHEN    "CLICKED"       ALSO    "INGAI"
                   PERFORM 4101-INGAI-SYORI-SEC
      *        処方（頭書）
               WHEN    "CLICKED"       ALSO    "BSH1"
                   MOVE    1                   TO  FLG-SYOHOU
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *        処方（前回）
               WHEN    "CLICKED"       ALSO    "BSH2"
                   MOVE    2                   TO  FLG-SYOHOU
                   PERFORM 4124-MAESYOHOU-SYORI-SEC
      *        入力コード設定へ
               WHEN    "CLICKED"       ALSO    "B05"
                   PERFORM 4106S-INPUTCD-SYORI-SEC
      *        ヘルプ
               WHEN    "CLICKED"       ALSO    "BHELP"
                   PERFORM 4199-HELP-SYORI-SEC
      *        メモ登録へ
               WHEN    "CLICKED"       ALSO    "BC10"
                   PERFORM 4188-MEMO-SYORI-SEC
      *        受診履歴 前頁
               WHEN    "CLICKED"       ALSO    "BC06"
                   PERFORM 4109-BC06-SYORI-SEC
      *        受診履歴 次頁
               WHEN    "CLICKED"       ALSO    "BC07"
                   PERFORM 4109-BC07-SYORI-SEC
      *!!!!!!!!!!!
      *        １行目へ
               WHEN    "CLICKED"       ALSO    "CTRLHOME"
                   PERFORM 4501-CTRHOME-SEC
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
      *    入力項目の決定
           IF      MCP-EVENT           =   "ACTIVATE"   OR
                                           "ACTIVATE2"
      *!!!      IF     (MCP-WIDGET(1:07)    =   "INPUTCD"  )  OR
      *!!!             (MCP-WIDGET(1:6)     =   "MEISYO"   )
               IF      MCP-WIDGET      =   "PANDATABLE1"
                   PERFORM 2001-INPUT-SET-SEC
                   IF      WRK-IDX2        =   ZERO
                       GO      TO      200-ENTRY-EXT
                   END-IF 
               ELSE
                   MOVE    MCP-WIDGET          TO  WRK-WIDGET
               END-IF
           ELSE
               MOVE    MCP-WIDGET          TO  WRK-WIDGET
           END-IF
      *
           EVALUATE    MCP-EVENT       ALSO    WRK-WIDGET
      *        患者番号入力
               WHEN    "ACTIVATE"      ALSO    "PTNUM"
                   PERFORM 41010-PTNUM-SYORI-SEC
      *        保険番号入力
               WHEN    "ACTIVATE"      ALSO    "HKNCOMBI"
                   PERFORM 41012-HKNCOMBI-SYORI-SEC
      *        診療日入力
               WHEN    "ACTIVATE"      ALSO    "SRYYMD"
                   PERFORM 41017-SRYYMD-SYORI-SEC
      *        診療科入力
               WHEN    "ACTIVATE"      ALSO    "SRYKA"
                   PERFORM 41013-SRYKAI-SYORI-SEC
      *        ドクター入力
               WHEN    "ACTIVATE"      ALSO    "DRCD"
                   PERFORM 41013-DRCD-SYORI-SEC
      *        入力コード
               WHEN    "ACTIVATE"      ALSO    "INPUTCD"
                   PERFORM 41011-KOUI-ALLHEN-SEC
      *        名称
               WHEN    "ACTIVATE"      ALSO    "MEISYO"
                    PERFORM 41011-KOUI-ALLHEN-SEC
      *        ＤＯ選択
               WHEN    "ACTIVATE"      ALSO    "DOSELNUM"
                   PERFORM 41014-DOSELNUM-SEC
      *        診療日選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 41015-RRKLIST-SEC
           END-EVALUATE
           .
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力項目の決定
      *****************************************************************
       2001-INPUT-SET-SEC               SECTION.
      *
           MOVE    SPACE               TO  FLG-TOROKU
      *    入力行NOの設定
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-WIDGET
           IF      MCP-WIDGET          =   "PANDATABLE1"
               MOVE    K02-PANDATABLE1-TROW    TO  WRK-IDX2
               IF      K02-PANDATABLE1-TCOLUMN =   2
                   MOVE    "INPUTCD"           TO  WRK-WIDGET
                   MOVE    K02-PANDATABLE1-TVALUE
                                               TO  K02-INPUTCD(WRK-IDX2)
               ELSE
                   MOVE    "MEISYO"            TO  WRK-WIDGET
                   MOVE    K02-PANDATABLE1-TVALUE
                                               TO  K02-MEISYO(WRK-IDX2)
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                 TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE            TO  IDX
               END-IF
           END-PERFORM
      *
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
      *
           .
       2001-INPUT-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号入力処理
      *****************************************************************
       41010-PTNUM-SYORI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SPA-KIDCD
      *
           MOVE    ZERO                TO  WRK-GMN-PTID
           IF      SPA-PTID        NOT =   ZERO
      *        ワーク診療行為クリア
               PERFORM 4900-WKSRYACT-CLEAR-SEC
               MOVE    SPA-PTID        TO  WRK-GMN-PTID
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-CUR
           MOVE    1                   TO  FLG-GMN-INIT
      *    ＳＰＡクリア処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-K01SET-AREA
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-CONTFLG
           INITIALIZE                  SPA-KYOTUKEY
           INITIALIZE                  SPA-K02SET-AREA
      *    受付一覧からの時、ドクターを編集する
           IF      WRK-MOTOPG          =   "U01"
               MOVE    WRK-SPA-DRCD        TO  SPA-DRCD
               MOVE    WRK-SPA-P02-UKEID   TO  SPA-P02-UKEID
           END-IF
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *
           MOVE    K02-PTNUM           TO  SPA-GMN-PTNUM
      *
           EVALUATE    TRUE
               WHEN   (SPA-GMN-PTNUM(1:1)  =   "N" OR "n")  AND
                      (SPA-GMN-PTNUM(2:)   =   SPACE     )  AND
                      (SPA-BEDFLG          >   ZERO      )
      *        Ｎのとき、入院へ
                   MOVE    SPACE               TO  WRK-PUTTYPE
                   PERFORM 450-K02N-SYORI-SEC
               WHEN    SPA-GMN-PTNUM(1:1)      =   "*"
      *       ＊のとき、患者新規登録へ
                   MOVE    "*"                 TO  SPA-PTNUM
                   PERFORM 440-KNJADD-SEC
      *!!
               WHEN   (SPA-GMN-PTNUM(1:1)  =   "@"   )  AND
                      (SPA-GMN-PTNUM(2:2)  NUMERIC   )  AND
                      (SPA-GMN-PTNUM(4:)   =   SPACE )  AND
                      (SPA-GRPHOSPKBN      =   1     )
      *        @のとき、本院分院切替へ
                   PERFORM 409-GRPCHK-SEC
      *!!
               WHEN   (SPA-GMN-PTNUM           =   SPACE)
                   MOVE    WRK-GMN-PTID        TO  SPA-GMN-PTID
      *        空白は患者取消と同様
                   PERFORM 2221-PTNUMCLEAR-SYORI-SEC
               WHEN    OTHER
      *        患者決定処理
                   PERFORM 410101-PTNUM-IN-SYORI-SEC
           END-EVALUATE
      *
           .
       41010-PTNUM-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者番号検索処理
      *****************************************************************
       410101-PTNUM-IN-SYORI-SEC         SECTION.
      *
           INITIALIZE                      ORCSPTNUMAREA
           MOVE    SPA-GMN-PTNUM       TO  SPTNUM-PTNUM
           CALL    "ORCSPTNUM"         USING   ORCSPTNUMAREA
                                               SPA-AREA
           EVALUATE    SPTNUM-RC
               WHEN    10
               WHEN    99
      *            患者番号なし
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    "1003"              TO  SPA-ERRCD
               WHEN    20
      *            氏名検索へ
                   MOVE    SPTNUM-PTNUM        TO  SPA-GMN-PTNUM
                   MOVE    SPTNUM-PTNUM        TO  WRK-P97-NAME
                   PERFORM 470-P97-KENSAKU-SEC
               WHEN    OTHER
      *            患者番号決定
                   MOVE    SPTNUM-PTID         TO  SPA-PTID
                   MOVE    SPTNUM-PTNUM        TO  SPA-PTNUM
                   MOVE    SPA-PTID            TO  SPA-GMN-PTID
                   MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
      *            排他制御処理
                   PERFORM 990-LOCK-IN-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 410101-PTNUM-SYOKI-SEC
                   END-IF
      *            メモ登録有無
                   PERFORM 41021-PTMEMOINF-SEC
      *
      *            患者情報通知
                   PERFORM 41023-POPUP-SYORI-SEC
      *
           END-EVALUATE
           .
      *
       410101-PTNUM-IN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *   患者番号決定初期処理
      *****************************************************************
       410101-PTNUM-SYOKI-SEC         SECTION.
      *
      *    初期ＳＰＡ編集処理（システム日付を診療日とする）
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-LAST-PTID
           INITIALIZE                  SPA-LAST-PTNUM
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *    診療日に変更があったらＳＰＡを指定する
           IF      SPA-SRYYMD          =   SPA-SYSYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
      *        院外処方区分
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           ELSE
               MOVE    "1"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
           END-IF
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
      *    受付選択
           IF      WRK-MOTOPG          =   "U01"
               MOVE    "U01"               TO  LNK-SC95-PG
           ELSE
               INITIALIZE                  SPA-SRYKA
           END-IF
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
           MOVE    LNK-SC95-LAST-RENNUM    TO  WRK-SC95-LAST-RENNUM
           IF      SPA-ERRCD           =   SPACE
      *        患者決定共編集処理
               MOVE    1               TO  FLG-CHKSET
               PERFORM 4102-PTNUM-HENSYU-SEC
           ELSE
               PERFORM 310-SPA-SET-SEC
               IF      SPA-ERRCD           =   "1999"
                                           OR  "9999"
                   MOVE    1               TO  FLG-END2
                   MOVE    90              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
      *        システム管理よりカーソル指定
               EVALUATE    SPA-GAI-CURSOR-SET
               WHEN    "1"
      *            保険組合せ
                   MOVE    05              TO  SPA-GMN-CUR
               WHEN    "2"
               WHEN    "3"
      *            診療科
                   MOVE    06              TO  SPA-GMN-CUR
               END-EVALUATE
               IF      LNK-WKSRYACT-MAX        >   ZERO
                   MOVE    01              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       410101-PTNUM-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者決定共編集処理
      *****************************************************************
       4102-PTNUM-HENSYU-SEC         SECTION.
      *
      *    スパ共通編集処理
           IF      SPA-NAI-NYUIN   NOT =   1
               PERFORM 310-SPA-SET-SEC
           END-IF
      *    誕生日以前の時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               IF      SPA-SRYYMD          <   SPA-BIRTHDAY
                    MOVE    "1011"         TO  SPA-ERRCD
                    MOVE    7              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *    チェック用月内診療行為セット
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
              AND (FLG-CHKSET          =   1     )
               PERFORM                 320-SPA-CHKSET-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
      *        継続中の判定（現在なし）
               IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
                  MOVE    "0103"          TO  SPA-KIDCD
               END-IF
           END-IF
      *    保険番号が未確定の時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    "1006"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-HKN-ERR
               END-IF
           END-IF
      *!!
      *    受診履歴が９まで登録済みの時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               PERFORM 41022-JYURRK-NUM-CHK-SEC
           END-IF
      *!!
      *    受診履歴が３で、通常の時、警告
      *ver.4.8 から解除
      *    IF     (SPA-ERRCD           =   SPACE )
      *       AND (SPA-KIDCD           =   SPACE )
      *        IF     (SPA-RRK-LSTYMD       =   SPA-SRYYMD )  AND
      *               (WRK-SC95-LAST-RENNUM    >=  3      )
      *            MOVE    "0106"          TO  SPA-KIDCD
      *        END-IF
      **** END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
      *
      *        パラメタファイルより表示
               MOVE    3                   TO  FLG-PARASET
               PERFORM 3002-PARA-SET-SEC
      *
      *        画面用スパ編集処理
               MOVE    ZERO                TO  FLG-TEISEI
               PERFORM 310-SPA-GMNSET-SEC
      *
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               MOVE    1               TO  SPA-MAP-IDX
           END-IF
      *
           .
       4102-PTNUM-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ登録有無処理
      *****************************************************************
       41021-PTMEMOINF-SEC         SECTION.
      *
           MOVE    SPACE               TO  PTMEMOINF-REC
           INITIALIZE                  PTMEMOINF-REC
           MOVE    SPA-HOSPNUM         TO  PTMEMOINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTMEMOINF-PTID
      *D   MOVE    2                   TO  PTMEMOINF-MEMOKBN
           MOVE    SPA-SYSYMD          TO  PTMEMOINF-SYSYMD
      *
           MOVE    PTMEMOINF-REC       TO  MCPDATA-REC
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-PTMEMOINF-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTMEMOINF
               INITIALIZE                  PTMEMOINF-REC
           END-IF
           MOVE    "tbl_ptmemoinf"     TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-PTMEMOINF       =   ZERO
               MOVE    "1"             TO  SPA-NAI-MEMOKBN
           ELSE
               MOVE    SPACE           TO  SPA-NAI-MEMOKBN
           END-IF
      *
           .
       41021-PTMEMOINF-EXT.
           EXIT.
      *****************************************************************
      *    診療履歴連番チェック処理
      *****************************************************************
       41022-JYURRK-NUM-CHK-SEC         SECTION.
      *
           PERFORM VARYING     IDX     FROM    1  BY  1
                   UNTIL      (IDX     >   SPA-GMN-MAX1 )
                          OR  (SPA-ERRCD   NOT =   SPACE)
      *        診療日より後の履歴は対象外
               IF      (SPA-SRYYMD         =   SPA-NAI-RRKYMD (IDX) )
                 AND   (SPA-SRYKA          =   SPA-NAI-RRK-SRYKA (IDX))
                   IF      SPA-NAI-RENNUM (IDX)    =   9
                       MOVE    "0220"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-HKN-ERR
                       MOVE    6                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF      (SPA-SRYYMD         >   SPA-NAI-RRKYMD (IDX))
                   MOVE    SPA-GMN-MAX1        TO  IDX
               END-IF
           END-PERFORM
      *
           .
       41022-JYURRK-NUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    患者情報通知処理
      *****************************************************************
       41023-POPUP-SYORI-SEC         SECTION.
      *
           INITIALIZE                  SPA-GMN-POPUP-STATUS
      *
           IF     (SPA-POPUP-HKNCHK    =   "1" )
             OR   (SPA-POPUP-MEMO1     =   "1" )
             OR   (SPA-POPUP-MEMO2     =   "1" )
               INITIALIZE                  ORCSPOPUPSETAREA
      *        保険最終確認日表示
               MOVE    SPA-POPUP-HKNCHK    TO  ORCSPOPUPSET-HKNCHK
      *        メモ１
               MOVE    SPA-POPUP-MEMO1     TO  ORCSPOPUPSET-MEMO1
      *        メモ２
               MOVE    SPA-POPUP-MEMO2     TO  ORCSPOPUPSET-MEMO2
               CALL    "ORCSPOPUPSET"      USING
                                           SPA-AREA
                                           ORCSPOPUPSETAREA
      *
               IF     ORCSPOPUPSET-BODY    NOT =   SPACE
                   MOVE    ORCSPOPUPSET-BODY   TO  SPA-GMN-POPUP-BODY
                   MOVE    "患者情報"          TO  SPA-GMN-POPUP-SUMMARY
                   MOVE    SPACE               TO  SPA-GMN-POPUP-ICON
                   MOVE    SPA-POPUP-TIMEOUT   TO  SPA-GMN-POPUP-TIMEOUT
               END-IF
           END-IF
      *
           .
       41023-POPUP-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療日入力処理
      *****************************************************************
       41017-SRYYMD-SYORI-SEC         SECTION.
      *
           MOVE    7                   TO  SPA-GMN-CUR
           MOVE    ZERO                TO  SPA-NAI-ROUSTR
      *
      *    訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02-SRYYMD      NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
               GO      TO      41017-SRYYMD-SYORI-EXT
           END-IF
      *
           MOVE    K02-SRYYMD          TO  SPA-GMN-SRYYMD
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
           ELSE
               MOVE    "1010"              TO  SPA-ERRCD
           END-IF
           IF    SPA-ERRCD             =   SPACE
      *        患者の確定してない時
               IF      SPA-PTID            =   ZERO
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   MOVE    ZERO                TO  SPA-GMN-CUR
                   GO      TO      41017-SRYYMD-SYORI-EXT
               END-IF
      *        誕生日以前の時、エラー
               IF      SGDAY-SDAY           <   SPA-BIRTHDAY
                    MOVE    "1011"          TO  SPA-ERRCD
               END-IF
               MOVE   SPACE                TO  SPA-KIDCD
      *        未来日は確認メッセージ
               IF      SGDAY-SDAY          >   SPA-SYSYMD
                   MOVE    "0111"              TO  SPA-KIDCD
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
                   MOVE    ZERO                TO  SPA-HKN-ERR
                   MOVE    ZERO                TO  SPA-RJN-ERR
                   MOVE    ZERO                TO  SPA-NAI-KOHFLG-ERR
                   MOVE    ZERO                TO  SPA-NAI-KOHFLG
                   MOVE    ZERO                TO  SPA-NAI-ZENKIFLG-ERR
                   MOVE    ZERO                TO  SPA-NAI-ZENKIFLG
                   MOVE    ZERO                TO  SPA-NAI-ROUSTR
               END-IF
           END-IF
           IF    (SPA-ERRCD            =   SPACE )  AND
                 (SPA-KIDCD            =   SPACE )
      *        変更のない時、以降の処理はそのまま
               IF      SGDAY-SDAY          =   SPA-NAI-SRYYMD
                    MOVE    1                  TO  SPA-GMN-CUR
               ELSE
                   MOVE    SGDAY-SDAY          TO  SPA-NAI-SRYYMD
      *            診療日確定後、編集
                   PERFORM 410171-SRYYMD-OK-SEC
               END-IF
           END-IF
           .
       41017-SRYYMD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日確定後、編集処理
      *****************************************************************
       410171-SRYYMD-OK-SEC         SECTION.
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           INITIALIZE                  SPA-DENPNUM
           INITIALIZE                  SPA-SYORIFLG
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    "2"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    LNK-SC95-LAST-RENNUM    TO  WRK-SC95-LAST-RENNUM
      *    患者決定共編集処理
           MOVE    1               TO  FLG-CHKSET
           PERFORM 4102-PTNUM-HENSYU-SEC
      *
           .
       410171-SRYYMD-OK-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号入力処理
      *****************************************************************
       41012-HKNCOMBI-SYORI-SEC         SECTION.
      *
           MOVE    5                   TO  SPA-GMN-CUR
      *
      *    保険番号入力
           IF    ( K02-HKNCOMBI        =   SPA-GMN-HKNCOMBI-G)  OR
                 ( K02-PTNUM           =   SPACE             )
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41012-HKNCOMBI-SYORI-EXT
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
      *        訂正で、継続確定の時、変更不可
               IF      (SPA-CONTKBN        =   "1"   OR  "2")  AND
                       (SPA-DENPJTIKBN     =   "1"   )
                   MOVE    "1013"              TO  SPA-ERRCD
                   GO      TO      41012-HKNCOMBI-SYORI-EXT
               END-IF
      *
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    SPA-CHIKENKBN       TO  SPA-MAE-CHIKENKBN
               MOVE    SPA-NAI-ROUJIN      TO  SPA-MAE-ROUJIN
               MOVE    K02-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
      *        包括まとめの変更不可
               IF      (SPA-GMN-HKNCOMBINUM     =   "9999" )  OR
                       (SPA-MAE-HKNCOMBINUM     =   "9999" )
                   MOVE    "1033"              TO  SPA-ERRCD
                   MOVE    SPA-MAE-HKNCOMBI-G  TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-MAE-HKNKBN      TO  SPA-HKNKBN
                   MOVE    SPA-MAE-CHIKENKBN   TO  SPA-CHIKENKBN
                   GO      TO      41012-HKNCOMBI-SYORI-EXT
               END-IF
      *
               MOVE    1                   TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
               IF      SPA-KIDCD           =   SPACE
      *            訂正の時、確認メッセージ表示
                   MOVE    "0105"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    SPA-GMN-HKNCOMBI-G  TO  SPA-MAE-HKNCOMBI-G
               MOVE    K02-HKNCOMBI        TO  SPA-GMN-HKNCOMBI-G
               MOVE    SPA-HKNKBN          TO  SPA-MAE-HKNKBN
               MOVE    SPA-CHIKENKBN       TO  SPA-MAE-CHIKENKBN
               MOVE    SPA-NAI-ROUJIN      TO  SPA-MAE-ROUJIN
               MOVE    ZERO                TO  FLG-CHK2
               PERFORM 41012-HKNCOMBI-CHANGE-SEC
           END-IF
           .
       41012-HKNCOMBI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険番号変更入力処理
      *****************************************************************
       41012-HKNCOMBI-CHANGE-SEC         SECTION.
      *
           IF    ((SPA-RJN-ERR         =   ZERO )  AND
                  (SPA-HKN-ERR         =   ZERO
                                       OR  4    ) )
              OR ( SPA-MAE-HKNCOMBINUM =   ZERO  )
               MOVE    ZERO                TO  FLG-HKNCHK
           ELSE
               MOVE    1                   TO  FLG-HKNCHK
           END-IF
      *    保険組合
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41012-HKNCOMBI-CHANGE-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-PAGE
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    包括まとめで変更があった場合再編集
           IF     (SPA-GMN-HKNCOMBINUM     =   "9999" )  OR
                  (SPA-MAE-HKNCOMBINUM     =   "9999" )
               PERFORM 410122-HOUKATU-HKNCOMBI-SEC
               GO      TO      41012-HKNCOMBI-CHANGE-EXT
           END-IF
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-SYORIFLG        =   1   )  AND
                  (SPA-HKNKBN          =   SPA-MAE-HKNKBN)  AND
                  (SPA-NAI-ROUJIN      =   SPA-MAE-ROUJIN)
      *            労災の時、初回算定日を再編集する
             AND  (SPA-HKNKBN          =   ZERO)
      *           変更前がエラーでないこと
             AND  (FLG-HKNCHK          =   ZERO )
               CONTINUE
           ELSE
               PERFORM 410121-HKNCOMBI-SAI-SEC
           END-IF
      *
      *    保険・診療科の再編集
           PERFORM 410121-COM-HKNCOMBI-SET-SEC
      *
           .
       41012-HKNCOMBI-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険・診療科の再編集処理
      *****************************************************************
       410121-COM-HKNCOMBI-SET-SEC         SECTION.
      *
           MOVE    ZERO                TO  FLG-HKN-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX              >   SPA-MAX-LINE  )   OR
                             ((SPA-GMN-INPUTCD(IDX) =   SPACE ) AND
                              (SPA-COM-INPUTCD(IDX) =   SPACE ))  OR
                              (FLG-HKN-ARI          =   1     )
               IF      SPA-GMN-INPUTCD (IDX)   =   "#"
                   MOVE    1                   TO  FLG-HKN-ARI
               END-IF
               IF     (FLG-HKN-ARI             =   ZERO  )  AND
                      (SPA-COM-HKNCOMBI (IDX)  =   SPA-MAE-HKNCOMBINUM)
                   MOVE    SPA-HKNCOMBINUM    TO  SPA-COM-HKNCOMBI
                                                               (IDX)
               END-IF
           END-PERFORM
      *
           .
       410121-COM-HKNCOMBI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分入力変換処理
      *****************************************************************
       410122-HOUKATU-HKNCOMBI-SEC         SECTION.
      *
      *    包括まとめの時、フラグクリア
      *    寝たきり老人在宅総合診療科
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI
           MOVE    ZERO                TO  SPA-NETAKIRI-ARI2
      *    在宅末期医療総合診察料
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI
           MOVE    ZERO                TO  SPA-ZAIMATU-ARI2
      *    生活習慣病
           MOVE    ZERO                TO  SPA-SEIKATU-ARI
           MOVE    ZERO                TO  SPA-SEIKATU-ARI2
      *    慢性疼痛疾病管理料
           MOVE    ZERO                TO  SPA-TOKUTEI-KNJ2
      *    痴呆患者在宅指導管理料（外来加算付加算定付加）
           MOVE    ZERO                TO  SPA-GAIRAI-NASI
      *    消炎鎮痛等処置
           MOVE    ZERO                TO  SPA-SYOEN-CNT
      *
           PERFORM 410121-HKNCOMBI-SAI-SEC
           .
       410122-HOUKATU-HKNCOMBI-EXT.
           EXIT.
      *****************************************************************
      *    保険区分変換処理
      *****************************************************************
       410121-HKNCOMBI-SAI-SEC         SECTION.
      *
      *    複数科まとめチェック
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-HKNCOMBINUM     =   SPA-FUKU-HKNCOMBI
                   MOVE    SPA-FUKU-MATKBN     TO  SPA-FUKU-SYORI
               END-IF
           END-IF
      *
           MOVE    SPA-WKSRY-HKNCOMBI  TO  WRK-WKSRY-HKNCOMBI
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "7"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *    ワーク診療行為に存在あり
           IF      SPA-WKSRY-HKNCOMBI  NOT =   ZERO
               MOVE    "0118"          TO  SPA-KIDCD
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
           MOVE    WRK-WKSRY-HKNCOMBI  TO  SPA-WKSRY-HKNCOMBI
      *    訂正の時、確認メッセージ表示へ
           IF      FLG-CHK2            =   1
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
      *
      *    訂正でなく、同日に他の保険を算定している時、メッセージ表示
           IF     (SPA-SYORIFLG        =   ZERO        )
      *????  AND  (SPA-LSTYMD          =   SPA-SRYYMD  )
               PERFORM 4101213-RRKHKNCHK-SEC
               IF      SPA-KIDCD       NOT =   SPACE
                   GO      TO      410121-HKNCOMBI-SAI-EXT
               END-IF
           END-IF
      *
      *    訂正でなく、包括保険に変更した場合、メッセージ表示
           IF      SPA-GMN-HKNCOMBINUM     =   "9999"
               MOVE    "0190"              TO  SPA-KIDCD
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
      *
      *!!!!
      *H24.5
           IF     (SPA-MAE-HKNCOMBINUM =   "0000" )
             AND  (SPA-SRYYMD          >=  "20120401" )
      *    前保険組合せがゼロの時、老人区分の変更なし
               MOVE    SPA-NAI-ROUJIN      TO  SPA-MAE-ROUJIN
           END-IF
      *!!
      *H25.1
      *    入院中は再編集なし
           IF      SPA-NAI-NYUIN       =   1
               GO      TO      410121-HKNCOMBI-SAI-EXT
           END-IF
      *
      *
      *    保険区分が変更された時、再編集を行う
           IF     (SPA-HKNKBN          =   SPA-MAE-HKNKBN  )  AND
                  (SPA-HKNKBN          =   ZERO            )  AND
                  (SPA-NAI-ROUJIN      =   SPA-MAE-ROUJIN)
             AND  (SPA-MAE-HKNCOMBINUM NOT =   "9999"  )
             AND  (FLG-HKNCHK          =   ZERO )
             AND  (SPA-CHIKENKBN       =   SPA-MAE-CHIKENKBN )
               CONTINUE
           ELSE
               IF     (SPA-CHIKENKBN   NOT =   SPA-MAE-CHIKENKBN )
               AND    (SPA-CHIKENKBN       =   "1"     )
                   CONTINUE
               ELSE
                   PERFORM 410131-SINSATUHEN-SEC
               END-IF
           END-IF
      *
           .
       410121-HKNCOMBI-SAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    同日再診他保険の算定チェック処理
      *****************************************************************
       4101213-RRKHKNCHK-SEC         SECTION.
      *
      *    包括分入力は対象外
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      4101213-RRKHKNCHK-EXT
           END-IF
      *
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX-HKN     FROM    1   BY  1
                   UNTIL      (IDX-HKN     >   10  )  OR
                              (SPA-TA-HKNCOMBI (IDX-HKN)
                                           =   ZERO )
               IF     SPA-TA-HKNCOMBI (IDX-HKN)
                                           NOT =   SPA-GMN-HKNCOMBINUM
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
      *    他科受診済みがないこと
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE           )  OR
                              (SPA-COM-INPUTCD(IDX) =   SPACE  AND
                               SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF     (SPA-COM-INPUTCD (IDX)   =   "830000020"   OR
                                                   "830000021" )  AND
                      (SPA-COM-SRYKBN  (IDX)   =   "12"  OR  "11")
                   MOVE    ZERO                TO  FLG-OK
               END-IF
           END-PERFORM
      *
           IF     (FLG-OK              =   1   )
           AND    (SPA-INITAUTO-FLG    =   "1" )
      *    入院中以外
           AND    (SPA-NAI-NYUIN       =   ZERO )
      *        他保険あり確認メッセージ表示
               MOVE    "0119"              TO  SPA-KIDCD
           END-IF
           .
       4101213-RRKHKNCHK-EXT.
           EXIT.
      *****************************************************************
      *    診察料再編集処理
      *****************************************************************
       410131-SINSATUHEN-SEC         SECTION.
      *
           MOVE    SPA-NAI-ROUJIN      TO  WRK-ROUJIN
      *    スパ共通編集処理
           PERFORM                     310-SPA-SET-SEC
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           IF     (SPA-NAI-ROUJIN  NOT =   WRK-ROUJIN )  OR
                  (SPA-NAI-ROUJIN  NOT =   SPA-MAE-ROUJIN) OR
                  (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN )
               PERFORM 4101311-ROUJINCHK-SEC
           END-IF
      *
           IF     (SPA-HKNKBN      NOT =   SPA-MAE-HKNKBN ) OR
                  (SPA-HKNKBN      NOT =   ZERO           )
      *        リハビリ情報クリア
               INITIALIZE              SPA-RIGAKU-NCNT-AREA
           END-IF
      *    他保険受診がある時、診察料はないものとする
           IF      SPA-TAHOKEN-FLG     =   1
               GO      TO      410131-SINSATUHEN-EXT
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "S"                 TO  LNK-SC10S-SYORI
           IF      SPA-TAHOKEN-FLG     =   9
      *        他保険ありの時
               MOVE    "H"                 TO  LNK-SC10S-SYORI
           END-IF
           IF      SPA-TAHOKEN-FLG     =   8
      *        他保険・診療科ありの時、診察料のみ置換え
               MOVE    "J"                 TO  LNK-SC10S-SYORI
           END-IF
           IF      SPA-TAHOKEN-FLG     =   2
      *        科変更時
               MOVE    "K"                 TO  LNK-SC10S-SYORI
           END-IF
      *
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    ZERO                TO  SPA-TAHOKEN-FLG
           IF      SPA-TAHOKEN-FLG     =   2
               CONTINUE
           ELSE
               IF      SPA-KIDCD           =   "0109"
                   MOVE    SPACE               TO  SPA-KIDCD
               END-IF
           END-IF
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       410131-SINSATUHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    チェックフラグクリア処理
      *****************************************************************
       4101311-ROUJINCHK-SEC         SECTION.
      *
      *    老人区分が変更された時、チェックフラグをクリアする
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               MOVE    SPACE               TO  SPA-COM-CHKFLG (IDX)
               MOVE    SPACE               TO  SPA-COM-HCHKFLG (IDX)
           END-PERFORM
           .
       4101311-ROUJINCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       410132-SAIHENSYU-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           MOVE    1                   TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
      *
      *    患者決定共編集処理（チェック用月内診療行為セットなし）
           MOVE    ZERO                TO  WRK-SC95-LAST-RENNUM
           MOVE    ZERO                TO  FLG-CHKSET
           PERFORM 4102-PTNUM-HENSYU-SEC
      *
           .
       410132-SAIHENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科入力処理
      *****************************************************************
       41013-SRYKAI-SYORI-SEC         SECTION.
      *
           MOVE    6                   TO  SPA-GMN-CUR
      *
      *    診療科変更あり
           IF      K02-SRYKA           =   SPA-GMN-SRYKA-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
      *H25.7
               IF      SPA-GAI-CURSOR-SET  =   "3"
      *            ドクターへ
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
           IF      SPA-PTID            =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR
               MOVE    K02-SRYKA           TO  SPA-GMN-SRYKA-G
               GO      TO      41013-SRYKAI-SYORI-EXT
           END-IF
      *    訂正の時、エラー
           IF      SPA-SYORIFLG        =   1
      *        訂正で継続のある時、エラーとする
               IF      (SPA-CONTKBN        =   "1"   OR  "2")  AND
                       (SPA-DENPJTIKBN     =   "1"   )
                   MOVE    "1029"              TO  SPA-ERRCD
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    SPA-GMN-SRYKA-G     TO  SPA-MAE-SRYKA-G
           MOVE    K02-SRYKA           TO  SPA-GMN-SRYKA-G
      *    診療科
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-SRYKA-MAX
               IF      SPA-GMN-SRYKA   =   SPA-GMN-SRYKAL (IDX)
                   MOVE    SPA-GMN-SRYKALST (IDX)
                                           TO  SPA-GMN-SRYKA-G
                   MOVE    SPA-SRYKA-MAX   TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *    科が無い時、前の科に戻す
           IF      FLG-CHK             =   ZERO
               IF      SPA-MAE-SRYKA       =   SPACE
                   MOVE    SPA-GMN-SRYKALST (1)
                                           TO  SPA-GMN-SRYKA-G
               ELSE
                   MOVE    SPA-MAE-SRYKA-G TO  SPA-GMN-SRYKA-G
                   GO      TO      41013-SRYKAI-SYORI-EXT
               END-IF
           END-IF
      *
           IF      SPA-HKN-ERR         =   5
               MOVE    ZERO                TO  SPA-HKN-ERR
           END-IF
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "0123"              TO  SPA-KIDCD
           ELSE
               IF      SPA-GMN-MAX         >   ZERO
                   MOVE    "0107"              TO  SPA-KIDCD
               ELSE
                   PERFORM 41013-SRYKA-CHANGE-SEC
               END-IF
           END-IF
      *
           IF    SPA-ERRCD             =   SPACE
               IF      SPA-GAI-CURSOR-SET  =   "3"
      *            ドクターへ
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       41013-SRYKAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE-SEC         SECTION.
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
           MOVE    0                   TO  SPA-GMN-FLG
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
      *        診療内容を残さない時、展開中のクリア
               IF      SPA-KID1-FLG        =   "OK"
                   MOVE    1               TO  SPA-GMN-FLG
               END-IF
      *        保険組合せ変更判定
               MOVE    SPA-K02             TO  HZN-SPA-K02
               MOVE    SPA-K01KYOUTU       TO  HZN-SPA-K01KYOUTU
               MOVE    SPA-AREA            TO  WRK-SPA-AREA
               MOVE    SPACE               TO  WRK-SPA-HKNCOMBINUM
               MOVE    SPACE               TO  WRK-SPA-DRCD
               MOVE    SPACE               TO  HZN-SPA-DRCD-NAME
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
               MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
               MOVE    "L"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           WRK-SPA-AREA
                                           HZN-SPA-K01KYOUTU
                                           HZN-SPA-K02
               IF      SPA-HKNCOMBINUM     =   WRK-SPA-HKNCOMBINUM
      *            保険変更がない時、処理を行う
                   MOVE    SPACE               TO  SPA-KID1-FLG
                   MOVE    WRK-SPA-DRCD        TO  SPA-DRCD
                   MOVE    HZN-SPA-DRCD-NAME   TO  SPA-DRCD-NAME
                   PERFORM 41013-SRYKA-CHANGE02-SEC
               ELSE
                   MOVE    "0138"              TO  SPA-KIDCD
               END-IF
           ELSE
               PERFORM 41013-SRYKA-CHANGE02-SEC
           END-IF
           .
       41013-SRYKA-CHANGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科変換処理
      *****************************************************************
       41013-SRYKA-CHANGE02-SEC         SECTION.
      *
      *    対象の科の保険組合せにする
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-KID1-FLG        =   "OK"
                   MOVE    SPACE               TO  SPA-HKNCOMBINUM
               END-IF
               MOVE    SPACE               TO  SPA-DRCD
               MOVE    SPACE               TO  SPA-DRCD-NAME
      *        診療内容を残さない時、展開中のクリア
      *        IF      SPA-KID1-FLG        =   "OK"
               IF      SPA-GMN-FLG         =   1
                   CONTINUE
               ELSE
                   MOVE    ZERO            TO  FLG-MOD-FLG
                   PERFORM 4900-WKSRYACT-CLEAR-SEC
               END-IF
           END-IF
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
           MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
           MOVE    "K"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    複数科まとめチェック
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-HKNCOMBINUM     =   SPA-FUKU-HKNCOMBI
                   MOVE    SPA-FUKU-MATKBN     TO  SPA-FUKU-SYORI
               END-IF
           END-IF
      *
           MOVE    WRK-SCR-REC         TO  SPA-SCR-REC
      **** IF     (SPA-KID1-FLG    NOT =   "OK"  )  AND
           IF     (SPA-GMN-FLG         =   0     )  AND
                  (SPA-SYORIFLG        =   ZERO  )
               INITIALIZE                  SPA-SCR-REC
           END-IF
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
      *!!
      *    受診履歴が９まで登録済みの時、エラー
           IF     (SPA-ERRCD           =   SPACE )
              AND (SPA-KIDCD           =   SPACE )
               PERFORM 41022-JYURRK-NUM-CHK-SEC
           END-IF
      *!!
      *    訂正の時、以下の処理なし
           IF      SPA-SYORIFLG        =   1
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
      *
      *    再表示編集処理
           IF      LNK-SC95-WKSRYACT   =   1
               MOVE    "0122"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
           IF      LNK-SC95-WKSRYACT   =   2
               MOVE    "0124"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
      *
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
           MOVE    SPA-GMN-SRYKAMEI    TO  SPA-SRYKAMEI
      *
           IF      LNK-SC95-DOUJI-RENNUM   NOT =   ZERO
               MOVE    "0103"          TO  SPA-KIDCD
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
      *    保険番号が未確定の時、エラー
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
      *    受診履歴が３で、通常の時、警告
      *ver.4.8 から解除
      *    IF     (SPA-RRK-LSTYMD          =   SPA-SRYYMD )  AND
      *           (LNK-SC95-LAST-RENNUM    >=  3      )
      *        MOVE    "0106"          TO  SPA-KIDCD
      *        GO      TO      41013-SRYKA-CHANGE02-EXT
      **** END-IF
      *    保険組合編集
           MOVE    SPA-HKNCOMBINUM     TO  SPA-GMN-HKNCOMBINUM
           PERFORM 3101-HKNCOMBI-HEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )
               OR (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41013-SRYKA-CHANGE02-EXT
           END-IF
      *
      *    診察料のみ変換する
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "K"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-KIDCD           TO  WRK-KIDCD2
      *
      *    診療行為、計計算処理、編集
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           MOVE    ZERO            TO  SPA-SAISIN-CHK
      *
           MOVE    1               TO  SPA-GMN-PAGE
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-MAP-IDX
           MOVE    WRK-KIDCD2      TO  SPA-KIDCD
           .
       41013-SRYKA-CHANGE02-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクター入力処理
      *****************************************************************
       41013-DRCD-SYORI-SEC         SECTION.
      *
           MOVE    10                   TO  SPA-GMN-CUR
      *
      *    変更あり
           IF      K02-DRCD            =   SPA-GMN-DRCD-G
               MOVE    1               TO  SPA-GMN-PAGE
               MOVE    1               TO  SPA-GMN-CUR
               GO      TO      41013-DRCD-SYORI-EXT
           END-IF
      *H28.01
      *    医師一覧へ
           IF      K02-DRCD (1:1)      =   "*"
               PERFORM 410131-UDR-SYORI-SEC
               GO      TO      41013-DRCD-SYORI-EXT
           END-IF
      *!!
           IF      SPA-PTID            =   ZERO
               MOVE    ZERO                TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
               GO      TO      41013-DRCD-SYORI-EXT
           END-IF
      *
           MOVE    SPA-GMN-DRCD-G      TO  WRK-GMN-DRCD-G
           MOVE    K02-DRCD            TO  SPA-GMN-DRCD-G
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-DRCD-MAX
               IF      SPA-GMN-DRCD    =   SPA-GMN-DRCDL (IDX)
                   MOVE    SPA-GMN-DRCDLST (IDX)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    SPA-DRCD-MAX    TO  IDX
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-GMN-DRCD-G
               MOVE    SPACE               TO  SPA-DRCD-NAME
               MOVE    SPACE               TO  SPA-DRCD
               MOVE    1                   TO  SPA-GMN-CUR
           ELSE
               IF      FLG-CHK             =   ZERO
                   PERFORM 410131-DRCD-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   MOVE    SPA-GMN-DRCD-G      TO  SPA-DRCD-NAME
                   MOVE    "1"                 TO  WRK-DRCD1
                   MOVE    SPA-GMN-DRCD        TO  WRK-DRCD2
                   MOVE    WRK-DRCD            TO  SPA-DRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       41013-DRCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       410131-DRCD-CHK-SEC                   SECTION.
      *
      *    ドクター編集
           INITIALIZE                  ORCSDRSETAREA
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-SRYYMD          TO  ORCSDR-SYSYMD
           MOVE    SPA-SRYKA           TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD        TO  ORCSDR-IN-DRCD
           MOVE    SPA-GMN-DRCDLST-G   TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           IF      ORCSDR-ERRCD        =   ZERO
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCDLST-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (SPA-DRCD-MAX)
                                           TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    WRK-GMN-DRCD-G      TO  SPA-GMN-DRCD-G
               MOVE    "1054"              TO  SPA-ERRCD
           END-IF
      *
           .
       410131-DRCD-CHK-EXT.
           EXIT.
      *
      *
      *H28.01
      *****************************************************************
      *    選択一覧処理
      *****************************************************************
       410131-UDR-SYORI-SEC         SECTION.
      *
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG3
           MOVE    "UDR"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "CHANGE"             TO  MCP-PUTTYPE.
           MOVE   "UDR"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
           .
      *
       410131-UDR-SYORI-EXT.
           EXIT.
      *!!! 
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-KOUI-ALLHEN-SEC               SECTION.
      *
      *    患者の入力チェック
           IF      SPA-PTID            =   ZERO
               IF      K02-INPUTCD(1)      NOT =   SPACE
                   MOVE    00              TO  SPA-GMN-CUR
                   MOVE    "1005"          TO  SPA-ERRCD
                   GO      TO      41011-KOUI-ALLHEN-EXT
                END-IF
           END-IF
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
      *    画面→ＳＰＡ編集処理
           PERFORM 410110-INPUT-GMNSET-SEC
      *
      *!!!!!!!!!!!!!!!!!!
      *!!!!!!!!!!!!!!!!!!
      *!!  連続挿入
           IF     (FLG-INPUT-NASI2      =   1  )  AND
                  (SPA-RENZ-FLG         =   1  )
               MOVE    ZERO                TO  SPA-RENZ-FLG
               MOVE    ZERO                TO  FLG-INSEND
               PERFORM VARYING     IDX     FROM   1    BY  1
                       UNTIL     ( IDX     >   SPA-MAX-LINE)
                           OR    ( FLG-INSEND  =   1       )
                   IF     (SPA-COM-GMNINS (IDX)    =   "1" )
                   AND    (SPA-GMN-INPUTCD (IDX)   =  SPA-RENZ-INPUT)
                       MOVE    1                   TO  FLG-INSEND
                       IF      SPA-GMN-INPUTCD (IDX - 1)
                                              NOT  =   SPACE
                           MOVE    IDX                 TO  SPA-GMN-IDX
                           MOVE    1                   TO  WRK-INSCNT
                           PERFORM 41014-GYOINSN-SEC
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    "3"             TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
                               ADD     1               TO  SPA-GMN-IDX
                               MOVE    1               TO  SPA-RENZ-FLG
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF      SPA-RENZ-FLG        =   ZERO
                   MOVE    SPACE           TO  SPA-RENZ-INPUT
                   PERFORM VARYING     IDX     FROM   1    BY  1
                           UNTIL     ( IDX     >   SPA-MAX-LINE)
                       MOVE    SPACE           TO  SPA-COM-GMNINS (IDX)
                   END-PERFORM
               ELSE
                   GO      TO  41011-KOUI-ALLHEN-EXT
               END-IF
           END-IF
      *!!!!!!!!!!!!!!!!!!
      *
           IF      FLG-INPUT-NASI      =   1
      *        空白行をなくす
               PERFORM 420-SYOKYO-SEC
      *        カーソル位置
               PERFORM 410119-SAIINPUT-SEC
           ELSE
      *        入力コード・名称チェック処理
               MOVE    ZERO                TO  FLG-FUKU
               PERFORM 410112-KOUI-SPACHK-SEC
           END-IF
      *
           .
       41011-KOUI-ALLHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面→ＳＰＡ編集処理
      *****************************************************************
       410110-INPUT-GMNSET-SEC               SECTION.
      *
      *    入力コード入力時、カーソルの保存
           IF     (MCP-EVENT           =   "ACTIVATE" ) AND
                  (WRK-WIDGET          =   "INPUTCD"  )
               MOVE    WRK-IDX2            TO  WRK-IN-MAP-IDX
           ELSE
               MOVE    ZERO                TO  WRK-IN-MAP-IDX
           END-IF
      *
      *    前回のＳＰＡを保存する
           MOVE    SPA-SCR-REC         TO  MAE-SCR-REC
      *
           MOVE    1                   TO  FLG-INPUT-NASI
      *!!!!!!!!!!!!!!!!!!
           MOVE    1                   TO  FLG-INPUT-NASI2
      *!!!!!!!!!!!!!!!!!!
      *    カーソル位置のクリア等
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-CUR (IDX)   =   "1"  OR  "2"
                   IF      SPA-GMN-INPUTCD (IDX)   NOT =   SPACE
                       MOVE    ZERO                TO  FLG-INPUT-NASI
                   END-IF
               END-IF
      *        数量チェック
               IF      SPA-COM-SURCHK (IDX)    =   "2"
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
               MOVE    SPACE               TO  SPA-COM-CUR (IDX)
               MOVE    SPACE               TO  SPA-COM-IN  (IDX)
               MOVE    SPACE               TO  SPA-COM-IN2 (IDX)
               MOVE    SPACE               TO  SPA-COM-IN4 (IDX)
           END-PERFORM
      *
           MOVE    ZERO                TO  WRK-STR
           MOVE    ZERO                TO  FLG-KUHAKU
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     WRK-IDX2    FROM    1   BY  1
                   UNTIL       WRK-IDX2    >   MAX-GMN
      *        ＳＰＡ内の行決定
               PERFORM 41011-GYOU-SET-SEC
               IF     (K02-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (K02-MEISYO  (WRK-IDX2)  NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
                   MOVE    SPACE               TO  SPA-COM-KZMFLG
                                                           (SPA-GMN-IDX)
      *************MOVE    "1"                 TO  SPA-COM-IN
      ************                                        (SPA-GMN-IDX)
                   MOVE    ZERO                TO  FLG-INPUT-NASI
      *!!!!!!!!!!!!!!!!!!
                   MOVE    ZERO                TO  FLG-INPUT-NASI2
      *!!!!!!!!!!!!!!!!!!
               END-IF
      *
      *ver.4.7
      *        今回変更位置
               IF     (K02-INPUTCD (WRK-IDX2)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX))
               AND    (K02-INPUTCD(WRK-IDX2 + 1) NOT =   SPACE )
                   MOVE    "1"                 TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
               END-IF
               IF       (K02-MEISYO  (WRK-IDX2)   NOT =
                                       SPA-GMN-MEISYO-G (SPA-GMN-IDX))
                 AND    (K02-INPUTCD(WRK-IDX2 + 1) NOT =   SPACE )
      *H30.9
                 AND   ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                               =   "8"   )  OR
                        (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                               =   "008"))
                   MOVE    "2"                 TO  SPA-COM-IN4
                                                           (SPA-GMN-IDX)
               END-IF
      *ver.4.7
      *
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           NOT =   SPA-MAE-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    ZERO                TO  FLG-INPUT-NASI
               END-IF
      *
               MOVE    K02-INPUTCD (WRK-IDX2)  TO  SPA-GMN-INPUTCD
                                                           (SPA-GMN-IDX)
               MOVE    K02-MEISYO  (WRK-IDX2)  TO  SPA-GMN-MEISYO-G
                                                           (SPA-GMN-IDX)
      *        入力行の保存
               IF     (WRK-IN-MAP-IDX  NOT =   ZERO )  AND
                      (WRK-IDX2            =   WRK-IN-MAP-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN
                                                          (SPA-GMN-IDX)
                   MOVE    "1"                 TO  SPA-COM-IN2
                                                           (SPA-GMN-IDX)
      *            診療種別判定
                   IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           =   SPA-MAE-INPUTCD
                                                         (SPA-GMN-IDX))
                     AND  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "."          )
                        MOVE    SPACE               TO  SPA-COM-IN2
                                                          (SPA-GMN-IDX)
                   END-IF
                   IF     (K02-INPUTCD (WRK-IDX2)  =   SPACE )  AND
                          (SPA-MAE-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                                   =   SPACE )  AND
                          (SPA-GMN-IDX             >   SPA-GMN-MAX)
                       MOVE    1                   TO  FLG-KUHAKU
                   END-IF
               END-IF
      *
               IF      WRK-STR             =   ZERO
                   MOVE    SPA-GMN-IDX         TO  WRK-STR
               END-IF
           END-PERFORM
      *
           IF     (SPA-GMN-IDX     >=  SPA-MAX-LINE )  AND
                  (WRK-IDX2        <=  MAX-GMN  )
               PERFORM VARYING     WRK-IDX3    FROM    WRK-IDX2 BY  1
                       UNTIL      (WRK-IDX3    >   MAX-GMN   )   OR
                                  (SPA-ERRCD   NOT =   SPACE )
                   IF      K02-INPUTCD (WRK-IDX3)  NOT =  SPACE
                       MOVE    "7004"          TO  SPA-ERRCD
                   END-IF
               END-PERFORM
           END-IF
           .
       410110-INPUT-GMNSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-GYOU-SET-SEC               SECTION.
      *    行決定
           MOVE    ZERO                TO  SPA-GMN-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-IDX2     )
                   MOVE    IDX                     TO  SPA-GMN-IDX
                   MOVE    SPA-MAX-LINE            TO  IDX
               END-IF
           END-PERFORM
      *    同一行がない時、計算で求める
           IF      SPA-GMN-IDX         =   ZERO
               COMPUTE SPA-GMN-IDX     =   (SPA-GMN-PAGE   -   1)
                                       *   MAX-GMN
                                       +   WRK-IDX2
           END-IF
      *
           .
       41011-GYOU-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡチェック処理
      *****************************************************************
       410112-KOUI-SPACHK-SEC               SECTION.
      *
           IF      WRK-STR             =   ZERO
               MOVE    1               TO  WRK-STR
           END-IF
      *
      *    変換・削除処理
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  WRK-IDX2
           MOVE    SPACE               TO  WRK-ERRCD
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
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力がない時、以下をしない
           IF      FLG-INPUT-NASI      =   1
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    入力のチェック
           MOVE    WRK-STR             TO  SPA-GMN-IDX
           MOVE    ZERO                TO  FLG-INS
           MOVE    ZERO                TO  FLG-FUKU
           MOVE    SPACE               TO  WRK-ERRCD2
           PERFORM
                   UNTIL      (SPA-GMN-IDX     >   SPA-MAX-LINE )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-END     NOT =   ZERO  )
      ***************      OR (FLG-FUKU    NOT =   ZERO  )
               IF     (SPA-MAE-INPUTCD (SPA-GMN-IDX)  NOT =
                                       SPA-GMN-INPUTCD (SPA-GMN-IDX)) OR
                      (SPA-COM-IN2 (SPA-GMN-IDX)   =   "1"  )
                   PERFORM 41011-KOUI-HEN2-SEC
               END-IF
      *        フリーコメントの名称の全角チェックはすべて行う
               IF     (SPA-ERRCD           =   SPACE )
                   IF    ((SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "810000001")  OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "008500000" ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                           =   "83" OR "84") OR
      *R02.3
                          (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =  "85"   ) OR
                          (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                           =   "0083" 
                                           OR  "0084"
                                           OR  "0086"
                                           OR  "0085"    ))  AND
                          (SPA-GMN-MEISYO-G(SPA-GMN-IDX) 
                                       NOT =   SPACE  )
                       PERFORM 401131-MEISYO-HENKAN-SEC
                   END-IF
               END-IF
               IF     (SPA-ERRCD       =   SPACE )  AND
                      (FLG-END         =   ZERO  )
                   ADD     1                   TO  SPA-GMN-IDX
               END-IF
           END-PERFORM
      *
           IF     (FLG-END             =   ZERO ) AND
                  (FLG-FUKU            =   1    )
               MOVE    SPA-ERRCD           TO  WRK-HZN-ERRCD
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    SPA-K01KYOUTU       TO  SPA-WORK
               INITIALIZE                      ORCSC10SAREA
               MOVE    "A"                 TO  LNK-SC10S-SYORI
               CALL    "ORCSC10S"          USING
                                           ORCSC10SAREA
                                           SPA-AREA
                                           SPA-SCR-REC
                                           SPA-K02
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
               IF      SPA-ERRCD           =   SPACE
                   MOVE    WRK-HZN-ERRCD       TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   "0021"
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
               GO      TO      410112-KOUI-SPACHK-EXT
           END-IF
      *
      *    数量ゼロエラー
           IF      WRK-ERRCD2      NOT =   SPACE
               MOVE    WRK-ERRCD2          TO  SPA-ERRCD
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
      *    点数計算・剤別チェック処理
           MOVE    SPACE           TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
      *
           IF     (SPA-KIDCD       =   SPACE  )  AND
                  (SPA-ERRCD       =   SPACE
                                   OR  "K839"
                                   OR  "0021" )  AND
                  (FLG-END         =   ZERO   )
      *        診療コード入力
               PERFORM 410119-SAIINPUT-SEC
           END-IF
      *
      *    他画面遷移時
           IF      FLG-HENKOU          =   1
      *H24.10
      *        警告はクリア
               IF      SPA-ERRCD (1:1)     =   "K"
                   PERFORM 410129-KEI-INIT-SEC
               END-IF
               MOVE    SPACE           TO  SPA-KIDCD
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    ZERO            TO  FLG-END
           END-IF
      *
           .
       410112-KOUI-SPACHK-EXT.
           EXIT.
      *H24.10
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       410129-KEI-INIT-SEC              SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
      *
               IF      SPA-COM-CUR (IDX)   =   "1"   OR  "2"
      *        警告フラグクリア
                   EVALUATE    SPA-ERRCD
                   WHEN    "K236"
                   WHEN    "K111"
                   WHEN    "K112"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG2(IDX)
                   WHEN    "K839"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG3(IDX)
                   WHEN    "K200"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG4(IDX)
                   WHEN    "K201"
                       MOVE    SPACE           TO  SPA-COM-KZMFLG5(IDX)
                   WHEN    "K122" 
                       MOVE    SPACE           TO  SPA-COM-KZMFLG6(IDX)
                   WHEN    "K108" 
                       MOVE    SPACE           TO  SPA-COM-KZMFLG7(IDX)
                   WHEN    "K227"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG1
                                                                  (IDX)
                   WHEN    "K228"
                       MOVE    SPACE           TO  SPA-COM-TUKIJGNFLG
                                                                  (IDX)
                   WHEN    "K210"
                   WHEN    "K211"
                   WHEN    "K212"
                   WHEN    "K213"
                       MOVE    SPACE           TO  SPA-COM-KINKIFLG
                                                                  (IDX)
                   WHEN    OTHER
                      MOVE    SPACE            TO  SPA-COM-KZMFLG (IDX)
                   END-EVALUATE
               END-IF
           END-PERFORM
           .
       410129-KEI-INIT-EXT.
           EXIT.
      *H24.10
      *
      *****************************************************************
      *    画面変換・削除処理
      *****************************************************************
       41011-KOUI-HENDEL-SEC               SECTION.
      *
           INITIALIZE                      WRK-MOJI-AREA
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    "K02"               TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
           MOVE    "1"                 TO  LNK-SC97-KBN
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
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
           MOVE    LNK-SC97-INCD       TO  WRK-CD
      *
           EVALUATE    LNK-SC97-INPUTCD(1:1)
           WHEN    SPACE
      *        削除時
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S") OR
                     ((SPA-GMN-IDX             <   SPA-MAX-LINE )  AND
                      (SPA-COM-SET  (SPA-GMN-IDX + 1)      =   "1"))
                   MOVE    SPA-GMN-IDX         TO  IDX
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
                   PERFORM 410151-SETDEL2-SEC
               ELSE
                   MOVE    SPACE       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               END-IF
               MOVE    1               TO  FLG-INS
      *
      *    剤削除時
           WHEN    "-"
               MOVE    LNK-SC97-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 41015-ZAIDEL-SEC
               MOVE    1               TO  FLG-INS
           END-EVALUATE
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
           MOVE    "K02"               TO  LNK-SC97-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC97-GMN-IDX
      *    時間外特例区分
           MOVE    SPA-JIKANTOKU       TO  LNK-SC97-JIKANTOKU
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
      *    行挿入
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "+"
               MOVE    SPACE           TO  WRK-HEN-INPUTCD
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)
                                       TO  WRK-HEN-INPUTCD
               MOVE    WRK-HEN-INPUTCD
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *H25.4
               MOVE    SPACE           TO  SPA-COM-IN4 (SPA-GMN-IDX)
      *        挿入行数
               MOVE    LNK-SC97-INSCNT TO  WRK-INSCNT
      *        約束セット前は１行
               IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)
                                           =   "S"  )  AND
                      (LNK-SC97-INSCNT     >   1    )
                   MOVE    1               TO  WRK-INSCNT
               END-IF
      *
               PERFORM 41014-GYOINSN-SEC
               IF      SPA-ERRCD       =   SPACE
                   MOVE    "3"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   ADD     1               TO  SPA-GMN-IDX
                   MOVE    1               TO  FLG-INS
      *!!!!!!!!!!!!!!
      *            連続挿入
                   IF     (SPA-RENZ-FLG        =   ZERO )
                   AND    (LNK-SC97-INSREN     =   1    )
                       MOVE    1               TO  SPA-RENZ-FLG
                       MOVE    "1"             TO  SPA-COM-GMNINS
                                                      (SPA-GMN-IDX)
                       MOVE    SPA-GMN-INPUTCD (SPA-GMN-IDX)
                                               TO  SPA-RENZ-INPUT
                   END-IF
      *!!!!!!!!!!!!!!
      *
               END-IF
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
      *H28.12
      *    電子お薬手帳画面
           IF     (LNK-SC97-INPUTCD(1:2)
                                       =   "/O"
                                       OR  "/o" )
            AND   (LNK-SC97-INPUTCD(3:)    =   SPACE )
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 410-K024-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *H28.5
      *    院外投薬名称変換
           IF     (LNK-SC97-INPUTCD(1:2)
                                       =   "/G"
                                       OR  "/g" )
            AND   (LNK-SC97-INPUTCD(3:)    =   SPACE )
            AND   (SPA-SRYYMD          >=  "20160401")
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    "0"             TO  SPA-GENERICFLG
               ELSE
                   MOVE    "1"             TO  SPA-GENERICFLG
               END-IF
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *TEST
      *TEST
      *    Ｐ入力へ
           IF      LNK-SC97-INPUTCD(1:1)
                                       =  "/"
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 430-K99-SYORI-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *    再診時、初診料算定へ変更する
           IF    ((SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".110"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(5:)   =   SPACE )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:3)  =   ".11"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(4:)   =   SPACE )) AND
                  (SPA-GMN-IDX                        >   1     )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX + 1)   =   SPACE )
      *            包括保険は対象外
              AND (SPA-HKNCOMBINUM     NOT =   "9999"  )
              AND (SPA-COM-HKNCOMBI(SPA-GMN-IDX)
                                       NOT =   "9999" )
      *       診察料自動発生なしは対象外
              AND (SPA-INITAUTO-FLG    =   "1"        )
      *       治験は対象外
              AND (SPA-CHIKENKBN   NOT =   "1"        )
              AND (SPA-COM-CHIKENKBN(SPA-GMN-IDX)
                                   NOT =   "1"        )
      *
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               PERFORM 4101120-SAISIN-SYOSIN-SEC
               GO      TO                  41011-KOUI-HEN2-EXT
           END-IF
      *
      *    初診料を再診料へ変更する
           IF    ((SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".120"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(5:)   =   SPACE )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:3)  =   ".12"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(4:)   =   SPACE )) AND
                  (SPA-GMN-IDX                        >   1     )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX + 1)   =   SPACE )
      *            包括保険は対象外
              AND (SPA-HKNCOMBINUM     NOT =   "9999"  )
              AND (SPA-COM-HKNCOMBI(SPA-GMN-IDX)
                                       NOT =   "9999" )
      *       診察料自動発生なしは対象外
              AND (SPA-INITAUTO-FLG    =   "1"        )
      *       治験は対象外
              AND (SPA-CHIKENKBN   NOT =   "1"        )
              AND (SPA-COM-CHIKENKBN(SPA-GMN-IDX)
                                   NOT =   "1"        )
      *
               PERFORM 4101122-SYOSIN-SAISIN-SEC
               IF      FLG-OK          NOT =   ZERO
                   GO      TO                  41011-KOUI-HEN2-EXT
               END-IF
           END-IF
      *
      *H26.4
      *    小児科外来診療料を初・再診料へ変更する
           IF    ( SPA-SRYYMD          >=  "20140401" )
             AND ((SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".113"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(5:)   =   SPACE )   OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".123"  AND
                   SPA-GMN-INPUTCD(SPA-GMN-IDX)(5:)   =   SPACE ))
            AND   (SPA-GMN-IDX                        >   1     )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX + 1)   =   SPACE )
      *            包括保険は対象外
              AND (SPA-HKNCOMBINUM     NOT =   "9999"  )
              AND (SPA-COM-HKNCOMBI(SPA-GMN-IDX)
                                       NOT =   "9999" )
      *       診察料自動発生なしは対象外
              AND (SPA-INITAUTO-FLG    =   "1"        )
      *       治験は対象外
              AND (SPA-CHIKENKBN   NOT =   "1"        )
              AND (SPA-COM-CHIKENKBN(SPA-GMN-IDX)
                                   NOT =   "1"        )
      *
               PERFORM 4101123-SYOUNIKA-CHENGE-SEC
               IF      FLG-OK          NOT =   ZERO
                   GO      TO                  41011-KOUI-HEN2-EXT
               END-IF
           END-IF
      *
      *
           IF     LNK-SC97-NYURYOKU    =   1
      *///     セットコードに変更がない時
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1) =   "S" ) AND
                      (SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                           =   SPA-GMN-INPUTCD
                                                         (SPA-GMN-IDX))
                   MOVE    1                   TO  FLG-SET-HEN
               ELSE
                   MOVE    ZERO                TO  FLG-SET-HEN
      *            コードが同一の時
                   IF    (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:6)
                                               =   SPA-GMN-INPUTCD
                                                   (SPA-GMN-IDX)(1:6))
                       MOVE    2               TO  FLG-SET-HEN
                   END-IF
               END-IF
      *        セットが変更削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S" ) OR
                      ((SPA-GMN-IDX            <   SPA-MAX-LINE )  AND
                       (SPA-COM-SET  (SPA-GMN-IDX + 1)     =   "1"))
                   PERFORM 41016-SETDEL-SEC
               END-IF
               IF      FLG-SET-HEN         =   ZERO
                                           OR  2
      *        セットコードが同じ時、禁忌チェック済みを保存する
               IF      FLG-SET-HEN         =   2
                   MOVE    SPA-COM-KINKIFLG (SPA-GMN-IDX)
                                               TO  WRK-KINKIFLG
                   MOVE    SPA-COM-PTKINKI-ARI (SPA-GMN-IDX)
                                               TO  WRK-PTKINKI-ARI
                   MOVE    SPA-COM-TUKIJGNFLG (SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG
                   MOVE    SPA-COM-TUKIJGNFLG1(SPA-GMN-IDX)
                                               TO  WRK-TUKIJGNFLG1
               ELSE
                   MOVE    SPACE               TO  WRK-KINKIFLG
                   MOVE    ZERO                TO  WRK-PTKINKI-ARI
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG
                   MOVE    SPACE               TO  WRK-TUKIJGNFLG1
               END-IF
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)    TO  WRK-TIMEFLG
               MOVE    SPA-COM-IN     (SPA-GMN-IDX)    TO  WRK-FLG-IN
               MOVE    SPA-COM-IN4    (SPA-GMN-IDX)    TO  WRK-FLG-IN4
               MOVE    SPA-GMN-MEISYO-G(SPA-GMN-IDX)   TO  WRK-MEISYO-G
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-TIMEFLG     TO  SPA-COM-TIMEFLG(SPA-GMN-IDX)
               MOVE    WRK-FLG-IN      TO  SPA-COM-IN  (SPA-GMN-IDX)
               MOVE    WRK-FLG-IN4     TO  SPA-COM-IN4 (SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-IN2 (SPA-GMN-IDX)
               MOVE    WRK-KINKIFLG    TO  SPA-COM-KINKIFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-PTKINKI-ARI TO  SPA-COM-PTKINKI-ARI
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG  TO  SPA-COM-TUKIJGNFLG
                                                        (SPA-GMN-IDX)
               MOVE    WRK-TUKIJGNFLG1 TO  SPA-COM-TUKIJGNFLG1
                                                        (SPA-GMN-IDX)
      *
               END-IF
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
               MOVE    "3"                 TO  SPA-COM-CUR (SPA-GMN-IDX)
      *
      *        診療行為一覧へ
               PERFORM 410-K98-SYORI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *!
      *    保険組合せ入力
           IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "#"
               PERFORM 410113-GMN-HKNCOMBI-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *!
      *    診療科、ドクター入力
           IF       SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   "$"
               PERFORM 410114-GMN-SRYKA-SEC
               GO      TO                 41011-KOUI-HEN2-EXT
           END-IF
      *!
      *    診療種別入力
           IF      (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)  =   ".") AND
                   (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)  NUMERIC)
               MOVE    SPA-COM-IN4    (SPA-GMN-IDX)    TO  WRK-FLG-IN4
               INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
               INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
               MOVE    WRK-FLG-IN4     TO  SPA-COM-IN4 (SPA-GMN-IDX)
               MOVE    LNK-SC97-INPUTCD(1:4)   TO  SPA-GMN-INPUTCD
                                                          (SPA-GMN-IDX)
               MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:3)
                                               TO  WRK-SRYSYUKBN
      *        診療種別区分名称チェック
               PERFORM 510-SRYKBN-MEI-SEC
               MOVE    WRK-SRYSYUKBNMEI    TO  SPA-GMN-MEISYO
                                                    (SPA-GMN-IDX)
               MOVE    "*"                 TO  SPA-GMN-MEIH
                                                    (SPA-GMN-IDX)
               MOVE    WRK-SRYKBN          TO  SPA-GMN-SRYKBN
                                                    (SPA-GMN-IDX)
           ELSE
      *        文字変換・編集処理
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 410112-INPUTCD-HENKAN-SEC
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-HENKOU          =   1
                   CONTINUE
               ELSE
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               END-IF
           ELSE
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
      *    数量ゼロエラーはエラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPA-ERRCD           TO  WRK-ERRCD2
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
           .
       41011-KOUI-HEN2-EXT.
           EXIT.
      *!
      *****************************************************************
      *    保険組合せ入力チェック処理
      *****************************************************************
       410113-GMN-HKNCOMBI-SEC       SECTION.
      *    継続
           IF     (SPA-CONTKBN         =   SPACE )  AND
                  (SPA-FUKU-SYORI      =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1057"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                   UNTIL      (IDX             <   1    )  OR
                              (SPA-ERRCD   NOT =   SPACE )  OR
                              (FLG-CHK         =    1   ) 
               IF     (SPA-GMN-IDX             NOT =   IDX )  AND
                      (SPA-COM-INPUTCD  (IDX)  NOT =   SPACE )
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF     (FLG-CHK             =   ZERO ) AND
                  (SPA-ERRCD           =   SPACE )
               MOVE    "1056"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     410113-GMN-HKNCOMBI-EXT
           END-IF
      *
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)    =   SPACE
               PERFORM 4101131-K97-SYORI-SEC
               GO      TO     410113-GMN-HKNCOMBI-EXT
           END-IF
      *
           IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       =   SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    1               TO  FLG-OK
           ELSE
               MOVE    ZERO            TO  FLG-OK
           END-IF
      *
      *    保険の有無
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:4)
                                       TO  WRK-HKNCOMBI
           MOVE    SPA-COM-IN4    (SPA-GMN-IDX)    TO  WRK-FLG-IN4
           INITIALIZE                  SPA-GMN-TBLREC (SPA-GMN-IDX)
           INITIALIZE                  SPA-COM-TBLREC (SPA-GMN-IDX)
           MOVE    "#"                 TO  SPA-GMN-INPUTCD
                                                  (SPA-GMN-IDX)(1:1)
           MOVE    WRK-HKNCOMBI        TO  SPA-GMN-INPUTCD
                                                  (SPA-GMN-IDX)(2:4)
           MOVE    WRK-FLG-IN4         TO  SPA-COM-IN4 (SPA-GMN-IDX)
      *    包括分入力、労災・自賠責はエラー
      *    Ｈ１８．４から包括保険可能
           IF     (SPA-HKNCOMBINUM     =   "9999")  OR
                  (SPA-HKNKBN          =   1     )
            OR   ((WRK-HKNCOMBI        =   "9999")  AND
                  (SPA-SRYYMD          <   "20060401"))
      *        公害はエラー
             OR   (SPA-HKNKBN          =   2     )
               MOVE    "1050"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO     410113-GMN-HKNCOMBI-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           MOVE    SPACE               TO  WRK-SEL-HKNNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-HKN-MAX )  OR
                              (FLG-CHK     =    1   ) 
               IF      WRK-HKNCOMBI        =   SPA-GMN-THKNCOMBINUM
                                                             (IDX)
                   MOVE    SPA-NAI-HKNNUM (IDX)  TO  WRK-SEL-HKNNUM
                   MOVE    IDX               TO  IDY
                   MOVE    1                 TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF       FLG-CHK            =   ZERO
               MOVE    "1051"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO     410113-GMN-HKNCOMBI-EXT
           END-IF
      *    労災はエラー
           IF     (SPA-NAI-RSI-HKNKBN (IDY)   NOT =  SPACE )
      *        公害はエラー
             OR   (SPA-HKNKBN          =   2     )
               MOVE    "1050"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO     410113-GMN-HKNCOMBI-EXT
           END-IF
      *ver.4.6
      *    入院中は自費保険のみ
           IF     (SPA-NAI-NYUIN       =   1    )
               IF      (SPA-NAI-HKNNUM(IDY)(1:2)   NOT =  "98" )
                   MOVE    "1059"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD
                                                    (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-CUR(SPA-GMN-IDX)
                   GO      TO     410113-GMN-HKNCOMBI-EXT
               END-IF
           END-IF
      *    保険名称を名称へ
           STRING  "《"                DELIMITED  BY  SIZE
                   SPA-GMN-THKNCOMBIMEI(IDY)
                                       DELIMITED  BY  "  "
                   "》"                DELIMITED  BY  SIZE
               INTO                    SPA-GMN-MEISYO-G (SPA-GMN-IDX)
           END-STRING
      *
      *!   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *!                               TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
      *    老人・一般判定
      *    ６５歳以上で老人公費のある場合、
           IF     (SPA-NAI-ROUJIN2     =   1   )
             AND  (WRK-HKNCOMBI    NOT =   "9999" )
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               MOVE    WRK-HKNCOMBI        TO  AGECHK-HKNCOMBINUM
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF      SPA-NAI-ROUJIN  NOT =   AGECHK-ROUJIN
                   MOVE    "1052"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  SPA-MAE-INPUTCD
                                                          (SPA-GMN-IDX)
                   MOVE    "1"             TO  SPA-COM-CUR(SPA-GMN-IDX)
                   GO      TO     410113-GMN-HKNCOMBI-EXT
               END-IF
           END-IF
           MOVE    WRK-HKNCOMBI        TO  SPA-COM-HKNCOMBI(SPA-GMN-IDX)
      *
           IF     (FLG-OK              =   1  )  OR
                  (SPA-ERRCD       NOT =   SPACE)
               CONTINUE
           ELSE
               MOVE    1               TO  FLG-FUKU
           END-IF
      *
           .
       410113-GMN-HKNCOMBI-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９７画面表示へ処理
      *****************************************************************
       4101131-K97-SYORI-SEC       SECTION.
      *
           IF      SPA-GMN-IDX         >=  SPA-MAX-LINE
               MOVE    ZERO            TO  SPA-GMN-IDX
           END-IF
           INITIALIZE                  SPA-K97-AREA
           IF     (SPA-GMN-IDX         >   ZERO  )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "$")
               IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:2)
                                                   NUMERIC
                   MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:2)
                                           TO  SPA-K97-SRYKA
               END-IF
           END-IF
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK97"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K97"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF     (SPA-GMN-IDX         >   ZERO  )  AND
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "$")
               IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:2)
                                                   NUMERIC
                   MOVE    "SELNUM3"           TO  MCP-WIDGET
               ELSE
                   MOVE    "SELNUM2"           TO  MCP-WIDGET
               END-IF
           ELSE
               IF      (SPA-HKNCOMBINUM    =   "9999" )  OR
                       (SPA-HKNKBN         =   1      )
                 OR    (SPA-HKNKBN         =   2      )
                   MOVE    "SELNUM2"           TO  MCP-WIDGET
               ELSE
                   MOVE    "SELNUM1"           TO  MCP-WIDGET
               END-IF
           END-IF
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4101131-K97-SYORI-EXT.
           EXIT.
      *!
      *****************************************************************
      *    診療科、ドクター入力処理
      *****************************************************************
       410114-GMN-SRYKA-SEC       SECTION.
      *
      *    継続
           IF     (SPA-CONTKBN         =   SPACE )  AND
                  (SPA-FUKU-SYORI      =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1057"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO     410114-GMN-SRYKA-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  -1
                   UNTIL      (IDX         <   1    )  OR
                              (FLG-CHK     =   1    )   OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF     (SPA-GMN-IDX             NOT =   IDX )  AND
                      (SPA-COM-INPUTCD  (IDX)  NOT =   SPACE )
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
           IF      FLG-CHK             =   ZERO
               MOVE    "1056"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-MAE-INPUTCD(SPA-GMN-IDX)
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               GO      TO     410114-GMN-SRYKA-EXT
           END-IF
      *
           IF     (SPA-GMN-INPUTCD(SPA-GMN-IDX)(2:)    =   SPACE ) OR
                  (SPA-GMN-INPUTCD(SPA-GMN-IDX)(4:1)   =   "D"
                                                       OR  "d"   )
               PERFORM 4101131-K97-SYORI-SEC
               GO      TO     410114-GMN-SRYKA-EXT
           END-IF
      *
           IF      SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       =   SPA-GMN-INPUTCD(SPA-GMN-IDX)
               MOVE    ZERO            TO  FLG-FUKU
           ELSE
               MOVE    1               TO  FLG-FUKU
          END-IF
      *
           .
       410114-GMN-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    Ｐ入力画面へ移行
      *****************************************************************
       430-K99-SYORI-SEC       SECTION.
      *
      *    画面編集
           INITIALIZE                  SPA-K99GMN-AREA
           MOVE    SPACE               TO  K99
           INITIALIZE                      K99
      *
      *    画面カーソルセット処理
           MOVE    "PIN"               TO  MCP-WIDGET
      *
           MOVE    SPACE               TO  SPA-K99-DATA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K99"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-K99-MOTOPG
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
           INITIALIZE                  SPA-K98GMN-AREA
      *****IF      SPA-K98-INPUTCD         =   SPACE
           IF      SPA-K98-INPUTCD(1:2)    =   "//"
               MOVE    SPA-K98-INPUTCD     TO  SPA-K98-GMN-INPUT
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
                       MOVE    SPA-K98-GMN-INPUT(3:) TO  SPA-K98-INPUTCD
                   WHEN    "/"
                       MOVE    SPA-K98-GMN-INPUT(3:) TO  SPA-K98-INPUTCD
      *H25.6       労災一覧
                   WHEN    "R"
                   WHEN    "r"
                       MOVE    SPA-K98-INPUTCD     TO  SPA-K98-GMN-INPUT
      *
      *H30.9
      *            選択式コメント一覧
                   WHEN    "S"
                   WHEN    "s"
                       IF      SPA-SRYYMD      <   "20180401"
                           MOVE    "1107"          TO  SPA-ERRCD
                           MOVE    SPACE           TO  SPA-K98-GMN-INPUT
                       ELSE
                           PERFORM 41019-K98-RECEKISAI-SEC
                       END-IF
                       IF      SPA-ERRCD       NOT =   SPACE
                           GO      TO  410-K98-SYORI-EXT
                       END-IF
      *
                   WHEN    OTHER
                       MOVE    SPACE               TO  SPA-K98-INPUTCD
               END-EVALUATE
      *        画面編集
               PERFORM 4101-K98-SET-SEC
           ELSE
               INITIALIZE                  SPA-K98GMN-AREA
      *        内服薬剤
               MOVE    "3"                 TO  SPA-K98-GMN-SYORI
               IF     (SPA-GMN-IDX         =   1  )  OR 
                      (SPA-COM-KAIFLG (SPA-GMN-IDX - 1)    =   "1" ) OR
                      (SPA-GMN-INPUTCD(SPA-GMN-IDX - 1)(1:1)   =   "S")
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
      *                    内服
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
      *
      *    エラークリア
           MOVE    SPACE               TO  SPA-ERRCD
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
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-K98-MOTOPG
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
      *
      *H30.9
      *****************************************************************
      *    選択式コメント一覧編集処理
      *****************************************************************
       41019-K98-RECEKISAI-SEC       SECTION.
      *
           IF       SPA-K98-INPUTCD(4:1)   =   "S"
                                           OR  "s"
               MOVE    "R"             TO  SPA-K98-GMN-SYORI
               MOVE    SPACE           TO  SPA-K98-INPUTCD
               MOVE    SPACE           TO  SPA-K98-GMN-SYORI2
               MOVE    "2"             TO  SPA-K98-TYPE
           ELSE
               MOVE    "C"             TO  SPA-K98-GMN-SYORI
               MOVE    SPACE           TO  SPA-K98-INPUTCD
               IF     (SPA-SELCOM-IDX  =   ZERO )
                 AND  (SPA-GMN-IDX     >   1    )
                   PERFORM 510112-SELCOM-CHK-SEC
               END-IF
               IF     (SPA-SELCOM-IDX  =   ZERO  )
                   MOVE    SPACE           TO  SPA-K98-GMN-SYORI
                   MOVE    SPACE           TO  SPA-K98-INPUTCD
                   MOVE    "1106"          TO  SPA-ERRCD
               ELSE
                   MOVE    SPA-COM-INPUTCD (SPA-SELCOM-IDX)
                                               TO  SPA-K98-SELCOM-SRYCD
               END-IF
           END-IF
           .
       41019-K98-RECEKISAI-EXT.
           EXIT.
      *****************************************************************
      *    Ｋ９８画面編集処理
      *****************************************************************
       4101-K98-SET-SEC       SECTION.
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
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
      *    診療コード入力処理
      *****************************************************************
       410119-SAIINPUT-SEC       SECTION.
      *
           MOVE    ZERO                TO  WRK-IN-IDX
           MOVE    ZERO                TO  WRK-IN-IDX2
           MOVE    ZERO                TO  WRK-MAX-IDX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MAX-LINE
               IF      SPA-COM-IN  (IDX)   =   "1"
                   MOVE    IDX                 TO  WRK-IN-IDX
               END-IF
               IF      SPA-COM-IN  (IDX)   =   "3"
                   MOVE    IDX                 TO  WRK-IN-IDX2
               END-IF
               IF     (SPA-COM-SET (IDX)   =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)    NOT =   SPACE )
                   MOVE    IDX             TO  WRK-MAX-IDX
               END-IF
      *        '_'をなくす
               INSPECT SPA-GMN-INPUTCD (IDX)
                       REPLACING   ALL "_"  BY  " "
      *        ＊をなくす
               PERFORM 51011-KAISU-CHK-SEC
      *
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
           IF     (FLG-INPUT-NASI      =   1  )  AND
                  (FLG-KUHAKU          =   1  )
               MOVE    ZERO                TO  WRK-IN-IDX
           END-IF
           IF      WRK-IN-IDX          =   ZERO
               MOVE    WRK-MAX-IDX         TO  SPA-GMN-IDX
           ELSE
               MOVE    WRK-IN-IDX          TO  SPA-GMN-IDX
           END-IF
           IF      SPA-GMN-IDX         =   ZERO
               GO      TO      410119-SAIINPUT-EXT
           END-IF
      *    他画面遷移時カーソル移動なし
           IF      FLG-HENKOU          =   1
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
               MOVE    "3"                  TO  SPA-COM-CUR
                                                       (SPA-GMN-IDX)
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)  =   "001")
                  AND (SPA-COM-YCOMKBN (SPA-GMN-IDX)       =   ZERO )
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
               IF      SPA-ERRCD           =   "0021"
      *            数量下限エラーはなし
                   MOVE    SPACE                TO  SPA-ERRCD
               END-IF
           END-IF
      *
      *TEST
      *    名称入力へのカーソル移動
           IF      WRK-IN-IDX          >   ZERO
               IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "810000001"  ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)
                                           =   "008500000"  ) OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83")  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"
                                           OR  "0085"
                                           OR  "0086"  )
                   IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83"         )  AND
                          (SPA-COM-ATAI1 (SPA-GMN-IDX)
                                       NOT =   SPACE        ))
      *R02.4
                     OR   (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                           =   "831"       )
                       CONTINUE
                   ELSE
                       MOVE    "3"             TO  SPA-COM-IN2
                                                      (SPA-GMN-IDX)
                   END-IF
               END-IF
           END-IF
      *
      *    空白行入力時、前の行へ
           IF     (FLG-KUHAKU          =   1  )  AND
                  (SPA-GMN-IDX         >   ZERO )
               IF     (SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                               NOT =   "."
                                                   AND "#" 
                                                   AND "$"  )  AND
                      (SPA-COM-KAIFLG (SPA-GMN-IDX)    =   SPACE )
      *            回数入力へ
                   MOVE    ZERO                TO  WRK-IN-IDX2
                   MOVE    ZERO                TO  FLG-ARI
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL       IDX     >   54
                       IF      SPA-GMN-INPUTCD (SPA-GMN-IDX)(IDX:1)
                                                   =   "*"
                                                   OR  "_"
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
                       IF    ((SPA-COM-IN3 (SPA-GMN-IDX)  =   "1" ) AND
                              (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                      NOT =   "001"))
                         OR  ((SPA-GMN-INPUTCD (SPA-GMN-IDX)(1:1)
                                                       =   "S" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "84" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0084" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      **                 用量コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992000" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      **                 自賠責金額入力
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                                   =   "0959"   ) AND
                              (SPA-COM-SRYSYUKBN(SPA-GMN-IDX)
                                                       =   "809" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *                  用法コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                                   =   "001"     ) AND
                              (SPA-COM-YCOMKBN(SPA-GMN-IDX) =   1 ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *H30.4             分割調剤など
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:7)
                                                   =   "0992081" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *R02.4
      *        新コメント
                         OR  ((SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                                   =   "85"   ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
                         OR  ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:3)
                                                       =   "831" ) AND
                              (SPA-COM-SURFLG (SPA-GMN-IDX)
                                                       =   SPACE ))
      *
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
               MOVE    "3"                 TO  SPA-COM-CUR
                                                     (SPA-GMN-IDX)
           END-IF
      *
           .
       410119-SAIINPUT-EXT.
           EXIT.
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
      *
      *    入力内容チェック、編集
           IF      LNK-SC97-NYURYOKU   =   1
               MOVE    LNK-SC97-INCD   TO  SPA-COM-INPUTCD(SPA-GMN-IDX)
           END-IF
           PERFORM 4101113-INPUTCD-CHK-SEC
      *
      *    名称チェック
           PERFORM 40113-MEISYOU-CHK-SEC
      *
           .
       410112-INPUTCD-HENKAN-EXT.
           EXIT.
      *****************************************************************
      *    入力内容チェック、編集処理
      *****************************************************************
       4101113-INPUTCD-CHK-SEC       SECTION.
      *
      *    初診料でコードの後ろの日付の入っている時初診料算定済みとする
           IF     (SPA-GMN-IDX         =   1  )     AND
                  (SPA-GMN-MAX1        =   ZERO  )  AND
                  (LNK-SC97-SURYOCNT   =   7     )  AND
                  (SPA-SYOYMD          =   SPACE )
      *            包括保険は対象外
              AND (SPA-HKNCOMBINUM     NOT =   "9999"  )
               PERFORM 4101125-SYOSIN-CHK-SEC
               IF     (SPA-KIDCD       NOT =   SPACE )
                   OR (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
           END-IF
      *
      *    算定履歴のみ作成
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)
                                       =   "1"       )  AND
                  (LNK-SC97-SURYOCNT   =   7     )
              AND (SPA-HKNCOMBINUM NOT =   "9999"  )
              AND (SPA-SYORIFLG        =   ZERO     )
              AND (SPA-COM-HKNCOMBI(SPA-GMN-IDX)
                                       NOT =   "9999" )
               PERFORM 4101126-SYOKAI-CHK-SEC
               IF     (SPA-KIDCD       NOT =   SPACE )
                   OR (SPA-ERRCD       NOT =   SPACE )
                   GO      TO      4101113-INPUTCD-CHK-EXT
               END-IF
           END-IF
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
               MOVE    SPACE               TO  LNK-SC92-KBN
           ELSE
               MOVE    "5"                 TO  LNK-SC92-KBN
           END-IF
           IF      FLG-HENKOU          =   1
      *        画面遷移前
               IF     (LNK-SC97-NYURYOKU       =   1 ) OR
                      (SPA-COM-CHKFLG (SPA-GMN-IDX)  =   SPACE)
                   MOVE    "6"                 TO  LNK-SC92-KBN
               END-IF
           END-IF
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
      *
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG
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
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
           MOVE    LNK-SC92-TIMEFLG    TO  SPA-NAI-TIMEFLG
      *
           MOVE    LNK-SC92-GMN-IDX    TO  SPA-GMN-IDX
      *
      *    カーソル位置設定
           PERFORM 41011139-INFLG-HEN-SEC
      *
      *    画像診断のＣＴ．ＭＲＩは２回目のコードが違うので
      *    算定回数のチェックなし
           IF     (SPA-COM-TNSSRYSYUKBN (SPA-GMN-IDX)
                                               =   "723"
                                               OR  "724"  )  AND
                  (SPA-COM-HOUKATUTEIGENKBN(SPA-GMN-IDX)
                                               =   "201"
                                               OR  "202"
                                               OR  "203"  )  AND
                   (SPA-COM-KAISU (SPA-GMN-IDX)
                                               =   1      )
               MOVE    SPACE           TO  LNK-SC92-ERRCD(3)
           END-IF
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
                   IF      IDX-ERR             =   3   OR  6
                       MOVE    LNK-SC92-ERRMSG     TO  SPA-ERRMSG2
                   END-IF
               END-IF
           END-PERFORM
      *
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
      *    小児科外来診療科の時、年齢エラーはそのまま
      *    IF     (SPA-SYONIKA-ARI3    NOT =   ZERO )   AND
      *           (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:1)  =   "1" )
      *H26.6
      *      AND  (SPA-COM-TNSSRYKBN (SPA-GMN-IDX)     =   "13")
      *H28.5
      *      AND  (SPA-COM-INPUTCD (SPA-GMN-IDX)
      *                                    NOT =   "113018210")
      *        IF      SPA-ERRCD           =   "0032"
      *            MOVE    SPACE               TO  SPA-ERRCD
      *        END-IF
      ***  END-IF
      *---(01.00.01) LINE ADD  START  ----
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
               MOVE    SPACE           TO  SPA-COM-HCHKFLG (SPA-GMN-IDX)
               GO      TO      4101113-INPUTCD-CHK-EXT
           END-IF
      *
      *    時間外加算の時、ＳＰＡへ
           IF      SPA-COM-TIMEFLG(SPA-GMN-IDX)  NOT =   ZERO
               MOVE    SPA-COM-TIMEFLG(SPA-GMN-IDX)
                                           TO  SPA-NAI-TIMEFLG
           END-IF
      *!!  保険適用区分が保険適用外のもの（ユーザー登録の自費分）
           IF      SPA-COM-HKNTEKKBN(SPA-GMN-IDX)  =   2
      *        数量を金額とする
      *****    IF      SPA-COM-KISOTEN (SPA-GMN-IDX)  =   ZERO
               IF      SPA-COM-TEN     (SPA-GMN-IDX)  =   ZERO
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
      *!!         すべての自費コードに数量可能とする（自費で判定）
      *            定額の時、数量入力エラー
      *            IF      WRK-SURYO       NOT =   ZERO
      *                MOVE    "1002"          TO  SPA-ERRCD
      *!!          END-IF
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-COM-KEIFLG
                                                          (SPA-GMN-IDX)
           END-IF
      *
           .
       4101113-INPUTCD-CHK-EXT.
           EXIT.
      *
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
                   MOVE    1                   TO  FLG-IN1
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
      *        自賠責金額
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:4)
                                               =   "0959" )
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
      *
      *R02.4
      *        新コメント
               IF     (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:2)
                                               =   "85"  )
                  OR  (SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                               =   "831"  )
                   MOVE    1                   TO  FLG-IN1
               END-IF
      *
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
      *
      *****************************************************************
      *    初診料算定済みチェック処理
      *****************************************************************
       4101125-SYOSIN-CHK-SEC       SECTION.
      *
           IF       SPA-COM-INPUTCD(SPA-GMN-IDX)
                                       =   "111700110"  OR
                                           "111700210"  OR
                                           "111000110"  OR
                                           "111003610"  OR
                                           "113003510"  OR
                                           "113003710"
      *        労災追加
                                       OR  "101110010"
      *        紹介状なし追加
                                       OR  "111012510"
      *        妥結率５割以下
                                       OR  "111012710"
      *       小児科かかりつけ診療料
                                       OR  "113019710"
                                       OR  "113019910"
               CONTINUE
           ELSE
               GO      TO      4101125-SYOSIN-CHK-EXT
           END-IF
      *
      *    数量の日付変換
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SURYO-S1        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
      *H25.4   紹介状なしは、Ｈ２５.4から
               IF      (SPA-COM-INPUTCD(SPA-GMN-IDX) =  "111012510")
                  AND  (SGDAY-SDAY             <   "20130401" )
                   MOVE    "1103"              TO  SPA-ERRCD
                   GO      TO      4101125-SYOSIN-CHK-EXT
               END-IF
      *
      *H27.1   妥結率５割以下　Ｈ２７.１から
               IF      (SPA-COM-INPUTCD(SPA-GMN-IDX) =  "111012710")
                  AND  (SGDAY-SDAY             <   "20150101" )
                   MOVE    "1104"              TO  SPA-ERRCD
                   GO      TO      4101125-SYOSIN-CHK-EXT
               END-IF
      *
      *H28.4   小児科かかりつけ診療料　Ｈ２８.４から
               IF      (SPA-COM-INPUTCD(SPA-GMN-IDX) =  "113019710"
                                                     OR "113019910" )
                  AND  (SGDAY-SDAY             <   "20160401" )
                   MOVE    "1105"              TO  SPA-ERRCD
                   GO      TO      4101125-SYOSIN-CHK-EXT
               END-IF
      *
               IF      SGDAY-SDAY          >   SPA-SRYYMD 
                   MOVE    "1101"              TO  SPA-ERRCD
               ELSE
                   MOVE    SGDAY-SDAY          TO  SPA-SYOSIN-YMD
                   MOVE    SPA-COM-INPUTCD(SPA-GMN-IDX)
                                               TO  SPA-SYOSIN-SRYCD
                   MOVE    "0108"              TO  SPA-KIDCD
               END-IF
           ELSE
               MOVE    "1102"              TO  SPA-ERRCD
           END-IF
      *
           .
       4101125-SYOSIN-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初回算定日チェック処理
      *****************************************************************
       4101126-SYOKAI-CHK-SEC       SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYCD
      *    数量の日付変換
           INITIALIZE                      ORCSGDAYAREA
           MOVE    WRK-SURYO-S1        TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-SDAY          TO  WRK-SYOKAI-YMD
      *        初診料のみ
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)
                                           =   "111700110"  OR
                                               "111700210"  OR
                                               "111000110"  OR
                                               "111003610"  OR
                                               "113003510"  OR
                                               "113003710"
                                           OR  "101110010"
      *                紹介状なし追加
                                           OR  "111012510"
      *                妥結率５割以下
                                           OR  "111012710"
                   IF      SPA-SYOSINFLG       =   ZERO
                       MOVE    "099110001"         TO  WRK-SRYCD
                   ELSE
                       MOVE    "1207"              TO  SPA-ERRCD
                   END-IF
               END-IF
      *        日付判定
               IF      WRK-SRYCD       NOT =   SPACE
                   IF      WRK-SYOKAI-YMD       >   SPA-SRYYMD
                       MOVE    "1205"              TO  SPA-ERRCD
                   END-IF
                   IF     (WRK-SRYCD           =   "099110001") AND
                          (SPA-LSTYMD      NOT =   SPACE      ) AND
                          (WRK-SYOKAI-YMD      >   SPA-LSTYMD )
                       MOVE    "1206"              TO  SPA-ERRCD
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    WRK-SYOKAI-YMD      TO  SPA-SYOKAI-YMD
                       MOVE    WRK-SRYCD           TO  SPA-SYOKAI-SRYCD
                       MOVE    SPA-GMN-IDX         TO  SPA-SYOKAI-IDX
                       MOVE    "0140"              TO  SPA-KIDCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4101126-SYOKAI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    再診時、初診料算定へ変更処理
      *****************************************************************
       4101120-SAISIN-SYOSIN-SEC       SECTION.
      *
      *    前に再診があること
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=   SPA-GMN-IDX
               IF     (SPA-COM-SRYKBN (IDX) =   "12" )  OR
                      (SPA-COM-INPUTCD(IDX) =   "113003610" OR
                                                "113003810")
                 OR   (SPA-COM-INPUTCD(IDX) =   "113019710" OR
                                                "113019910")
                  MOVE    1               TO  FLG-OK
               END-IF
               IF     (SPA-COM-SRYKBN (IDX) =   "12" )  OR
                      (SPA-COM-SRYKBN (IDX) =   "11" )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-PERFORM
           IF      FLG-OK             =   ZERO
           AND    (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".110" )
               GO      TO      4101120-SAISIN-SYOSIN-EXT
           END-IF
      *
      *    再診で、当日以降受診が無いこと
           IF      (SPA-SYORIFLG       =   ZERO )  AND
      *D           (SPA-SYOSIN         =   ZERO )  AND
                  ((SPA-LSTYMD         <   SPA-SRYYMD)
      *ver.4.7     同日に診察料なし
             OR   ((SPA-LSTYMD         =   SPA-SRYYMD) AND
                   (SPA-DOUJITU        =   ZERO      )))
      *        病名をチェックする
               PERFORM 41011201-BYOMEI-CHK-SEC
      *        初診料算定時、算定不可のコードチェック
               IF      FLG-OK              =   1
                   PERFORM 41011202-SYOSIN-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   IF      FLG-KEIZOKU         =   ZERO
                       MOVE    "0110"              TO  SPA-KIDCD
                   ELSE
                       MOVE    "0112"              TO  SPA-KIDCD
                   END-IF
                   IF      FLG-OK              =   ZERO
      *                再度、自動発生
                       MOVE    "0132"              TO  SPA-KIDCD
                   END-IF
      *H28.4
      *            小児かかりつけ診療料算定可能へ
                   IF     (SPA-SYUGAKUKBN      =   1  )
                      AND (SPA-SYOUNI-ARI      =   1  )
                      AND (FLG-OK2             =   1  )
                       MOVE    8               TO  SPA-SYONIKKR-ARI
                   END-IF
      *H26.4
      *            小児科外来診療料算定可能へ
                   IF     (SPA-SYONIKA         =   1  )
                      AND (FLG-OK2             =   1  )
                      AND (SPA-SYONIKA-ARI     =   ZERO)
                      AND (SPA-SYONIKKR-ARI    =   ZERO)
                       MOVE    8               TO  SPA-SYONIKA-ARI
                   END-IF
               END-IF
           ELSE
               MOVE    "1016"              TO  SPA-ERRCD
           END-IF
      *
           .
       4101120-SAISIN-SYOSIN-EXT.
           EXIT.
      *****************************************************************
      *    初診料を再診料へ変更処理
      *****************************************************************
       4101122-SYOSIN-SAISIN-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-OK2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=   SPA-GMN-IDX
               IF    ((SPA-COM-SRYKBN (IDX) =   "11" )  AND
                      (SPA-COM-INPUTCD(IDX)(1:1)   =   "1" )) OR
                     ( SPA-COM-INPUTCD(IDX) =   "113003510" OR
                                                "113003710")
                OR    (SPA-COM-INPUTCD(IDX) =   "113019710" OR
                                                "113019910")
                   MOVE    1               TO  FLG-OK
               END-IF
               IF     (SPA-COM-SRYKBN (IDX) =   "12" )  OR
                      (SPA-COM-SRYKBN (IDX) =   "11" )
                   MOVE    1               TO  FLG-OK2
               END-IF
           END-PERFORM
      *
           IF      FLG-OK             =   ZERO
           AND    (SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:4)  =   ".120" )
               CONTINUE
           ELSE
               MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               IF      (SPA-SYORIFLG       =   ZERO )  AND
                       (SPA-SYOSINFLG      =   1    )  AND
                       (SPA-LSTYMD     NOT =   SPACE)  AND
      *                Ｈ１８．４から
                       (SPA-SRYYMD         >=  "20060401" )
                   MOVE    "0117"              TO  SPA-KIDCD
                   IF      FLG-OK              =   ZERO
      *                再度、自動発生
                       MOVE    "0137"              TO  SPA-KIDCD
                   END-IF
      *H28.4
      *            小児かかりつけ診療料算定可能へ
                   IF     (SPA-SYUGAKUKBN      =   1  )
                      AND (SPA-SYOUNI-ARI      =   1  )
                      AND (FLG-OK2             =   1  )
                      AND (SPA-SYONIKA-ARI     =   ZERO)
                       MOVE    8               TO  SPA-SYONIKKR-ARI
                   END-IF
      *H26.4
      *            小児科外来診療料算定可能へ
                   IF     (SPA-SYONIKA         =   1  )
                      AND (FLG-OK2             =   1  )
                      AND (SPA-SYONIKA-ARI     =   ZERO)
                      AND (SPA-SYONIKKR-ARI    =   ZERO)
                       MOVE    8               TO  SPA-SYONIKA-ARI
                   END-IF
               ELSE
                   IF      SPA-SYOSINFLG       =   ZERO
                       MOVE    "1017"              TO  SPA-ERRCD
                   ELSE
                       MOVE    "1046"              TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
      *
           .
       4101122-SYOSIN-SAISIN-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定へ変更の病名をチェックする処理
      *****************************************************************
       41011201-BYOMEI-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-KEIZOKU
      *    患者病名チェック
           MOVE    SPACE               TO  PTBYOMEI-REC
           INITIALIZE                  PTBYOMEI-REC
           MOVE    SPA-HOSPNUM         TO  PTBYO-HOSPNUM
           MOVE    SPA-PTID            TO  PTBYO-PTID
           MOVE    SPA-SRYYMD          TO  PTBYO-SRYYMD
           MOVE    SPA-SRYYMD          TO  PTBYO-TENKIYMD
      *
           MOVE    PTBYOMEI-REC        TO  MCPDATA-REC
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTBYOMEI
               MOVE    SPACE               TO  PTBYOMEI-REC
               INITIALIZE                  PTBYOMEI-REC
           END-IF
           PERFORM UNTIL       FLG-PTBYOMEI    =   1   OR
                               FLG-KEIZOKU     =   1
               IF      PTBYO-SRYYMD        <   SPA-SRYYMD
                   EVALUATE    PTBYO-TENKIKBN
      *                継続
                       WHEN    " "
                           MOVE    1               TO  FLG-KEIZOKU
                   END-EVALUATE
               END-IF
               MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 980-PTBYOMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       41011201-BYOMEI-CHK-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定時、算定不可のコードチェック処理
      *****************************************************************
       41011202-SYOSIN-CHK-SEC       SECTION.
      *
           MOVE    SPA-SCR-REC         TO  WRK-SCR-REC
           MOVE    SPA-GMN-IDX         TO  WRK-GMN-IDX
      *    初診料算定チェック
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "9"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       WRK-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           MOVE    ZERO                TO  SPA-SYOSIN-KEI
           MOVE    ZERO                TO  SPA-SYOSIN-KEIFLG
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    "1025"          TO  SPA-ERRCD
           END-IF
      *
           .
       41011202-SYOSIN-CHK-EXT.
           EXIT.
      *H26.4
      *****************************************************************
      *    小児科外来診療料を初・再診料へ変更処理
      *****************************************************************
       4101123-SYOUNIKA-CHENGE-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-KIDCD-MSG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=   SPA-GMN-IDX
               IF      SPA-COM-INPUTCD(IDX) =   "113003510"
                                            OR  "113003710"
                                            OR  "113003610"
                                            OR  "113003810"
                   MOVE    1                   TO  FLG-OK
                   MOVE    "小児科外来診療料"  TO  WRK-KIDCD-MSG
               END-IF
      *H28.4   小児かかりつけ
               IF      SPA-COM-INPUTCD(IDX) =   "113019710"
                                            OR  "113019810"
                                            OR  "113019910"
                                            OR  "113020010"
                   MOVE    2                   TO  FLG-OK
                   MOVE    "小児かかりつけ診療料"
                                              TO  WRK-KIDCD-MSG
               END-IF
           END-PERFORM
      *
           IF      FLG-OK             =   ZERO
               MOVE    "1318"              TO  SPA-ERRCD
               GO      TO      4101123-SYOUNIKA-CHENGE-EXT
           END-IF
      *
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                           =   ".113"
               IF      (SPA-SYORIFLG       =   ZERO      )  AND
                       (SPA-LSTYMD         <   SPA-SRYYMD)
      *        初診料へ変換
                   MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
      *            病名をチェックする
                   PERFORM 41011201-BYOMEI-CHK-SEC
      *            初診料算定時、算定不可のコードチェック
                   IF      FLG-OK              =   1
                       PERFORM 41011202-SYOSIN-CHK-SEC
                   END-IF
                   IF      SPA-ERRCD           =   SPACE
                       IF      FLG-KEIZOKU         =   ZERO
                           MOVE    "0133"              TO  SPA-KIDCD
                       ELSE
                           MOVE    "0134"              TO  SPA-KIDCD
                       END-IF
                   END-IF
               ELSE
                   MOVE    "1316"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        再診料へ変換
               IF      (SPA-SYORIFLG       =   ZERO )  AND
                       (SPA-SYOSINFLG      =   1    )  AND
                       (SPA-LSTYMD     NOT =   SPACE)
                   MOVE    "0135"              TO  SPA-KIDCD
                   MOVE    SPA-MAE-INPUTCD(SPA-GMN-IDX)
                                       TO  SPA-GMN-INPUTCD(SPA-GMN-IDX)
               ELSE
                   MOVE    "1317"              TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       4101123-SYOUNIKA-CHENGE-EXT.
           EXIT.
      *H26.4
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
           .
       4101121-MOJI-SET-EXT.
           EXIT.
      *****************************************************************
      *    入力項目編集処理
      *****************************************************************
       4101122-KOMOKU-HENSYU-SEC      SECTION.
      *
      *    セットの入力値
           INITIALIZE                  ORCSC92AREA
           MOVE    SPACE               TO  LNK-SC92-KBN
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
      *    入力値編集あり
           MOVE    "1"                 TO  LNK-SC92-INFLG
           MOVE    LNK-SC97-INPUT-G    TO  LNK-SC92-INPUT-G
           MOVE    SPA-NAI-TIMEFLG     TO  LNK-SC92-TIMEFLG
           MOVE    "K02"               TO  LNK-SC92-PG
           MOVE    SPA-GMN-IDX         TO  LNK-SC92-GMN-IDX
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  LNK-SC92-SRYCD
           CALL    "ORCSC92"           USING
                                       MCPAREA
                                       ORCSC92AREA
                                       SPA-SCR-REC
                                       SPA-AREA
           MOVE    LNK-SC92-INPUT-G    TO  LNK-SC97-INPUT-G
      *
           .
       4101122-KOMOKU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       410119-INPSET-SEC           SECTION.
      *
      *    時間外がないこと
           IF      LNK-SC97-JIKAN      NOT =   ZERO
               MOVE    "0010"              TO  SPA-ERRCD
               GO  TO                 410119-INPSET-EXT
           END-IF
      *
           MOVE    1                   TO  SPA-SINSATU-SAI
      *
           MOVE    SPACE               TO  WRK-ERRCD
           PERFORM 4101191-INPUTSET-SYORI-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    WRK-ERRCD       TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       410119-INPSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット入力処理
      *****************************************************************
       4101191-INPUTSET-SYORI-SEC      SECTION.
      *
      *    約束処方の初期処理
           IF      SPA-GMN-INPUTCD(SPA-GMN-IDX)(1:1)   =   "S"
               IF      FLG-TOROKU      NOT =   "2"
                   PERFORM 4101192-INPSET-SET-SEC
               END-IF
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4101191-INPUTSET-SYORI-EXT
           END-IF
      *    入力位置クリア
      *?   MOVE    SPACE               TO  SPA-COM-IN4 (SPA-GMN-IDX)
      *
           INITIALIZE                  ORCSCSETHENAREA
           MOVE    "K02"               TO  ORCSCSETHEN-PG
      *    セット名
           MOVE    SPA-GMN-INPUTCD(SPA-GMN-IDX)
                                       TO  ORCSCSETHEN-SRYCD
      *    対象行
           MOVE    SPA-GMN-IDX         TO  ORCSCSETHEN-IDX
      *    ORCSC92検索用
           MOVE    SPA-SYORIFLG        TO  ORCSCSETHEN-SYORIFLG
           MOVE    SPA-NENREI          TO  ORCSCSETHEN-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  ORCSCSETHEN-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  ORCSCSETHEN-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  ORCSCSETHEN-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  ORCSCSETHEN-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  ORCSCSETHEN-TEISEI-AREA
      *
           CALL    "ORCSCSETHEN"       USING
                                       ORCSCSETHENAREA
                                       SPA-AREA
                                       SPA-SCR-REC
      *    展開後の位置
           MOVE    ORCSCSETHEN-IDX     TO  SPA-GMN-IDX
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    ORCSCSETHEN-ERRMSG2     TO  SPA-ERRMSG2
           END-IF
      *
           .
       4101191-INPUTSET-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    約束処方の処理
      *****************************************************************
       4101192-INPSET-SET-SEC           SECTION.
      *
      *    入力文字を数字へ変換
      **** MOVE    1               TO  WRK-IDX-FT
      **** PERFORM 4101121-MOJI-SET-SEC
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
      *
           .
       4101192-INPSET-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為、計計算処理、編集処理
      *****************************************************************
       510-TBLHEN-SEC           SECTION.
      *
           MOVE    SPA-AKUSEI-FLG      TO  FLG-MAE-AKUSEI
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "1"                 TO  ORCSC100-KBN
           MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
      *H28.4   中途終了
           IF      FLG-CHUEND          =   1
               MOVE    "4"                 TO  ORCSC100-KBN
           END-IF
      *
           MOVE    FLG-INIT            TO  ORCSC100-INIT
           MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
           MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                       ORCSC100AREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    悪性検査一覧選択後のエラー
           IF     (FLG-MAE-AKUSEI      =   1   )
             AND  (SPA-ERRCD       NOT =   SPACE)
               GO      TO      510-TBLHEN-EXT
           END-IF
      *
      *    悪性腫瘍検査一覧へ
           IF      SPA-AKUSEI-FLG      =   1
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO  )
               PERFORM 5101-AKUSEI-K98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *!!!!
      *H30.9
      *    選択コメント一覧へ
           IF     (SPA-SELCOM-FLG      =   1   )
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO )
           AND    (SPA-SRYYMD          >=  "20180401")
               PERFORM 51011-SELCOM-K98-SEC
               GO    TO  510-TBLHEN-EXT
           END-IF
      *!!!!
      *
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180" )
               GO      TO      510-TBLHEN-EXT
           END-IF
      *    禁忌チェックを行う
           IF     (SPA-ERRCD           =   SPACE )
           AND    (FLG-TOROKU          =   SPACE )
      *ver.4.7
      *    訂正展開時はチェックなし
           AND    (FLG-TEISEI          =   ZERO  )
      *    中途データ展開時はなし
           AND    (FLG-INIT            =   ZERO  )
               PERFORM 51010-KNKCHK-SEC
               IF      FLG-KINKI           =   1
                   GO      TO      510-TBLHEN-EXT
               END-IF
           END-IF
      *    包括診療チェック
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )  AND
                  (FLG-HKTCHK          =   1     ) AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
      *       治験は対象外
      ******  AND (SPA-CHIKENKBN   NOT =   "1"        )
               IF     (SPA-HKTSANTEI-FLG   =   1    ) OR
                      (SPA-HKTSANTEI-08    =   1    )
                   PERFORM 5109-HKTSANTEI-CHK-SEC
               END-IF
           END-IF
      *    ７０歳到達の警告
           IF      SPA-ERRCD       =   SPACE
               IF      SPA-NAI-ROUFLG      =   1
                   IF      SPA-NAI-ROUSTR      =   ZERO
                       MOVE    "K008"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
        510-TBLHEN-EXT.
           EXIT.
      *!!!!
      *
      *TEST
      *H30.9
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       510112-SELCOM-CHK-SEC       SECTION.
      *
           COMPUTE IDX-STR             =   SPA-GMN-IDX  -  1
      *    直前に＊があれば、エラー
           IF     (SPA-COM-ZAIEND (IDX-STR)    =   "1" )
              AND (SPA-COM-KAIFLG (IDX-STR)    =   "1" )
               GO      TO      510112-SELCOM-CHK-EXT
           END-IF
      *
           PERFORM VARYING    IDX      FROM    IDX-STR  BY  -1
                   UNTIL     (IDX      <       1   )
                        OR   (SPA-SELCOM-IDX   >   ZERO  )
               IF     (SPA-COM-INPUTCD (IDX)(1:1)  =   "1" )
                  AND (SPA-COM-RECEKISAI(IDX)      =   "1"
                                                   OR  "2"  )
                   MOVE    IDX                 TO  SPA-SELCOM-IDX
                   MOVE    1                   TO  SPA-SELCOM-FLG
               END-IF
      *        同じ剤内での検索
               IF      (IDX            NOT =   IDX-STR )
                   AND (SPA-COM-ZAIEND (IDX)   =   "1" )
                   MOVE    1               TO  IDX
               END-IF
           END-PERFORM
           .
       510112-SELCOM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    選択コメント一覧へ処理
      *****************************************************************
       51011-SELCOM-K98-SEC       SECTION.
      *
           MOVE    SPA-SELCOM-IDX      TO  SPA-GMN-IDX
      *
           INITIALIZE                      SPA-K98-AREA
           MOVE    "C"                 TO  SPA-K98-GMN-SYORI
           MOVE    "//S"               TO  SPA-K98-INPUTCD
           MOVE    "3"                 TO  SPA-COM-CUR (SPA-SELCOM-IDX)
      *
      *    診療行為一覧へ
           PERFORM 410-K98-SYORI-SEC
           .
       51011-SELCOM-K98-EXT.
           EXIT.
      *!!!!
      *
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
      *    禁忌チェック処理
      *****************************************************************
       51010-KNKCHK-SEC                SECTION.
      *
           MOVE    ZERO                TO  FLG-KINKI
      *    相互作用チェック期間
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "2"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKNKCHK"       USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               IF      SPA-K021-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    1                   TO  FLG-ALLFLG
                   PERFORM 51020-K021-SYORI-SEC
               END-IF
           END-IF
           .
       51010-KNKCHK-EXT.
           EXIT.
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
           .
       510-SRYKBN-MEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為名入力処理
      *****************************************************************
       40113-MEISYOU-CHK-SEC        SECTION.
      *
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)   =  "810000001"
                                                   OR "008500000"
                                                   OR "008600000" )
               MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               MOVE    "1"                 TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF     (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "83" OR
                                                         "84") OR
      *R02.3
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  = "85") OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  = "0083" OR
                                                         "0084" OR
                                                         "0085"
                                                     OR  "0086")
               IF     (LNK-SC97-NYURYOKU   =   ZERO )  AND
                      (FLG-IN-ATAI         =   ZERO )  AND
                      (SPA-COM-ATAI1 (SPA-GMN-IDX)
                                           =   SPACE)  AND
                     ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)
                                           =   "83"   )  OR
                      (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)
                                           =   "0083"
                                           OR  "0085"
                                           OR  "0086" ))
                   MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G
                                                         (SPA-GMN-IDX)
               END-IF
               MOVE    "1"             TO  SPA-COM-MEIFLG
                                                       (SPA-GMN-IDX)
           END-IF
           IF    ((SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "81"
                                                       OR "83"
                                                       OR "84") OR
      *R02.3
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:2)  =  "85") OR
                  (SPA-COM-INPUTCD (SPA-GMN-IDX)(1:4)  =  "0083"
                                                       OR "0084"
                                                       OR "0085"
                                                       OR "0086")) AND
                  (SPA-GMN-MEISYO-G(SPA-GMN-IDX) NOT =   SPACE)
      *
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
      *R02.4
      *        MOVE    80                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
           ELSE
               MOVE    3                   TO  IDX-STR
      *R02.4
      *        MOVE    78                  TO  WRK-LEN
               MOVE    100                 TO  WRK-LEN
               IF      SPA-COM-INPUTCD(SPA-GMN-IDX)(1:3)
                                           =   "831" 
                   MOVE    138                 TO  WRK-LEN
               END-IF
               IF     (SPA-GMN-MEISYO-G (SPA-GMN-IDX)(1:2)
                                           =   "* "
                                           OR  SPACE  )
                   CONTINUE
               ELSE
      *            開始位置より前に入力があれば元に戻してエラー
                   MOVE    "0100"          TO  SPA-ERRCD
                   MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
                   MOVE    2               TO  SPA-GMN-CUR
                   MOVE    SPACE           TO  SPA-GMN-MEISYO-G
                                                (SPA-GMN-IDX)
                   MOVE    SPA-COM-NAME (SPA-GMN-IDX)
                                           TO  SPA-GMN-MEISYO
                                                (SPA-GMN-IDX)
               END-IF
           END-IF
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-LEN             TO  KANACHK-MAX-CNT
           MOVE    SPA-GMN-MEISYO-G (SPA-GMN-IDX)(IDX-STR:)
                                       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           IF     (KANACHK-RC      NOT =   ZERO  )  OR
                  (KANACHK-SYUBETU     =   1     )
              OR  (KANACHK-RC4     NOT =   ZERO  )
               MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-MEISYO-G
                                                       (SPA-GMN-IDX)
                                                            (IDX-STR:)
               MOVE    "0080"          TO  SPA-ERRCD
               MOVE    "1"             TO  SPA-COM-CUR (SPA-GMN-IDX)
               MOVE    2               TO  SPA-GMN-CUR
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
           IF      SPA-GMN-MAX         =   SPA-MAX-LINE
               MOVE    "0007"              TO  SPA-ERRCD
               GO      TO      41014-GYOINSN-EXT
           END-IF
      *
      *    挿入行のチェック
           IF     WRK-INSCNT           >   1
               IF      (SPA-GMN-MAX + (WRK-INSCNT - 1))  >= SPA-MAX-LINE
                   MOVE    "0007"              TO  SPA-ERRCD
                   GO      TO      41014-GYOINSN-EXT
               END-IF
           END-IF
      *
           IF      SPA-GMN-IDX         =   SPA-MAX-LINE
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
      *!!!!!!!!!!!!!!!!!!
      *            複数行挿入
                   IF     WRK-INSCNT   >   1
                       COMPUTE IDX         =    IDX
                                           +   (WRK-INSCNT  -  1)
                   END-IF
      *!!!!!!!!!!!!!!!!!!
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
           MOVE    1                   TO  SPA-GMN-CUR
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
      *                    診療区分行まで
                           IF      SPA-GMN-SRYKBN(IDX)
                                               NOT =   SPACE
                               MOVE    1       TO  IDX
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *    後ろ
           PERFORM VARYING     IDX     FROM    SPA-GMN-IDX   BY  1
                   UNTIL      (IDX     >  SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX)   =   SPACE)
      *        セットが削除の時セット内容を削除する
               IF     (SPA-MAE-INPUTCD(IDX)(1:1)   =   "S") OR
                     ((IDX                     <   SPA-MAX-LINE  )  AND
                      (SPA-COM-SET  (IDX + 1)  =   "1"))
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   PERFORM 410151-SETDEL2-SEC
                   MOVE    SPA-MAX-LINE        TO  IDX
               ELSE
      *
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX)
                   IF    ((IDX                 <   SPA-MAX-LINE )  AND
                          (SPA-GMN-SRYKBN (IDX + 1)
                                               NOT =   SPACE)) OR
                          (SPA-COM-ZAIEND(IDX)     =   "1"   )
                       MOVE    SPA-MAX-LINE            TO  IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       41015-ZAIDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット削除処理（剤削除）
      *****************************************************************
       410151-SETDEL2-SEC              SECTION.
      *
           COMPUTE IDX2                =   IDX     +   1
      *
           PERFORM VARYING     IDX3    FROM    IDX2    BY  1
                   UNTIL      (IDX3    >   SPA-MAX-LINE )  OR
                              (SPA-GMN-INPUTCD (IDX3)  =   SPACE)
               IF      SPA-COM-SET     (IDX3)   =   "1"
                   MOVE    SPACE           TO  SPA-GMN-INPUTCD(IDX3)
                   ADD     1               TO  SPA-GMN-IDX
               ELSE
                   MOVE    SPA-MAX-LINE        TO  IDX3
               END-IF
           END-PERFORM
      *
           .
       410151-SETDEL2-EXT.
           EXIT.
      *
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
                   MOVE    SPA-MAX-LINE            TO  IDX
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
                          (WRK-COM-DATAKBN(IDX)        =   "1"  OR
                           WRK-COM-INPUTCD(IDX)        =   SPACE)
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
      *
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                     <   SPA-MAX-LINE) AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
                       MOVE    ZERO            TO  FLG-ZAIEND
                   END-IF
      *!!!!!!!!!!!!!!!!!!
      *連続挿入解除
                   IF     (IDX                 =  SPA-SYOKAI-IDX )
                      OR  (WRK-COM-IN (IDX)    =   "1"   )
                       MOVE    SPACE           TO  SPA-SYOKAI-SRYCD
                       MOVE    SPACE           TO  SPA-SYOKAI-YMD
                       MOVE    ZERO            TO  SPA-SYOKAI-IDX
                   END-IF
      *!!!!!!!!!!!!!!!!!!
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
                       IF     (WRK-COM-AUTOKBN(IDX)    NOT =   SPACE)
                         AND  (WRK-COM-TIMEKSNKBN(IDX) NOT =   ZERO )
                           MOVE    SPACE           TO  WRK-GMN-INPUTCD
                                                                (IDX)
                       END-IF
                   END-IF
                   IF     (WRK-COM-ZAIEND (IDX)    =   "1" )  OR
                         ((IDX                 <   SPA-MAX-LINE )  AND
                          (WRK-GMN-SRYKBN (IDX + 1)    NOT =   SPACE))
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
                   MOVE    ZERO                TO  FLG-ERR
      *            登録前チェックの時、診療種別のみの剤は削除する
                   IF      FLG-TOROKU          =   "1"
                       IF      WRK-GMN-INPUTCD (IDX)(1:1)  =  "."
                           IF      (IDX            =   SPA-MAX-LINE ) OR
                                   (WRK-GMN-INPUTCD(IDX + 1)(1:1)
                                                   =   "."     )  OR
                                   (WRK-COM-INPUTCD(IDX + 1)
                                                   =   SPACE  AND
                                    WRK-GMN-INPUTCD(IDX + 1)
                                                   =   SPACE   )
      *?H24.11              OR     (WRK-GMN-SRYKBN (IDX + 1)
      *?                                           NOT =   SPACE )
                               MOVE    1               TO  FLG-ERR
                           END-IF
                       END-IF
                   END-IF
      *
                   IF      FLG-ERR             =   ZERO
      *
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
               END-IF
           END-PERFORM
           .
       420-SYOKYO-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療チェック処理
      *****************************************************************
       5109-HKTSANTEI-CHK-SEC              SECTION.
      *
           MOVE    SPA-K01KYOUTU       TO   SPA-WORK
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    "1"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02"               TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      ORCSCHKTCHK-HKTFLG  =   1
               MOVE    1               TO  SPA-HKTSANTEI-CHK
           ELSE
               MOVE    ZERO            TO  SPA-HKTSANTEI-CHK
           END-IF
           IF      ORCSCHKTCHK-HKTERR  =   1
               MOVE    "0150"          TO  SPA-KIDCD
           END-IF
           .
       5109-HKTSANTEI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括診療エラー削除処理
      *****************************************************************
       51091-HKTSANTEI-DEL-SEC              SECTION.
      *
           INITIALIZE                  ORCSCHKTCHKAREA
           MOVE    "3"                 TO  ORCSCHKTCHK-SYORI
           MOVE    "K02"               TO  ORCSCHKTCHK-PG
           MOVE    SPA-K02-TEISEI-AREA TO   LNK-ORCSC-TEISEI-AREA
           CALL    "ORCSCHKTCHK"       USING
                                       ORCSCHKTCHKAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       ORCSC00AREA
      *    診療行為、計計算処理、編集
           MOVE    SPACE               TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           .
       51091-HKTSANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ選択処理
      *****************************************************************
       41014-DOSELNUM-SEC              SECTION.
      *
           MOVE    4                   TO  SPA-GMN-CUR
      *
           MOVE    K02-DOSELNUM        TO  SPA-GMN-DOSELNUM
           IF      K02-DOSELNUM        =   SPACE
               MOVE    ZERO            TO  SPA-NAI-SELNUM
           ELSE
               INITIALIZE                      ORCSNUMAREA
               MOVE    K02-DOSELNUM        TO  SNUM-INX
               CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
               IF     (SNUM-RC         NOT =   ZERO  )   OR
                      (SNUM-MINKBN     NOT =   SPACE )   OR
                      (SNUM-SYOKBN     NOT =   SPACE )
                   MOVE    "1000"              TO  SPA-ERRCD
                   GO      TO      41014-DOSELNUM-EXT
               ELSE
                   MOVE    SNUM-OUTNUM       TO  SPA-NAI-SELNUM
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL       IDX     >   SPA-GMN-MAX1
              IF      SPA-GMN-NO (IDX)     =   SPA-NAI-SELNUM
                  MOVE    IDX                  TO  SPA-NAI-DOSELNUM
                  MOVE    SPA-GMN-MAX1         TO  IDX
              END-IF
           END-PERFORM
           MOVE    SPA-NAI-SELNUM      TO  WRK-ZZ
           MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               MOVE    "1000"              TO  SPA-ERRCD
           ELSE
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
           .
       41014-DOSELNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療日選択処理
      *****************************************************************
       41015-RRKLIST-SEC           SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM
           MOVE    ZERO                TO  WRK-DOSELNUM2
      *
           PERFORM VARYING     IDX   FROM   1   BY  1
                   UNTIL       IDX   >   SPA-GMN-MAX1
              IF      K02-SELECT (IDX)        =  "T"
                  IF      IDX                 =   SPA-NAI-TESELNUM
                      CONTINUE
                  ELSE
                      MOVE    SPA-GMN-NO (IDX)    TO  WRK-SELNUM2
                      MOVE    IDX                 TO  WRK-DOSELNUM2
                  END-IF
                  MOVE    SPA-GMN-NO (IDX)    TO  WRK-SELNUM
                  MOVE    IDX                 TO  WRK-DOSELNUM
              END-IF
           END-PERFORM
      *
           IF      WRK-DOSELNUM2       >   ZERO
               MOVE    WRK-DOSELNUM2       TO  SPA-NAI-DOSELNUM
               MOVE    WRK-SELNUM2         TO  SPA-NAI-SELNUM
           ELSE
               MOVE    WRK-DOSELNUM        TO  SPA-NAI-DOSELNUM
               MOVE    WRK-SELNUM          TO  SPA-NAI-SELNUM
           END-IF
      *
           IF      SPA-NAI-DOSELNUM    <   01   OR
                                       >   SPA-GMN-MAX1
               CONTINUE
           ELSE
               MOVE    SPA-NAI-SELNUM      TO  WRK-ZZ
               MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
               IF      SPA-NAI-SYORI       =   ZERO
      *
      *            ＤＯ検索処理へ
                   PERFORM 4801-K09-SYORI-SEC
               ELSE
      *            訂正日編集処理
                   PERFORM 410141-TEISEI-SET-SEC
               END-IF
           END-IF
      *
      *
           .
       41015-RRKLIST-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正日編集処理
      *****************************************************************
       410141-TEISEI-SET-SEC           SECTION.
      *
      *H25.2
      *    レセ電移行分はエラー
           IF      SPA-NAI-RRK-SRYKA (SPA-NAI-DOSELNUM)(1:1)
                                       =   "A"
               MOVE    "7011"              TO  SPA-ERRCD
               GO      TO      410141-TEISEI-SET-EXT
           END-IF
      *
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
           IF      SPA-ERRCD   NOT =   SPACE
               GO      TO      410141-TEISEI-SET-EXT
           END-IF
      *
      *    診療日付
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMDW
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
                                                         OR  "4"
                                                         OR  "5"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-SRYYMDW
           END-IF
      *    複数診療科・保険
           IF      SPA-NAI-GRP-DENPNUM(SPA-NAI-DOSELNUM)
                                   NOT =   ZERO
               IF      SPA-NAI-GRP-RENNUM (SPA-NAI-DOSELNUM)
                                           >   1
                   MOVE    SPA-NAI-DOSELNUM    TO  IDX-F2
                   MOVE    ZERO                TO  FLG-FUKU-END
                   PERFORM VARYING     IDX-F   FROM    IDX-F2 BY -1
                           UNTIL       FLG-FUKU-END    =   1
                       IF      SPA-NAI-GRP-DENPNUM(SPA-NAI-DOSELNUM)
                                           NOT =   SPA-NAI-GRP-DENPNUM
                                                                (IDX-F)
                           MOVE    1           TO  FLG-FUKU-END
                       ELSE
                           IF      SPA-NAI-GRP-RENNUM (IDX-F) =   1
                               MOVE    IDX-F       TO  SPA-NAI-DOSELNUM
      ***                      MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
      ****                     MOVE    WRK-ZZ      TO  SPA-GMN-DOSELNUM
                               MOVE    SPA-GMN-NO (IDX-F)
                                                   TO  SPA-NAI-SELNUM
                               MOVE    SPA-NAI-SELNUM  TO  WRK-ZZ
                               MOVE    WRK-ZZ      TO  SPA-GMN-DOSELNUM
                               MOVE    1           TO  FLG-FUKU-END
                           END-IF
                       END-IF
                   END-PERFORM
               END-IF
           END-IF
      *
           MOVE    SPA-NAI-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYYMD
           MOVE    SPA-NAI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-RENNUM
           MOVE    SPA-NAI-DOUJI-RENNUM (SPA-NAI-DOSELNUM)
                                       TO  SPA-DOUJI-RENNUM
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
           MOVE    SPA-SRYYMDW         TO  SPA-GMN-SRYYMD
      *    診療科
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-SRYKA
           MOVE    SPA-NAI-RRK-SRYKA(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-SRYKA
      *    保険組合せ
           MOVE    SPA-NAI-RRK-HKNCOMBI(SPA-NAI-DOSELNUM)
                                       TO  SPA-OLD-HKNCOMBI
      *    ドクター
           MOVE    SPA-NAI-RRK-DRCD (SPA-NAI-DOSELNUM)
                                       TO  SPA-DRCD
      *
      *    訂正処理
           MOVE    1                   TO  SPA-SYORIFLG
      *    初診算定日は変更しない
           MOVE    SPA-GMN-SYOSINYMD   TO  WRK-SPA-GMN-SYOSINYMD
      *
      *    ＳＰＡ編集処理
           INITIALIZE                  SPA-SCR-REC
           INITIALIZE                  ORCSC95AREA
           MOVE    "4"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           MOVE    SPA-SRYYMD          TO  LNK-SC95-RRKYMD
           MOVE    SPA-RENNUM          TO  LNK-SC95-RENNUM
           MOVE    SPA-DOUJI-RENNUM    TO  LNK-SC95I-DOUJI-RENNUM
           MOVE    SPA-NAI-GRP-DENPNUM(SPA-NAI-DOSELNUM)
                                       TO  LNK-SC95-GRP-DENPNUM
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
      *    保険組合せ変更
           IF      SPA-HKNCOMBINUM NOT =   SPA-OLD-HKNCOMBI
               MOVE    1               TO  SPA-HKN-ERR2
           END-IF
      *
      *H31.2
           MOVE    SPA-NAI-NYUIN       TO  FLG-NAI-NYUIN
      *
      *    パラメタファイルより表示
           MOVE    3               TO  FLG-PARASET
           PERFORM 3002-PARA-SET-SEC
      *
      *    スパ共通編集処理（保険組合せ編集しない（ORCSC95AREA)
           MOVE    ORCSC95AREA         TO  WRK-ORCSC95AREA
           PERFORM 310-SPA-SET-SEC
      *
      *H31.2
      *    入院中メッセージは再表示なし
           MOVE    SPACE               TO  WRK-KIDCD
           IF      SPA-KIDCD           =   "0121"
               MOVE    SPA-KIDCD           TO  WRK-KIDCD
               MOVE    SPACE               TO  SPA-KIDCD
               IF      FLG-NAI-NYUIN       =   1
                   MOVE    1                   TO  SPA-NAI-NYUIN
                   MOVE    SPACE               TO  WRK-KIDCD
               END-IF
           END-IF
      *
           MOVE    WRK-ORCSC95AREA     TO  ORCSC95AREA
      *
      *    初診算定日は変更しない
           MOVE    WRK-SPA-GMN-SYOSINYMD   TO  SPA-GMN-SYOSINYMD
      *
           MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)
                                       TO  SPA-GMN-SRYYMD
      *    労災・自賠責の時、日付のみ
           IF      SPA-NAI-RRK-HKNKBN(SPA-NAI-DOSELNUM)  =   "1"
                                                         OR  "2"
                                                         OR  "4"
                                                         OR  "5"
               MOVE    SPA-GMN-RRKYMD (SPA-NAI-DOSELNUM)(1:12)
                                           TO  SPA-GMN-SRYYMD
           END-IF
      *
      *    当月診療行為チェック用編集、当月累計点数計算
           MOVE    ZERO                TO  SPA-ZAITENTOTAL
           INITIALIZE                      SPA-NAIACCT2-AREA
           INITIALIZE                      SPA-RIGAKU-NCNT-AREA
           MOVE    SPACE               TO  SPA-ACCT2-SRYCD
           MOVE    ZERO                TO  SPA-ACCT2-ZAINUM
           MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
           PERFORM 3201-TOUCHKSET-SEC
      *    点滴の変更
           IF      LNK-SC95-TENTEKI    =   1
               MOVE    SPACE           TO  SPA-ACCT2-SRYCD
           END-IF
           IF      SPA-ACCT2-CSYYOURYO     <=  LNK-SC95-CHUSYA
               MOVE    ZERO                TO  SPA-ACCT2-CSYYOURYO
           ELSE
               COMPUTE SPA-ACCT2-CSYYOURYO
                                       =   SPA-ACCT2-CSYYOURYO
                                       -   LNK-SC95-CHUSYA
           END-IF
      *
            PERFORM VARYING     IDX     FROM    1   BY  1
                    UNTIL      (IDX     >   SPA-MAX-LINE )  OR
                               (SPA-K02-TEI-SRYCD (IDX)
                                        =   SPACE )
               IF     (SPA-K02-TEI-SRYCD (IDX)
                                       =   SPA-ACCT2-SRYCD-JYO1)  AND
                      (SPA-K02-TEI-KAISU (IDX)  <=  1      )
                   MOVE    SPACE           TO  SPA-ACCT2-SRYCD-JYO1
               END-IF
               IF     (SPA-K02-TEI-SRYCD (IDX)
                                       =   SPA-ACCT2-SRYCD-JYO2) 
                   MOVE    SPACE           TO  SPA-ACCT2-SRYCD-JYO2
               END-IF
           END-PERFORM
      *
      *    画面用スパ編集処理
           MOVE    1                   TO  FLG-TEISEI
           PERFORM 310-SPA-GMNSET-SEC
      *
      *    累計点数−訂正点数
           COMPUTE SPA-ZAITENTOTAL     =   SPA-ZAITENTOTAL
                                       -   LNK-SC95-GOK-TENSU
      *    ＤＯ編集へ戻す
           MOVE    "F"                 TO  SPA-GMN-TEISEI
           MOVE    ZERO                TO  SPA-NAI-SYORI
           MOVE    SPA-NAI-DOSELNUM    TO  SPA-NAI-TESELNUM
           MOVE    ZERO                TO  SPA-NAI-SELNUM
           MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    SPACE               TO  SPA-GMN-DOSELNUM
      *
           MOVE    1                   TO  SPA-GMN-PAGE
      *??  MOVE    1                   TO  SPA-GMN-CUR
      *    １行目へ
           MOVE    98                  TO  SPA-GMN-CUR
           MOVE    1                   TO  SPA-MAP-IDX
      *
      *    伝票番号がゼロの時エラーとする
           IF     (SPA-DENPNUM         =   ZERO ) AND
                  (SPA-HKNCOMBINUM NOT =   "9999")
               MOVE    "1036"              TO  SPA-ERRCD
           END-IF
      *
      *H31.2
      *    入院中確認メッセージ
           IF      WRK-KIDCD           =   "0121"
               MOVE    WRK-KIDCD       TO  SPA-KIDCD 
           END-IF
      *
           .
       410141-TEISEI-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                    SECTION.
      *
           IF      SPA-MACHIPG         =   "U02"
                                       OR  "Q02"
                                       OR  "Q02A"
                                       OR  "Q02B"
               MOVE    SPA-MACHIPG         TO  WRK-SAKIPG
               PERFORM 210-BACK-SYORI-SEC
           ELSE
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "M01"               TO  WRK-SAKIPG
                   PERFORM 210-BACK-SYORI-SEC
               ELSE
      *        ガイドメッセージ表示へ
                   MOVE    "0104"              TO  SPA-KIDCD
                   PERFORM 520-KIDSET-SEC
               END-IF
           END-IF
      *
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る処理
      *****************************************************************
       210-BACK-SYORI-SEC               SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    WRK-SAKIPG          TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    MOVE    ZERO                TO  IF-RESULT
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID22
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM22
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME22         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
           MOVE    1                   TO  FLG-END2
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "K02"               TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理（確定）
      *****************************************************************
       210-BACK-OK-SEC                SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
      *****MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "M01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    SPACE               TO  SPA-MOTOPG2
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "JOIN"              TO  MCP-PUTTYPE
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
      *    患者取消処理
      *****************************************************************
       222-PTNUMCLEAR-SEC             SECTION.
      *
      *     クリア確認チェック
           IF      (SPA-CLEARCHK-FLG   =   "1"   )  AND
                   (SPA-GMN-PTID   NOT =   ZERO  )
               AND (SPA-GMN-MAX        >   ZERO  )
               MOVE    "0125"              TO  SPA-KIDCD
               PERFORM 520-KIDSET-SEC
           ELSE
               PERFORM 2221-PTNUMCLEAR-SYORI-SEC
           END-IF
      *
           .
       222-PTNUMCLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者取消処理
      *****************************************************************
       2221-PTNUMCLEAR-SYORI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        排他制御解除処理
               PERFORM 990-LOCK-OUT-SEC
           END-IF
      *
           PERFORM 210-PARA-DEL-SEC
           MOVE    ZERO                TO  FLG-END2
      *
           INITIALIZE              SPA-K02
           INITIALIZE              SPA-KYOTUKEY
           INITIALIZE              SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    0               TO  SPA-GMN-CUR
      *
           .
       2221-PTNUMCLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SEC                   SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
           AND    (SPA-KIDCD           =   SPACE )
      *        確認メッセージ
               MOVE    "0116"              TO  SPA-KIDCD
           END-IF
      *
           .
       220-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア　処理
      *****************************************************************
       220-CLEAR-SYORI-SEC            SECTION.
      *
      **** IF      SPA-GMN-PTID    NOT =   ZERO
      *        ワーク診療行為クリア
      *        MOVE    ZERO            TO  FLG-MOD-FLG
      *        PERFORM 4900-WKSRYACT-CLEAR-SEC
      *****END-IF
      *
           INITIALIZE              SPA-SCR-REC
      *
           MOVE    ZERO            TO  SPA-GMN-GOKEI
      *
           MOVE    ZERO            TO  SPA-NAI-TIMEFLG
      *
           MOVE    1               TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           IF      SPA-SYORIFLG        =   ZERO
      *        排他中は削除なし
             AND  (SPA-LOCK            =   ZERO )
               PERFORM 2201-WKSRYACT-CHK-SEC
           ELSE
               MOVE    ZERO            TO  FLG-WKSRYACT-ARI
           END-IF
           IF      FLG-WKSRYACT-ARI    =   ZERO
               CONTINUE
           ELSE
               MOVE    "0127"              TO  SPA-KIDCD
           END-IF
           .
       220-CLEAR-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開中の判定処理
      *****************************************************************
       2201-WKSRYACT-CHK-SEC            SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           MOVE    ZERO                TO  FLG-WKSRYACT-ARI
           PERFORM
               UNTIL  (FLG-WKSRYACT        =   1   )  OR
                      (FLG-WKSRYACT-ARI    =   1   )
               IF      WKSRY-MOD-FLG   NOT =   ZERO
                   MOVE    1               TO  FLG-WKSRYACT-ARI
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       2201-WKSRYACT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    中途終了展開中の削除処理
      *****************************************************************
       2202-CLEAR-WKSRYACT-SEC            SECTION.
      *
      *    展開中の削除
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
           MOVE    SPA-SRYYMD          TO  WKSRY-SRYYMD
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               IF      WKSRY-MOD-FLG       NOT =   ZERO
                   MOVE    WKSRYACT-REC        TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_wksryact"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "1999"          TO  SPA-ERRCD
                       DISPLAY "K02 WKSRYACT DEL MCP-RC:" MCP-RC
                               ",KEY:" WKSRY-KEY
                   END-IF
               END-IF
      *
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       2202-CLEAR-WKSRYACT-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
      *
           IF      SPA-LOCK            =   ZERO
               CONTINUE
           ELSE
               GO      TO      4900-WKSRYACT-CLEAR-EXT
           END-IF
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SPA-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  WKSRY-PTID
      *
           MOVE    WKSRYACT-REC        TO  MCPDATA-REC
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               EVALUATE    FLG-MOD-FLG
                   WHEN    1
      *                展開中止
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    ZERO                TO  WKSRY-MOD-FLG
                       END-IF
                   WHEN    2
      *                前の展開
                       IF      WKSRY-MOD-FLG       =   9
                           MOVE    1                   TO  WKSRY-MOD-FLG
                       ELSE
                           IF      WKSRY-MOD-FLG       =   1
                               MOVE    ZERO            TO  WKSRY-MOD-FLG
                           END-IF
                       END-IF
                   WHEN    0
                       MOVE    ZERO                TO  WKSRY-MOD-FLG
               END-EVALUATE
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "1999"          TO  SPA-ERRCD
                   DISPLAY "K02 WKSRYACT UPD MCP-RC:" MCP-RC
                               ",KEY:" WKSRY-KEY
               END-IF
      *
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           END-PERFORM
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4900-WKSRYACT-CLEAR-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外院内ボタン処理
      *****************************************************************
       4101-INGAI-SYORI-SEC             SECTION.
      *
           EVALUATE    SPA-GMN-INGAI
      *        院外へ
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "0"             TO  SPA-INGAIKBN
      *        院内 
               WHEN    "F"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "1"             TO  SPA-INGAIKBN
           END-EVALUATE
      *
      *    コード振替え
           PERFORM 4101-INGAI-HENKOU-SEC
      *
      *    診療行為、計計算処理、編集
           MOVE    "2"             TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE           TO  FLG-TOROKU
      *
           .
       4101-INGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    小児科外来診察料、老人外総診断の算定変換処理
      *****************************************************************
       4101-INGAI-HENKOU-SEC             SECTION.
      *
      *    院外院内振替え処理
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                      ORCSC10SAREA
           MOVE    "4"                 TO  LNK-SC10S-SYORI
           CALL    "ORCSC10S"          USING
                                       ORCSC10SAREA
                                       SPA-AREA
                                       SPA-SCR-REC
                                       SPA-K02
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
           .
       4101-INGAI-HENKOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    前回処方箋発行画面へ処理
      *****************************************************************
       4124-MAESYOHOU-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4124-MAESYOHOU-SYORI-EXT
           END-IF
      *
      *!!!!
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
      *********   (SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4124-MAESYOHOU-SYORI-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG3
           MOVE    "KA01"              TO  SPA-SAKIPG
      *
      *****MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "KA01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4124-MAESYOHOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方箋発行処理
      *****************************************************************
       4124-SYOHOU-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4124-SYOHOU-SYORI-EXT
           END-IF
      *
      *    帳票プログラム取得
      *    処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000002"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1032-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1030"              TO  SPA-ERRCD
               GO      TO      4124-SYOHOU-SYORI-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
      *    処方箋印刷（Ａ５）
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           MOVE    "01"                TO  SPA-PRT-FLG
           INITIALIZE                  ORCHC19AREA
           EVALUATE    FLG-SYOHOU
               WHEN    1
      *            頭書
                   MOVE    "2"                 TO  ORCHC19-KBN
      *            再印刷なし
                   MOVE    2                   TO  SPA-CLIENT-TEMP-FLG
      *            クライアント印刷用
                   IF    ( SPA-CLIENT-PRT-FLG  =   2 )
                     OR  ( SPA-PRTCONF         =   "1" )
                       PERFORM 41241-SYOHOU-CLIENTPRT-SEC
                   END-IF
               WHEN    3
      *            中途終了時（院外）
                   MOVE    "4"                 TO  ORCHC19-KBN
               WHEN    4
      *            中途終了時（院内）
                   MOVE    "5"                 TO  ORCHC19-KBN
               WHEN    OTHER
      *            前回処方
                   MOVE    "1"                 TO  ORCHC19-KBN
           END-EVALUATE
      *
           CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                               ORCHC19AREA
      *
      *    頭書印刷のクライアント印刷処理
           IF      (FLG-SYOHOU         =   1  )
           AND     (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                    TO  SPA-CLIENT-TEMP-FLG
      *
           .
       4124-SYOHOU-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理（頭書）
      *****************************************************************
       41241-SYOHOU-CLIENTPRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      **** MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(01)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *    MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      *    MOVE    1                   TO  FLG-CLIENT-OK
           .
       41241-SYOHOU-CLIENTPRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    セット登録画面遷移処理
      *****************************************************************
       420-KOUISET-SEC             SECTION.
      *
      *    患者が確定してない時、戻った時表示を空白
           IF      SPA-PTID            =   ZERO
               MOVE    SPACE           TO  SPA-GMN-PTNUM
           ELSE
      *        診療行為入力チェック
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE )
                  OR  (SPA-KIDCD       NOT =   SPACE )
                   GO      TO      420-KOUISET-EXT
               END-IF
      *
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K05"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K05"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       420-KOUISET-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新画面　処理
      *****************************************************************
       430-SANTEIRRK-SEC             SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"          TO  SPA-ERRCD
               MOVE    00              TO  SPA-GMN-CUR
               MOVE    3               TO  SPA-HKN-ERR
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
      *    診療行為入力チェック
           MOVE    1                   TO  FLG-HENKOU
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
      ****        (SPA-KIDCD           =   "0180" ) 
               GO      TO      430-SANTEIRRK-EXT
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  SPA-PTNUM
           MOVE    SPA-GMN-PTID        TO  SPA-PTID
           MOVE    SPA-GMN-NAME        TO  SPA-NAME
           MOVE    SPA-GMN-KANANAME    TO  SPA-KANANAME
           MOVE    SPA-GMN-SEX         TO  SPA-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  SPA-BIRTHDAYW
           MOVE    SPA-GMN-SRYYMD      TO  SPA-SRYYMDW
           MOVE    SPA-NAI-BIRTHDAY    TO  SPA-BIRTHDAY
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
           MOVE    SPA-GMN-SRYKA       TO  SPA-SRYKA
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "K06"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K06"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       430-SANTEIRRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コード設定登録　処理
      *****************************************************************
       4106S-INPUTCD-SYORI-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD       NOT =   SPACE )
      *****       (SPA-KIDCD           =   "0180" )
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    初期表示
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK023"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           IF      SPA-K023-MAX        =   ZERO
               MOVE    "1012"              TO  SPA-ERRCD
               GO      TO      4106S-INPUTCD-SYORI-EXT
           END-IF
      *    画面カーソルセット処理
           MOVE    1                   TO  SPA-K023-CUR
           MOVE    1                   TO  SPA-K023-PAGE
           MOVE    1                   TO  SPA-K023-IDX
           MOVE    1                   TO  SPA-K023-MAP
           MOVE    "INPUTCD101"        TO  MCP-WIDGET
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K023"              TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K023"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4106S-INPUTCD-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録　処理
      *****************************************************************
       440-BYOTOROKU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID      =   ZERO
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
               GO      TO      440-BYOTOROKU-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-HENKOU
      *    基本チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
      ******      (SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE  )  OR
                  (FLG-END             =   1      )
               CONTINUE
           ELSE
      *        病名登録遷移処理
               PERFORM 4401-C02SET-SEC
           END-IF
      *
           .
       440-BYOTOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    病名登録遷移処理
      *****************************************************************
       4401-C02SET-SEC             SECTION.
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "C02"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "C02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4401-C02SET-EXT.
           EXIT.
      *    
      *****************************************************************
      *    患者登録へ
      *****************************************************************
       440-KNJADD-SEC             SECTION.
      *
      *    患者登録の権限がない時、エラー
           IF      SPA-GSRAUTH-12      =   "1"
               CONTINUE
           ELSE
               MOVE    "9300"              TO  SPA-ERRCD
               MOVE    ZERO                TO  SPA-GMN-CUR
               GO  TO  440-KNJADD-EXT
           END-IF
      *
           IF      SPA-PTNUM(1:1)      =   "*"
               MOVE    ZERO                TO  SPA-PTID
           ELSE
      *
      *        患者存在チェック
               IF      SPA-PTID            =   ZERO
                   MOVE    SPACE               TO  SPA-PTNUM
               ELSE
      *
      *            診療行為入力チェック
                   MOVE    1                   TO  FLG-HENKOU
                   PERFORM 41011-KOUI-ALLHEN-SEC
                   IF     (SPA-ERRCD       NOT =   SPACE )  OR
      **************      (SPA-KIDCD           =   "0180")  OR
                          (SPA-KIDCD       NOT =   SPACE )  OR
                          (FLG-END             =   1     )
                      GO      TO      440-KNJADD-EXT
                   END-IF
      *R02.1
      *            患者登録で変更
                   IF      SPA-NAI-PTINFFLG    =   1
                       MOVE    "0179"              TO  SPA-ERRCD
                       MOVE    90                  TO  SPA-GMN-CUR
                       GO      TO      440-KNJADD-EXT
                   END-IF
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
           MOVE    "P02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       440-KNJADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０２Ｎ処理へ
      *****************************************************************
       450-K02N-SYORI-SEC                   SECTION.
      *
           MOVE    "K02N"              TO  SPA-SAKIPG
           MOVE    SPA-MACHIPG         TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE     NOT =   SPACE
               MOVE    WRK-PUTTYPE         TO  MCP-PUTTYPE
           ELSE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           END-IF
           MOVE    "K02N"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       450-K02N-SYORI-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    本院分院切替へ
      *****************************************************************
       409-GRPCHK-SEC             SECTION.
      *
           MOVE    SPA-GMN-PTNUM(2:2)  TO  WRK-HOSPNUM
           MOVE    ZERO                TO  FLG-OK
           MOVE    SPACE               TO  WRK-GRPHOSPUSERID
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   9   )
                          OR  (FLG-OK  =   1 )
                          OR  (SPA-GRPHOSPNUM (IDX)    =   ZERO )
               IF      WRK-HOSPNUM     =   SPA-GRPHOSPNUM (IDX)
                   MOVE    SPA-GRPHOSPUSERID (IDX)
                                               TO  WRK-GRPHOSPUSERID
                   MOVE    1                   TO  FLG-OK
               END-IF
           END-PERFORM
           IF      WRK-GRPHOSPUSERID   =   SPACE
               MOVE    "9210"              TO  SPA-ERRCD
           ELSE
               MOVE    SPA-MACHIPG         TO  WRK-MACHIPG
               INITIALIZE                      SPA-AREA
               MOVE    WRK-GRPHOSPUSERID   TO  SPA-OPID
               INITIALIZE                  ORCSCOMMONAREA
               INITIALIZE                  SYS-1010-REC
               MOVE    "K02"               TO  ORCSCOMMON-PG
               MOVE    WRK-HOSPNUM         TO  ORCSCOMMON-HOSPNUM
               CALL    "ORCSCOMMON"        USING
                                           MCPAREA
                                           SPA-AREA
                                           ORCSCOMMONAREA
                                           SYS-1010-REC
               IF      SPA-BEDFLG          =   ZERO
                   MOVE    "M01"               TO  SPA-MACHIPG
               ELSE
                   MOVE    "M01N"              TO  SPA-MACHIPG
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        入院機能の判定
      *        IF      (SPA-BEDFLG         >   ZERO )
      *            PERFORM 3009-NYUINCHK-SEC
      *        END-IF
      *        業務選択画面より
      *        IF      FLG-END             =   ZERO
                   PERFORM 30010-INIT-GMN-SEC
      *        END-IF
           END-IF
      *
           .
       409-GRPCHK-EXT.
           EXIT.
      *!!
      *
      *****************************************************************
      *    会計カードへ
      *****************************************************************
       450-KAIKEI-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                      (SPA-KIDCD       NOT =   SPACE  )  OR
                      (FLG-END             =   1      )
                   GO      TO      450-KAIKEI-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
           MOVE    "J02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "J02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       450-KAIKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納登録へ
      *****************************************************************
       460-SYUNOU-SEC             SECTION.
      *
           IF      SPA-GMN-PTID    NOT =   ZERO
      *        更新前の基本チェック
               MOVE    1                   TO  FLG-HENKOU
               PERFORM 41011-KOUI-ALLHEN-SEC
               IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                      (SPA-KIDCD       NOT =   SPACE  )  OR
                      (FLG-END             =   1      )
                   GO      TO      460-SYUNOU-EXT
               END-IF
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG2
           MOVE    "S02"               TO  SPA-SAKIPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           INITIALIZE                      SPA-WORK
           MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "S02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
          .
       460-SYUNOU-EXT.
           EXIT.
      *
      *****************************************************************
      *   ワーク行為負担パラメタを作成　処理
      *****************************************************************
       4410-WSINPARA-SEC             SECTION.
      *
      *    ＳＰＡ保存パラメタを作成
      **   PERFORM 410021-PARA-SYORI-SEC
      *
      *    MOVE    ZERO                TO  IDX-P
      *    MOVE    "2"                 TO  FLG-SYORI
      *    IF      SPA-PTID            =   ZERO
      *        CONTINUE
      *    ELSE
      *        PERFORM 45101-SNRYO-SYORI-SEC
      **** END-IF
      *
           INITIALIZE                  ORCSCWKSRYAREA
           MOVE    "4"                 TO  ORCSCWKSRY-KBN
           MOVE    "K02"               TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    1               TO  FLG-END2
           END-IF
      *
           .
       4410-WSINPARA-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    途中終了　処理
      *****************************************************************
       4110-KYOSEIEND-SEC             SECTION.
      *
      *    初診算定日の自動算定時は、中途終了できない。
           IF     (SPA-SYOSIN-YMD      =   SPACE ) AND
                  (SPA-SYOSIN-SRYCD    =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1023"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *    訂正中の時、中途終了できない。
           IF      SPA-SYORIFLG        =   1
               MOVE    "1024"          TO  SPA-ERRCD
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
      *H26.12
           IF      SPA-ERRCD           =   "0178"
               MOVE    SPACE           TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-KIDCD
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE  )  OR
                  (SPA-KIDCD       NOT =   SPACE  )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    基本チェックのみ
           MOVE    "2"                 TO  FLG-TOROKU
      *H28.4    湿布薬チェックの為
           MOVE    1                   TO  FLG-CHUEND
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           MOVE    ZERO                TO  FLG-CHUEND
      *    外来管理加算確認はしない
           IF      SPA-KIDCD           =   "0102"   OR
                                           "0113"
                                       OR  "0213"
               MOVE    SPACE           TO  SPA-KIDCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      4110-KYOSEIEND-EXT
           END-IF
      *
      *    ワーク行為負担データを作成後、選択画面（Ｋ０１）へ
           PERFORM 45109-SNRYO-NYU-SEC
      *
           .
       4110-KYOSEIEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    前頁　処理
      *****************************************************************
       450-MAEGMN-SEC             SECTION.
      *
      *H25.5
      *    警告メッセージ非表示へ戻す
      **** MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE  )  OR
                  (FLG-END             =   1     )
               GO      TO      450-MAEGMN-EXT
           END-IF
      *
      *    行位置とカーソル位置の決定
           PERFORM 10021-K02SPA-INIT-SET-SEC
      *
           IF      K02-PANDATABLE1-TROW    =  ZERO
               MOVE    ZERO                TO  WRK-IDX2
           ELSE
               MOVE    K02-PANDATABLE1-TROW    TO  WRK-IDX2
               IF      K02-INPUTCD(WRK-IDX2)   =   SPACE
                   MOVE    ZERO                TO  WRK-IDX2
               END-IF
           END-IF
      *!!
      *画面の位置取得できないのでゼロ設定
      *!!!
      ***  MOVE    ZERO                TO  WRK-IDX2
      *!!
           IF      WRK-IDX2            =   ZERO
               MOVE    SPA-MAP-IDX     TO  WRK-IDX2
           END-IF
           IF     (WRK-IDX2            >   WRK-MAP-MAX )
               MOVE    WRK-MAP-MAX         TO  WRK-IDX2
           END-IF
           IF      WRK-IDX2            >   SPA-GMLINCNT
               COMPUTE WRK-GYO         =   WRK-IDX2    -   SPA-GMLINCNT
           ELSE
               MOVE    1               TO  WRK-GYO
           END-IF
      *
           PERFORM VARYING     IDX     FROM    WRK-IDX2  BY  -1
                   UNTIL      (IDX     <   1   )
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-GYO      )
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   MOVE    1                   TO  IDX
                   MOVE    1                   TO  FLG-F6
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
           IF      FLG-F6              =   ZERO
               MOVE    "1"                 TO  SPA-COM-CUR (1)
           END-IF
      *
           .
       450-MAEGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    次頁　処理
      *****************************************************************
       460-ATOGMN-SEC             SECTION.
      *
      *H25.5  警告メッセージ表示対応
      ***  MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      460-ATOGMN-EXT
           END-IF
      *
      *    行位置とカーソル位置の決定
           PERFORM 10021-K02SPA-INIT-SET-SEC
      *
           IF     (K02-PANDATABLE1-TROW     =  ZERO )
               MOVE    ZERO                TO  WRK-IDX2
           ELSE
               MOVE    K02-PANDATABLE1-TROW    TO  WRK-IDX2
               IF      K02-INPUTCD(WRK-IDX2)   =   SPACE
                   MOVE    ZERO                TO  WRK-IDX2
               END-IF
           END-IF
      *
           IF      WRK-IDX2            =   ZERO
               MOVE    SPA-MAP-IDX     TO  WRK-IDX2
           END-IF
           COMPUTE WRK-GYO             =   WRK-IDX2 +   SPA-GMLINCNT
      *
           MOVE    ZERO                TO  FLG-F7
           PERFORM VARYING     IDX     FROM    WRK-IDX2  BY  1
                   UNTIL      (IDX     >   SPA-MAX-LINE   )
                           OR (IDX     >   SPA-GMN-MAX    )
               IF     (SPA-COM-PAGE (IDX)  =   SPA-GMN-PAGE )  AND
                      (SPA-COM-GYOU (IDX)  =   WRK-GYO      )
                   MOVE    "1"                 TO  SPA-COM-CUR (IDX)
                   MOVE    SPA-MAX-LINE        TO  IDX
                   MOVE    1                   TO  FLG-F7
               END-IF
           END-PERFORM
      *
           MOVE    1                   TO  SPA-GMN-CUR
      *
           .
       460-ATOGMN-EXT.
           EXIT.
      *
      *****************************************************************
      *    １行目へ　処理
      *****************************************************************
       4501-CTRHOME-SEC             SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE  )  OR
                  (FLG-END             =   1     )
               GO      TO      4501-CTRHOME-EXT
           END-IF
      *
           MOVE    01                  TO  SPA-GMN-CUR
           MOVE    "1"                 TO  SPA-COM-CUR (1)
           .
       4501-CTRHOME-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 前頁　処理
      *****************************************************************
       4109-BC06-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "7008"              TO  SPA-ERRCD
           ELSE
               IF      SPA-RRK-PAGE        <=  1
                   MOVE    "7010"              TO  SPA-ERRCD
               ELSE
                   COMPUTE SPA-RRK-PAGE    =   SPA-RRK-PAGE  -  1
      *
                   INITIALIZE                  ORCSC95AREA
                   MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
                   MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
                   MOVE    "N"                 TO  LNK-SC95-KBN
                   MOVE    "K02"               TO  LNK-SC95-PG
                   CALL    "ORCSC95"           USING
                                               MCPAREA
                                               ORCSC95AREA
                                               SPA-AREA
                                               SPA-K01KYOUTU
                                               SPA-K02
                   MOVE    1                   TO  FLG-GMN-INIT
               END-IF
           END-IF
           .
       4109-BC06-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴 次頁　処理
      *****************************************************************
       4109-BC07-SYORI-SEC             SECTION.
      *
           IF      SPA-SYORIFLG        =   1
               MOVE    "7008"              TO  SPA-ERRCD
           END-IF
           IF      SPA-RRK-NEXT        =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "7009"              TO  SPA-ERRCD
               END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               COMPUTE SPA-RRK-PAGE    =   SPA-RRK-PAGE  +  1
      *
               INITIALIZE                  ORCSC95AREA
               MOVE    SPA-NAI-SRYYMD      TO  LNK-SC95-SRYYMD
               MOVE    SPA-GMN-SRYYMD      TO  LNK-SC95-SRYYMDG
               MOVE    "N"                 TO  LNK-SC95-KBN
               MOVE    "K02"               TO  LNK-SC95-PG
               CALL    "ORCSC95"           USING
                                           MCPAREA
                                           ORCSC95AREA
                                           SPA-AREA
                                           SPA-K01KYOUTU
                                           SPA-K02
               MOVE    1                   TO  FLG-GMN-INIT
           END-IF
      *
           .
       4109-BC07-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科保険選択　処理
      *****************************************************************
       450-MAECUR-SEC             SECTION.
      *
           IF     (SPA-CONTKBN         =   SPACE )  AND
                  (SPA-FUKU-SYORI      =   SPACE )
               CONTINUE
           ELSE
               MOVE    "1057"          TO  SPA-ERRCD
           END-IF
           IF      SPA-GMN-MAX         =   ZERO
               MOVE    "1056"          TO  SPA-ERRCD
           END-IF
      *    診療行為入力チェック
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41011-KOUI-ALLHEN-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (SPA-KIDCD           =   SPACE )
               IF      SPA-GMN-MAX     >   398
                   MOVE    "7001"              TO  SPA-ERRCD
               ELSE
                   PERFORM 4101131-K97-SYORI-SEC
               END-IF
           END-IF
      *
           .
       450-MAECUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       480-DOSYORI-SEC            SECTION.
      *
      *    患者存在チェック
           IF      SPA-PTID            =   ZERO
               MOVE    "1005"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
           IF      SPA-HKNCOMBINUM     =   SPACE
               MOVE    "1006"              TO  SPA-ERRCD
               MOVE    3                   TO  SPA-HKN-ERR
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
      *H28.6
      *    数量ゼロはＤＯへ
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE           TO  SPA-ERRCD
           END-IF
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      480-DOSYORI-EXT
           END-IF
      *
      **** MOVE    ZERO                TO  SPA-NAI-DOSELNUM
           MOVE    SPACE               TO  SPA-GMN-DOSELNUM
           MOVE    1                   TO  SPA-NAI-DOSELNUM
           MOVE    SPA-GMN-NO (1)      TO  SPA-NAI-SELNUM
      *****MOVE    SPA-NAI-DOSELNUM    TO  WRK-ZZ
      *****MOVE    WRK-ZZ              TO  SPA-GMN-DOSELNUM
      *
      *    ＤＯ検索処理へ
           PERFORM 4801-K09-SYORI-SEC
           .
       480-DOSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＯ処理へ
      *****************************************************************
       4801-K09-SYORI-SEC            SECTION.
      *
      *    行挿入行をなくす
           PERFORM 420-SYOKYO-SEC
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK09"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K09"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
      *****MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K09"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4801-K09-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｋ０１処理へ
      *****************************************************************
       4001-K01-SYORI-SEC                   SECTION.
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
     *************(SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE  )  OR
                  (FLG-END             =   1     )
               GO      TO      4001-K01-SYORI-EXT
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    "K01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    "K01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4001-K01-SYORI-EXT.
           EXIT.
      *H28.12
      *****************************************************************
      *    電子お薬手帳情報画面処理
      *****************************************************************
       410-K024-SYORI-SEC             SECTION.
      *
      *    更新前の基本チェック
      *    PERFORM 41001-KIHON-CHK-SEC
      *    IF     (SPA-ERRCD       NOT =   SPACE  )  OR
      *           (SPA-KIDCD       NOT =   SPACE )
      *****       (SPA-KIDCD           =   "0180" )
      *        GO      TO      410-K022-SYORI-EXT
      *    END-IF
      *
      *    更新前のチェック
      *    MOVE    "1"                 TO  FLG-TOROKU
      *    PERFORM 510-TBLHEN-SEC
      *    MOVE    SPACE               TO  FLG-TOROKU
      *    IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      410-K022-SYORI-EXT
      *    END-IF
      *
      *    初期表示
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK024"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    画面カーソルセット処理
           MOVE    1                   TO  SPA-K024-CUR
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    診療行為一覧へ
           MOVE    "K024"              TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K024"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       410-K024-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    算定処理へ
      *****************************************************************
      *470-SANTEI-SEC             SECTION.
      *
      *    MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
      *    PERFORM 41011-KOUI-ALLHEN-SEC
      *    IF     (SPA-ERRCD       NOT =   SPACE )  OR
      *           (SPA-KIDCD           =   "0180" )  OR
      *           (FLG-END             =   1     )
      *        GO      TO      470-SANTEI-EXT
      *    END-IF
      *
      *    MOVE    SPACE               TO  LINKAREA
      *    MOVE    SPACE               TO  SPA-MOTOPG
      *    MOVE    SPA-AREA            TO  SPA-COMMON
      *    MOVE    "LINK"              TO  MCP-STATUS
      *    MOVE    SPACE               TO  MCP-EVENT
      *    CALL    "ORCGK04"           USING
      *            MCPAREA
      *            SPAAREA
      *            LINKAREA
      *            SCRAREA.
      *
      *    初期カーソル設定
      *    MOVE    "B01"               TO  MCP-WIDGET
      *
      *    MOVE    "K04"               TO  SPA-SAKIPG
      *    MOVE    "K02"               TO  SPA-MOTOPG
      *
      *    MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *    MOVE    SPACE               TO  LINKAREA
      *
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *    MOVE    "K04"               TO  MCP-WINDOW
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    MOVE    1                   TO  FLG-END
      *
      *    .
      *470-SANTEI-EXT.
      **** EXIT.
      *    
      *****************************************************************
      *    前回患者番号編集処理
      *****************************************************************
       230-MAEKPTNUM-SEC             SECTION.
      *
           IF      SPA-LAST-PTNUM      =   SPACE
               CONTINUE
           ELSE
               MOVE    SPA-LAST-PTNUM      TO  K02-PTNUM
               PERFORM 41010-PTNUM-SYORI-SEC
           END-IF
      *
           .
       230-MAEKPTNUM-EXT.
           EXIT.
      *
      *****************************************************************
      *    氏名検索へ
      *****************************************************************
       470-P97-KENSAKU-SEC             SECTION.
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "P97"               TO  SPA-SAKIPG
      *
      *    氏名を設定
           MOVE    WRK-P97-NAME        TO  SPA-NAME
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "P97"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       470-P97-KENSAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    予約登録へ
      *****************************************************************
       480-YOYAKU-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
      **********  (SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      480-YOYAKU-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-MOTOPG3
           MOVE    "Y01"               TO  SPA-SAKIPG
      *
      *    MOVE    SPACE               TO  SPA-KYOTUKEY
      *    INITIALIZE                      SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE   "Y01"                TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       480-YOYAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付一覧へ
      *****************************************************************
       490-UKEICHI-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
      *****       (SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      490-UKEICHI-EXT
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
           MOVE    "U01"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "U01"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       490-UKEICHI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付画面へ
      *****************************************************************
       4102-UKETUKE-SEC             SECTION.
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
      ****        (SPA-KIDCD           =   "0180" )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4102-UKETUKE-EXT
           END-IF
      *
      *    保存パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
      *
           MOVE    "U02"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    SPA-K02             TO  LNK-SCRSPA
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    "U02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4102-UKETUKE-EXT.
           EXIT.
      *
      *****************************************************************
      *    メモ登録処理へ
      *****************************************************************
       4188-MEMO-SYORI-SEC             SECTION.
      *
      *    患者の確定されてない時、処理をしない
           IF     (SPA-GMN-PTNUM       =   SPACE )  OR
                  (SPA-PTID            =   ZERO  )
               GO      TO      4188-MEMO-SYORI-EXT
           END-IF
      *
           MOVE    1                   TO  FLG-HENKOU
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )  OR
                  (FLG-END             =   1     )
               GO      TO      4188-MEMO-SYORI-EXT
           END-IF
      *
      *    ワーク行為負担パラメタを作成
           PERFORM 4410-WSINPARA-SEC
      *
      *    戻り時、画面を元へ戻す
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K02             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "XF01"              TO  SPA-SAKIPG
      *
      **** MOVE    SPACE               TO  SPA-KYOTUKEY
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "XF01"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4188-MEMO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ヘルプ処理
      *****************************************************************
       4199-HELP-SYORI-SEC             SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKHELP"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *
      *    戻り先
           MOVE    "K02"               TO  SPA-ETC-MOTOPG
           MOVE    "KHELP"             TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KHELP"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4199-HELP-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正処理
      *****************************************************************
       4101-TEISEI-SEC             SECTION.
      *
           MOVE    04                  TO  SPA-GMN-CUR
      *
           EVALUATE    SPA-GMN-TEISEI
      *        ＤＯ検索
               WHEN    "T"
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
      *        訂正診療日選択
               WHEN    "F"
      *            診療室入力の時、訂正はできないこととする
                   IF      SPA-BUNSAN          =   "1"
                       MOVE    "1026"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "T"             TO  SPA-GMN-TEISEI
                       MOVE    1               TO  SPA-NAI-SYORI
                   END-IF
           END-EVALUATE
      *
           .
       4101-TEISEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新前の基本チェック処理
      *****************************************************************
       41001-KIHON-CHK-SEC             SECTION.
      *
      *    患者マスター存在チェック
           IF      SPA-PTNUM           =   SPACE
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
           IF     (FLG-PTINF           =   1     )  OR
                  (SPA-PTNUM           =   SPACE )
               MOVE    "1003"              TO  SPA-ERRCD
               MOVE    00                  TO  SPA-GMN-CUR
           ELSE
               IF      SPA-HKNCOMBINUM     =   SPACE
                   MOVE    "1006"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-HKN-ERR
                   MOVE    00              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-RJN-ERR
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1027"              TO  SPA-ERRCD
      *            老人保険編集エラーがある時、エラーとする
                   WHEN    2
                       MOVE    "1070"              TO  SPA-ERRCD
      *            後期高齢者の保険がない時
                   WHEN    3
                       MOVE    "1076"              TO  SPA-ERRCD
      *            退職国保で６５歳以上
                   WHEN    9
                       MOVE    "1077"              TO  SPA-ERRCD
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD           =   SPACE
               EVALUATE    SPA-HKN-ERR
      *            保険エラーがある時、エラーとする
                   WHEN    1
                       MOVE    "1028"              TO  SPA-ERRCD
                   WHEN    2
                       MOVE    "1032"              TO  SPA-ERRCD
      *           保険未選択の場合、エラー
                   WHEN    3
                       MOVE    "1006"              TO  SPA-ERRCD
      *            保険未選択の場合、エラー
                   WHEN    4
                       MOVE    "1034"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
      *            受診履歴上限、エラー
                   WHEN    5
                       MOVE    "0220"              TO  SPA-ERRCD
                       MOVE    6                   TO  SPA-GMN-CUR
      *            入院中保険エラー
                   WHEN    6
                       MOVE    "9036"              TO  SPA-ERRCD
                       MOVE    5                   TO  SPA-GMN-CUR
               END-EVALUATE
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        会計照会で受診履歴変更
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JYURRKFLG   =   2
                   MOVE    "K178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
                   MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
               END-IF
           END-IF
      *
      *R02.1
      *        患者登録で変更
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NAI-PTINFFLG    =   1
                   MOVE    "0179"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      **   IF      SPA-ERRCD       NOT =   SPACE
      *        GO      TO      41001-KIHON-CHK-EXT
      ***  END-IF
      *    地方公費のみの場合、警告
           IF     (SPA-NAI-KOHFLG      =   1      )
             AND  (SPA-ERRCD           =   SPACE  )
               IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KOHFLG-ERR
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF     (SPA-NAI-ZENKIFLG    =   1      )
             AND  (SPA-ERRCD           =   SPACE  )
               IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                   MOVE    "K171"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-ZENKIFLG-ERR
               END-IF
           END-IF
      *H25.2
      *    前回保険組合せの判定チェック
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-HKNCHK-FLG  NOT =   ZERO   )  AND 
                  (SPA-HKNCHK-CHK      =   ZERO   )
               IF      SPA-HKNCHK-FLG      =   2
                   MOVE    "K173"              TO  SPA-ERRCD
               ELSE
                   MOVE    "K172"              TO  SPA-ERRCD
               END-IF
               MOVE    5                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-HKNCHK-CHK
               MOVE    ZERO                TO  SPA-HKNCHK-FLG
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        訂正時の保険自動変換
               IF      SPA-HKN-ERR2        =   1
                   MOVE    "K176"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-HKN-ERR2
               END-IF
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   1      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K174"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   2      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K175"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *
      *    診療行為入力チェック
           PERFORM 41011-KOUI-ALLHEN-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD           =   "0180")  OR
                  (FLG-END             =   1     )
               GO      TO      41001-KIHON-CHK-EXT
           END-IF
      *
      *    診療日付のチェック訂正の時、入力エラー
           IF      SPA-SYORIFLG        =   1
               IF      K02-SRYYMD      NOT =   SPA-GMN-SRYYMD
                   MOVE    1                   TO  SPA-GMN-CUR
                   MOVE    "1009"              TO  SPA-ERRCD
               END-IF
           ELSE
      *
               INITIALIZE                      ORCSGDAYAREA
               MOVE    SPA-GMN-SRYYMD      TO  SGDAY-INDAY
               CALL    "ORCSGDAY"          USING
                                           ORCSGDAYAREA
               IF      SGDAY-RC            =   ZERO
                   MOVE    SGDAY-OTDAY         TO  SPA-GMN-SRYYMD
      *            誕生日以前の時、エラー
                   IF      SGDAY-SDAY          <   SPA-BIRTHDAY
                        MOVE    "1011"         TO  SPA-ERRCD
                        MOVE    7              TO  SPA-GMN-CUR
                   END-IF
      *            変更のない時、以降の処理はそのまま
                   IF      SGDAY-SDAY      NOT =   SPA-NAI-SRYYMD
                        MOVE    SGDAY-SDAY     TO  SPA-NAI-SRYYMD
                        GO      TO      41001-KIHON-CHK-EXT
                   END-IF
               ELSE
                   MOVE    "1010"          TO  SPA-ERRCD
                   MOVE    7               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           MOVE   ZERO                 TO  IDX-SET
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   SPA-MAX-LINE ) OR
                              (SPA-ERRCD       NOT =   SPACE )   OR
                              (SPA-GMN-INPUTCD(IDX) =   SPACE )
               IF    ((SPA-COM-CHKFLG (IDX)    =   SPACE )  AND
                      (SPA-GMN-INPUTCD(IDX)(1:1)
                                           NOT =   "."  AND "S"
                                               AND "#" 
                                               AND "$"  ))
      *
      *            点数マスタ編集・基本チェック
                   INITIALIZE                  ORCSC92AREA
                   MOVE    SPACE               TO  LNK-SC92-KBN
                   MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
                   MOVE    "K02"               TO  LNK-SC92-PG 
      *            警告チェックあり
                   MOVE    "1"                 TO  LNK-SC92-KEIFLG
                   MOVE    IDX                 TO  LNK-SC92-GMN-IDX
                   MOVE    SPA-COM-INPUTCD (IDX)
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
      *            小児科外来診療科の時、年齢エラーはそのまま
      *            IF      SPA-SYONIKA-ARI3    NOT =   ZERO
      *                IF      LNK-SC92-ERRCD5 NOT =   SPACE
      *                    MOVE    SPACE           TO  LNK-SC92-ERRCD5
      *                END-IF
      *            END-IF
      *
                   PERFORM VARYING     IDX-ERR FROM    1  BY  1
                           UNTIL      (IDX-ERR     >   10     )  OR
                                      (SPA-ERRCD   NOT =   SPACE)
                       IF      LNK-SC92-ERRCD(IDX-ERR) NOT =   SPACE
                           MOVE    LNK-SC92-ERRCD(IDX-ERR)
                                                       TO  SPA-ERRCD
                           IF      IDX-ERR             =   3 OR 6
                               MOVE    LNK-SC92-ERRMSG TO  SPA-ERRMSG2
                           END-IF
                       END-IF
                   END-PERFORM
      *---(01.00.01) LINE ADD  START  ----
      *            警告判定
                   IF      SPA-ERRCD(1:1)      =   "K"
                     AND  (SPA-ERRCD       NOT =   "K033" )
                       IF      SPA-COM-KZMFLG (IDX)  =   SPACE
                           MOVE    "1"         TO  SPA-COM-KZMFLG(IDX)
                       ELSE
                           MOVE    SPACE           TO  SPA-ERRCD
                       END-IF
                   END-IF
      *---(01.00.01) LINE ADD  END    ----
                   IF      SPA-ERRCD       NOT =   SPACE
                       IF      SPA-ERRCD(1:1)      =   "K"
                           MOVE    "2"             TO  SPA-COM-CUR(IDX)
                       ELSE
                           IF      SPA-COM-SET(IDX)    =   SPACE
                               MOVE    "1"         TO  SPA-COM-CUR(IDX)
                           ELSE
                               MOVE    "S001"      TO  SPA-ERRCD
                               MOVE    "1"         TO  SPA-COM-CUR
                                                           (IDX-SET)
                           END-IF
                       END-IF
                   END-IF
               END-IF
      *
               IF      SPA-GMN-INPUTCD(IDX)(1:1)   =   "S"
                   MOVE   IDX              TO  IDX-SET
               END-IF
           END-PERFORM
      *
      *
           .
       41001-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    途中終了画面へ（Ｋ１０）
      *****************************************************************
       4110S-K10-SYORI-SEC            SECTION.
      *
           MOVE    SPA-KIDCD           TO  WRK-KIDCD
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK10"           USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    存在しない時、メッセージ表示
           IF      SPA-K10-MAX         =   ZERO
               IF      SPA-GMN-PTNUM       =   SPACE
                   MOVE    "1019"          TO  SPA-ERRCD
               ELSE
                   MOVE    "1018"          TO  SPA-ERRCD
                   MOVE    WRK-KIDCD       TO  SPA-KIDCD
               END-IF
               GO      TO      4110S-K10-SYORI-EXT
           END-IF
      *
           MOVE    "K10"               TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K10"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       4110S-K10-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定（包括算定チェック）処理
      *****************************************************************
       4911-HKTSANTEI-CHEK-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
      *    更新前のチェック
           MOVE    SPACE               TO  FLG-TOROKU
      *    包括診療チェック
           MOVE    ZERO                TO  SPA-HKTKEI-FLG
           MOVE    1                   TO  FLG-HKTCHK
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           .
       4911-HKTSANTEI-CHEK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       4100-TOROKU-SEC             SECTION.
      *
      *    更新前の基本チェック
           PERFORM 41001-KIHON-CHK-SEC
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
             OR   (FLG-KINKI           =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *TEST
      *TEST
      *****MOVE    1               TO  SPA-ETENSU-CHK
      *    電子点数表のチェック
      *    IF      SPA-ETENSU-CHK  NOT =   ZERO
      *        PERFORM 41001-ETENSU-CHK-SEC
      *        IF     (SPA-ERRCD       NOT =   SPACE )
      *            GO      TO      4100-TOROKU-EXT
      *        END-IF
      *    END-IF
      *TEST
      *TEST
      *
      *    特定疾患処方管理・長期投薬加算有無クリア
           MOVE    ZERO                TO  SPA-CHOKITOFLG
           MOVE    ZERO                TO  SPA-TOKUTEIFLG
      *    更新前のチェック
           MOVE    "1"                 TO  FLG-TOROKU
      *    包括診療チェック
           MOVE    1                   TO  FLG-HKTCHK
           PERFORM 510-TBLHEN-SEC
           MOVE    SPACE               TO  FLG-TOROKU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
      *H30.9
             OR   (FLG-END             =   1     )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *    新規で明細がない時、エラー
           IF     (SPA-GMN-MAX         =   ZERO )  AND
                  (SPA-SYORIFLG        =   ZERO )  AND
                  (SPA-SYOSIN-YMD      =   SPACE)
               MOVE    "9301"          TO  SPA-ERRCD
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    後期高齢者で当月生活習慣病管理料算定中
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-SEIKATU-CHK     =   1
                   IF     (SPA-GMN-MAX         =   ZERO )
                       MOVE    1               TO  SPA-SEIKATU-FLG
                   END-IF
                   IF      SPA-SEIKATU-FLG     =   ZERO
                       MOVE    "K177"          TO  SPA-ERRCD
                       MOVE    1               TO  SPA-SEIKATU-FLG
                   END-IF
               END-IF
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        禁忌更新前のチェック
               PERFORM 41001-YAKKNK-CHK-SEC
           END-IF
      *
      *     禁忌エラーの時、エラー表示へ
           IF     (FLG-KINKI           =   1   )  AND
                  (FLG-END             =   1   )
               GO      TO      4100-TOROKU-EXT
           END-IF
      *
      *    更新処理
      *****MOVE    SPACE           TO  WRK-PUTTYPE
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41002-KOUSIN-SYORI-SEC
           END-IF
           .
       4100-TOROKU-EXT.
           EXIT.
      *TEST
      *****************************************************************
      *    電子点数表のチェック処理
      *****************************************************************
       41001-ETENSU-CHK-SEC          SECTION.
      *
      *    電子点数表一括チェック
           INITIALIZE                  ORCSC92AREA
           MOVE    "G"                 TO  LNK-SC92-KBN
           MOVE    SPA-SYORIFLG        TO  LNK-SC92-SYORIFLG
           MOVE    "K02"               TO  LNK-SC92-PG 
      *    警告チェックあり
           MOVE    "1"                 TO  LNK-SC92-KEIFLG
           MOVE    SPA-NENREI          TO  LNK-SC92-NENREI
           MOVE    SPA-15MIMAN-ARI     TO  LNK-SC92-15MIMAN-ARI
           MOVE    SPA-NAI-ROUJIN      TO  LNK-SC92-ROUJIN
           MOVE    SPA-LSTSRYKA        TO  LNK-SC92-LSTSRYKA
           MOVE    SPA-LSTSRYKAMEI     TO  LNK-SC92-LSTSRYKAMEI
           MOVE    SPA-K02-TEISEI-AREA TO  LNK-SC92-TEISEI-AREA
           CALL    "ORCSC92ECHK"       USING
                                           MCPAREA
                                           ORCSC92AREA
                                           SPA-SCR-REC
                                           SPA-AREA
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           END-IF
      *
           .
       41001-ETENSU-CHK-EXT.
           EXIT.
      *TEST
      *
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       41002-KOUSIN-SYORI-SEC          SECTION.
      *
      *    診療室入力の時、ワーク行為負担データを作成し、選択へ
           IF      SPA-BUNSAN          =   "1"
               PERFORM 45109-SNRYO-NYU-SEC
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           INITIALIZE                  ORCSC100AREA
           MOVE    "2"                 TO  ORCSC100-KBN
           MOVE    FLG-TOROKU          TO  ORCSC100-TOROKU
           MOVE    FLG-INIT            TO  ORCSC100-INIT
           MOVE    FLG-KENSADEL        TO  ORCSC100-KENSADEL
           MOVE    FLG-RIHTEI          TO  ORCSC100-RIHTEI
           CALL    "ORCSC100"          USING
                                       ORCSC100AREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF     (SPA-ERRCD       NOT =   SPACE )  OR
                  (SPA-KIDCD       NOT =   SPACE )
               GO      TO      41002-KOUSIN-SYORI-EXT
           END-IF
      *H28.2
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
      *    再度、排他のチェック
           IF      SPA-LOCK            =   ZERO
               MOVE    1                   TO  SLOCK-MODE
               MOVE    SPA-PTID            TO  SLOCK-PTID
               CALL    "ORCSLOCK"          USING
                                           MCPAREA
                                           SPA-AREA
                                           ORCSLOCKAREA
               MOVE    ZERO            TO  SPA-LOCK
               IF      SLOCK-RETURN        =   10
      *            排他中
                   MOVE    "9997"              TO  SPA-ERRCD
                   GO      TO      41002-KOUSIN-SYORI-EXT
               END-IF
           END-IF
      *
           MOVE    "K08"               TO  SPA-NAI-SAKIPG
           MOVE    SPA-NAI-SAKIPG      TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
      *
           IF      WRK-PUTTYPE         =   SPACE
               MOVE    "CHANGE"            TO  MCP-PUTTYPE
           ELSE
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
           MOVE    SPA-NAI-SAKIPG      TO  MCP-WINDOW
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
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45109-SNRYO-NYU-SEC              SECTION.
      *
      *    確認メッセージ
           MOVE    "0117"              TO  SPA-KIDCD2
           MOVE    SPACE               TO  SPA-KID2-FLG
      *
           .
       45109-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    ワーク行為負担データを作成、Ｋ０１へ
      *****************************************************************
       4510-SNRYO-NYU-SEC              SECTION.
      *
           MOVE    "1"                 TO  FLG-SYORI
           PERFORM 45101-SNRYO-SYORI-SEC
      *
      *    処方せん発行の有無
           MOVE    ZERO                TO  FLG-SYOHOU-ARI
           MOVE    ZERO                TO  FLG-YAKUZAI-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   SPA-GMN-MAX  )  OR
                              (FLG-SYOHOU-ARI  =   1     )
      *    薬評以外
             IF     (SPA-COM-YAKUHYO (IDX)     =   SPACE )
      *        薬剤チェック
               IF     (SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
                                                   "23"   )  AND
                     ((SPA-COM-INPUTCD(IDX)(1:1)   =   "6" )  OR
                      (SPA-COM-INPUTCD(IDX)(1:3)   =   "059" ))
                   MOVE    1                   TO  FLG-YAKUZAI-ARI
               END-IF
      *
      *        院外処方
               IF    ((SPA-COM-SRYKBN(IDX)     =   "21"  OR  "22"  OR
                                                   "23"   )  AND
                      (SPA-COM-ZAIEND(IDX)     =   "1"    )  AND
                      (SPA-COM-TENSU (IDX)     =   ZERO   )  AND
                      (FLG-YAKUZAI-ARI         =   1      ))
      *            薬剤のみの時、院内分とする
                   IF      SPA-COM-SRYSYUKBN(IDX)  =  "213"  OR
                                                      "223"  OR
                                                      "233"
      *H28.4           院内投薬
                                                   OR "211"
                                                   OR "221"
                                                   OR "231"
                                                   OR "291"
                                                   OR "294"
                                                   OR "297"
                       CONTINUE
                   ELSE
                       MOVE    1               TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
      *        院外の診療種別の時、院外とする
               IF      SPA-COM-SRYSYUKBN(IDX)  =  "212"  OR
                                                  "222"  OR
                                                  "232"  OR
                                                  "292"
      *            外用臨時
                                               OR "295"
      *            頓服臨時
                                               OR "298"
                   MOVE    1               TO  FLG-SYOHOU-ARI
               END-IF
      *        在宅注射院外処方
               IF     (SPA-COM-SRYKBN(IDX)     =   "14"  )
                   IF      SPA-COM-SRYSYUKBN(IDX)  =   "148"  OR
                                                       "149"
      *                院外分
                       MOVE    1                   TO  FLG-SYOHOU-ARI
                   END-IF
               END-IF
               END-IF
      *
               IF      SPA-COM-ZAIEND(IDX)     =   "1" 
                   MOVE    ZERO            TO  FLG-YAKUZAI-ARI
               END-IF
           END-PERFORM
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
      *
           IF    ( SPA-CLIENT-PRT-FLG  =   2 )
             OR  ( SPA-PRTCONF         =   "1" )
               IF      SPA-KID2-FLG        =   "OK"
                                           OR  SPACE
                   CONTINUE
               ELSE
      *            クライアント印刷制御情報取得処理
                   PERFORM 4901-CLIENT-PRT-SEC
               END-IF
           END-IF
      *
      *    中途終了時、処方せんの有無
           EVALUATE    SPA-KID2-FLG
      *        処方せん発行
               WHEN    "P1"
                   IF      FLG-SYOHOU-ARI      =   1
      *                院外
                       MOVE    3                   TO  FLG-SYOHOU
                    ELSE
      *                院内
                       MOVE    4                   TO  FLG-SYOHOU
                   END-IF
                   PERFORM 4124-SYOHOU-SYORI-SEC
               WHEN    "P2"
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
               WHEN    "P3"
      *            処方せん発行
                   IF      FLG-SYOHOU-ARI      =   1
                       MOVE    3                   TO  FLG-SYOHOU
                    ELSE
                       MOVE    4                   TO  FLG-SYOHOU
                   END-IF
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
               WHEN    "P4"
      *            お薬手帳発行
                   PERFORM 4126-OKUSURI-SYORI-SEC
               WHEN    "P5"
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
      *            お薬手帳発行
                   PERFORM 4126-OKUSURI-SYORI-SEC
               WHEN    "P6"
      *            処方せん発行
                   IF      FLG-SYOHOU-ARI      =   1
                       MOVE    3                   TO  FLG-SYOHOU
                    ELSE
                       MOVE    4                   TO  FLG-SYOHOU
                   END-IF
                   PERFORM 4124-SYOHOU-SYORI-SEC
      *            薬剤情報発行
                   PERFORM 4125-YAKJYO-SYORI-SEC
      *            お薬手帳発行
                   PERFORM 4126-OKUSURI-SYORI-SEC
           END-EVALUATE
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
           MOVE    ZERO                TO  SPA-CLIENT-TEMP-FLG
      *
           MOVE    SPACE               TO  SPA-KID1-FLG
      *
      *    ユーザプログラム起動
           PERFORM 4907-USERPG-SEC
           IF      FLG-END             =   1
            OR     FLG-END2            =   1
               CONTINUE
           ELSE
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
      *    患者ＩＤを初期化する
           INITIALIZE                  SPA-KYOTUKEY
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
      *    初期ＳＰＡ編集処理
           INITIALIZE                  ORCSC95AREA
           MOVE    SPA-SYSYMD          TO  LNK-SC95-SRYYMD
           MOVE    SPA-SYSYMDWH        TO  LNK-SC95-SRYYMDG
           MOVE    "1"                 TO  LNK-SC95-KBN
           MOVE    "K02"               TO  LNK-SC95-PG
           CALL    "ORCSC95"           USING
                                       MCPAREA
                                       ORCSC95AREA
                                       SPA-AREA
                                       SPA-K01KYOUTU
                                       SPA-K02
      *
           MOVE    ZERO            TO  SPA-GMN-CUR
           MOVE    1               TO  SPA-GMN-PAGE
      *
           END-IF
           .
       4510-SNRYO-NYU-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    INITIALIZE                  KDMY01
      *    MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "お薬情報"          TO  ORCSPRTCTRL-TITLE-I(02)
      *    MOVE    "お薬手帳"          TO  ORCSPRTCTRL-TITLE-I(03)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      **   MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      **   MOVE    1                   TO  FLG-CLIENT-OK
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    クライアント印刷画面処理
      *****************************************************************
      *600-CLIENT-DMY-SEC              SECTION.
      *
      *    MOVE    MCP-WINDOW          TO  SPA-SAKIPG
      *    MOVE    MCP-PUTTYPE         TO  SPA-PUTTYPE
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *
      *    .
      *600-CLIENT-DMY-EXT.
      *    EXIT.
      *
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC  SECTION.
      *
      *    追加
           MOVE    1               TO  SPA-SYORI-FLG
           IF      SPA-GMN-USERPG      =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "K02"               TO  ORCSUSERCHK-GMNPG
                   MOVE    "2"                 TO  ORCSUSERCHK-SYORI
                   MOVE    SPA-SYSYMD          TO  ORCSUSERCHK-SYSYMD
                   MOVE    SPA-SRYYMD          TO  ORCSUSERCHK-SRYYMD
                   MOVE    SPA-PTID            TO  ORCSUSERCHK-PTID
                   MOVE    SPA-PTNUM           TO  ORCSUSERCHK-PTNUM
                   MOVE    SPA-SRYKA           TO  ORCSUSERCHK-SRYKA
                   MOVE    SPA-HKNCOMBINUM     TO  ORCSUSERCHK-HKNCOMBI
                   MOVE    SPA-DRCD            TO  ORCSUSERCHK-DRCD
                   MOVE    SPA-SYORI-FLG       TO  ORCSUSERCHK-SYORI-FLG
                   CALL    "ORCSUSERCHK"       USING
                                               ORCSUSERCHKAREA
                                               SPA-AREA
               END-IF
           END-IF
      *
           .
       4907-USERPG-EXT.
           EXIT.
      *
      *****************************************************************
      *    ユーザプログラム実行画面表示処理
      *****************************************************************
       49071-XD01-SYORI-SEC              SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
           INITIALIZE                  SPA-K02
           INITIALIZE                  SPA-SCR-REC
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "K02"               TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           MOVE    "JOIN"              TO  MCP-PUTTYPE
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    薬剤情報発行処理
      *****************************************************************
       4125-YAKJYO-SYORI-SEC             SECTION.
      *
           INITIALIZE                      ORCSCH30AREA
           IF      FLG-SYOHOU-ARI      =   1
      *        院外分
               MOVE    "4"                 TO  ORCSCH30-KBN
           ELSE
      *        院内分
               MOVE    "3"                 TO  ORCSCH30-KBN
           END-IF
           MOVE    "02"                TO  SPA-PRT-FLG
           CALL    "ORCSCH30"          USING   SPA-AREA
                                               ORCSCH30AREA
      *
           .
       4125-YAKJYO-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    お薬手帳発行処理
      *****************************************************************
       4126-OKUSURI-SYORI-SEC             SECTION.
      *
           INITIALIZE                      ORCSCH62AREA
           IF      FLG-SYOHOU-ARI      =   1
      *        院外分
               MOVE    "4"                 TO  ORCSCH62-KBN
           ELSE
      *        院内分
               MOVE    "3"                 TO  ORCSCH62-KBN
           END-IF
           MOVE    "2"                 TO  ORCSCH62-PGKBN
           MOVE    "03"                TO  SPA-PRT-FLG
           CALL    "ORCSCH62"          USING   SPA-AREA
                                               ORCSCH62AREA
      *
           .
       4126-OKUSURI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ出力処理
      *****************************************************************
       45101-SNRYO-SYORI-SEC              SECTION.
      *
           INITIALIZE                  ORCSCWKSRYAREA
           IF      FLG-SYORI           =   "1"
               MOVE    "2"                 TO  ORCSCWKSRY-KBN
           ELSE
               MOVE    "3"                 TO  ORCSCWKSRY-KBN
           END-IF
           MOVE    "K02"               TO  ORCSCWKSRY-PG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           CALL    "ORCSCWKSRY"        USING
                                       ORCSCWKSRYAREA
                                       SPA-AREA
                                       SPA-K02
                                       SPA-SCR-REC
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
      *    DBエラー
           IF      SPA-ERRCD           =   "1093"
                                       OR  "9999"
               MOVE    1               TO  FLG-END2
           END-IF
           .
       45101-SNRYO-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌更新前のチェック処理
      *****************************************************************
       41001-YAKKNK-CHK-SEC            SECTION.
      *
      *    相互作用チェック期間
           MOVE    ZERO                TO  FLG-KINKI
           IF      SPA-INTERACT-KIKAN  >   ZERO
               INITIALIZE                  ORCSCKNKCHKAREA
               MOVE    "3"                 TO  ORCSCKNKCHK-KBN 
               CALL    "ORCSCKNKCHK"       USING
                                           ORCSCKNKCHKAREA
                                           SPA-AREA
                                           SPA-K02
                                           SPA-SCR-REC
               IF      SPA-K021-MAX        >   ZERO
                   MOVE    1                   TO  FLG-KINKI
                   MOVE    2                   TO  FLG-ALLFLG
                   MOVE    "K03"               TO  SPA-K021-SAKI
                   PERFORM 51020-K021-SYORI-SEC
               END-IF
           END-IF
      *
           .
       41001-YAKKNK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    禁忌エラーメッセージ表示処理
      *****************************************************************
       51020-K021-SYORI-SEC              SECTION.
      *
      *    初期画面セット
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K02             TO  SPA-FREE
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK021"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K02
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    "K021"              TO  SPA-SAKIPG
           MOVE    "K02"               TO  SPA-MOTOPG
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K021"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       51020-K021-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       520-SEIWA-HEN-SEC             SECTION.
      *
           IF      WRK-SYMD            =   ALL "9"  OR   SPACE
               MOVE    SPACE               TO  WRK-HGO
               MOVE    WRK-SYMD(3:2)       TO  WRK-HYY
               MOVE    WRK-SYMD(5:2)       TO  WRK-HMM
               MOVE    WRK-SYMD(7:2)       TO  WRK-HDD
               MOVE    WRK-HENYMD          TO  WRK-HENYMDG
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
       520-SEIWA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    自画面編集処理
      *****************************************************************
       500-SET-SCREEN              SECTION.
      *
           PERFORM 500-GMNHEN-SEC
      *
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   1
               MOVE    "1027"              TO  SPA-ERRCD
           END-IF
      *    老人保険編集エラーがある時、エラーとする
           IF      SPA-RJN-ERR         =   2
               MOVE    "1070"              TO  SPA-ERRCD
           END-IF
      *    後期高齢者の保険がない時
           IF      SPA-RJN-ERR         =   3
               MOVE    "1076"              TO  SPA-ERRCD
           END-IF
      *    退職国保の年齢
           IF      SPA-RJN-ERR         =   9
               MOVE    "1077"              TO  SPA-ERRCD
           END-IF
      *    保険エラーがある時、エラーとする
           IF      SPA-HKN-ERR         =   1
               MOVE    "1028"              TO  SPA-ERRCD
           END-IF
           IF      SPA-HKN-ERR         =   2
               MOVE    "1032"              TO  SPA-ERRCD
           END-IF
      *    保険未選択の場合、エラー
           IF      SPA-HKN-ERR         =   3
               MOVE    "1006"              TO  SPA-ERRCD
           END-IF
      *    保険選択エラーの場合
           IF      SPA-HKN-ERR         =   4
               MOVE    "1034"              TO  SPA-ERRCD
               MOVE    5                   TO  SPA-GMN-CUR
           END-IF
      *    保険選択エラーの場合
           IF      SPA-HKN-ERR         =   5
               MOVE    "0220"              TO  SPA-ERRCD
               MOVE    6                   TO  SPA-GMN-CUR
           END-IF
      *
      *    数量ゼロの場合、エラー表示しない
           IF      SPA-ERRCD           =   "A001"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
      *
      *    訂正時の登録エラー
           IF     (SPA-TEISEI-ERR      =   1  )  AND
                  (SPA-TEISEI-CHK      =   ZERO)
               IF      SPA-ERRCD       NOT =   "9100"
                   MOVE    "K999"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-TEISEI-CHK
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
      *    地方公費のみの場合、警告
           IF     (SPA-NAI-KOHFLG      =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR   "K034")
               IF      SPA-NAI-KOHFLG-ERR  =   ZERO
                   MOVE    "K034"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-KOHFLG-ERR
               END-IF
           END-IF
      *    国保 前期高齢者変更チェック
           IF     (SPA-NAI-ZENKIFLG    =   1   )  AND
                  (SPA-ERRCD           =   SPACE
                                       OR   "K171")
               IF      SPA-NAI-ZENKIFLG-ERR    =   ZERO
                   MOVE    "K171"              TO  SPA-ERRCD
                   MOVE    5                   TO  SPA-GMN-CUR
                   MOVE    1                   TO  SPA-NAI-ZENKIFLG-ERR
               END-IF
           END-IF
      *    前回保険組合せの判定チェック
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-HKNCHK-FLG  NOT =   ZERO   )  AND 
                  (SPA-HKNCHK-CHK      =   ZERO   )
               IF      SPA-HKNCHK-FLG      =   2
                   MOVE    "K173"              TO  SPA-ERRCD
               ELSE
                   MOVE    "K172"              TO  SPA-ERRCD
               END-IF
               MOVE    5                   TO  SPA-GMN-CUR
               MOVE    1                   TO  SPA-HKNCHK-CHK
               MOVE    ZERO                TO  SPA-HKNCHK-FLG
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        訂正時の保険自動変換
               IF      SPA-HKN-ERR2        =   1
                   MOVE    "K176"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-HKN-ERR2
               END-IF
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   1      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K174"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
      *    初診料の併用算定判定
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-SYOSIN-KEI      =   2      )  AND
                  (SPA-SYOSIN-KEIFLG   =   ZERO   )
               MOVE    "K175"              TO  SPA-ERRCD
               MOVE    1                   TO  SPA-SYOSIN-KEIFLG
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-LOCK        =   2
                   MOVE    "9998"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-LOCK
               END-IF
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )
      *H26.12  中途終了は可能
             AND  (SPA-KIDCD2          =   SPACE )
               IF      SPA-NAI-JYURRKFLG   =   1
                   MOVE    "0178"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
               IF      SPA-NAI-JYURRKFLG   =   2
                   MOVE    "K178"              TO  SPA-ERRCD
                   MOVE    ZERO                TO  SPA-NAI-JYURRKFLG
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
      *
      *R02.1
      *        患者登録で変更
               IF     (SPA-NAI-PTINFFLG    =   1     )
                  AND (SPA-ERRCD           =   SPACE )
                   MOVE    "0179"              TO  SPA-ERRCD
                   MOVE    90                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *!!
      *    中途データＤＢ更新エラー表示
           IF      SPA-K01-YOBI(1:1)   =   "E"
               MOVE    "1999"          TO  SPA-ERRCD
               MOVE    SPACE           TO  SPA-K01-YOBI(1:1)
               MOVE    90              TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD           =   "1999"
               MOVE    90              TO  SPA-GMN-CUR
           END-IF
      *!!
      *
      *    コメント入力位置エラーはカーソル移動のみ
           IF      SPA-ERRCD           =   "0100"
               MOVE    SPACE               TO  SPA-ERRCD
           END-IF
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
           IF      SPA-KIDCD2      NOT =   SPACE
               PERFORM 530-KID2SET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K02    "           TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
           .
       500-SET-SCREEN-EXT.
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
      *****MOVE    ZERO                TO  SPA-GMN-MAX
      *ver.4.7
           MOVE    ZERO                TO  WRK-MAP-CUR4
           MOVE    ZERO                TO  FLG-MAP-CUR4
      *!!
      *訂正展開時、１行目にカーソル移動
           IF      SPA-GMN-CUR         =   98
               IF      SPA-GMN-MAX     >=  SPA-GMLINCNT
      *            頁行以上であれば１行目へ
                   MOVE    "1"             TO  SPA-COM-CUR (1)
               END-IF
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-MAX
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL       WRK-IDX     >   SPA-MAX-LINE
               MOVE    ZERO                TO  SPA-COM-PAGE (WRK-IDX)
               MOVE    ZERO                TO  SPA-COM-GYOU (WRK-IDX)
      *********MOVE    SPACE               TO  SPA-COM-IN3  (WRK-IDX)
      *H29.3  （減）（精減）の自動コメントは対象外
               IF     ( SPA-COM-INPUTCD(WRK-IDX)   =   "820000047"
                                                   OR  "820000166")
                 AND  ( SPA-COM-AUTOKBN(WRK-IDX)   =   "3"        )
                    MOVE    1               TO  FLG-TAISYO
               ELSE
                    MOVE    ZERO            TO  FLG-TAISYO
               END-IF
      *        画面表示外のものに頁、行位置を設定する
               IF     (SPA-COM-SET    (WRK-IDX)  =   SPACE )
                AND   (FLG-TAISYO            =   ZERO  )
                   ADD     1                 TO  IDX
      *ページなし
      ****         IF      IDX               >   MAX-GMN
      *                ADD     1             TO  WRK-PAGE
      *                MOVE    1             TO  IDX
      ****         END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF     (SPA-GMN-INPUTCD(WRK-IDX)  NOT =   SPACE) OR
                          (SPA-COM-CUR (WRK-IDX)     NOT =   SPACE)
                       MOVE    IDX             TO  WRK-MAP-MAX
                       MOVE    WRK-PAGE        TO  WRK-MAP-MAXPAGE
      *                エラー位置
                       IF      SPA-COM-CUR (WRK-IDX)   =   "1"
                                                       OR  "3"
                           IF      WRK-MAP-CUR         =   ZERO
                               MOVE    IDX            TO  WRK-MAP-CUR
                               MOVE    WRK-PAGE       TO  WRK-MAP-PAGE
                           END-IF
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
      *
      *ver.4.7
      *                今回入力位置
                       IF     (SPA-COM-IN4 (WRK-IDX)   =   "1" )
                           MOVE    IDX             TO  WRK-MAP-CUR4
                           MOVE    1               TO  FLG-MAP-CUR4
                       END-IF
                       IF     (SPA-COM-IN4 (WRK-IDX)   =   "2" )
                           MOVE    IDX             TO  WRK-MAP-CUR4
                           MOVE    2               TO  FLG-MAP-CUR4
                       END-IF
      *
                   END-IF
      *
                   MOVE    WRK-PAGE        TO  SPA-COM-PAGE(WRK-IDX)
                   MOVE    IDX             TO  SPA-COM-GYOU(WRK-IDX)
      *
                   IF      SPA-COM-CUR (WRK-IDX)   =   "1"
      *!!
      *            セットで警告の時
                       IF     (SPA-GMN-INPUTCD (WRK-IDX)(1:1) =  "S")
                          AND (SPA-ERRCD(1:1)      =   "K"   )
                          AND ((SPA-COM-KZMFLG4 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KZMFLG5 (WRK-IDX)  =  "1" )
                           OR  (SPA-COM-KINKIFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG(WRK-IDX)  =  "1" )
                           OR  (SPA-COM-TUKIJGNFLG1(WRK-IDX) =  "1" ))
                          CONTINUE
                       ELSE
                           MOVE    SPACE       TO  SPA-MAE-INPUTCD
                                                            (WRK-IDX)
                       END-IF
                   ELSE
                       IF      SPA-COM-CUR (WRK-IDX)   NOT =   "2"
                           MOVE    SPACE       TO  SPA-COM-CUR (WRK-IDX)
                       END-IF
                   END-IF
               END-IF
      *
               IF      (SPA-COM-INPUTCD(WRK-IDX)   =   SPACE ) AND
                       (SPA-GMN-INPUTCD(WRK-IDX)   =   SPACE )
                   CONTINUE
               ELSE
      ************ ADD     1               TO  SPA-GMN-MAX
                   MOVE    WRK-IDX         TO  SPA-GMN-MAX
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
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *    名称入力へ
           IF      WRK-MAP-CUR3    NOT =   ZERO
               MOVE    WRK-MAP-CUR3        TO  SPA-MAP-IDX
               MOVE    WRK-MAP-PAGE3       TO  SPA-GMN-PAGE
               MOVE    2                   TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *ver.4.7
      *    カーソル指定のない時、入力行へ
           IF     (WRK-MAP-CUR4    NOT =   ZERO )
            AND   (WRK-MAP-MAX         >   WRK-MAP-CUR4)
               MOVE    WRK-MAP-CUR4        TO  SPA-MAP-IDX
               MOVE    1                   TO  SPA-GMN-PAGE
               MOVE    FLG-MAP-CUR4        TO  SPA-GMN-CUR
               GO      TO      5002-GMNSPA-HEN-EXT
           END-IF
      *
      *    カーソル指定のない時、次ぎの空白行へ
           COMPUTE SPA-MAP-IDX         =   WRK-MAP-MAX
                                       +   1
           MOVE    1                   TO  SPA-GMN-PAGE
      *
           .
       5002-GMNSPA-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       500-GMNHEN-SEC                  SECTION.
      *
      *    行位置とカーソル位置の決定
           PERFORM 5002-GMNSPA-HEN-SEC
      *H27.8
      *    画面ＳＰＡ書込み
      **** PERFORM 1003-K02SPA-OUT-SEC
      *
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-GMN-MAX1        =   ZERO )
               MOVE    ZERO                TO  WRK-K02-ROW
               MOVE    ZERO                TO  WRK-K02-ROWATTR
           ELSE
               MOVE    K02-ROW             TO  WRK-K02-ROW
               MOVE    K02-ROWATTR         TO  WRK-K02-ROWATTR
           END-IF
      *
           MOVE    SPACE               TO  K02
           INITIALIZE                      K02
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  K02-ROW
           MOVE    ZERO                TO  K02-ROWATTR
           IF      WRK-K02-ROW     NOT =   ZERO
               MOVE    WRK-K02-ROW         TO  K02-ROW
               MOVE    WRK-K02-ROWATTR     TO  K02-ROWATTR
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    1                   TO  WRK-PAGE
           MOVE    ZERO                TO  FLG-BGCOLORS
           MOVE    ZERO                TO  IDX2
      *H29.1
      *    院外投薬あり
           MOVE    ZERO                TO  FLG-INGAI-ARI
      *
           PERFORM VARYING     WRK-IDX     FROM    1   BY  1
                   UNTIL      (WRK-IDX     >   SPA-MAX-LINE)  OR
                              (IDX         >=  MAX-GMN )
      *        画面表示
               IF      SPA-COM-PAGE(WRK-IDX)   =   SPA-GMN-PAGE
                   ADD     1                   TO  IDX
      *            スクロール背景色（１行毎）
                   IF      FLG-BGCOLORS        =   ZERO
      *                白色設定
      **************   MOVE    "#FFFFFF"       TO
                       MOVE    CL-WHITE        TO
                                       K02-SRYKBN-BGCOLOR  (IDX)
                                       K02-INPUTCD-BGCOLOR (IDX)
                                       K02-MEISYO-BGCOLOR  (IDX)
                                       K02-SURYO-BGCOLOR   (IDX)
                       IF      SPA-BGLINEKBN       =   "1"
      *                    剤毎
                           IF     (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                            AND  ((WRK-IDX         <  MAX-GMN  )  AND
                                  (SPA-GMN-INPUTCD(WRK-IDX + 1)
                                                   NOT =  SPACE))
                               MOVE    1               TO  FLG-BGCOLORS
                           END-IF
                       ELSE
      *                    行毎
                           MOVE    1               TO  FLG-BGCOLORS
                       END-IF
                   ELSE
                       MOVE    SPA-BGCOLORS        TO
                                       K02-SRYKBN-BGCOLOR (IDX)
                                       K02-INPUTCD-BGCOLOR(IDX)
                                       K02-MEISYO-BGCOLOR (IDX)
                                       K02-SURYO-BGCOLOR  (IDX)
                       IF      SPA-BGLINEKBN       =   "1"
      *                    剤毎
                           IF     (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                             OR  ((WRK-IDX         <  MAX-GMN  )  AND
                                  (SPA-GMN-INPUTCD(WRK-IDX + 1)
                                                   =  SPACE))
                               MOVE    ZERO            TO  FLG-BGCOLORS
                           END-IF
                       ELSE
      *                    行毎
                           MOVE    ZERO            TO  FLG-BGCOLORS
                       END-IF
                   END-IF
      *
                   MOVE    SPA-GMN-SRYKBN (WRK-IDX)
                                               TO  K02-SRYKBN(IDX)
                   MOVE    SPA-GMN-INPUTCD(WRK-IDX)
                                               TO  K02-INPUTCD(IDX)
      *
      *H30.9       選択式コメントあり
                   IF      SPA-COM-RECEKISAI (WRK-IDX) NOT =  SPACE
                       MOVE   "S"       TO  SPA-GMN-MEIH (WRK-IDX)(2:1)
                   END-IF
      *
                   MOVE    SPA-GMN-MEISYO-G(WRK-IDX)
                                               TO  K02-MEISYO(IDX)
      *H29.1
      *!!          ユーザー管理
                   IF      SPA-COM-MASTER-CLASS (WRK-IDX)  =   "2"
                       MOVE    CL-GREEN        TO
      *?                              K02-SRYKBN-FGCOLOR (IDX)
                                      K02-INPUTCD-FGCOLOR(IDX)
      *?                              K02-MEISYO-FGCOLOR(IDX)
      *?                              K02-SURYO-FGCOLOR(IDX)
                   END-IF
      *
                   PERFORM 5002-SURYO-HEN-SEC
      *H26.10
      *            診療人数合計
                   IF      SPA-COM-INPUTCD (WRK-IDX)
                                               =   "099140012"
                       PERFORM 50021-MEISYOHEN-SEC
                   END-IF
      *@
      *            包括行
                   IF     (SPA-COM-HOUKATU-ZAI (WRK-IDX) NOT =   ZERO)
      *H26.10 （包括エラー分も）
                      OR  (SPA-COM-HOUKATU-ERR (WRK-IDX) NOT =   ZERO)
                      OR ((SPA-COM-HKNCOMBI(WRK-IDX)     =   "9999" )
                      AND (SPA-HKNCOMBINUM    NOT =   "9999" ))
                           MOVE    CL-BLUE        TO
                                       K02-SRYKBN-FGCOLOR (IDX)
                                       K02-MEISYO-FGCOLOR (IDX)
                       IF      SPA-COM-HOUKATU-FLG2(WRK-IDX)   =   1
                           MOVE    CL-RED         TO
                                       K02-SRYKBN-FGCOLOR (IDX)
                                       K02-MEISYO-FGCOLOR (IDX)
                       END-IF
                   ELSE
                       MOVE    CL-BLACK      TO
                                       K02-SRYKBN-FGCOLOR (IDX)
                                       K02-MEISYO-FGCOLOR (IDX)
                   END-IF
      *
      *!!          禁忌薬剤
                   IF      SPA-COM-PTKINKI-ARI (WRK-IDX)   =   1
                       MOVE    CL-RED        TO
                                       K02-MEISYO-FGCOLOR (IDX)
                   END-IF
      *!
      *!
                   IF      SPA-GMN-INPUTCD(WRK-IDX)(1:1)   =   "#"
                        MOVE    ALL "#"             TO  K02-SRYKBN(IDX)
                        MOVE    ALL "#"             TO  K02-SURYO (IDX)
                   END-IF
      *!
                   IF      SPA-GMN-INPUTCD(WRK-IDX)(1:1)   =   "$"
                        MOVE    ALL "="             TO  K02-SRYKBN(IDX)
                        MOVE    ALL "="             TO  K02-SURYO (IDX)
                    END-IF
      *!
      *             最終行
                    IF      K02-INPUTCD (IDX)  NOT =   SPACE
                        MOVE    IDX                TO  IDX2
                    END-IF
      *
               ELSE
      *            剤毎（約束セット対応）
                   IF     (SPA-BGLINEKBN       =   "1" )  AND
                          (SPA-COM-ZAIEND(WRK-IDX) =  "1"  )
                       IF      FLG-BGCOLORS        =   ZERO
                           MOVE    1               TO  FLG-BGCOLORS
                       ELSE
                           MOVE    ZERO            TO  FLG-BGCOLORS
                       END-IF
                   END-IF
               END-IF
      *H29.1
      *        院外投薬あり
               IF      (SPA-COM-SRYSYUKBN(WRK-IDX) =   "212"
                                                   OR  "292"
                                                   OR  "222"
                                                   OR  "298"
                                                   OR  "232"
                                                   OR  "148"
                                                   OR  "295")
                   OR (SPA-COM-INGAITEN (WRK-IDX)  NOT =   ZERO)
                   MOVE    1               TO  FLG-INGAI-ARI
               END-IF
      *
           END-PERFORM
      *
           MOVE    "複数科保険"        TO  K02-B06S-LABEL
      *
      *    ページ表示
      *    MOVE    SPACE               TO  WRK-HEN-PAGE
      *    MOVE    "頁"                TO  WRK-HEN1
      *    IF      WRK-MAP-MAXPAGE     >   ZERO
      *        MOVE    SPA-GMN-PAGE        TO  WRK-HEN-PAGE1
      *        MOVE    "/"                 TO  WRK-HEN2
      *        MOVE    WRK-MAP-MAXPAGE     TO  WRK-HEN-PAGE2
      *    END-IF
      **   MOVE    WRK-HEN-PAGE        TO  K02-MODE
      *ver.4.7
      *    行数表示
           MOVE    SPACE               TO  K02-MODE
           MOVE    IDX2                TO  WRK-HEN-PAGE1
           STRING  "行数:"
                   WRK-HEN-PAGE1       DELIMITED  BY  SIZE
                                       INTO      K02-MODE
           END-STRING
      *
           MOVE    SPA-GMN-PTNUM       TO  K02-PTNUM
           MOVE    SPA-GMN-KANANAME    TO  K02-KANANAME
           MOVE    SPA-GMN-NAME        TO  K02-NAME
      *    排他中
           IF      SPA-LOCK                =   ZERO
               MOVE    "black"             TO  K02-KANANAME-STYLE
               MOVE    "black"             TO  K02-NAME-STYLE
           ELSE
               MOVE    "red"               TO  K02-KANANAME-STYLE
               MOVE    "red"               TO  K02-NAME-STYLE
           END-IF
           MOVE    SPA-GMN-SEX         TO  K02-SEX
           MOVE    SPA-GMN-HKNCOMBI-G
                                       TO  K02-HKNCOMBI
           MOVE    SPA-GMN-FTNRATEMEI  TO  K02-RATE
      *    診療日
           IF      SPA-GMN-SRYYMD      =   SPACE
               MOVE    SPA-SRYYMDW         TO  K02-SRYYMD
           ELSE
               MOVE    SPA-GMN-SRYYMD      TO  K02-SRYYMD
           END-IF
           MOVE    SPA-GMN-BIRTHDAY    TO  K02-BIRTHDAY
      *    年齢
           MOVE    SPA-NENREI-HEN      TO  K02-NENREI
      *
           IF      SPA-GMN-GOKEI       >=   ZERO
               MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z2
               MOVE    WRK-GOKEI-X2        TO  K02-GOKEITEN
           ELSE
               MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z3
               MOVE    WRK-GOKEI-X3        TO  K02-GOKEITEN
           END-IF
           MOVE    SPA-GMN-LASTYMD-G   TO  K02-LASTYMD
           MOVE    SPA-GMN-SYOSINYMD   TO  K02-SYOSINYMD
           IF      SPA-GMN-LASTMID     =   SPACE
               IF      SPA-BEDFLG          =   ZERO
                   MOVE    "最終来院日"            TO  SPA-GMN-LASTMID
               ELSE
                   MOVE    "最終来院日（退院日）"  TO  SPA-GMN-LASTMID
               END-IF
           END-IF
           MOVE    SPA-GMN-LASTMID     TO  K02-LASTMID
      *
           MOVE    SPA-GMN-MISYUMONEY  TO  WRK-GOKEI-Z3
           MOVE    WRK-GOKEI-X3        TO  K02-MISYUMONEY
      *    累計点数
      *        訂正の時、”＊”
      *    IF      SPA-SYORIFLG        =   1
      *        MOVE    ALL "*"             TO  K02-ZAITENTOTAL
      *    ELSE
               COMPUTE WRK-GOKEI-Z2        =   SPA-GMN-GOKEI
                                           +   SPA-ZAITENTOTAL
               MOVE    WRK-GOKEI-Z2        TO  K02-ZAITENTOTAL
      *    END-IF
      *
      *    包括分入力は、累計に集計しない
           IF      SPA-HKNCOMBINUM     =   "9999"
               MOVE    SPA-ZAITENTOTAL     TO  WRK-GOKEI-Z2
               MOVE    WRK-GOKEI-Z2        TO  K02-ZAITENTOTAL
           END-IF
      *
      *    保険組合せ
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-HKN-MAX
               MOVE    SPA-GMN-HKN-LIST (IDX)  TO  K02-HKNCOMBI-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-HKN-MAX         TO  K02-HKNCOMBI-COUNT
      *
      *    診療科
           MOVE    SPA-GMN-SRYKA-G     TO  K02-SRYKA
           PERFORM  VARYING   IDX     FROM    1   BY  1
                    UNTIL     IDX         >   SPA-SRYKA-MAX
               MOVE    SPA-GMN-SRYKALST (IDX)  TO  K02-SRYKA-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-SRYKA-MAX       TO  K02-SRYKA-COUNT
      *
      *    ＤＯ診療日
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-GMN-MAX1
               MOVE    SPA-GMN-NO     (IDX)    TO  WRK-ZZ
               MOVE    WRK-ZZ                  TO  K02-NO  (IDX)
                                                       (1:4)
               MOVE    SPA-GMN-RRKYMD (IDX)    TO  K02-RRKYMD(IDX)
               MOVE    SPA-GMN-RRKSRYKA(IDX)   TO  K02-RRKSRYKA(IDX)
               MOVE    SPA-GMN-RRKHKNCOMBI(IDX)
                                               TO  K02-RRKHKNCOMBI(IDX)
      *T
               IF      SPA-NAI-SYU-UPDKBN (IDX)    =   "1"
                   MOVE    "*"                 TO  K02-NO  (IDX)
                                                          (1:1)
               END-IF
      *T
      *D       IF      SPA-GMN-NO (IDX)    =   SPA-NAI-TESELNUM
               IF      IDX                 =   SPA-NAI-TESELNUM
                   MOVE    "T"             TO  K02-SELECT  (IDX)
      *            訂正の時
                   IF      SPA-SYORIFLG        =   1
                       MOVE    IDX             TO  K02-ROW
                       MOVE    2               TO  K02-ROWATTR
                   END-IF
               ELSE
                   MOVE    "F"             TO  K02-SELECT  (IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-MAX1        TO  K02-COUNT
      *
           MOVE    SPA-GMN-DOSELNUM    TO  K02-DOSELNUM
      *
      *    院内院外
           IF      SPA-INGAIKBN        =   "1"  OR  "0"
               CONTINUE
           ELSE
               MOVE    SPA-SYOHOKBN        TO  SPA-INGAIKBN
           END-IF
           EVALUATE    SPA-INGAIKBN
               WHEN    "1"
                   MOVE    "T"             TO  SPA-GMN-INGAI
                   MOVE    "T"             TO  K02-INGAI-VALUE
                   MOVE    "院外"          TO  K02-INGAI-LABEL
               WHEN    "0"
                   MOVE    "F"             TO  SPA-GMN-INGAI
                   MOVE    "F"             TO  K02-INGAI-VALUE
                   MOVE    "院内"          TO  K02-INGAI-LABEL
           END-EVALUATE
      *
      *    訂正・処理
           IF      SPA-GMN-TEISEI      =   "T"
      *        訂正診療日選択以外の時、ＤＯ検索へ戻す
               IF      SPA-GMN-CUR         NOT =   4
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    ZERO            TO  SPA-NAI-SYORI
               END-IF
           END-IF
           EVALUATE    SPA-GMN-TEISEI
               WHEN    "T"
                   MOVE    "T"             TO  K02-TEISEI-VALUE
                   MOVE    "訂正診療日"    TO  K02-TEISEI-LABEL
                   MOVE    1               TO  SPA-NAI-SYORI
                   MOVE    "T"             TO  SPA-GMN-TEISEI
               WHEN    "F"
               WHEN    SPACE
                   MOVE    "F"             TO  SPA-GMN-TEISEI
                   MOVE    "F"             TO  K02-TEISEI-VALUE
                   MOVE    "ＤＯ検索"      TO  K02-TEISEI-LABEL
                   MOVE    ZERO            TO  SPA-NAI-SYORI
           END-EVALUATE
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K02-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K02-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K02-SYORIMEI-VALUE
           END-EVALUATE
      *
      *    継続
           EVALUATE    SPA-CONTKBN
               WHEN    SPACE
                   MOVE    SPACE           TO  K02-CONTKBN-VALUE
                   IF      SPA-FUKU-SYORI      =   "1"  OR  "2"
                       MOVE    "red"       TO  K02-CONTKBN-STYLE
                       MOVE    "［複］"    TO  K02-CONTKBN-VALUE
                   END-IF
                   IF      SPA-FUKU-SYORI      =   "F"
                       MOVE    "black"     TO  K02-CONTKBN-STYLE
                       MOVE    "［複］"    TO  K02-CONTKBN-VALUE
                   END-IF
               WHEN    "1"
                   MOVE    "red"           TO  K02-CONTKBN-STYLE
                   MOVE    "［継］"        TO  K02-CONTKBN-VALUE
               WHEN    "2"
                   MOVE    "red"           TO  K02-CONTKBN-STYLE
                   MOVE    "［確］"        TO  K02-CONTKBN-VALUE
           END-EVALUATE
      *    入院期間中
           IF      SPA-NAI-NYUIN       =   1
               MOVE    "blue"              TO  K02-CONTKBN-STYLE
               MOVE    "［入］"            TO  K02-CONTKBN-VALUE
           END-IF
      *
      *    リハビリ・患者情報編集
           PERFORM 5003-TABBOHEN-SEC
      *
      *    小児科時間外
           IF      SPA-GMN-JIKANMSG    =   SPACE
               MOVE    SPACE               TO  K02-JIKANMSG-VALUE
      *
      *Ｈ２０．４から施設入居中の特記事項を編集
               IF      SPA-PTTOKK-NAIYO    NOT =   SPACE
                   MOVE    "blue"              TO  K02-JIKANMSG-STYLE
                   MOVE    SPA-PTTOKK-NAIYO    TO  K02-JIKANMSG-VALUE
               END-IF
           ELSE
               MOVE    "red"               TO  K02-JIKANMSG-STYLE
               MOVE    SPA-GMN-JIKANMSG    TO  K02-JIKANMSG-VALUE
           END-IF
      *    公費期限切れ名称
           IF      SPA-GMN-KOHMSG      =   SPACE
               MOVE    SPACE               TO  K02-KOHMSG-VALUE
           ELSE
               MOVE    "red"               TO  K02-KOHMSG-STYLE
               MOVE    SPA-GMN-KOHMSG      TO  K02-KOHMSG-VALUE
           END-IF
      *
      *H28.5
      *    処方名切替え
           IF     (SPA-GMN-PTNUM   NOT =   SPACE     )
            AND   (SPA-SRYYMD          >=  "20160401")
      *     院外投薬がある時
            AND   (FLG-INGAI-ARI       =   1         )
               IF      SPA-GENERICFLG      =   "1"
                   MOVE    SPACE               TO  K02-MODE2-STYLE
                   MOVE    "(一般名)"          TO  K02-MODE2-VALUE
               ELSE
                   MOVE    SPACE               TO  K02-MODE2-STYLE
                   MOVE    "(銘柄名)"          TO  K02-MODE2-VALUE
               END-IF
           END-IF
      *
      *H30.4
           IF     (SPA-SRYYMD          >=  "20180401")
               IF     (SPA-NINPUKBN-ARI    =   1    )
                   MOVE    "（妊婦）"      TO  K02-MODE3-VALUE
                   MOVE    "blue"          TO  K02-MODE3-STYLE
               ELSE
      *            手入力で妊婦
                   IF     (SPA-NINPUKBN-IN     =   1    )
                   MOVE    "（妊婦）"      TO  K02-MODE3-VALUE
                   MOVE    "black"         TO  K02-MODE3-STYLE
                   END-IF
               END-IF
           END-IF
      *
      *    MOVE    SPA-DRCD-NAME       TO  K02-DRNAME
      *    ドクター
           MOVE    SPA-GMN-DRCD-G      TO  K02-DRCD
           PERFORM  VARYING   IDX      FROM    1   BY  1
                    UNTIL     IDX         >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCDLST (IDX)  TO  K02-DRCD-ITEM
                                                             (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  K02-DRCD-COUNT
      *H28.1
           IF      SPA-DRCD-MAX        <   99
               MOVE    SPA-DRCD-MAX        TO  IDX
               ADD     1                   TO  IDX
               MOVE    "****  医師一覧"
                                           TO  K02-DRCD-ITEM (IDX)
               MOVE    IDX                 TO  K02-DRCD-COUNT
            END-IF
      *!!
      *
      *    病名
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-BYOMEI-MAX
               MOVE    SPA-GMN-BYOMEI-NO(IDX)  TO  K02-BYOMEIKA(IDX)
               MOVE    SPA-GMN-BYOMEI   (IDX)  TO  K02-BYOMEI(IDX)
           END-PERFORM
           MOVE    SPA-BYOMEI-MAX      TO  K02-BYOMEI-COUNT
      *
      *    メモボタン
           IF      SPA-NAI-MEMOKBN     =   "1"
               MOVE    "red"               TO  K02-BC10-STYLE
           ELSE
               MOVE    "black"             TO  K02-BC10-STYLE
           END-IF
           MOVE    "メモ"              TO  K02-BC10-LABEL
      *
      *    他院紐付けあり
           IF      SPA-NAI-GRPKBN      =   1
               MOVE    "darkgreen"         TO  K02-GRPMEI-STYLE
               IF      SPA-NAI-GRPKBN2     =   1
                   MOVE    "red"           TO  K02-GRPMEI-STYLE
               END-IF
               MOVE    "【他】"            TO  K02-GRPMEI-VALUE
           ELSE
               MOVE    "black"             TO  K02-GRPMEI-STYLE
               MOVE    SPACE               TO  K02-GRPMEI-VALUE
           END-IF
      *
      *ver.4.7
      *    医院名称
      *****MOVE    SPA-HOSPNAME        TO  K02-TITLE
           MOVE    SPACE               TO  K02-TITLE
           STRING  SPA-HOSPNAME        DELIMITED BY SPACE
                   "  ["               DELIMITED BY SIZE
                   SPA-OPID            DELIMITED BY SPACE
                   "]"                 DELIMITED BY SIZE
                                       INTO  K02-TITLE
           END-STRING
      *ver.4.8
      *    患者情報通知
           MOVE    SPA-GMN-POPUP-SUMMARY   TO  K02-POPUP-SUMMARY
           MOVE    SPA-GMN-POPUP-BODY      TO  K02-POPUP-BODY
           MOVE    SPA-GMN-POPUP-ICON      TO  K02-POPUP-ICON
           MOVE    SPA-GMN-POPUP-TIMEOUT   TO  K02-POPUP-TIMEOUT
           IF      SPA-GMN-POPUP-SUMMARY   NOT = SPACE
      *!!!
               IF      SPA-KIDCD           =   SPACE
                   INITIALIZE SPA-GMN-POPUP-STATUS
               END-IF
      *
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           IF      FLG-END             =   ZERO
      *         カーソルセット
                PERFORM 5001-MAPCUR-SEC
            END-IF
      *H27.8
      *    画面ＳＰＡ書込み
           PERFORM 1003-K02SPA-OUT-SEC
      *
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
      *業務処理権限設定
      *    受付
           IF      SPA-GSRAUTH-11      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02-B04S-STATE
      *ver.4.7  照会からは非活性
               IF      SPA-MACHIPG          =   "Q02"
                                            OR  "Q02A"
                                            OR  "Q02B"
                   MOVE    WIDGET-INSENSITIVE  TO  K02-B04S-STATE
               END-IF
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B04S-STATE
           END-IF
      *    患者登録
           IF      SPA-GSRAUTH-12      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02-B05S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B05S-STATE
           END-IF
      *    病名登録
           IF      SPA-GSRAUTH-22      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02-B07S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B07S-STATE
           END-IF
      *    収納登録
           IF      SPA-GSRAUTH-23      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02-B08S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B08S-STATE
           END-IF
      *    会計照会
           IF      SPA-GSRAUTH-24      =   "1"
               MOVE    WIDGET-NORMAL       TO  K02-B09S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B09S-STATE
           END-IF
      *
      *    排他中
           IF      SPA-LOCK            =   ZERO
               MOVE    WIDGET-NORMAL       TO  K02-B12-STATE
               MOVE    WIDGET-NORMAL       TO  K02-B12S-STATE
           ELSE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B04S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B05S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B07S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B08S-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B09S-STATE
      *
               MOVE    WIDGET-INSENSITIVE  TO  K02-B12-STATE
               MOVE    WIDGET-INSENSITIVE  TO  K02-B12S-STATE
           END-IF
           .
       51O-GSRAUTH-EXT.
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
           MOVE    SPACE               TO  WRK-KEI-H 
      *
      *    点数
           MOVE    SPA-GMN-TENSU(WRK-IDX)  TO  WRK-TENSU-X
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
               IF     (SPA-GMN-KEI   (WRK-IDX) =   ZERO )
      *            院外処方の時、回数表示
                   IF     (SPA-COM-SRYKBN(WRK-IDX)
                                               =   "21"  OR "22"
                                               OR  "23" )   OR
                          (SPA-COM-SRYSYUKBN(WRK-IDX)
                                               =   "148"  OR
                                                   "149"  )
                       MOVE    " X "               TO  WRK-KAKERU
      *ver.4.7
                       IF      SPA-COM-INGAITEN (WRK-IDX) >  ZERO
                           MOVE    SPACE           TO  WRK-TENSU-GX
                           MOVE    SPA-COM-INGAITEN(WRK-IDX)
                                                   TO  WRK-TENSU-GZ
                           MOVE    "("             TO  WRK-TENSU-G1
                           MOVE    ")"             TO  WRK-TENSU-G2
                           MOVE    WRK-TENSU-GX    TO  WRK-TENSU-X
                        END-IF
      *ver.4.7
                   ELSE
      *H26.10
      *                診療人数合計
                       IF      SPA-COM-INPUTCD (WRK-IDX)
                                                   =   "099140012"
                           MOVE    " X "               TO  WRK-KAKERU
                       ELSE
      *!! 
                       MOVE    SPACE               TO  WRK-KAISU-H
                       MOVE    SPACE               TO  WRK-KAKERU
      *!! 
                       END-IF
                   END-IF
               ELSE
                   MOVE    " X "               TO  WRK-KAKERU
               END-IF
           END-IF
           STRING  WRK-TENSU-X             DELIMITED   BY  SIZE
                   WRK-KAKERU              DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI-H
           END-STRING
      *
      *    画面編集
           MOVE    SPACE               TO  WRK-SURYO-HENSYU
           MOVE    SPA-GMN-SURYO   (WRK-IDX)   TO  WRK-H-SURYO
           MOVE    SPA-GMN-TANINAME(WRK-IDX)   TO  WRK-H-TANINAME
      *
           MOVE    WRK-ZAIKEI-H                TO  WRK-H-ZAIKEI
           IF      SPA-GMN-KEI (WRK-IDX)   >=  ZERO
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z
               MOVE    WRK-KEI-Z           TO  WRK-H-KEI
           ELSE
               MOVE    SPA-GMN-KEI (WRK-IDX)
                                           TO  WRK-KEI-Z2
               MOVE    WRK-KEI-Z2          TO  WRK-H-KEI
           END-IF
      *
           MOVE    WRK-SURYO-HENSYU    TO  K02-SURYO (IDX)
      *!!! MOVE    "user-font"         TO  K02-SURYO-STYLE(IDX)
      *@
      *    包括剤
           IF    ( SPA-COM-HOUKATU-ZAI (WRK-IDX) NOT =   ZERO)   OR
                 ( SPA-COM-HOUKATU-ERR (WRK-IDX) NOT =   ZERO)   OR
                 ((SPA-COM-HKNCOMBI(WRK-IDX)     =   "9999" )
              AND (SPA-HKNCOMBINUM           NOT =   "9999" ))
              MOVE    CL-BLUE        TO  K02-SURYO-FGCOLOR(IDX)
           END-IF
           .
       5002-SURYO-HEN-EXT.
           EXIT.
      *
      *H26.10
      *****************************************************************
      *    診療人数合計編集処理
      *****************************************************************
       50021-MEISYOHEN-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO-G
           STRING  SPA-COM-NAME (WRK-IDX)  DELIMITED  BY  SPACE
                   "　　　【"              DELIMITED  BY  SIZE
                   SPA-SRYYMD (7:2)        DELIMITED  BY  SPACE
                   "日（"
                   WRK-KAISU-H             DELIMITED  BY  SPACE
                   "人）】"                DELIMITED  BY  SIZE
                   INTO                    WRK-MEISYO
           END-STRING
      *
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-MEISYO
      *
           MOVE    WRK-MEISYO-G        TO  K02-MEISYO(IDX)
           MOVE    WRK-MEISYO-G        TO  SPA-GMN-MEISYO-G(WRK-IDX)
           .
       50021-MEISYOHEN-SEC-EXT.
           EXIT.
      *
      *****************************************************************
      *    リハビリ・患者情報編集処理
      *****************************************************************
       5003-TABBOHEN-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX2    FROM    1   BY  1
      **           UNTIL       IDX2    >   5
                   UNTIL       IDX2    >   6
               MOVE    ZERO                TO  IDY2
               EVALUATE    SPA-GMNHYOKBN (IDX2)
               WHEN    "1"
      *            リハビリ
                   MOVE    9              TO  IDY2
               WHEN    "2"
      *            禁忌
                   MOVE    1              TO  IDY2
               WHEN    "3"
      *            アレルギー
                   MOVE    2              TO  IDY2
               WHEN    "4"
      *            感染症
                   MOVE    3              TO  IDY2
               WHEN    "5"
      *            コメント
                   MOVE    4              TO  IDY2
      *H28.10
               WHEN    "6"
      *            介護
                   MOVE    6              TO  IDY2
               END-EVALUATE
      *
               EVALUATE    IDY2
               WHEN    9
      *            リハビリ開始日
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   7  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-RIHAYMD (IDX)  NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-RIHAYMD (IDX)
                                                   TO  K02-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *            急性発症日
                   IF     (SPA-GMN-KYUSEI     NOT =   SPACE )
                      AND (IDY                    <   40    )
                       ADD     1                  TO  IDY
                       MOVE    SPA-GMN-KYUSEI     TO  K02-TABOO-ITEM
                                                              (IDY)
                   END-IF
               WHEN    1   THRU   4
      *            患者情報
                   PERFORM  VARYING   IDX     FROM    1   BY  1
                            UNTIL    (IDX         >   8  )
                                 OR  (IDY         >=  40 )
                       IF      SPA-GMN-TABOO (IDY2 IDX)
                                                   NOT =   SPACE
                           ADD     1               TO  IDY
                           MOVE    SPA-GMN-TABOO (IDY2 IDX)
                                                   TO  K02-TABOO-ITEM
                                                               (IDY)
                       END-IF
                   END-PERFORM
      *H28.10
               WHEN    6
      *            介護情報
                   IF      (IDY                <   40 )
                      AND  (SPA-GMN-YOUKAIGO   NOT =   SPACE)
                       ADD     1               TO  IDY
                       MOVE    SPA-GMN-YOUKAIGO
                                                   TO  K02-TABOO-ITEM
                                                               (IDY)
                   END-IF
               END-EVALUATE
           END-PERFORM
      *
           MOVE    IDY                 TO  K02-TABOO-COUNT
           MOVE    K02-TABOO-ITEM(1)   TO  K02-TABOO
           .
       5003-TABBOHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
      *    カーソルセット
           MOVE    SPA-MAP-IDX         TO  WRK-IDX2
           IF      WRK-IDX2            =   ZERO
               MOVE    1               TO  WRK-IDX2
           END-IF
      *
      *    明細入力の時、コードが入力されていること
           IF      SPA-GMN-CUR         =   2
               IF      K02-INPUTCD(SPA-MAP-IDX)    =   SPACE
                   MOVE    1               TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           IF      SPA-GMN-CUR         =   1
               IF      WRK-IDX2            >   MAX-GMN
      *********    MOVE    08              TO  SPA-GMN-CUR
      *            最終行へ
                   MOVE    MAX-GMN         TO  WRK-IDX2
               END-IF
           END-IF
      *
      *    患者番号が空白の時、患者番号へ
           IF      K02-PTNUM       =  SPACE
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    排他エラーの時、患者番号へ
           IF      SPA-ERRCD           =   "9999"
               MOVE    00              TO  SPA-GMN-CUR
           END-IF
      *
      *    ROW が優先される為、クリア
           MOVE    ZERO                TO  K02-PANDATABLE1-TROW
           MOVE    ZERO                TO  K02-PANDATABLE1-TCOLUMN
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    08
                   MOVE    "B07"           TO  MCP-WIDGET
               WHEN    00
                   MOVE    "PTNUM"         TO  MCP-WIDGET
               WHEN    01
      *            入力コード
      *            MOVE    "INPUTCD"       TO  MCP-WIDGET
      *            MOVE    WRK-IDX2        TO  MCP-WIDGET(8:2)
                   MOVE    "PANDATABLE1"   TO  MCP-WIDGET
                   MOVE    4               TO  K02-PANDATABLE1-TROWATTR
      *TEST
      *            最終行は画面の最後へ
                   IF     (WRK-IDX2        >=  WRK-MAP-MAX  )
                       MOVE    1           TO  K02-PANDATABLE1-TROWATTR
                   END-IF
      *
      *            前次頁の時は、画面の先頭へ
                   IF     (FLG-F6          =   1)  OR
                          (FLG-F7          =   1)
                       MOVE    ZERO            TO
                                               K02-PANDATABLE1-TROWATTR
                       MOVE    ZERO            TO  FLG-F6
                       MOVE    ZERO            TO  FLG-F7
                   END-IF
      *TEST
                   MOVE    2               TO  K02-PANDATABLE1-TCOLUMN
                   MOVE    WRK-IDX2        TO  K02-PANDATABLE1-TROW
      *
      *            エラー位置の背景色
                   IF     (SPA-ERRCD       =   SPACE )
                   OR     (SPA-ERRCD       =   "K999"
                                           OR  "9100"
                                           OR  "7001"
                                           OR  "7007"
                                           OR  "7004")
                   OR    ((WRK-MAP-CUR     =   ZERO  )   AND
                          (WRK-MAP-CUR2    =   ZERO  ))
                   OR     (K02-INPUTCD (WRK-IDX2)  =   SPACE )
                       CONTINUE
                   ELSE
                       IF      SPA-ERRCD(1:1)  =   "K"
      *                    白の時、設定なし
                           IF      SPA-KEICOLORS   NOT =   CL-WHITE
                               MOVE    SPA-KEICOLORS
                                   TO  K02-INPUTCD-BGCOLOR (WRK-IDX2) 
                           END-IF
                       ELSE
      *                    白の時、設定なし
                           IF      SPA-ERRCOLORS   NOT =   CL-WHITE
                               MOVE    SPA-ERRCOLORS
                                   TO  K02-INPUTCD-BGCOLOR (WRK-IDX2) 
                           END-IF
                       END-IF
                   END-IF
      *
               WHEN    02
      *            名称
      *            MOVE    "MEISYO"        TO  MCP-WIDGET
      *            MOVE    WRK-IDX2        TO  MCP-WIDGET(7:2)
                   MOVE    "PANDATABLE1"   TO  MCP-WIDGET
                   MOVE    4               TO  K02-PANDATABLE1-TROWATTR
      *TEST
      *            最終行は画面の最後へ
                   IF     (WRK-IDX2        >=  WRK-MAP-MAX  )
                       MOVE    1           TO  K02-PANDATABLE1-TROWATTR
                   END-IF
      *TEST
                   MOVE    3               TO  K02-PANDATABLE1-TCOLUMN
                   MOVE    WRK-IDX2        TO  K02-PANDATABLE1-TROW
               WHEN    04
                   MOVE    "DOSELNUM"      TO  MCP-WIDGET
               WHEN    05
                   MOVE    "HKNCOMBI"      TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SRYKA"         TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SRYYMD"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "DRCD"          TO  MCP-WIDGET
               WHEN    90
                   MOVE    "B03"           TO  MCP-WIDGET
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
           MOVE    "K02"               TO  LNK-SC96-PG
           CALL    "ORCSC96"           USING    SPA-AREA
                                                ORCSC96AREA
           IF      SPA-ERRCD           =  "K008"
               MOVE    1               TO  SPA-NAI-ROUSTR
           END-IF
      *
           EVALUATE    SPA-ERRCD
               WHEN    "9999"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9998"
                   STRING  "他端末で使用中です。"
                           "更新はできません。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9997"
                   STRING  "他端末で使用中です。"
                           SLOCK-MSG           DELIMITED  BY  SIZE
                                               INTO  SPA-ERRMSG
                   END-STRING
           END-EVALUATE
      *
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
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K02"               TO  SPA-MOTOPG
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
           MOVE    "K02"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  WRK-KIDMSG
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING  "すでに当日診察料を算定しています。"
                           "ＯＫで同日再診へ振り替えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *
               WHEN    "0100"
                   IF      (SPA-INITAUTO-FLG    =   "0"  )
                       STRING  "当日に受診があります。"
                       "ＯＫで同日再診入力、ＮＯで訂正表示を行います。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                       END-STRING
                   ELSE
                       STRING  "当日に受診があります。"
                       "ＯＫで同日再診、ＮＯで訂正表示を行います。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                       END-STRING
                   END-IF
               WHEN    "0131"
                   IF      (SPA-INITAUTO-FLG    =   "0"  )
                       STRING  "当日に受診があります。"
                       "ＯＫで訂正、ＮＯで同日再診入力をして下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                       END-STRING
                   ELSE
                       STRING  "当日に受診があります。"
                       "ＯＫで訂正、ＮＯで同日再診表示を行います。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                       END-STRING
                   END-IF
               WHEN    "0102"
                   MOVE    "外来管理加算は算定できません。削除します"
                                       TO  WRK-KIDMSG
               WHEN    "0103"
                   MOVE    "継続中です。患者設定画面へ遷移します。"
                                       TO  WRK-KIDMSG
               WHEN    "0104"
                   MOVE    "処理を終了します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0105"
                   STRING  "保険組合せが変更されました。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0106"
                   STRING  "同日再診が３回以上になります。"
                           "訂正のみ行うようにして下さい。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0107"
                   STRING   "診療科が変更されました。"
                            "現在の診療内容を残しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0123"
                   STRING   "診療科が変更されました。"
                            "よろしいですか。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0108"
                   STRING  "初診算定日を登録します。"
                           "現在の診療内容はクリアします。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0109"
                   STRING  "既に診察料を他科で算定しています。"
                           "ＯＫで同日再診料へ変更します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0110"
               MOVE
                   "初診料算定へ変更します。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0117"
                   STRING   "再診料算定へ変更します。"
                            "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0132"
                   STRING   "初診料を再度自動発生させます。"
                            "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0137"
                   STRING   "再診料を再度自動発生させます。"
                            "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0111"
                   MOVE
                   "診療日が未来日です。よろしいですか？"
                                       TO  WRK-KIDMSG
               WHEN    "0112"
                   STRING  "初診料算定へ変更しますが、"
                           "転帰が未入力の病名が存在します。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0113"
                   STRING  "特定疾患処方管理加算が算定できます。"
                            "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "特定疾患処方管理加算１が算定できます。"
                           "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
                   END-IF
               WHEN    "0130"
                   STRING  "長期投薬加算が算定できます。"
                           "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "特定疾患処方管理加算２が算定できます。"
                           "ＯＫで自動算定します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
                   END-IF
      *!!!!
               WHEN    "0213"
                   STRING  "特定疾患処方管理加算が算定できます。"
                            "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "特定疾患処方管理加算１が算定できます。"
                            "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
                   END-IF
               WHEN    "0230"
                   STRING  "長期投薬加算が算定できます。"
                           "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H30.4 から
                   IF      SPA-SRYYMD      >=  "20180401"
                   STRING  "特定疾患処方管理加算２が算定できます。"
                           "ＯＫで自動算定します。"
                            "（併用算定警告該当有）"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
                   END-IF
      *!!!!
               WHEN    "0114"
                   STRING  "厚生労働大臣が定める患者の再診４回目以降へ"
                           "振替えます。よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0115"
                   STRING  "人工腎臓の再診４回目以降へ振替えます。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0116"
                   STRING  "診療行為内容をクリアします。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0127"
                   STRING  "ＯＫで展開中の中途終了データを削除します。"
                           "よろしいですか？"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0118"
                   STRING  "この保険の中途終了データが存在します。"
                           "ＯＫで中途終了内容へ置換えます。"
                                           DELIMITED   BY  SIZE
                                   INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0119"
                   STRING  "既に他保険で受診済みです。"
                           "ＯＫで他保険にて算定済へ置換えます"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0120"
                   MOVE    "再診料（３月超）（５回以内）へ"
                                       TO  WRK-KIDMSG
                   MOVE    "振替えます。よろしいですか？"
                                       TO  WRK-KIDMSG(31:)
               WHEN    "0121"
                   STRING  "入院中の患者です。入院画面へ遷移します。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0128"
      *            STRING  "入院中です。"
      *                    "アフターケア以外ですので入院画面へ"
      *                    "遷移します。よろしいですか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO   WRK-KIDMSG
      *            END-STRING
                   STRING  "入院中です。"
                           "アフターケア・自費保険以外ですので"
                           "入院画面へ遷移します。"
                                       DELIMITED  BY  SIZE
                                       INTO   WRK-KIDMSG
                   END-STRING
               WHEN    "0122"
                   STRING  "この科の中途終了データが存在します。"
                           "ＯＫで中途終了内容へ置換えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0124"
                   MOVE    "この科の前回内容へＯＫで置換えます。"
                                       TO  WRK-KIDMSG
               WHEN    "0125"
                   STRING  "患者取消が選択されました。"
                           "表示内容をクリアします。よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0160"
                   STRING  "包括検査で同じ検査があります。"
                           "ＯＫですべての同一検査を削除します。"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0180"
                   STRING  "個別療法が１０単位を超えます。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0190"
                   STRING  "包括分入力に変更されました。"
                           "現在の診療内容をクリアしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "2001"
                   STRING   "内服薬剤が７種類以上になります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "2002"
                   STRING  "内服薬剤が７種類以上になります。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *ver.4.7
               WHEN    "2004"
      *********    STRING  "内服薬剤が７種類以上ですが、"
      *                    "用法まとめで７種類以下です。"
      *                    "７種類以上の処方せん料にしますか？"
      *                                DELIMITED   BY  SIZE
      *                                INTO    WRK-KIDMSG
      *            END-STRING
      ***********  MOVE    SPACE       TO  WRK-KIDMSG
                   STRING  "用法まとめでは内服薬剤は７種類未満です。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "2005"
                   STRING  "用法まとめでは内服薬剤が７種類以上に"
                           "なります。"
                           "７種類以上の処方せん料にしますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H26.10　改正
               WHEN    "2006"
                   STRING  "向精神薬多剤投与となります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *H27.4　改正
               WHEN    "2007"
                   STRING  "３０日以上の投薬があります。"
                           "逓減しますか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *ver.4.7
      *
               WHEN    "0140"
                   STRING  "初回算定日を算定履歴に作成します。"
                                           DELIMITED  BY  SIZE
                           "取消はできません。よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0150"
                   STRING  "包括算定エラーの項目があります。"
                                           DELIMITED  BY  SIZE
                           "エラー項目を削除します。"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *!!!
      *H22.4
               WHEN    "2003"
                   STRING  "手帳記載加算（薬剤情報提供料）を"
                           "算定します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
      *!!!
      *VER.4.6
               WHEN    "0138"
                   STRING   "診療科の保険組合せに変更しますか？"
                            "【"
                                       DELIMITED   BY  SIZE
                             HZN-SPA-GMN-HKNCOMBI-G
                                       DELIMITED   BY  "  "
                             "】"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0129"
      *            STRING  "自費保険で外来診療行為を入力しますか？"
      *                                DELIMITED  BY  SIZE
      *                                INTO   WRK-KIDMSG
      *            END-STRING
                   STRING  "ＯＫで自費保険の当日の外来診療を、"
                           "ＮＯで入院日の外来診療を入力します。"
                                       DELIMITED  BY  SIZE
                                       INTO   WRK-KIDMSG
                   END-STRING
      *H26.4
               WHEN    "0133"
      *H28.4       STRING  "小児科外来診療料を初診料へ変更します。"
                   STRING  WRK-KIDCD-MSG
                           "を初診料へ変更します。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0134"
      *H28.4       STRING  "小児科外来診療料を初診料へ変更しますが、"
                   STRING  WRK-KIDCD-MSG
                           "を初診料へ変更しますが、"
                           "転帰が未入力の病名が存在します。"
                           "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
                   END-STRING
               WHEN    "0135"
      *H28.4       STRING   "小児科外来診療料を再診料へ変更します。"
                   STRING  WRK-KIDCD-MSG
                            "を再診料へ変更します。"
                            "よろしいですか？"
                                       DELIMITED   BY  SIZE
                                       INTO    WRK-KIDMSG
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
           EVALUATE    SPA-KIDCD
               WHEN    "0100"
               WHEN    "0109"
               WHEN    "0113"
               WHEN    "0130"
      *
               WHEN    "0230"
               WHEN    "0213"
      *
               WHEN    "0118"
               WHEN    "0119"
               WHEN    "0107"
               WHEN    "0122"
               WHEN    "0124"
               WHEN    "0180"
               WHEN    "0190"
               WHEN    "2001"
               WHEN    "2002"
               WHEN    "0127"
               WHEN    "0131"
               WHEN    "2003"
               WHEN    "0138"
               WHEN    "0121"
               WHEN    "0128"
               WHEN    "0129"
      *ver.4.7
               WHEN    "2004"
               WHEN    "2005"
               WHEN    "2006"
               WHEN    "2007"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
      *ver.4.7
           EVALUATE    SPA-KIDCD
      *    処方せん料７種類以上の判定
               WHEN    "2004"
                   MOVE    "B01"               TO  MCP-WIDGET
           END-EVALUATE
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "KID1"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID1"              TO  MCP-WINDOW
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
      *    確認メッセージ表示理
      *****************************************************************
       530-KID2SET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD2
               WHEN    "0117"
                   STRING  "登録後、帳票を発行する時は各帳票の発行を、"
                           "発行しない時は登録を押下して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KIDCD2
                                       TO  WRK-KIDMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KID2
           INITIALIZE                      KID2
           MOVE    SPA-KIDCD2          TO  KID2-ID2CODE
           MOVE    WRK-KIDMSG          TO  KID2-ID2MSG
      *    ユーザプログラム起動初期表示
      *v4.8
      *    ginbee対応
      **   IF      SPA-MW              =   SPA-GINBEE
      *        MOVE    WIDGET-INSENSITIVE  TO  KID2-USERPG-STATE
      **   ELSE
               PERFORM 5301-USERPG-SET-SEC
      ***  END-IF
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K02"               TO  SPA-MOTOPG
           MOVE    "KID2"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KID2"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-KID2SET-EXT.
           EXIT.
      *****************************************************************
      *     ユーザプログラム起動初期処理
      *****************************************************************
       5301-USERPG-SET-SEC              SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "K02"               TO  ORCSUSERCHK-GMNPG
           MOVE    "1"                 TO  ORCSUSERCHK-SYORI
           CALL    "ORCSUSERCHK"       USING
                                       ORCSUSERCHKAREA
                                       SPA-AREA
           MOVE    ORCSUSERCHK-GYOUMU-FLG  TO  SPA-NAI-GYOUMU-FLG
           MOVE    ORCSUSERCHK-GYOUMU-ARI  TO  SPA-NAI-GYOUMU-ARI
      *
           MOVE    "0 Ｕ・Ｐ指示なし"      TO  SPA-GMN-USERPG-LIST (1)
           IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *        実行するプログラムがある時
               MOVE    "1 Ｕ・Ｐ指示あり"  TO  SPA-GMN-USERPG-LIST (2)
               MOVE    2                   TO  SPA-USERPG-MAX
               IF      SPA-NAI-GYOUMU-FLG  =   "1"
                   MOVE    SPA-GMN-USERPG-LIST (2) TO  SPA-GMN-USERPG-G
               ELSE
                   MOVE    SPA-GMN-USERPG-LIST (1) TO  SPA-GMN-USERPG-G
               END-IF
           ELSE
      *        実行するプログラムがない時
               MOVE    1                   TO  SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LIST(1)  TO  SPA-GMN-USERPG-G
           END-IF
      *
           MOVE    SPA-GMN-USERPG-G    TO  KID2-USERPG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LIST (IDX)
                                       TO  KID2-USERPG-ITEM (IDX)
           END-PERFORM
           MOVE    SPA-USERPG-MAX      TO  KID2-USERPG-COUNT
      *
           MOVE    WIDGET-NORMAL       TO  KID2-USERPG-STATE
            .
       5301-USERPG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACT-REC
               MOVE    ZERO                TO  FLG-SRYACT
           ELSE
               INITIALIZE                      SRYACT-REC
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *
           .
       900-SRYACT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCT-REC
               MOVE    ZERO                TO  FLG-SRYACCT
           ELSE
               INITIALIZE                      SRYACCT-REC
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
      *
           .
       940-SINACCT-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    SPA-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNS-YUKOEDYMD
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
      *
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
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力コードマスター読込
      *****************************************************************
       930-INPUTCD-READ-SEC         SECTION.
      *
           MOVE    INPUTCD-REC         TO  MCPDATA-REC
           MOVE    "tbl_inputcd"       TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_inputcd"       TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
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
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       930-INPUTCD-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ次読込
      *****************************************************************
       970-WKSRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  WKSRYACT-REC
               MOVE    ZERO                TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                      WKSRYACT-REC
               MOVE    1                   TO  FLG-WKSRYACT
           END-IF
      *
           .
       970-WKSRYACT-READ-EXT.
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
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptinf"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTINF-REC
                   MOVE    ZERO                TO  FLG-PTINF
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
           PERFORM 990-DBCLOSE-SEC
      *
           .
       900-PTINF-READ-EXT.
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
      *
      *****************************************************************
      *    患者病名読込
      *****************************************************************
       980-PTBYOMEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptbyomei"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTBYOMEI-REC
               MOVE    ZERO            TO  FLG-PTBYOMEI
           ELSE
               INITIALIZE                  PTBYOMEI-REC
               MOVE    1               TO  FLG-PTBYOMEI
           END-IF
      *
           .
       980-PTBYOMEI-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-KANRIMST-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  PTNYUINRRK-REC
               MOVE    ZERO            TO  FLG-PTNYUINRRK
           ELSE
               INITIALIZE                  PTNYUINRRK-REC
               MOVE    1               TO  FLG-PTNYUINRRK
           END-IF
      *
           .
       910-PTNYUINRRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    患者メモ読込処理
      *****************************************************************
       910-PTMEMOINF-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  PTMEMOINF-REC
               MOVE    ZERO                TO  FLG-PTMEMOINF
           ELSE
               INITIALIZE                      PTMEMOINF-REC
               MOVE    1                   TO  FLG-PTMEMOINF
           END-IF
      *
           .
       910-PTMEMOINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定歴マスタ読み込み
      *****************************************************************
       920-SANTEI-READ-SEC         SECTION.
      *
           MOVE    "tbl_santei"        TO  MCP-TABLE
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SANTEI-REC
               MOVE    ZERO                TO  FLG-SANTEI
           ELSE
               INITIALIZE                      SANTEI-REC
               MOVE    1                   TO  FLG-SANTEI
           END-IF
      *
           .
       920-SANTEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       32012-SRYACCTPLUS-READ-SEC       SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
                   MOVE    ZERO                TO  FLG-SRYACCTPLUS
               ELSE
                   INITIALIZE                      SRYACCTPLUS-REC
                   MOVE    1                   TO  FLG-SRYACCTPLUS
               END-IF
           ELSE
               INITIALIZE                  SRYACCTPLUS-REC
               MOVE    1               TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       32012-SRYACCTPLUS-READ-EXT.
           EXIT.
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
      *    ＤＢクロース処理
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
      *    一時保存テーブル読み込み
      *****************************************************************
       9100-SPATMP-KEYREAD-SEC        SECTION.
      *
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
           .
       9100-SPATMP-KEYREAD-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"           TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA2-REC    TO  SPATMP-REC
               MOVE    ZERO            TO  FLG-SPATMP
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
      *
           .
       990-SPATMP-READ-EXT.
           EXIT.
      *****************************************************************
      *    テーブル検索処理
      *****************************************************************
       910-SPATMP-DBSELECT-SEC                SECTION.
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
      *
           MOVE    "DBSELECT"          TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       910-SPATMP-DBSELECT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＤＢクロース処理
      *****************************************************************
       990-SPATMP-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"     TO  MCP-FUNC
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       990-SPATMP-DBCLOSE-EXT.
           EXIT.
      *****************************************************************
      *    一時保存テーブル検索処理
      *****************************************************************
       910-SPATMP-CALL-SEC                SECTION.
      *
           MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
      *
           CALL    "ORCDBSPATMP"       USING   MCPAREA
                                               MCPDATA2-REC
                                               SPA-AREA
      *
           .
       910-SPATMP-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-GMN-PTID        TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    排他制御処理
      *****************************************************************
       990-LOCK-IN2-SEC         SECTION.
      *
      *    排他制御処理
           MOVE    1                   TO  SLOCK-MODE
           MOVE    SPA-PTID            TO  SLOCK-PTID
           CALL    "ORCSLOCK"          USING
                                       MCPAREA
                                       SPA-AREA
                                       ORCSLOCKAREA
           IF      SLOCK-RETURN    NOT =   ZERO
               MOVE    "9999"              TO  SPA-ERRCD
           END-IF
      *    排他中
           IF      SPA-LOCK            =   1
               MOVE    SPACE               TO  SPA-ERRCD
               MOVE    2                   TO  SPA-LOCK
           END-IF
           .
       990-LOCK-IN2-EXT.
           EXIT.
      *
      *****************************************************************
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           MOVE    ZERO            TO  FLG-MOD-FLG
           PERFORM 4900-WKSRYACT-CLEAR-SEC
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
