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
       PROGRAM-ID.                 ORCGK08.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 診療行為確認　（Ｋ０８）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  01/05/25    MCC−多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    MCC-多々納   02/04/12  自費の修正
      *  01.00.02    NACL-多々納  02/07/19  判断料の削除
      *  01.00.03    NACL-多々納  02/09/13  自賠責の追加
      *  01.00.04    NACL-多々納  02/09/18  内服薬粉砕の追加
      *  01.00.05    NACL-多々納  02/10/18  入院の追加
      *  01.00.06    NACL-多々納  02/11/20  自賠責器材追加
      *  01.00.07    NACL-多々納  02/12/02  食事の追加
      *  01.00.08    NACL-多々納  02/12/13  継続区分再編集、継続訂正修正
      *  01.00.09    NACL-多々納  03/01/16  自費回数表示修正
      *  01.00.10    NACL-多々納  03/03/05  排他制御処理追加
      *  01.00.11    NACL-多々納  03/04/30  自費編集修正
      *  01.00.12    NACL-多々納  03/05/13  自賠責の腰部固定帯加算修正
      *  01.00.13    NACL-多々納  03/08/25  福岡県地方公費対応
      *  01.00.14    NACL-多々納  03/09/04  算定済メッセージ編集追加
      *  01.00.15    NACL-多々納  05/06/28  訂正時の展開追加による修正
      *  01.00.16    NACL-多々納  05/06/30  プレビュー追加
      *  01.00.17    NACL-多々納  05/11/17  MONFUNC 対応
      *  01.00.18    NACL-多々納  05/12/21  登録順追加
      *  01.00.19    NACL-多々納  06/01/23  用量割合・包括検査数追加
      *  01.00.20    NACL-多々納  06/07/06  収納変更対応
      *  01.00.21    NACL-多々納  06/08/18  福岡県地方公費に同日初診追加
      *  03.04.00    NACL-多々納  06/11/14  検査正式名称表示追加他
      *  03.04.01    NACL-多々納  07/02/XX  包括診療処理追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  03.05.00    NACL-多々納  07/05/22  再印刷処理追加
      *  04.01.00    NACL-多々納  07/12/06  公害保険対応
      *  04.03.00    NACL-多々納  08/07/08  お薬手帳発行の連動
      *  04.03.00    NACL-多々納  08/08/01  入院調剤料保険組合せ期間チェック追加
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.06.01    NACL-伊藤    11/01/28  リアルタイム送信
      *  04.06.00    NACL-多々納  11/02/18  用法コメント対応
      *  04.07.00    NACL-多々納  11/12/16  商品名単位コード対応
      *  04.07.00    NACL-多々納  12/01/06  クライアント印刷対応
      *  04.07.00    NACL-多々納  12/04/09  Ｈ２４年４月改正対応
      *                                     （一般名記載編集）
      *  04.07.00    NACL-多々納  13/03/XX  Ｈ２５年４月改正対応
      *  04.08.00    NACL-多々納  14/05/XX  一時保存データ対応
      *  04.08.00    NACL-多々納  14/08/11  クライアント印刷変更
      *  04.08.00    NACL-多々納  15/02/XX  後発品変更不可処方対応
      *  04.08.00    NACL-多々納  15/07/28  負担金計算サブ対応他
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
      *    SELECT  PARA-FILE02     ASSIGN  FILE-NAME02
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PARA02.
      *
      *    パラメタデータ（プレビュー）
      *    SELECT  PARA-PREVIEW     ASSIGN  FILE-PREVIEW
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-PREVIEW.
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
      *    パラメタデータ
      *FD  PARA-FILE02.
      *01  PARA-REC02.
      *    03  PARA-KEY02.
      *        05  PARA-FILEMEI02         PIC X(08).
      *        05  PARA-EDANUM02          PIC 9(03).
      *    03  PARA-DATA-REC02            PIC X(60000).
      *
      *    パラメタデータ（プレビュー）
      *FD  PARA-PREVIEW.
      *01  PARA-RECPRV.
      *    03  PARA-KEYPRV.
      *        05  PARA-FILEMEIPRV         PIC X(08).
      *        05  PARA-EDANUMPRV          PIC 9(03).
      *    03  PARA-DATA-RECPRV            PIC X(60000).
      *
       WORKING-STORAGE             SECTION.
      *
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "K01PARA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(09)   VALUE   "K02SPASCR".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME02.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(10)   VALUE   "WRKK01PARA".
      *    03  FILE-HOSPNUM02      PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID02       PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *    パラメタファイル名（プレビュー）
      *01  FILE-PREVIEW.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(10)   VALUE   "PRVK01PARA".
      *    03  FILE-HOSPNUMPRV     PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMIDPRV      PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *    画面Ｋ０８１スパ領域
           COPY    "K081SCR-SPA".
      *
      *    画面スパ領域
       01  SPA-K08.
           03  SPA-K08-AREA.
               05  SPA-GMN-PAGE                  PIC 9(02).
               05  SPA-MAX-PAGE                  PIC 9(02).
               05  SPA-GMN-LINE                  PIC 9(02).
               05  SPA-K08-MAX                   PIC 9(04).
               05  SPA-GMN-CUR                   PIC 9(02).
      *
               05  SPA-GMN-TBL.
                   07  SPA-GMN-TBLREC-G    OCCURS  800.
                       09  SPA-GMN-TBANGO      PIC 9(03).
                       09  SPA-GMN-TDELFLG     PIC 9(01).
                       09  SPA-GMN-TMEISYO     PIC X(150).
                       09  SPA-GMN-TMEISYO-1   REDEFINES
                                                   SPA-GMN-TMEISYO.
                           11  SPA-GMN-TMEISYO1    PIC X(150).
                       09  SPA-GMN-TMEISYO-3   REDEFINES
                                                   SPA-GMN-TMEISYO.
                           11  SPA-GMN-TYOBI3      PIC X(02).
                           11  SPA-GMN-TMEISYO3    PIC X(148).
                       09  SPA-GMN-TMEISYO-2   REDEFINES
                                               SPA-GMN-TMEISYO.
                           11  SPA-GMN-TBANGO2     PIC X(02).
                           11  SPA-GMN-TMEISYO2    PIC X(70).
                           11  SPA-GMN-HEN1.
                               13  SPA-GMN-TSURYOU     PIC X(22).
                               13  SPA-GMN-TTANI1      PIC X(08).
                               13  SPA-GMN-TTANMEI     PIC X(06).
                               13  SPA-GMN-TTANI2      PIC X(04).
                               13  SPA-GMN-TYOBI1      PIC X(28).
                           11  SPA-GMN-HEN2    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TYOBI       PIC X(02).
                               13  SPA-GMN-TZAIKEI     PIC X(60).
                               13  SPA-GMN-TYOBI2      PIC X(06).
                           11  SPA-GMN-HEN3    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TYOBI33     PIC X(26).
                               13  SPA-GMN-TZAIKEI3    PIC X(40).
                               13  SPA-GMN-TZAIKEI3-R  REDEFINES
                                                       SPA-GMN-TZAIKEI3.
                                   15  SPA-GMN-TYOBI32     PIC X(06).
                                   15  SPA-GMN-TYOBI33     PIC X(34).
                           11  SPA-GMN-HEN4    REDEFINES
                                               SPA-GMN-HEN1.
                               13  SPA-GMN-TYOBI41     PIC X(16).
                               13  SPA-GMN-TZAIKEI4    PIC X(10).
                               13  SPA-GMN-TKINGAK4    PIC X(40).
      *                内部位置
                       09  SPA-GMN-NAIIDX      PIC 9(03).
                       09  SPA-GMN-NAIBANGO    PIC 9(03).
      *H24.4           一般名称
                       09  SPA-GMN-GE-MEISYO   PIC X(100).
                       09  SPA-GMN-ME-MEISYO   PIC X(100).
      *
               05  SPA-GMN-GOKEI             PIC S9(11).
               05  SPA-GMN-LASTYMD           PIC X(09).
               05  SPA-GMN-SYOSINYMD         PIC X(09).
               05  SPA-GMN-MISYUMONEY        PIC S9(08).
      *
               05  SPA-GMN-HKNTEN-TBL        OCCURS  16.
                   07  SPA-GMN-HKNTEN        PIC S9(07).
      *
               05  SPA-GMN-SELNUM-G.
                   07  SPA-GMN-SELNUM        PIC 9(03)   OCCURS  10.
      *        メッセージ
               05  SPA-GMN-MSGDATA           PIC X(50).
      *        ユーザＰＧ起動（入院）
               05  SPA-GMN-USERPG-G.
                   07  SPA-GMN-USERPG            PIC X(01).
                   07  SPA-GMN-UP1               PIC X(01).
                   07  SPA-GMN-USERPGMEI         PIC X(18).
               05  SPA-GMN-USERPG-LSTG.
                    07  SPA-GMN-USERPG-LIST     PIC X(20)
                                                   OCCURS  2.
               05  SPA-USERPG-MAX                 PIC 9(02).
      *
           03  SPA-NAI-AREA.
               05  SPA-NAI-TBL.
                   07  SPA-NAI-TBLREC  OCCURS  800.
                       09  SPA-NAI-BANGO           PIC 9(03).
                       09  SPA-NAI-EDANUM          PIC 9(03).
                       09  SPA-NAI-ZAINUM          PIC 9(08).
                       09  SPA-NAI-SRYKBN          PIC X(02).
                       09  SPA-NAI-SRYSYUKBN       PIC X(03).
                       09  SPA-NAI-HKNTEN          PIC 9(08).
                       09  SPA-NAI-TENSFLG         PIC 9(01).
                       09  SPA-NAI-DELFLG          PIC 9(01).
                       09  SPA-NAI-HKNSFLG         PIC 9(01).
                       09  SPA-NAI-HKNCOMBI        PIC X(04).
                       09  SPA-NAI-SRYKA           PIC X(02).
      *
                       09  SPA-NAI-JIDFLG          PIC 9(01).
               05  SPA-KEI-FLG         PIC 9(01).
      *        剤登録順区分
               05  SPA-ZAIJUNKBN-FLG           PIC 9(01).
      *        ユーザプログラム起動
               05  SPA-NAI-GYOUMU-FLG             PIC X(01).
               05  SPA-NAI-GYOUMU-ARI             PIC 9(01).
      *        レセプトプレビュー区分
               05  SPA-NAI-RESPRVFLG              PIC X(01).
      *H24.4
      *        一般名表示区分
               05  SPA-NAI-GENEINIT               PIC 9(01).
      *
      *    フラグ領域
       01  STS-AREA.
           03  STS-PARA            PIC X(02).
           03  STS-SRYKBN          PIC X(02).
      *
           03  STS-PARA02            PIC X(02).
           03  STS-PREVIEW           PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-SRYKBN          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-INPUTCD         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
           03  FLG-YAKUSOKU        PIC 9(01).
      *
           03  FLG-CHK             PIC 9(01).
           03  FLG-DEL             PIC 9(01).
           03  FLG-DEL-ARI         PIC 9(01).
           03  FLG-RSIFLG          PIC 9(01).
           03  FLG-KAISUU          PIC 9(01).
           03  FLG-KAISUU-ARI      PIC 9(01).
           03  FLG-KAISUU-ARI2     PIC 9(01).
           03  FLG-KAISUU-ARI3     PIC 9(01).
      *
           03  FLG-KAKU-FLG        PIC 9(01).
           03  FLG-SYORI-END       PIC 9(01).
      *
           03  FLG-SYOSIN-OK       PIC 9(01).
      *
           03  FLG-SIJISEN         PIC 9(01).
      *
           03  FLG-SYU40           PIC 9(01).
      *    ユーザプログラム起動
           03  FLG-USERPG              PIC 9(01).
      *    感染症削除
           03  FLG-INFEC-DLT           PIC 9(01).
      *    クライアント印刷有
      **** 03  FLG-CLIENT-OK           PIC 9(01).
      *H24.4
           03  FLG-GENERICNAME         PIC 9(01).
           03  FLG-TENSUPLUS           PIC 9(01).
      *
           03  FLG-RSIKSN              PIC 9(01).
      *H27.1
           03  FLG-INGIKSN-KBN         PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDS                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDG2                PIC 9(04).
           03  IDG3                PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDX3                PIC 9(04).
           03  IDX4                PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
      *
           03  IDX-Y               PIC 9(04).
           03  IDX-Y2              PIC 9(02).
           03  IDX-Y3              PIC 9(02).
      *
           03  IDH                 PIC 9(04).
           03  IDH1                PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDK                 PIC 9(04).
           03  IDX-MAX             PIC 9(04).
      *
           03  IDS1                PIC 9(04).
           03  IDS2                PIC 9(04).
           03  IDS3                PIC 9(04).
      *    ＳＰＡ内番号
           03  IDX-NAI             PIC 9(04).
      *
           03  IDX-S1              PIC 9(04).
      *    福岡県対応用
           03  IDX-SYU             PIC 9(04).
           03  IDX-SYU2            PIC 9(04).
      *
           03  IDX-TS              PIC 9(04).
      *
           03  IDM2                PIC 9(04).
      *
           03  IDR-2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYSYMD.
               05  WRK-SYSYY       PIC 9(04).
               05  WRK-SYSMM       PIC 9(02).
               05  WRK-SYSDD       PIC 9(02).
      *
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WFIL1       PIC X(01).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WFIL2       PIC X(01).
               05  WRK-WDD         PIC 9(02).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *    保険組合せ
           03  WRK-HKNCOMBI          PIC X(04).
           03  WRK-HKNCOMBIMEI       PIC X(60).
           03  WRK-SRYKA             PIC X(02).
           03  WRK-SRYKAMEI          PIC X(60).
           03  WRK-DRCD              PIC X(05).
           03  WRK-DRCD-MEI          PIC X(50).
      *    選択番号
           03  WRK-SELNUM          PIC 9(03).
      *    画面番号
           03  WRK-BANGO           PIC 9(04).
      *    診療種別
           03  WRK-SRYSYUKBN       PIC X(03).
      *
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(08).
           03  WRK-KAISU-X.
      *!*******05  WRK-KAISU       PIC Z(08).
               05  WRK-KAISU       PIC Z(03).
           03  WRK-KEI-X.
               05  WRK-KEI         PIC -(08).
           03  WRK-KEI-G           PIC 9(08).
           03  WRK-KEI-GS          PIC S9(08).
           03  WRK-KAISU-9         PIC 9(03).
      *
           03  WRK-ZZ              PIC ZZZ.
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
      *
      *R02 03  WRK-MEISYO          PIC X(80).
           03  WRK-MEISYO          PIC X(140).
      *    数量表示用
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
           03  WRK-SURYO-J         PIC X(22).
      *    分画数画面編集用
           03  WRK-BUNGSU-X.
               05  WRK-BUNGSU-Z    PIC ZZZ.
           03  WRK-BUNGSU-J        PIC X(06).
      *    剤回数等画面編集用
      *!***03  WRK-KAISU-H         PIC X(08).
           03  WRK-KAISU-H         PIC X(03).
           03  WRK-KAISU-H2        PIC X(03).
           03  WRK-KEI-H           PIC X(08).
           03  WRK-ZAIKEI          PIC X(40).
           03  WRK-IDG             PIC 9(04).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  WRK-ZAIKEIJIH       PIC X(15).
           03  WRK-ZAIKEIJIJ       PIC X(30).
           03  WRK-KAISU-J         PIC X(16).
      *
           03  WRK-Z9-X            PIC X(10).
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z92             PIC --,---,---.
           03  WRK-Z7              PIC Z(07).
      *
           03  WRK-KIN             PIC 9(07).
      *
           03  WRK-TJIHI           PIC X(40).
      *
           03  WRK-ZAITEN              PIC 9(08).
           03  WRK-SRYKAISUKEI         PIC 9(03).
           03  WRK-SRYSURYOKEI         PIC 9(07)V9(05).
           03  WRK-MEISAISU            PIC 9(07).
           03  WRK-JIHIMONEY           PIC 9(07).
           03  WRK-JIHIMONEY-KEI       PIC 9(07).
      *
           03  WRK-SEIKYU              PIC S9(07).
      *
           03  WRK-SINDAN              PIC S9(07).
      *
           03  WRK-DENPNUM         PIC 9(07).
           03  WRK-MEIBAN          PIC 9(03).
      *
           03  WRK-ERRMSG          PIC X(40).
           03  WRK-MCP-PUTTYPE     PIC X(8).
      *
           03  WRK-GOKEI-X2.
               05  WRK-GOKEI-Z2    PIC ZZZ,ZZZ,ZZZ.
           03  WRK-GOKEI-X3.
               05  WRK-GOKEI-Z3    PIC ---,---,---.
      *
           03  WRK-PATH                    PIC X(64).
           03  WRK-MCP-WIDGET              PIC X(20).
      *     一部負担金計算用
           03  WRK-S01-TENSU           PIC 9(08).
           03  WRK-ACCT-KAISU          PIC 9(03).
      *    院内、院外判定用
           03  WRK-CHK-SRYSYUKBN       PIC X(03).
      *     一部負担金計算用
           03  WRK-SYUTEN1             PIC 9(08).
           03  WRK-TEN                 PIC 9(08).
      *     一部負担金計算用
           03  WRK-SYUTEN1-KEI         PIC S9(08).
           03  WRK-LNK-S01-SYUTEN      PIC S9(08).
      *
      *    診療行為ＳＯＲＴ
           03  WRK-SORT-KEY.
               05  WRK-ZAINUM-X.
                   07  WRK-ZAINUM          PIC  9(08).
               05  WRK-RENNUM-X.
                   07  WRK-RENNUM          PIC  9(02).
               05  WRK-SRYKBN          PIC  X(02).
           03  WRK-MAX                 PIC  9(04).
      *    日数
           03  WRK-NISUU               PIC  9(03).
      *
      *    粉砕判定用
           03  WRK-FUNSA-AREA.
               05  FLG-FUNSAI          PIC 9(01).
               05  WRK-FUNSAI-CNT      PIC 9(04).
      *
           03  WRK-CONTKBN         PIC X(01).
      **** 03  WRK-SYU-CONTKBN         PIC X(01).
      *
           03  WRK-KIDMSG          PIC X(80).
      *
           03  WRK-K08-ROW         PIC S9(09).
           03  FLG-GMN-INIT        PIC  9(01).
      *請求金額
           03  WRK-SKYMONEY            PIC S9(07).
      *差額の絶対値
           03  WRK-GRP-SGKMONEY        PIC  9(07).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
           03  WRK-CHUSYA-PG           PIC X(20).
           03  WRK-SIJISEN-PG          PIC X(20).
      *
           03  WRK-EDANUM            PIC 9(03).
           03  FLG-FUKU              PIC 9(01).
      *
           03  WRK-HOUKNS-CNT          PIC 9(02).
           03  WRK-HOUKNS-IDX          PIC 9(04).
           03  WRK-HOUKNS-NAME         PIC X(100).
      *    包括剤の判定
           03  WRK-HKTZAI              PIC X(01).
      *
           03  WRK-JISANKBN            PIC X(01).
      *
      *    プレビュー用
           03  WRK-PRV-HKNKBN          PIC X(01).
           03  WRK-PRV-RSI-HKNKBN      PIC X(01).
           03  WRK-PRV-KBN             PIC X(01).
      *
           03  CNT-ZAI                 PIC 9(04).
      *
           03  WRK-PTID                PIC X(10).
      *
           03  WRK-SYO-TANINAME        PIC X(12).
      *
      *H24.4
           03  WRK-KOUMEI              PIC X(04).
           03  WRK-IPNMEI              PIC 9(01).
           03  IDG-Z                   PIC 9(04).
           03  IDG-X                   PIC 9(04).
      *ver.4.7
      *    レセプトプレビュー業務区分
           03  WRK-GYOUMUKBN           PIC X(01).
      *
      *H27.7
           03  WRK-S-ZAITEN            PIC 9(08).
           03  WRK-S-KEI-G             PIC 9(08).
           03  FLG-RSIEN               PIC 9(01).
      *
      *ORCAサーベイランス一時退避
           03  WRK-INF-AREA.
               05  WRK-INF-HOSPNUM     PIC 9(02).
               05  WRK-INF-HOSPID      PIC X(24).
               05  WRK-INF-SYORIFLG    PIC 9(01).
               05  WRK-INF-NYUGAIKBN   PIC X(01).
               05  WRK-INF-PTID        PIC 9(10).
               05  WRK-INF-SRYYMD      PIC X(08).
               05  WRK-INF-SRYKA       PIC X(02).
               05  WRK-INF-BEDFLG      PIC 9(01).
      *!
      *
      *    集計収納請求内容
       01  WRK-SUM-AREA.
           03  SUM-SYU-SKY-NAIYOU.
               05  SUM-SYU-SKY-TBL             OCCURS   17.
      *        保険点数
                   09  SUM-SYU-HKNTEN          PIC  S9(07).
      *
      *    継続の時、自動発生のチェック
       01  WRK-CONTCHK-TBL.
           03  WRK-IDX-Y               PIC 9(04).
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
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *   日本語数字
       01  TBL-SUJI-AREA.
           03  FILLER          PIC X(20)   VALUE
              "１２３４５６７８９０".
       01  TBL-SUJI-R          REDEFINES   TBL-SUJI-AREA.
           03  TBL-SUJI        PIC X(02)   OCCURS   10.
      *
      *    パラメタファイル内容保存
      *01  HZN-PARA-AREA.
      *    03  HZN-PARA-REC                 OCCURS  5.
      *        05  HZN-PARA-KEY.
      *            07  HZN-PARA-FILEMEI         PIC X(08).
      *            07  HZN-PARA-EDANUM          PIC 9(03).
      *        05  HZN-PARA-DATA-REC            PIC X(60000).
      *
      *画面行ＭＡＸ
       01  MAX-GMN                 PIC 9(04)   VALUE   800.
      *処理テーブルＭＡＸ
       01  MAX-LINE                PIC 9(04)   VALUE   600.
      *
      *    感染症ファイル名
       01  INFECTION-FILE          PIC X(17)   VALUE
                                   "infectiondata2.sh".
      *
      *H28.4
      * 湿布薬（減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
      *
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *
      *    労災医療機関情報
           COPY    "CPSK4001.INC".
      *    チェック機能制御情報 
           COPY    "CPSK1008.INC".
      *    診療行為機能制御情報 
           COPY    "CPSK1038.INC".
      *
      *    入院指示せん設定情報
           COPY    "CPSK5007.INC".
      *    帳票ＰＧ
           COPY    "CPSK1034.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *    附加情報を追加
           03  WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ−（ワーク）
      *01  TNSWK-TENSU-REC.
           COPY    "CPTENSUWK.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    収納マスタワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //WRK-SYU-//.
      *    収納マスタワーク
       01  LNK-SYUNOU-AREA.
           02  LNK-SYUNOU-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU-//.
       01  LNK-SYUNOU-MAX              PIC 9(04).
      *
      *    収納マスタワーク２
       01  LNK-SYUNOU2-AREA.
           02  LNK-SYUNOU2-REC      OCCURS   15.
           COPY    "CPSYUNOU.INC"      REPLACING
                                       //SYU-// BY //LNK-SYU2-//.
       01  LNK-SYUNOU2-MAX              PIC 9(04).
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
      *
           03  LNK-SORT-SRYKBN        PIC X(02).
       01  LNK-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  HZN-WKSRYACT-AREA.
           02  HZN-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //HZN-WKSRY-//.
      *    附加情報を追加
           03  HZN-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //HZN-WKSRYP-//.
      *
           03  HZN-SORT-SRYKBN        PIC X(02).
      *
       01  HZN-WKSRYACT-MAX           PIC 9(04).
      *
      *    診療行為マスタワーク
       01  SRT-WKSRYACT-AREA.
           02  SRT-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //SRT-WKSRY-//.
      *    附加情報を追加
           03  SRT-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //SRT-WKSRYP-//.
      *
      *    診療行為マスタワーク
       01  WRK-WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                    //WKSRY-// BY //WRK-WKSRY-//.
      *    附加情報を追加
           03  WRK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //WRK-WKSRYP-//.
      *
      *H27.7
      *    診療行為マスタワーク(パラメタ）
       01  PARA-WKSRYACT-AREA.
           02  PARA-WKSRYACT-REC      OCCURS   600.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //PARA-WKSRY-//.
      *    附加情報を追加
           03  PARA-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //PARA-WKSRYP-//.
      *H27.7
      *
      *H24.4
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *    一般名テーブル
       01  GENERICNAME-REC.
           COPY    "CPGENERICNAME.INC".
      *
      *v4.8
      *    パラメタテーブル
       01  PARA-REC.
           COPY    "CPPARA.INC". 
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
      *    数字変換領域
      *D   COPY    "CPORCSNUM.INC".
      *
      *    負担金額計算用パラメタ
           COPY    "CPORCS01.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    診療行為入力項目変換パラメタ
           COPY    "CPORCSC97.INC".
      *
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
      *    診療行為外来更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
      *    排他制御サブパラメタ
           COPY    "CPORCSLOCK.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *福岡県対応用
      *    給付対象外点数編集サブパラメタ
           COPY    "CPORCSSYU40.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    入院指示せんオーダ作成サブ
           COPY    "CPORCSORDERPRT.INC".
      *    入院処方箋発行サブ
           COPY    "CPORCHC501.INC".
      *    注射処方箋発行サブ
           COPY    "CPORCHC502.INC".
      *    入院指示箋発行サブ
           COPY    "CPORCHC503.INC".
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    ユーザプログラム起動サブパラメタ
           COPY    "CPORCSUSERCHK.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
      *
      *    ＳＰＡパラメタ（ORCSC97.CBL 用のDAMMY)
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *
      *    プレビュー用情報
           COPY    "CPXC01LNK.INC".
      *
       01  WRK-PREV-AREA.
           03  WRK-CONS-PRTKANRI-TBL-KEY
                                   PIC X(08)   VALUE   "RECEPTX".
      *
      *    for Infection
           COPY    "CPSHELLTBL.INC".
      *
      *H27.7
      *    外来収納負担金計算サブ
           COPY    "CPORCSCSYUNOUG.INC".
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
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  STS-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  SPA-K08
           INITIALIZE                  WRK-SUM-AREA
      *
           MOVE    SPA-COMMON      TO  SPA-AREA
           MOVE    SPA-FREE        TO  SPA-K08
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
      *    診療種別テーブルセット
           PERFORM 1001-SRYKBN-SET-SEC
      *
           MOVE    SPACE           TO  SPA-ERRCD
           MOVE    ZERO            TO  FLG-END
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
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-K01KYOUTU   TO  SPA-WORK
           MOVE    SPA-AREA        TO  SPA-COMMON
           MOVE    SPA-K08         TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療種別テーブルセット処理
      *****************************************************************
       1001-SRYKBN-SET-SEC             SECTION.
      *
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
           .
      *
       1001-SRYKBN-SET-EXT.
           EXIT.
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
               IF     (SPA-ERRMSG2         =   "K001")  AND
                      (SPA-KEI-FLG         =   1     )
                   MOVE    SPACE               TO  SPA-ERRMSG2
                   MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
                   PERFORM 45021-END-SYORI-SEC
               ELSE
                   MOVE    SPACE               TO  SPA-ERRMSG2
                   MOVE    SPACE               TO  SPA-MOTOPG
                   PERFORM 5001-MAPCUR-SEC
               END-IF
           ELSE
      *        初期画面編集
               PERFORM 300-SCREEN-SEC
      *        画面編集
               IF      FLG-END             =   ZERO
                   PERFORM 500-GMNHEN-SEC
               END-IF
           END-IF
      *
           IF      FLG-END             =   ZERO
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "K08"                TO  MCP-WINDOW
      *
               PERFORM 900-PUT-WINDOW
      *
               MOVE    1                   TO  FLG-END
           END-IF
           .
      *
       100-INIT-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       300-SCREEN-SEC             SECTION.
      *
      *    他画面より
           IF      LINKAREA        NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-KYOUTU
           END-IF
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KID1"
      *            確認画面より
                   PERFORM 30010-KID1-SET-SEC
               WHEN    "K081"
      *            指示せん発行画面
                   PERFORM 3014-K081-SET-SEC
               WHEN    "XC01"
      *            プレビューから
                   PERFORM 3015-XC01-SET-SEC
      *
               WHEN    OTHER
                   PERFORM 3000-INIT-SCREEN-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    プレビューから処理
      *****************************************************************
       3015-XC01-SET-SEC          SECTION.
      *
           MOVE    LNK-COMMON      TO  SPA-KYOUTU
           MOVE    LNK-SCRSPA      TO  SPA-K08
           MOVE    SPA-WORK        TO  SPA-K01KYOUTU
      *
           MOVE    SPACE           TO  LINKAREA
      *
           .
       3015-XC01-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面処理
      *****************************************************************
       3000-INIT-SCREEN-SEC          SECTION.
      *!
           CALL    "ORCSSPACHK"        USING    SPA-AREA
      *!
      *
           MOVE    SPACE               TO  SPA-KIDCD
           MOVE    SPACE               TO  SPA-ERRMSG2
      *
           INITIALIZE                  SPA-K08
           INITIALIZE                  SPA-K08-AREA
      *H24.4
      *    レセプトプレビュー区分初期処理
           PERFORM 3100-RESPRV-SET-SEC
      *
           PERFORM                     310-SPA-SET-SEC
      *
      *    削除区分設置
           PERFORM 41031-DELFLG-SET-SEC
      *
      *
           IF      FLG-DEL-ARI         =   ZERO
               MOVE    11              TO  SPA-GMN-CUR
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           .
       3000-INIT-SCREEN-EXT.
           EXIT.
      *H24.4
      *
      *****************************************************************
      *    確認画面（ＫＩＤ１）ＯＫ処理
      *****************************************************************
       30010-KID1-SET-SEC          SECTION.
      *
           EVALUATE    SPA-KIDCD
      *        入院削除確認
               WHEN    "0101"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-KAKU-FLG
                       PERFORM 4502-NYUKOUSIN-SYORI-SEC
                   END-IF
      *        包括まとめ確認
               WHEN    "0102"
               WHEN    "0103"
                   IF      SPA-KID1-FLG        =   "OK"
                       MOVE    1               TO  FLG-KAKU-FLG
                       PERFORM 4503-HOUKATU-SYORI-SEC
                   END-IF
      *        第三者行為確認
               WHEN    "1001"
      *            プレビュー処理
                   PERFORM 4130-PREVIEW-SEC
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KIDCD
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
      *
           .
       30010-KID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    指示せん発行画面（Ｋ０８１）より処理
      *****************************************************************
       3014-K081-SET-SEC                  SECTION.
      *
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           EVALUATE    SPA-K081-OKFLG
               WHEN    ZERO
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    1
      *        登録・発行
      *            発行有無判定
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
                       MOVE    1               TO  FLG-SIJISEN
                   ELSE
                       MOVE    ZERO            TO  FLG-SIJISEN
                   END-IF
      *            入院更新処理
                   MOVE    1               TO  FLG-KAKU-FLG
                   PERFORM 4502-NYUKOUSIN-SYORI-SEC
                   IF      SPA-ERRCD       NOT =   SPACE
                       PERFORM 510-ERRSET-SEC
                   END-IF
               WHEN    2
      *        発行のみ
                   IF      (SPA-K081GMN-SYOHOU     =   "1" )  OR
                           (SPA-K081GMN-CHUSYA     =   "1" )  OR
                           (SPA-K081GMN-SIJISEN    =   "1" )
      *                入院指示せん発行処理
                       PERFORM 45900-SIJISEN-SYORI-SEC
                   END-IF
                   MOVE    1               TO  SPA-GMN-CUR
           END-EVALUATE
      *
           .
       3014-K081-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       310-SPA-SET-SEC                  SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT
      *
      *    剤登録順
           PERFORM 3100-ZAIJUN-SET-SEC
      *    未収金
           MOVE    SPA-MISYUMONEY      TO  SPA-GMN-MISYUMONEY
      *    最終来院日
           IF      SPA-LSTYMD      NOT =   SPACE
               MOVE    SPA-LSTYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-LASTYMD
           END-IF
      *    入院の時、表示なしとする
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPACE           TO  SPA-GMN-LASTYMD
           END-IF
      *
      *    初診算定日
           IF     (SPA-SYOYMD      NOT =   SPACE )  AND
                  (SPA-SYOSIN          =   ZERO  )
               MOVE    SPA-SYOYMD      TO  WRK-SYMD
               PERFORM 520-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG     TO  SPA-GMN-SYOSINYMD
           END-IF
      *
           MOVE    ZERO                TO  SPA-GMN-GOKEI
      *!
      *    保険組合せ検索
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
      *!
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  IDX-NAI
           MOVE    ZERO                TO  WRK-BANGO
           MOVE    ZERO                TO  IDX-KOUI
      *
      *    パラメタファイルより、診療会計マスターを編集
      *    パラメタファイルよりワーク編集とＳＯＲＴ
           PERFORM 3101-WKSRYACT-SORT-SEC
      *
      *    最大件数
           IF      IDG                 >   MAX-GMN
               MOVE    MAX-GMN             TO  SPA-K08-MAX
           ELSE
               MOVE    IDG                 TO  SPA-K08-MAX
           END-IF
      *
      *H24.4
      *    院外処方名称設定
           PERFORM 4051-MEISYO-HEN-SEC
      *
      *    各点数
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC (IDX2)   TO  SYUNOU-REC
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   16
                   ADD     SYU-HKNTEN (IDX)    TO  SPA-GMN-HKNTEN (IDX)
               END-PERFORM
           END-PERFORM
      *H27.7
      *****合計点数集計
      *    MOVE    ZERO                TO  SPA-GMN-GOKEI
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   16
      *        ADD     SPA-GMN-HKNTEN (IDX)    TO  SPA-GMN-GOKEI
      ***  END-PERFORM
      *!!
      *    点数再計算
      **** IF     (SPA-SYORIFLG    NOT =   1   )  AND
           IF     (SPA-NYUGAIKBN       =   "1" )
               PERFORM 41032-TENSU-KEI-SEC
           END-IF
      *
      *    調剤料のメッセージ
           MOVE    SPACE               TO  SPA-GMN-MSGDATA
           IF      SPA-SYOHO-KBN   NOT =   ZERO
      *        保険空白日本語変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    SPA-SYOHO-HKNMEI    TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  SPA-SYOHO-HKNMEI
      *
               STRING    SPA-SYOHO-MEI     DELIMITED   BY  SPACE
                         "は他保険で算定済。"
                                           DELIMITED   BY  SIZE
                         "※"              DELIMITED   BY  SIZE
                         SPA-SYOHO-HKNMEI  DELIMITED   BY  SIZE
                                       INTO      SPA-GMN-MSGDATA
               END-STRING
           END-IF
      *
           IF      SPA-NYUGAIKBN       =   "1"
      *        ユーザプログラム起動初期
      *v4.8
      *    ginbee対応
      *        IF      SPA-MW              =   SPA-GINBEE
      *            MOVE    ZERO                TO  SPA-USERPG-MAX
      **       ELSE
                   PERFORM 3101-USERPG-SET-SEC
      ***      END-IF
           ELSE
               MOVE    ZERO                TO  SPA-USERPG-MAX
           END-IF
      **** レセプトプレビュー区分初期処理
      **** PERFORM 3100-RESPRV-SET-SEC
      *
            .
       310-SPA-SET-EXT.
           EXIT.
      *****************************************************************
      *     ユーザプログラム起動初期処理
      *****************************************************************
       3101-USERPG-SET-SEC              SECTION.
      *
           INITIALIZE                      ORCSUSERCHKAREA
           MOVE    "K08"               TO  ORCSUSERCHK-GMNPG
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
            .
       3101-USERPG-SET-EXT.
           EXIT.
      *****************************************************************
      *     剤登録順処理
      *****************************************************************
       3100-ZAIJUN-SET-SEC              SECTION.
      *
      *    システム管理検索（チェック機能制御情報）
           MOVE    SPACE               TO  SYS-1008-REC
           INITIALIZE                  SYS-1008-REC
           MOVE    "1008"              TO  SYS-1008-KANRICD
           MOVE    "*"                 TO  SYS-1008-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1008-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1008-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1008-HOSPNUM
           MOVE    SYS-1008-REC        TO  MCPDATA-REC
      *
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1008-REC
               INITIALIZE                  SYS-1008-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1008-REC
               IF      SYS-1008-ZAIJUNKBN-FLG  =   "0"  OR  "1"
                   MOVE    SYS-1008-ZAIJUNKBN-FLG
                                           TO  SPA-ZAIJUNKBN-FLG
               ELSE
                   MOVE    ZERO            TO  SPA-ZAIJUNKBN-FLG
               END-IF
           ELSE
               INITIALIZE                  SYS-1008-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
            .
       3100-ZAIJUN-SET-EXT.
           EXIT.
      *****************************************************************
      *     レセプトプレビュー区分初期処理
      *****************************************************************
       3100-RESPRV-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC        TO  SYSKANRI-REC
      *
           PERFORM 900-SYSYKANRI-KEY-READ-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
           ELSE
               INITIALIZE                  SYS-1038-REC
           END-IF
      *
           IF      SYS-1038-RESPRVFLG  =   "1"  OR  "2"
               MOVE    SYS-1038-RESPRVFLG  TO  SPA-NAI-RESPRVFLG
           ELSE
               MOVE    "1"                 TO  SPA-NAI-RESPRVFLG
           END-IF
      *    一般名記載初期処理
           PERFORM 30001-GENERIC-SET-SEC
      *
            .
       3100-RESPRV-SET-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *    一般名記載初期処理
      *****************************************************************
       30001-GENERIC-SET-SEC          SECTION.
      *
      *    一般名表示
           MOVE    ZERO                TO  SPA-NAI-GENEINIT
           IF      SYS-1038-GENERICFLG     =   "1"
      *        銘柄名表示
               MOVE    1                   TO  SPA-NAI-GENEINIT
           END-IF
      *
           .
       30001-GENERIC-SET-EXT.
           EXIT.
      *****************************************************************
      *     パラメタファイルよりワーク編集とＳＯＲＴ処理
      *****************************************************************
       3101-WKSRYACT-SORT-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-EDANUM
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  IDX-KOUI
      *
      *    ワーク診療行為・収納テーブル編集
           PERFORM 3101-PARA-WKSRYACT-HEN-SEC
           PERFORM 3101-PARA-SYUNOU-HEN-SEC
      *
           PERFORM 2101-PARA02-DELETE-SEC
      *
      *    入院の時、入院指示せんチェク
           IF      SPA-NYUGAIKBN       =   "1"
               PERFORM 30001-SIJISEN-CHK-SEC
           ELSE
               INITIALIZE                  SPA-K081
               MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
           END-IF
      *
           MOVE    ZERO                TO  IDX4
           MOVE    SPACE               TO  SRT-WKSRYACT-AREA
           INITIALIZE                  SRT-WKSRYACT-AREA
      *
      *    保険毎にＳＯＲＴする
           MOVE    SPACE               TO  HZN-WKSRYACT-AREA
           INITIALIZE                  HZN-WKSRYACT-AREA
           MOVE    LNK-WKSRYACT-AREA   TO  HZN-WKSRYACT-AREA
           IF      SPA-TBL-HKNCOMBI(1) =   SPACE
               MOVE    SPA-HKNCOMBINUM     TO  SPA-TBL-HKNCOMBI(1)
               MOVE    SPA-HKNCOMBIMEI-60  TO  SPA-TBL-HKNCOMBIMEI(1)
               MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA (1 1)
               MOVE    SPA-DRCD            TO  SPA-TBL-DRCD  (1 1)
           END-IF
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  WRK-HKNCOMBI
               MOVE    SPA-TBL-HKNCOMBIMEI(IDH1)  TO  WRK-HKNCOMBIMEI
               IF      SPA-TBL-SRYKA (IDH1 1)     =   SPACE
                   MOVE    SPA-SRYKA           TO  SPA-TBL-SRYKA(IDH1 1)
                   MOVE    SPA-DRCD            TO  SPA-TBL-DRCD (IDH1 1)
               END-IF
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)   TO  WRK-SRYKA
                   MOVE    SPA-TBL-DRCD  (IDH1 IDH2)   TO  WRK-DRCD
                   PERFORM 31012-WKSRYACT-SORT2-SEC
                   PERFORM 31013-WKSRYACT-HENSYORI-SEC
               END-PERFORM
           END-PERFORM
      *
            .
       3101-WKSRYACT-SORT-EXT.
           EXIT.
      *****************************************************************
      *     ワーク診療行為テーブル編集処理
      *****************************************************************
       3101-PARA-WKSRYACT-HEN-SEC      SECTION.
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  PARA-REC
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
               MOVE    1                   TO  FLG-PARA
           END-IF
           PERFORM
               UNTIL  (FLG-PARA            =   1     )
                  OR  (IDX                 >=  600   )
               ADD     1                   TO  IDX
               MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
               IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
                   MOVE    SPA-HKNCOMBINUM     TO
                                               LNK-WKSRY-HKNCOMBI(IDX)
               END-IF
               IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
                   MOVE    SPA-SRYKA       TO
                                              LNK-WKSRY-SRYKA   (IDX)
               END-IF
               MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  WRK-MAX
            .
       3101-PARA-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *     ワーク診療行為・収納テーブル編集処理
      *****************************************************************
       3101-PARA-SYUNOU-HEN-SEC        SECTION.
      *
      *
           MOVE    SPACE               TO  LNK-SYUNOU-AREA
           INITIALIZE                  LNK-SYUNOU-AREA
           MOVE    ZERO                TO  LNK-SYUNOU-MAX
           MOVE    SPACE               TO  SYUNOU-REC
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
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
      *        収納
               ADD     1                   TO  LNK-SYUNOU-MAX
               IF      LNK-SYUNOU-MAX      <=  15
                   MOVE    PARA-DATA-REC       TO  LNK-SYUNOU-REC
                                                       (LNK-SYUNOU-MAX)
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
            .
       3101-PARA-SYUNOU-HEN-EXT.
           EXIT.
      *****************************************************************
      *     診療行為編集処理
      *****************************************************************
       31012-WKSRYACT-SORT2-SEC              SECTION.
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-MAX
           MOVE    ZERO                TO  FLG-FUKU
      *H27.1
           MOVE    ZERO                TO  FLG-INGIKSN-KBN
      *
           PERFORM VARYING   IDX1      FROM   1   BY  1
                   UNTIL    (IDX1      >   WRK-MAX    )
               IF     (HZN-WKSRY-HKNCOMBI(IDX1)   =   WRK-HKNCOMBI) AND
                      (HZN-WKSRY-SRYKA   (IDX1)   =   WRK-SRYKA   )
                   ADD     1                  TO  IDX-MAX
                   MOVE    HZN-WKSRYACT-REC(IDX1)
                                              TO  LNK-WKSRYACT-REC
                                                               (IDX-MAX)
      *H27.1
      *            一般・銘柄名処方毎
                   IF      HZN-WKSRY-SRYCD(IDX1 1)    =    "099209910"
      *                後発品変更不可
                       MOVE    1                   TO  FLG-INGIKSN-KBN
                   END-IF
                   IF      HZN-WKSRY-SRYCD(IDX1 1)    =    "099209911"
      *                後発品変更可
                       MOVE    2                   TO  FLG-INGIKSN-KBN
                   END-IF
      *!!
                   IF      HZN-WKSRY-INPUTCD(IDX1 1)(1:1)  =   "#"
                       MOVE    1              TO  FLG-FUKU
                   END-IF
      *            複数保険
                   MOVE    HZN-WKSRY-SRYKBN (IDX1)
                                              TO  LNK-SORT-SRYKBN
                                                              (IDX-MAX)
                   IF     (HZN-WKSRY-SRYKBN (IDX1) =   "99" ) AND
                          (HZN-WKSRY-SRYCD (IDX1 1)(1:3)
                                                   =   "099" ) AND
                          (FLG-FUKU                =   1     )
                       MOVE    HZN-WKSRY-SRYKBN (IDX1 - 1)
                                                  TO  LNK-SORT-SRYKBN
                                                              (IDX-MAX)
                       IF      HZN-WKSRY-SRYKBN (IDX1 - 1)  =   SPACE
                           MOVE    "12"           TO  LNK-SORT-SRYKBN
                                                              (IDX-MAX)
                       END-IF
                       MOVE    ZERO           TO  FLG-FUKU
                   END-IF
                   MOVE    SPACE              TO  HZN-WKSRYACT-REC
                                                               (IDX1)
                   INITIALIZE                 HZN-WKSRYACT-REC(IDX1)
               END-IF
           END-PERFORM
      *
      *    ＳＯＲＴ（診療区分別にＳＯＲＴする）
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  IDX2
           MOVE    ZERO                TO  IDX4
           MOVE    SPACE               TO  SRT-WKSRYACT-AREA
           INITIALIZE                  SRT-WKSRYACT-AREA
      *
           PERFORM VARYING   IDX       FROM   1   BY  1
                   UNTIL     IDX       >   IDX-MAX
               MOVE    ALL "9"             TO  WRK-ZAINUM-X
               MOVE    ALL "9"             TO  WRK-RENNUM-X
               MOVE    ALL "9"             TO  WRK-SRYKBN
               MOVE    ZERO                TO  IDX2
               PERFORM VARYING   IDX1      FROM   1   BY  1
                       UNTIL     IDX1      >   IDX-MAX
                   IF   LNK-WKSRY-INPUTCD(IDX1 1)(1:1)  =   "#"
                                                        OR  "$"
                       MOVE    IDX1                TO  IDX2
                       MOVE    IDX-MAX             TO  IDX1
                   ELSE
                       IF      LNK-SORT-SRYKBN (IDX1) NOT =   SPACE
                           MOVE   ZERO                 TO  FLG-CHK
                           IF     LNK-SORT-SRYKBN  (IDX1)
                                                       <   WRK-SRYKBN
                               MOVE    1                   TO  FLG-CHK
                           END-IF
                           IF      LNK-SORT-SRYKBN (IDX1)
                                                   =   WRK-SRYKBN
                               IF      LNK-WKSRY-ZAINUM (IDX1)
                                                   <    WRK-ZAINUM
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                               IF     (LNK-WKSRY-ZAINUM (IDX1)
                                                   =   WRK-ZAINUM) AND
                                      (LNK-WKSRY-RENNUM (IDX1)
                                                   <=   WRK-RENNUM)
                                   MOVE    1                TO  FLG-CHK
                               END-IF
                           END-IF
                           IF      FLG-CHK         =   1
                               MOVE    LNK-SORT-SRYKBN (IDX1)
                                                   TO  WRK-SRYKBN
                               MOVE    LNK-WKSRY-ZAINUM (IDX1)
                                                   TO  WRK-ZAINUM
                               MOVE    LNK-WKSRY-RENNUM (IDX1)
                                                   TO  WRK-RENNUM
                               MOVE    IDX1            TO  IDX2
                           END-IF
                       END-IF
                   END-IF
               END-PERFORM
               IF     IDX2                 >   ZERO
                   ADD     1              TO  IDX4
                   MOVE    LNK-WKSRYACT-REC(IDX2)
                                          TO  SRT-WKSRYACT-REC (IDX4)
                   MOVE    SPACE          TO  LNK-WKSRYACT-REC(IDX2)
                   INITIALIZE                 LNK-WKSRYACT-REC(IDX2)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
      *
            .
       31012-WKSRYACT-SORT2-EXT.
           EXIT.
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       31013-WKSRYACT-HENSYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                      LNK-WKSRYACT-AREA
      *!
      *!   IF     (IDH1                >   1  )  AND
           IF     (IDH2                =   1  )
             AND  (SPA-TBL-HKNCOMBI(2) NOT =   SPACE )
               PERFORM 31012-HKNCOMBI-HEN-SEC
           END-IF
      *!   IF     (IDH1                =   1  )  AND
      *           (IDH2                =   1  )
      *!       CONTINUE
      *!   ELSE
           IF     (SPA-TBL-HKNCOMBI(2) NOT =   SPACE )  OR
                  (SPA-TBL-SRYKA   (IDH1 2)    NOT =   SPACE )
               PERFORM 31013-SRYKA-HEN-SEC
           END-IF
      *
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   IDX-MAX
      *
               IF     (IDX-KOUI        >   ZERO  )  AND
                      (SRT-WKSRY-ZAINUM (IDX2) NOT =
                                           LNK-WKSRY-ZAINUM (IDX-KOUI))
      *            剤毎に処理を行う
                   PERFORM 3101-WKSRYACT-HEN-SEC
                   MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                   INITIALIZE                  LNK-WKSRYACT-AREA
                   MOVE    ZERO            TO  IDX-KOUI
               END-IF
               ADD     1               TO  IDX-KOUI
               MOVE    SRT-WKSRYACT-REC (IDX2)
                                           TO  LNK-WKSRYACT-REC
                                                            (IDX-KOUI)
      *        画面表示順に登録する
               MOVE    SRT-WKSRYACT-REC (IDX2) TO  WRK-WKSRYACT-REC
      *
               ADD     1                   TO  WRK-EDANUM
      *
               MOVE    SPACE               TO  PARA-REC
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K08"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "WKSRYACT"          TO  PARA-FILEMEI
               MOVE    WRK-EDANUM          TO  PARA-EDANUM
               MOVE    WRK-WKSRYACT-REC    TO  PARA-DATA-REC
      *
               PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
      *    剤毎に処理を行う
           IF      IDX-KOUI        >   ZERO
               PERFORM 3101-WKSRYACT-HEN-SEC
           END-IF
      *
            .
       31013-WKSRYACT-HENSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    パラメタＤＢ出力処理
      *****************************************************************
       30010-PARA-DBINSERT-SEC          SECTION.
      *
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0051"              TO  SPA-ERRCD
               DISPLAY "K06 PARA INSERT ERR:"  MCP-RC
                       ",KEY:" PARA-KEY
           END-IF
           .
       30010-PARA-DBINSERT-EXT.
           EXIT.
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       3101-WKSRYACT-HEN-SEC              SECTION.
      *
      *!
           IF      LNK-WKSRY-INPUTCD(1 1)(1:1)     =   "#"
                                                   OR  "$"
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
      *!
      *    診療コード計計算
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  FLG-YAKUSOKU
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-JIHIMONEY
           MOVE    ZERO                TO  WRK-JIHIMONEY-KEI
      *H29.2
           MOVE    ZERO                TO  FLG-KAISUU
           MOVE    ZERO                TO  FLG-KAISUU-ARI
           MOVE    ZERO                TO  FLG-KAISUU-ARI2
           MOVE    ZERO                TO  FLG-KAISUU-ARI3
      *
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
      *
           MOVE    ZERO                TO  FLG-RSIEN
      *
      *    入院で日付指定の有無
           PERFORM VARYING     IDZ     FROM    1   BY  1
                   UNTIL      (IDZ         >   5   )  OR
                             ((LNK-WKSRY-SRYCD (IDX-KOUI IDZ)
                                                  =   SPACE ) AND
                              (LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)
                                                  =   SPACE ))
      *        回数入力の有無
               IF      LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)(1:1) =   "*"
                   MOVE    1                   TO  FLG-KAISUU-ARI2
                   PERFORM VARYING     IDX3    FROM   2   BY  1
                           UNTIL       IDX3    >   54
                       IF       LNK-WKSRY-INPUTCD(IDX-KOUI IDZ)
                                                   (IDX3:1) =   "/"
                           MOVE    1               TO  FLG-KAISUU-ARI3
                           MOVE    54              TO  IDX3
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    包括剤の判定
           MOVE    SPACE               TO  WRK-HKTZAI
           IF     (SPA-HKNCOMBINUM     =   "9999" )  OR
                  (WRK-HKNCOMBI        =   "9999" )
               MOVE    "2"                 TO  WRK-HKTZAI
           ELSE
               IF       (LNK-WKSRY-CONTKBN (1)   =   "H" )
                   MOVE    "1"                 TO  WRK-HKTZAI
               END-IF
           END-IF
      *
      *H28.9  持参薬
           IF     (LNK-WKSRY-ZAIKBN  (1)   =   3
                                           OR  4  )
               MOVE    "1"                 TO  WRK-JISANKBN
           ELSE
               MOVE    SPACE               TO  WRK-JISANKBN
           END-IF
      *
      *    剤別編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                           OR (IDG     >   MAX-GMN  )
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      *        自費
               MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                                  TO  WRK-JIHIMONEY
               ADD     LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                                  TO  WRK-JIHIMONEY-KEI
      *H27.7
      *        収納点数計算用
               IF      LNK-WKSRYP-RSIKINGAKU(IDX) NOT =   ZERO
      *            労災の円あり
                   MOVE    1                   TO  FLG-RSIEN
               END-IF
               IF     (FLG-RSIEN               =   1   )
      *            付加情報は剤１件目に記載
                   IF      IDX                 =   1
      *                労災の円と点数混在の点数
                       MOVE    LNK-WKSRYP-RSI-TENSU(IDX)
                                                       TO  WRK-S-ZAITEN
                   END-IF
               ELSE
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-S-ZAITEN
               END-IF
      *!!!
      *
      *        剤内１行目
               IF      IDX                 =   1
                   MOVE    ZERO                TO  FLG-RSIFLG
                   ADD     1                   TO  IDG
                   IF      IDG                 >   MAX-GMN
                       MOVE    "1000"              TO  SPA-ERRCD
                   ELSE
                       PERFORM 31011-GYO1-HEN-SEC
                   END-IF
              END-IF
      *
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >   MAX-GMN )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ)
                                                  =   SPACE  AND
                                   LNK-WKSRY-INPUTCD(IDX IDZ)
                                                  =   SPACE )
      *          回数入力の有無
                 PERFORM 310100-KAISUU-CHK-SEC
                 IF      LNK-WKSRY-INPUTCD(IDX IDZ)(1:1) =   "*"
                     CONTINUE
                 ELSE
      *
                   ADD     1                   TO  WRK-MEISAISU
      *            自費の時、金額を表示
                   IF      LNK-WKSRY-SRYKBN (IDX)  =   "95" OR "96"
                       PERFORM 31013-JIHIMEI-HEN-SEC
                   ELSE
                       PERFORM 31012-ZAIMEI-HEN-SEC
                   END-IF
      *            約束セットの時
                   IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
                       MOVE    1                  TO  FLG-YAKUSOKU
                   END-IF
      *            削除できるかの判定
                   IF      LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "3"
                                                         OR  "4"
                                                         OR  "6"
                                                         OR  "7"
                       MOVE    1                   TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
      *            削除できるかの判定（減）対象外
                   IF     (LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "3" ) AND
                          (LNK-WKSRY-INPUTCD (IDX IDZ)
                                                   =   "820000047"
      *H26.10
                                                   OR  "820000166"
                                                   OR  CONS-SPCM-SRYCD)
                       MOVE    ZERO                TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
                   IF     (LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "3" ) AND
                          (LNK-WKSRYP-PLUSKBN (IDX)      =   1   )
                       MOVE    ZERO                TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
      *            訂正で自動発生なしの時、削除なし
                   IF     (LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "4"
                                                         OR  "6" ) AND
                          (SPA-SYORIFLG        =   1     )  AND
                          (SPA-TEISEI-FLG      =   "0"   )
                       MOVE    ZERO                TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
      *            入院初回加算の訂正は削除なし
                   IF     (LNK-WKSRY-AUTOKBN (IDX IDZ)   =   "7" ) AND
                          (SPA-SYORIFLG        =   1     )
                       MOVE    ZERO                TO  SPA-NAI-JIDFLG
                                                              (IDX-NAI)
                   END-IF
                 END-IF
               END-PERFORM
           END-PERFORM
      *
      *!
      *    包括検査数
           IF      WRK-HOUKNS-CNT      >   1
               PERFORM 3009-INPUT-MEISYO-SEC
           END-IF
      *!
      *    合計点数集計
           IF      WRK-SRYKAISUKEI     =   ZERO
               MOVE   1                    TO  WRK-SRYKAISUKEI
           END-IF
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-SRYKAISUKEI
      *    包括分
           IF      WRK-HKTZAI      NOT =   SPACE
               MOVE    ZERO            TO  WRK-KEI-G
           END-IF
      *H27.7 (労災点数のみ）
           COMPUTE WRK-S-KEI-G         =   WRK-S-ZAITEN
                                       *   WRK-SRYKAISUKEI
           IF      WRK-HKTZAI      NOT =   SPACE
               MOVE    ZERO            TO  WRK-S-KEI-G
           END-IF
           MOVE    WRK-S-KEI-G         TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *??? MOVE    WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *    労災金額の時、ゼロ（指導料、その他のみ）
           IF      LNK-WKSRY-SRYKBN (01)   =   "13"  OR  "80"
               IF      FLG-RSIFLG          =   1
                   MOVE    ZERO            TO  SPA-NAI-HKNTEN
                                                             (IDX-NAI)
               END-IF
           END-IF
      *
      *    剤点数がゼロの時、自費を点数へ
      *    IF      WRK-ZAITEN          =   ZERO
      *        MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
      *    END-IF
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               COMPUTE IDZ             =   IDZ     -   1
               GO      TO      3101-WKSRYACT-HEN-EXT
           END-IF
      *
      *    自費の時、計を表示しない
           IF    ((WRK-JIHIMONEY       >   ZERO  )   AND
      *        自賠責金額入力以外
                 (LNK-WKSRY-SRYKBN(01)(1:1)    =   "9" ))
      *H29.2  自費金額ゼロで回数表示済み
           OR   ((LNK-WKSRY-SRYKBN(01)     =   "95"
                                           OR  "96") AND
                 (FLG-KAISUU               =   1   ))
      *        回数入力がなく、入院回数入力がある時、入院回数のみ
           OR    ((FLG-KAISUU-ARI      =   ZERO )  AND
                  (FLG-KAISUU-ARI2     =   1    ))
               MOVE    ZERO                TO  SPA-NAI-HKNTEN  (IDX-NAI)
               CONTINUE
           ELSE
      *        剤　点数＊日数　計　編集 （最終行に追加）
               IF      SPA-GMN-HEN1 (IDG)      =   SPACE  OR  ALL "　"
                   CONTINUE
               ELSE
                   ADD    1                    TO  IDG
               END-IF
               IF      IDG                 <=  MAX-GMN
      *
               PERFORM 31013-INPUTCD-HEN-SEC
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI (IDG)
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI   (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI2  (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
               END-IF
      *
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    入院で訂正以外の時、日付により回数を変更する
           IF      FLG-KAISUU-ARI2     =   1
               PERFORM 31019-NYUIN-SYORI-SEC
           END-IF
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               CONTINUE
           ELSE
               MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
            .
       3101-WKSRYACT-HEN-EXT.
           EXIT.
      *!
      *****************************************************************
      *     包括検査数再編集処理
      *****************************************************************
       3009-INPUT-MEISYO-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "2"                 TO  ORCSMEIHEN-KBN
           MOVE    WRK-HOUKNS-CNT      TO  ORCSMEIHEN-SURYO
           MOVE    WRK-HOUKNS-NAME     TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           IF      WRK-HOUKNS-NAME(45:)    =   SPACE
               MOVE    ORCSMEIHEN-MEISYO   TO  SPA-GMN-TMEISYO2
                                                  (WRK-HOUKNS-IDX)
           ELSE
               IF      IDG             =   WRK-HOUKNS-IDX
                   ADD     1               TO  IDG
                   MOVE    IDG             TO  WRK-HOUKNS-IDX
               ELSE
                   MOVE    ZERO                TO  IDG2
                   PERFORM VARYING     IDG3    FROM    IDG   BY  -1
                           UNTIL       IDG3    <=  WRK-HOUKNS-IDX
                       MOVE    SPA-GMN-TBLREC-G (IDG3)
                                               TO  SPA-GMN-TBLREC-G
                                                           (IDG3 + 1)
                   END-PERFORM
                   COMPUTE WRK-HOUKNS-IDX  =   WRK-HOUKNS-IDX  +  1
                   INITIALIZE                  SPA-GMN-TBLREC-G
                                                        (WRK-HOUKNS-IDX)
                   ADD     1               TO  IDG
               END-IF
               IF      WRK-HOUKNS-IDX      <=   MAX-GMN
                   MOVE    ORCSMEIHEN-MEISYO2
                                       TO  SPA-GMN-TMEISYO2
                                                  (WRK-HOUKNS-IDX)
                                                  (45:)
                   INSPECT SPA-GMN-TBANGO2 (WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
                   INSPECT SPA-GMN-TMEISYO2(WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
                   INSPECT SPA-GMN-TYOBI   (WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
                   INSPECT SPA-GMN-TZAIKEI (WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
                   INSPECT SPA-GMN-TYOBI2  (WRK-HOUKNS-IDX)   REPLACING
                                               ALL "  "  BY  "　"
               END-IF
           END-IF
      *
           MOVE    ZERO                TO  WRK-HOUKNS-CNT
           MOVE    ZERO                TO  WRK-HOUKNS-IDX
           MOVE    SPACE               TO  WRK-HOUKNS-NAME
           .
       3009-INPUT-MEISYO-EXT.
           EXIT.
      *!
      *****************************************************************
      *   保険組合せ編集処理
      *****************************************************************
       31012-HKNCOMBI-HEN-SEC          SECTION.
      *
      *    剤編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               GO      TO      31012-HKNCOMBI-HEN-EXT
           END-IF
           MOVE    ALL "#"             TO  SPA-GMN-TMEISYO (IDG)
           STRING  "《"                DELIMITED  BY  SIZE
                   WRK-HKNCOMBIMEI     DELIMITED  BY  "  "
                   "》"                DELIMITED  BY  SIZE
                  INTO  SPA-GMN-TMEISYO3(IDG)
           END-STRING
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               CONTINUE
           ELSE
               MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
            .
       31012-HKNCOMBI-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療科編集処理
      *****************************************************************
       31013-SRYKA-HEN-SEC          SECTION.
      *
           MOVE    SPACE               TO  WRK-SRYKAMEI
           PERFORM VARYING     IDK     FROM   1   BY  1
                   UNTIL       IDK     >   SPA-SRYKA-MAX
               IF     SPA-GMN-SRYKAL (IDK)
                                       =   WRK-SRYKA
                   MOVE    SPA-GMN-SRYKAMEIL (IDK)
                                               TO  WRK-SRYKAMEI
                   MOVE    SPA-SRYKA-MAX       TO  IDH
               END-IF
           END-PERFORM
           INITIALIZE                  SYS-1010-REC
           MOVE    "1010"              TO  SYS-1010-KANRICD
           MOVE    WRK-DRCD            TO  SYS-1010-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1010-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1010-EDYUKYMD
      *    システム管理検索
           MOVE    SPA-HOSPNUM         TO  SYS-1010-HOSPNUM
           MOVE    SYS-1010-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               INITIALIZE                  SYS-1010-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-1010-REC
               MOVE    SYS-1010-NAME       TO  WRK-DRCD-MEI
           ELSE
               MOVE    SPACE               TO  WRK-DRCD-MEI
           END-IF
      *
      *    剤編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               GO      TO      31013-SRYKA-HEN-EXT
           END-IF
           MOVE    ALL "="             TO  SPA-GMN-TMEISYO (IDG)
           STRING  "＜＜"              DELIMITED  BY  SIZE
                   WRK-SRYKAMEI        DELIMITED  BY  SPACE
                   "　ドクター："      DELIMITED  BY  SIZE
                   WRK-DRCD-MEI        DELIMITED  BY  SPACE
                    "＞＞"             DELIMITED  BY  SIZE
               INTO                    SPA-GMN-TMEISYO3 (IDG)
           END-STRING
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               GO      TO      31013-SRYKA-HEN-EXT
           END-IF
           MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
      *
            .
       31013-SRYKA-HEN-EXT.
           EXIT.
      *!
      *****************************************************************
      *   入院時回数編集処理
      *****************************************************************
       310100-KAISUU-CHK-SEC                  SECTION.
      *
           PERFORM VARYING     IDX3    FROM   1   BY  1
                   UNTIL       IDX3    >   54
               IF       LNK-WKSRY-INPUTCD(IDX IDZ)(IDX3:1) =   "*" 
                   IF       IDX3                >   1
                       MOVE    1               TO  FLG-KAISUU-ARI
                   ELSE
                       MOVE    1               TO  FLG-KAISUU-ARI2
                   END-IF
               END-IF
           END-PERFORM
            .
       310100-KAISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院時回数編集処理
      *****************************************************************
       31019-NYUIN-SYORI-SEC                  SECTION.
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL       IDZ         >   5
                   IF      LNK-WKSRY-INPUTCD (IDX IDZ)(1:1) =   "*"
                       PERFORM 310191-DAYSET-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
            .
       31019-NYUIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院編集処理
      *****************************************************************
       310191-DAYSET-SEC                  SECTION.
      *
           INITIALIZE                  ORCSC97AREA
           MOVE    SPACE               TO  SPA-SCR-REC
      *    ＰＧ名
           MOVE    "K02N"              TO  LNK-SC97-PG
           MOVE    LNK-WKSRY-INPUTCD (IDX IDZ)
                                       TO  LNK-SC97-INPUTCD
           CALL    "ORCSC97"           USING
                                       SPA-AREA
                                       ORCSC97AREA
                                       SPA-SCR-REC
      *    回数
           IF      LNK-SC97-KAISU      =   ZERO
               MOVE    1               TO  LNK-SC97-KAISU
           END-IF
      *    外用薬の時、回数は１回とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "23"
               MOVE    1               TO  LNK-SC97-KAISU
           END-IF
      *
           MOVE    ZERO                TO  IDX1
           MOVE    ZERO                TO  WRK-NISUU
           PERFORM VARYING     IDX4    FROM    1   BY  1
                   UNTIL       IDX4        >   31
               IF      LNK-SC97-NYU-DAY (IDX4) NOT =   ZERO
                   ADD     1               TO  WRK-NISUU
               END-IF
           END-PERFORM
      *    ゼロの時、
           IF      WRK-NISUU           =   ZERO
               MOVE    1                   TO  WRK-NISUU
           END-IF
      *
      *    計を表示
           IF      WRK-JIHIMONEY       >   ZERO
           AND    (LNK-WKSRY-SRYKBN (IDX)  =   "95"  OR  "96")
      *        自費
               IF      FLG-KAISUU-ARI3     =   1
                   ADD    1                    TO  IDG
                   PERFORM 3101913-NYUINPUTCD-JIHI-SEC
                   MOVE    WRK-BANGO       TO  SPA-GMN-NAIBANGO (IDG)
               END-IF
           ELSE
               ADD    1                    TO  IDG
               PERFORM 3101912-NYUINPUTCD-HEN-SEC
               MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-HEN2 (IDG)
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI   (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI2  (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
               MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
           END-IF
      *
            .
       310191-DAYSET-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院回数編集処理
      *****************************************************************
       3101912-NYUINPUTCD-HEN-SEC      SECTION.
      *
      *    点数＊回数　計
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-TENSU
      *
           COMPUTE WRK-KAISU-9         =   WRK-NISUU
                                       *   LNK-SC97-KAISU
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-KAISU-9
      *    包括分
           IF      WRK-HKTZAI      NOT =   SPACE
               MOVE    ZERO            TO  WRK-KEI-G
           END-IF
      *
      *H27.7
      *??? COMPUTE WRK-S-KEI-G         =   WRK-S-ZAITEN
      *                                *   WRK-KAISU-9
      *    IF      WRK-HKTZAI      NOT =   SPACE
      *        MOVE    ZERO            TO  WRK-S-KEI-G
      *    END-IF
      *??? ADD     WRK-S-KEI-G         TO  SPA-NAI-HKNTEN  (IDX-NAI)
           ADD     WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *
      **   労災金額の時、ゼロ（指導料、その他のみ）
           IF      LNK-WKSRY-SRYKBN (01)   =   "13"  OR  "80"
               IF      FLG-RSIFLG          =   1
                   MOVE    ZERO            TO  SPA-NAI-HKNTEN
                                                             (IDX-NAI)
               END-IF
           END-IF
      *H27.7
      *
      *    計集計
           MOVE    WRK-KEI-G           TO  WRK-KEI
           ADD     WRK-KEI-G           TO  SPA-GMN-GOKEI
      *
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-H2
           MOVE    SPACE               TO  WRK-KEI-H 
      *    回数
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1               TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    回数
           MOVE    WRK-NISUU           TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H2 (IDK2:1)
               END-IF
           END-PERFORM
      *    計
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KEI-X   (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-TENSU               DELIMITED   BY  SIZE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H             DELIMITED   BY  SPACE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H2            DELIMITED   BY  SPACE
                   "  "                    DELIMITED   BY  SIZE
                   WRK-KEI-H               DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
      *
      *
            .
       3101912-NYUINPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入院回数編集処理
      *****************************************************************
       3101913-NYUINPUTCD-JIHI-SEC     SECTION.
      *
           COMPUTE WRK-KAISU-9         =   WRK-NISUU
                                       *   LNK-SC97-KAISU
           COMPUTE WRK-KEI-G           =   WRK-JIHIMONEY
                                       *   WRK-KAISU-9
      *    包括分
           IF      WRK-HKTZAI      NOT =   SPACE
               MOVE    ZERO            TO  WRK-KEI-G
           END-IF
      *
           MOVE    WRK-KEI-G           TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEIJIJ
      *    計集計
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-H2
           MOVE    SPACE               TO  WRK-KEI-H 
      *    回数
           MOVE    LNK-SC97-KAISU      TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    回数
           MOVE    WRK-NISUU           TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H2 (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-KAISU-H             DELIMITED   BY  SPACE
                   "X"                     DELIMITED   BY  SIZE
                   WRK-KAISU-H2            DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
           MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI4(IDG)
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-KEI-X           TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
      *
           MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
           MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TKINGAK4(IDG)
      *
           INSPECT SPA-GMN-TBANGO2 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI41(IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TZAIKEI4 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
           INSPECT SPA-GMN-TKINGAK4 (IDG)  REPLACING
                                              ALL "  "  BY  "　"
      *
            .
       3101913-NYUINPUTCD-JIHI-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    番号セット
           ADD     1                   TO  WRK-BANGO
           MOVE    WRK-BANGO           TO  SPA-GMN-TBANGO (IDG)
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *    内部セット
           ADD     1                   TO  IDX-NAI
           MOVE    WRK-BANGO           TO  SPA-NAI-BANGO  (IDX-NAI)
      *    パラメタの連番
           MOVE    IDX2                TO  SPA-NAI-EDANUM  (IDX-NAI)
           MOVE    LNK-WKSRY-ZAINUM (IDX)
                                       TO  SPA-NAI-ZAINUM  (IDX-NAI)
           MOVE    LNK-WKSRY-SRYKBN (IDX)
                                       TO  SPA-NAI-SRYKBN  (IDX-NAI)
           MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                       TO  SPA-NAI-SRYSYUKBN (IDX-NAI)
           MOVE    LNK-WKSRY-HKNCOMBI(IDX)
                                       TO  SPA-NAI-HKNCOMBI(IDX-NAI)
           MOVE    LNK-WKSRY-SRYKA   (IDX)
                                       TO  SPA-NAI-SRYKA   (IDX-NAI)
      *****合計点数集計
      *    IF      WRK-SRYKAISUKEI     =   ZERO
      *        MOVE   1                    TO  WRK-SRYKAISUKEI
      *    END-IF
      *    COMPUTE WRK-KEI-G           =   WRK-ZAITEN
      *                                *   WRK-SRYKAISUKEI
      *****MOVE    WRK-KEI-G           TO  SPA-NAI-HKNTEN  (IDX-NAI)
      *****IF      LNK-WKSRY-SRYCD (01 01)     =   "630010002"
           IF      LNK-WKSRYP-PLUSKBN (IDX)    =   1
      *        薬剤料逓減　を減算
               MOVE    1                   TO  SPA-NAI-HKNSFLG(IDX-NAI)
           ELSE
               MOVE    ZERO                TO  SPA-NAI-HKNSFLG(IDX-NAI)
           END-IF
      *
           MOVE    IDX-NAI             TO  SPA-GMN-NAIIDX  (IDG)
      *
      *
      *     診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYSYUKBN (IDX) =   MSYU-SRYSYUKBN
                                                      (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                                TO  WRK-SRYSYUMEI
           END-SEARCH
      *
      *    診療種別がない時、診療行為区分より
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               SET     MSYU-IDX            TO  1
               SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE           TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYKBN(IDX) =   MSYU-SRYKBN
                                                         (MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO1(IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO1(IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO1(IDG)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                       TO  SPA-GMN-TMEISYO1(IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO1(IDG)(7:)
           END-IF
      *    包括剤
           IF      WRK-HKTZAI          =   "1"
               MOVE    "【包括診療】"  TO   SPA-GMN-TMEISYO1(IDG)(57:)
           END-IF
      *
      *H28.9  持参薬
           IF      WRK-JISANKBN        =   "1"
               MOVE    "【持参薬】"    TO   SPA-GMN-TMEISYO1(IDG)(57:)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO1 (IDG)  REPLACING  ALL "  "  BY "　"
      *
           .
       31011-GYO1-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細  編集処理
      *****************************************************************
       31012-ZAIMEI-HEN-SEC                  SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31012-ZAIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    労災の金額算定分の時、点数をゼロとする
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "101" ) AND
                  (TNS-TENSIKIBETU     =   1    )
               MOVE    1                   TO  FLG-RSIFLG
           END-IF
      *    自賠責で証明料等の時、点数をゼロとする
           IF     (SPA-HKNKBN          =   1    )  AND
      *********** (SPA-RSI-HKNKBN      =   "4"  )  AND
                  (LNK-WKSRY-SRYKBN(IDX)   =   "80" )  AND
                  (LNK-WKSRY-SRYCD (IDX IDZ)(1:5)  =   "09591" OR
                                                       "09592" OR
                                                       "09593" OR
                                                       "09594" )
               MOVE    1                   TO  FLG-RSIFLG
           END-IF
      *
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *
      *H26.10
      *    在宅　診療人数合計　再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099140012"
                PERFORM 31001214-ZAICOM-MEISYO-SEC
           END-IF
      *!!
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
      *R02.4    MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO3(IDG)
           END-IF
      *!
      *    用量割合再編集
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:7)   =   "0992000")  AND
                  (LNK-WKSRY-SRYSURYO(IDX IDZ) NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
      *R02.4    MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO3(IDG)
           END-IF
      *!
      *    後発医薬品変更再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
                                               OR  "099209907"
                                               OR  "099209908"
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    後発医薬品再編集
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6") AND
                  (TNS-KOUHATUKBN      =   1
                                       OR  2
                                       OR  3
                                       OR  4
                                       OR  7    )
                PERFORM 3100123-KOUHATU-HEN-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *H24.4  院外処方せんの一般名表示
           MOVE    SPACE               TO  SPA-GMN-GE-MEISYO(IDG)
           MOVE    SPACE               TO  SPA-GMN-ME-MEISYO(IDG)
           IF    (SPA-SRYYMD           >=  "20120401" )
             AND (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6")
             AND (LNK-WKSRY-SRYKBN (IDX)       =   "21"
                                               OR  "22"
                                               OR  "23" 
                                               OR  "14"  )
               IF      (LNK-WKSRY-ZAITENKEI (IDX-KOUI)
                                               NOT =   ZERO  )
                 OR    (LNK-WKSRY-SRYSYUKBN (IDX)  =   "213"
                                                   OR  "223"
                                                   OR  "233"
                                                   OR  "211"
                                                   OR  "221"
                                                   OR  "231"
                                                   OR  "291"
                                                   OR  "294"
                                                   OR  "297" )
                   CONTINUE
               ELSE
      *            銘柄名
                   MOVE    SPA-GMN-TMEISYO2(IDG)
                                       TO  SPA-GMN-ME-MEISYO(IDG)
      *            院外処方せん
                   PERFORM 310020-GENERICNAME-CHK-SEC
               END-IF
           END-IF
      *!
      *    器材商品名の器材コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:1)   =   "7"  )  AND
                  (LNK-WKSRY-AUTOKBN (IDX IDZ)     =   "T"  )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
      *ver.4.7
      *         商品名コードの単位へ
                IF      WRK-SYO-TANINAME   NOT =   SPACE
                    MOVE    WRK-SYO-TANINAME   TO  TNS-TANINAME
                END-IF
           END-IF
      *    器材商品名コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:3)   =   "058"  )
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
           END-IF
      *ver.4.7
      *!
      *
      *    約束の内容の時、再編集
           IF      FLG-YAKUSOKU        =   1
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-MEISYO
               MOVE    SPACE                   TO  SPA-GMN-TMEISYO2
                                                               (IDG)
               STRING     "＞"             DELIMITED   BY  SIZE
                          WRK-MEISYO       DELIMITED   BY  SIZE
                          INTO             SPA-GMN-TMEISYO2(IDG)
               END-STRING
      *H28.2
      *        銘柄名・一般名
               IF      SPA-GMN-ME-MEISYO (IDG) NOT =   SPACE
                   MOVE    SPA-GMN-ME-MEISYO (IDG) TO  WRK-MEISYO
                   MOVE    SPACE               TO  SPA-GMN-ME-MEISYO
                                                               (IDG)
                   STRING     "＞"             DELIMITED   BY  SIZE
                              WRK-MEISYO       DELIMITED   BY  SIZE
                              INTO             SPA-GMN-ME-MEISYO(IDG)
                   END-STRING
               END-IF
               IF      SPA-GMN-GE-MEISYO (IDG) NOT =   SPACE
                   MOVE    SPA-GMN-GE-MEISYO (IDG) TO  WRK-MEISYO
                   MOVE    SPACE               TO  SPA-GMN-GE-MEISYO
                                                               (IDG)
                   STRING     "＞"             DELIMITED   BY  SIZE
                              WRK-MEISYO       DELIMITED   BY  SIZE
                              INTO             SPA-GMN-GE-MEISYO(IDG)
                   END-STRING
               END-IF
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" )) AND
      *DDDDDDDDDD (LNK-WKSRY-INPUTNUM(IDX IDZ)    NOT =   ZERO OR
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
      *
      *R02.3
               IF      LNK-WKSRY-INPUTCOMENT(IDX IDZ)(71:) NOT =  SPACE
                   MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO3 (IDG)
               END-IF
      *TEST
      *TEST
           END-IF
      *H30.4
      *   分割調剤など,内容設定
           IF     (LNK-WKSRY-SRYCD   (IDX IDZ)(1:7)  =   "0992081")
             AND  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *
      *    数量（薬剤、器材のとき）
           IF    ((LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
             OR   (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"    ))
      *       減点以外
             AND  (LNK-WKSRYP-PLUSKBN (IDX)    =   ZERO  )
      *
               IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  )
                 AND  (TNS-TEN                          =   ZERO )
                 AND  (TNS-TENSIKIBETU                  =   1    )
                   CONTINUE
               ELSE
                   MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
                   PERFORM 310121-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
                   MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
               END-IF
           END-IF
      *    単位
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1) =   "1" )  AND
                  (TNS-TANICD                 NOT =   ZERO )
      *H28.2   画像診断もすべて表示
      *     AND  ((LNK-WKSRY-SRYKBN   (IDX)   NOT =   "70" ) OR
      *           (LNK-WKSRY-AUTOKBN (IDX IDZ)    =   "S"  ))
      *****    IF      LNK-WKSRY-SRYSURYO (IDX IDZ)   >   1
                   MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
                   PERFORM 310121-SURYO-HEN-SEC
                   MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
      ****     END-IF
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *    分画数
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "7"  )  AND
                  (TNS-DATAKBN             =   3    )  AND
                  (LNK-WKSRY-SRYKAISU (IDX IDZ)      >   ZERO )
               MOVE    LNK-WKSRY-SRYKAISU (IDX IDZ)
                                           TO  WRK-BUNGSU-Z
               PERFORM 310122-BUNGSU-HEN-SEC
               MOVE    WRK-BUNGSU-J        TO  SPA-GMN-TTANMEI (IDG)
               MOVE    "分画"              TO  SPA-GMN-TTANI2  (IDG)
           END-IF
      *!
      *    包括検査の判定
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1) =   "1" )  AND
                  (LNK-WKSRY-SRYKBN (IDX)      =   "60" ) AND
                  (TNS-DATAKBN                 =   1    ) AND
                  (TNS-HOUKNSKBN           NOT =   ZERO )
               ADD     1                   TO  WRK-HOUKNS-CNT
               MOVE    IDG                 TO  WRK-HOUKNS-IDX
               MOVE    SPA-GMN-TMEISYO2(IDG)   TO  WRK-HOUKNS-NAME
           END-IF
      *!
      *!
      *    自賠責金額入力
           IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   "809")  AND
                  (LNK-WKSRY-JIHIMONEY(IDX IDZ)  >   ZERO )
               PERFORM 310124-RSI-JIHIMONEY-HEN-SEC
           END-IF
      *!
      *!
           INSPECT SPA-GMN-TBANGO2 (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI2  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
      *
      *!!
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内明細（自費）  編集処理
      *****************************************************************
       31013-JIHIMEI-HEN-SEC           SECTION.
      *
           ADD     1                   TO  IDG
           IF      IDG                 >   MAX-GMN
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
           MOVE    WRK-BANGO           TO  SPA-GMN-NAIBANGO (IDG)
      *
      *    約束セット編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "S"
               PERFORM 310120-YAKUSOKU-SET-SEC
               GO      TO      31013-JIHIMEI-HEN-EXT
           END-IF
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU       NOT =   ZERO
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    後発医薬品変更再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" ))  AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *H30.4
      *   分割調剤など,内容設定
           IF     (LNK-WKSRY-SRYCD   (IDX IDZ)(1:7)  =   "0992081")
             AND  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    後発医薬品再編集
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6") AND
                  (TNS-KOUHATUKBN      =   1
                                       OR  2
                                       OR  3
                                       OR  4
                                       OR  7  )
                PERFORM 3100123-KOUHATU-HEN-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
           MOVE    ZERO                TO  FLG-KAISUU
      *    数量（薬剤、器材のとき）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
             OR   (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"    )
               MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU  (IDG)
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1   (IDG)
           END-IF
      *
      *    計
           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDZ)
                                       TO  WRK-KEI
           MOVE    SPACE               TO  WRK-ZAIKEIJIH
           MOVE    WRK-KEI             TO  WRK-ZAIKEIJIH
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEIJIH       TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
      *
           IF      LNK-WKSRY-JIHIMONEY(IDX IDZ)    =   ZERO
               MOVE    SPACE               TO  SPA-GMN-TZAIKEI3(IDG)
           ELSE
               MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
               MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TZAIKEI3(IDG)
           END-IF
      *
           INSPECT SPA-GMN-TBANGO2 (IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TMEISYO2(IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG)  REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TZAIKEI3(IDG)  REPLACING  ALL "  "  BY  "　"
      *
      *    回数
           MOVE    SPACE               TO  WRK-KAISU-H
           IF     (WRK-SRYKAISUKEI     >   1    )
             AND ((FLG-KAISUU-ARI2     =   ZERO )  OR
                  (FLG-KAISUU-ARI3     =   ZERO ))
               ADD     1                   TO  IDG
               IF      IDG                 >   MAX-GMN
                   GO      TO      31013-JIHIMEI-HEN-EXT
               END-IF
      *        回数
               MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
               MOVE    ZERO                TO  IDK2
               MOVE    SPACE               TO  WRK-KAISU-H
               PERFORM VARYING     IDK1    FROM    1   BY  1
                       UNTIL       IDK1    >   3
                   IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                       ADD     1           TO  IDK2
                       MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
                   END-IF
               END-PERFORM
               MOVE    SPACE               TO  WRK-KAISU-J
               MOVE    1                   TO  IDJ2
               PERFORM VARYING     IDJ     FROM    1  BY  1
                       UNTIL       IDJ     >   3
                   EVALUATE    WRK-KAISU-H (IDJ:1)
                       WHEN    SPACE
                           MOVE    "　"                TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                       WHEN    "1"  THRU  "9"
                           MOVE    WRK-KAISU-H (IDJ:1) TO  IDJ1
                           MOVE    TBL-SUJI (IDJ1)     TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                       WHEN    "0"
                           MOVE    TBL-SUJI (10)       TO  WRK-KAISU-J
                                                               (IDJ2:2)
                           ADD     2                   TO  IDJ2
                   END-EVALUATE
               END-PERFORM
               MOVE   "×"                 TO  SPA-GMN-TZAIKEI4 (IDG)
                                                               (3:2)
               MOVE   WRK-KAISU-J(1:6)     TO  SPA-GMN-TZAIKEI4 (IDG)
                                                               (5:)
      *        計
               COMPUTE WRK-KEI             =   LNK-WKSRY-JIHIMONEY
                                                            (IDX IDZ)
                                           *   WRK-SRYKAISUKEI
               MOVE    SPACE               TO  WRK-ZAIKEIJIH
               MOVE    WRK-KEI             TO  WRK-ZAIKEIJIH
      *        日本語変換
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-ZAIKEIJIH       TO  KANACHK-MAE-INPUT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJIJ
               IF      LNK-WKSRY-JIHIMONEY(IDX IDZ)    =   ZERO
                   MOVE    SPACE               TO  SPA-GMN-TKINGAK4(IDG)
               ELSE
                   MOVE    "円"                TO  WRK-ZAIKEIJIJ(17:2)
                   MOVE    WRK-ZAIKEIJIJ       TO  SPA-GMN-TKINGAK4(IDG)
               END-IF
               MOVE    1                   TO  FLG-KAISUU
      *
               INSPECT SPA-GMN-TBANGO2 (IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TMEISYO2(IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TYOBI41 (IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TZAIKEI4(IDG)
                                           REPLACING  ALL "  "  BY  "　"
               INSPECT SPA-GMN-TKINGAK4(IDG)
                                           REPLACING  ALL "  "  BY  "　"
           END-IF
      *
      *    コメント以外の最終行セット
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "0"  OR  "8"
               CONTINUE
           ELSE
               MOVE    IDG             TO  WRK-IDG
           END-IF
      *
           .
       31013-JIHIMEI-HEN-EXT.
           EXIT.
      *****************************************************************
      *     約束セットの時、再編集処理
      *****************************************************************
       310120-YAKUSOKU-SET-SEC        SECTION.
      *
           MOVE    SPACE               TO  INPUTCD-REC
           MOVE    SPA-HOSPNUM         TO  ICD-HOSPNUM
      *    セットの桁数は６とする
           MOVE    "6"                 TO  ICD-CDSYU
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)(1:6)
                                       TO  ICD-INPUTCD
      *
           PERFORM 930-INPUTCD-READ-SEC
      *
           IF      FLG-INPUTCD         =   ZERO
               MOVE    ICD-DSPNAME         TO  SPA-GMN-TMEISYO2(IDG)
           ELSE
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    数量（薬剤、器材のとき）
           MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
      *
           .
       310120-YAKUSOKU-SET-EXT.
           EXIT.
      *****************************************************************
      *     薬剤服用コメントの時、再編集処理
      *****************************************************************
       3100121-FUKIYO-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
      *    埋め込み有
           IF     (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    SPACE               TO  WRK-MEISYO
               STRING     "【"             DELIMITED   BY  SIZE
                          LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           DELIMITED   BY  SPACE
                          "】"             DELIMITED   BY  SIZE
                          INTO             WRK-MEISYO
               END-STRING
           END-IF
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品変更再編集処理
      *****************************************************************
       3100122-KOUHATU-MEISYO-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "【"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "】"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3100122-KOUHATU-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *     後発医薬品再編集処理
      *****************************************************************
       3100123-KOUHATU-HEN-SEC        SECTION.
      *
      *H24.4
      *    後発品区分名称
           PERFORM 3100123-KOUHATU-MSG-SEC
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *    後発医薬品再編集
            STRING  "【"
                    WRK-KOUMEI         DELIMITED   BY  SPACE
                    "】"               DELIMITED   BY  SIZE
                    TNS-NAME           DELIMITED   BY  SIZE
                                  INTO        WRK-MEISYO
           END-STRING
           .
       3100123-KOUHATU-HEN-EXT.
           EXIT.
      *
      *H24.4
      *****************************************************************
      *     後発品区分名称編集処理
      *****************************************************************
       3100123-KOUHATU-MSG-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-KOUMEI
           EVALUATE    TNS-KOUHATUKBN
               WHEN    1
                   MOVE    "後"            TO  WRK-KOUMEI
               WHEN    2
                   MOVE    "先"            TO  WRK-KOUMEI
      *H29.4 から
               WHEN    3
                   MOVE    "加"            TO  WRK-KOUMEI
               WHEN    4
                   MOVE    "加"            TO  WRK-KOUMEI
               WHEN    7
                   MOVE    "無"            TO  WRK-KOUMEI
           END-EVALUATE
           .
       3100123-KOUHATU-MSG-EXT.
           EXIT.
      *
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       3005-INPUT-MEISYO3-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                 TO  ORCSMEIHEN-KBN
           MOVE    LNK-WKSRY-SRYSURYO(IDX IDZ)
                                       TO  ORCSMEIHEN-SURYO
           MOVE    LNK-WKSRY-SRYCD(IDX IDZ)
                                       TO  ORCSMEIHEN-SRYCD
           MOVE    TNS-NAME            TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
           .
       3100121-FUKIYO-MEISYO-EXT.
           EXIT.
      *****************************************************************
      *    器材商品名コードの器材編集
      *****************************************************************
       3005-INPUT-MEISYO4-SEC        SECTION.
      *
           MOVE    SPACE               TO  WRK-MEISYO
      *
           STRING     "〔"             DELIMITED   BY  SIZE
                      TNS-NAME         DELIMITED   BY  "    "
                      "〕"             DELIMITED   BY  SIZE
                      INTO             WRK-MEISYO
           END-STRING
           .
       3005-INPUT-MEISYO4-EXT.
           EXIT.
      *!!
      *H24.4
      *****************************************************************
      *    院外処方せん一般名編集処理
      *****************************************************************
       310020-GENERICNAME-CHK-SEC        SECTION.
      *
           MOVE    ZERO                TO  WRK-IPNMEI
           MOVE    IDZ                 TO  IDG-Z
           MOVE    IDX                 TO  IDG-X
           ADD     1                   TO  IDG-Z
           IF      IDG-Z               >   5
               IF      IDG-X           <   IDX-KOUI
                   ADD     1               TO  IDG-X
                   MOVE    1               TO  IDG-Z
               ELSE
                   MOVE    IDZ             TO  IDG-Z
               END-IF
           END-IF
           IF      LNK-WKSRY-SRYCD (IDG-X IDG-Z) =   "099209903"
                                                 OR  "099209907"
      *        次の行に　後発品変更不可、銘柄名記載があれば変更なし
               GO      TO      310020-GENERICNAME-CHK-EXT
           END-IF
           IF      LNK-WKSRY-SRYCD (IDG-X IDG-Z) =   "099209908"
      *        次の行に一般名記載
               MOVE    1               TO  WRK-IPNMEI
           END-IF
      *
      *H27.1
      *    後発品変更不可（処方）
           IF     (FLG-INGIKSN-KBN     =   1    )
              AND (WRK-IPNMEI          =   ZERO )
               GO      TO      310020-GENERICNAME-CHK-EXT
           END-IF
      *
           PERFORM 9101-TENSUPLUS-READ-SEC
      *
      *    システム管理の変更可によってすべて一般名記載とする
           IF     (TNSP-IPN-KISAIKBN       =   0       )
      *?     AND  (SPA-KOUHATUKA           =   "1"     )
             AND  (TNS-KOUHATUKBN          =   1
                                           OR  2
      *H29.4
                                           OR  3
                                           OR  4
                                           OR  7       )
      *H27.1
               IF       (SPA-KOUHATUKA           =   "1" )
                   OR   (FLG-INGIKSN-KBN         =   2    )
                   MOVE    1               TO  TNSP-IPN-KISAIKBN
               END-IF
           END-IF
      *
      *    後発品区分名称
           PERFORM 3100123-KOUHATU-MSG-SEC
      *
      *    一般名記載あり
           IF     (TNSP-IPN-KISAIKBN   =   1  )
             OR   (WRK-IPNMEI          =   1  )
               PERFORM 9102-GENERICNAME-READ-SEC
      *        一般名
               IF      FLG-GENERICNAME     =   ZERO
                   MOVE    SPACE               TO  WRK-MEISYO
                   STRING  "【般"                  DELIMITED  BY  SIZE
                           WRK-KOUMEI              DELIMITED  BY  SPACE
                           "】"                    DELIMITED  BY  SIZE
                           GENERIC-GENERICNAME     DELIMITED  BY  SPACE
                                               INTO    WRK-MEISYO
                   END-STRING
                   MOVE    WRK-MEISYO    TO  SPA-GMN-GE-MEISYO(IDG)
               END-IF
           END-IF
      *    処方名記載
           IF     (TNSP-IPN-KISAIKBN   =   2   )
             AND  (WRK-IPNMEI          =   ZERO)
               MOVE    SPACE               TO  WRK-MEISYO
               STRING  "【処"                  DELIMITED  BY  SIZE
                       WRK-KOUMEI              DELIMITED  BY  SPACE
                       "】"                    DELIMITED  BY  SIZE
                       TNSP-SHONAME            DELIMITED  BY  SPACE
                                               INTO    WRK-MEISYO
               END-STRING
               MOVE    WRK-MEISYO    TO  SPA-GMN-GE-MEISYO(IDG)
           END-IF
      *    処方名記載
           IF     (TNSP-IPN-KISAIKBN   =   3   )
               MOVE    SPACE               TO  WRK-MEISYO
               STRING  "【般"                  DELIMITED  BY  SIZE
                       WRK-KOUMEI              DELIMITED  BY  SPACE
                       "】"                    DELIMITED  BY  SIZE
                       TNSP-SHONAME            DELIMITED  BY  SPACE
                                               INTO    WRK-MEISYO
               END-STRING
               MOVE    WRK-MEISYO    TO  SPA-GMN-GE-MEISYO(IDG)
           END-IF
           .
       310020-GENERICNAME-CHK-EXT.
           EXIT.
      *
      *
      *H26.10
      *****************************************************************
      *    在宅　診療人数合計　再編集処理
      *****************************************************************
       31001214-ZAICOM-MEISYO-SEC            SECTION.
      *
      *    回数を合計人数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                           TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  WRK-MEISYO
           STRING  TNS-NAME            DELIMITED  BY  SPACE
                   "　　　　【"        DELIMITED  BY  SIZE
                   SPA-SRYYMD(7:2)     DELIMITED  BY  SPACE
                   "日（"              DELIMITED  BY  SIZE
                   WRK-KAISU-H         DELIMITED  BY  SPACE
                   "人）"              DELIMITED  BY  SIZE
                   "】"                DELIMITED  BY  SIZE
                                               INTO    WRK-MEISYO
           END-STRING
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-MEISYO          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  SPA-GMN-TMEISYO2(IDG)
      *
           .
       31001214-ZAICOM-MEISYO-EXT.
           EXIT.
      *H26.10
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5  BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
      *
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-SURYO-J
           INSPECT WRK-SURYO-J         REPLACING
                                       ALL "Ｘ"  BY  "×"
      **** MOVE    SPACE               TO  WRK-SURYO-J
      *    MOVE    1                   TO  IDJ2
      *    PERFORM VARYING     IDJ     FROM    1  BY  1
      *            UNTIL       IDJ     >   9
      *       EVALUATE     WRK-SURYO-X (IDJ:1)
      *            WHEN    SPACE
      *                MOVE    "　"            TO  WRK-SURYO-J (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "."
      *                MOVE    "．"            TO  WRK-SURYO-J (IDJ2:2)
      *                ADD     2               TO  IDJ2
      *            WHEN    "1"  THRU  "9"
      *                MOVE    WRK-SURYO-X (IDJ:1) TO  IDJ1
      *                MOVE    TBL-SUJI (IDJ1)     TO  WRK-SURYO-J
      *                                                        (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *            WHEN    "0"
      *                MOVE    TBL-SUJI (10)       TO  WRK-SURYO-J
      *                                                        (IDJ2:2)
      *                ADD     2                   TO  IDJ2
      *        END-EVALUATE
      *****END-PERFORM
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310122-BUNGSU-HEN-SEC           SECTION.
      *
      *    日本語変換
           MOVE    SPACE               TO  WRK-BUNGSU-J
           MOVE    1                   TO  IDJ2
           PERFORM VARYING     IDJ     FROM    1  BY  1
                   UNTIL       IDJ     >   3
              EVALUATE     WRK-BUNGSU-X (IDJ:1)
                   WHEN    SPACE
                       MOVE    "　"            TO  WRK-BUNGSU-J(IDJ2:2)
                       ADD     2               TO  IDJ2
                   WHEN    "."
                       MOVE    "．"            TO  WRK-BUNGSU-J(IDJ2:2)
                       ADD     2               TO  IDJ2
                   WHEN    "1"  THRU  "9"
                       MOVE    WRK-BUNGSU-X (IDJ:1)    TO  IDJ1
                       MOVE    TBL-SUJI (IDJ1)     TO  WRK-BUNGSU-J
                                                              (IDJ2:2)
                       ADD     2                   TO  IDJ2
                   WHEN    "0"
                       MOVE    TBL-SUJI (10)       TO  WRK-BUNGSU-J
                                                              (IDJ2:2)
                       ADD     2                   TO  IDJ2
               END-EVALUATE
           END-PERFORM
           .
       310122-BUNGSU-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   自賠責金額入力編集処理
      *****************************************************************
       310124-RSI-JIHIMONEY-HEN-SEC                  SECTION.
      *
           MOVE    LNK-WKSRY-JIHIMONEY(IDX IDZ)    TO  WRK-Z7
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-Z7             TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
      *
           MOVE    SPACE               TO  WRK-TJIHI
           MOVE    1                   TO  IDM
           MOVE    1                   TO  IDM2
           PERFORM
                   UNTIL       IDM     >   22
               IF      WRK-ZAIKEIJ(IDM:1)  =   SPACE
                   ADD     1               TO  IDM
               ELSE
                   IF      WRK-ZAIKEIJ(IDM:2)  =   "　"
                       ADD     2               TO  IDM
                   ELSE
                       MOVE    WRK-ZAIKEIJ (IDM:2)
                                               TO  WRK-TJIHI
                                                        (IDM2:2)
                       ADD     2               TO  IDM2
                       ADD     2               TO  IDM
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  SPA-GMN-HEN1(IDG)
           STRING  "（"                DELIMITED   BY  SIZE
                   WRK-TJIHI           DELIMITED   BY  SPACE
                   "円）"              DELIMITED   BY  SIZE
                      INTO             SPA-GMN-HEN1 (IDG)
           END-STRING
           .
       310124-RSI-JIHIMONEY-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   入力コードの画面用再編集処理
      *****************************************************************
       31013-INPUTCD-HEN-SEC                  SECTION.
      *
      *    点数＊回数　計
      *    点数
           MOVE    WRK-ZAITEN          TO  WRK-TENSU
      *
           IF      WRK-SRYKAISUKEI     =   ZERO
               MOVE   1                    TO  WRK-SRYKAISUKEI
           END-IF
           COMPUTE WRK-KEI-G           =   WRK-ZAITEN
                                       *   WRK-SRYKAISUKEI
      *    包括分
           IF      WRK-HKTZAI      NOT =   SPACE
               MOVE    ZERO            TO  WRK-KEI-G
           END-IF
      *
           MOVE    WRK-KEI-G           TO  WRK-KEI
      *    合計点数集計
      *****IF      LNK-WKSRY-SRYCD (01 01)     =   "630010002"
           IF      LNK-WKSRYP-PLUSKBN (01)     =   1
      *        薬剤料逓減　を減算
               COMPUTE SPA-GMN-GOKEI       =   SPA-GMN-GOKEI
                                           -   WRK-KEI-G
               COMPUTE WRK-KEI-GS          =   WRK-KEI-G
                                           *   -1
               MOVE    WRK-KEI-GS          TO  WRK-KEI
           ELSE
               ADD     WRK-KEI-G           TO  SPA-GMN-GOKEI
           END-IF
      *    回数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-ZAIKEI 
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KEI-H 
           PERFORM VARYING     IDK1    FROM    1   BY  1
      *!           UNTIL       IDK1    >   8
                   UNTIL       IDK1    >   3
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *    計
           MOVE    ZERO                TO  IDK2
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KEI-X   (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KEI-X   (IDK1:1)
                                       TO  WRK-KEI-H   (IDK2:1)
               END-IF
           END-PERFORM
      *
           STRING  WRK-TENSU               DELIMITED   BY  SIZE
                   "X"                     DELIMITED   BY  SIZE
      *!           WRK-KAISU-H             DELIMITED   BY  SPACE
                   WRK-KAISU-H             DELIMITED   BY  SIZE
                   "  "                    DELIMITED   BY  SIZE
                   WRK-KEI-H               DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEI
           END-STRING
      *
      *    日本語変換
           INITIALIZE                  ORCSKANACHKAREA
           MOVE    "2"                 TO  KANACHK-SYORI
           MOVE    WRK-ZAIKEI          TO  KANACHK-MAE-INPUT
           CALL    "ORCSKANACHK"       USING
                                       ORCSKANACHKAREA
           MOVE    KANACHK-OUT-INPUT   TO  WRK-ZAIKEIJ
           INSPECT WRK-ZAIKEIJ         REPLACING
                                       ALL "Ｘ"  BY  "×"
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん発行チェック
      *****************************************************************
       30001-SIJISEN-CHK-SEC         SECTION.
      *
           INITIALIZE                  SPA-K081
      *    システム管理検索
           MOVE    SPACE               TO  SYS-5007-REC
           INITIALIZE                  SYS-5007-REC
           MOVE    "5007"              TO  SYS-5007-KANRICD
           MOVE    "01"                TO  SYS-5007-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-5007-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-5007-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-5007-HOSPNUM
           MOVE    SYS-5007-REC        TO  MCPDATA-REC
      *
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-5007-REC
               INITIALIZE                  SYS-5007-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-5007-REC
           ELSE
               INITIALIZE                  SYS-5007-REC
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SYS-5007-SIJISENKBN =   "1"
               MOVE    1               TO  SPA-NAI-SIJISENKBN
           ELSE
               MOVE    ZERO            TO  SPA-NAI-SIJISENKBN
           END-IF
      *
      *    診療内容判定
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-MAX
      *      薬評・器評以外
             IF     (LNK-WKSRY-ZAIKBN  (IDX)   =   1  )
             OR     (LNK-WKSRY-ZAIKBN  (IDX)   =   2  )
      *H28.9 持参薬
             OR     (LNK-WKSRY-ZAIKBN  (IDX)   =   3
                                               OR  4  )
               CONTINUE
             ELSE
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "21"
                                               OR  "22"
                                               OR  "23" )
      *            院外処方は対象外
                   IF     (LNK-WKSRY-SRYSYUKBN (IDX)   =   "212"
                                                       OR  "222"
                                                       OR  "232"
      *H26.10           臨時追加
                                                       OR  "292"
                                                       OR  "298"
                                                       OR  "295")
                       CONTINUE
                   ELSE
                       MOVE    1               TO  SPA-NAI-SYOHOU
                   END-IF
               END-IF
               IF     (LNK-WKSRY-SRYKBN (IDX)      =   "14" )
                  AND (LNK-WKSRY-SRYSYUKBN (IDX)   NOT =   "148"
                                                       AND "149" )
                    PERFORM VARYING     IDX2    FROM    1   BY  1
                            UNTIL      (IDX2    >   5   )  OR
                                       (SPA-NAI-SYOHOU  =   1  )
                       IF      LNK-WKSRY-SRYCD (IDX IDX2)(1:1)
                                                        =   "6"
                                                        OR  "7"
                           MOVE    1               TO  SPA-NAI-SYOHOU
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "31"
                                               OR  "32"
                                               OR  "33" )
                    PERFORM VARYING     IDX2    FROM    1   BY  1
                            UNTIL      (IDX2    >   5   )  OR
                                       (SPA-NAI-CHUSYA  =   1  )
                       IF      LNK-WKSRY-SRYCD (IDX IDX2)(1:1)
                                                        =   "6"
                                                        OR  "7"
                           MOVE    1               TO  SPA-NAI-CHUSYA
                       END-IF
                   END-PERFORM
               END-IF
      *
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "40"
                                               OR  "80" )
                   PERFORM VARYING     IDX2    FROM    1   BY  1
                           UNTIL      (IDX2    >   5   )  OR
                                      (SPA-NAI-SIJISEN  =   1  )
                       IF      LNK-WKSRY-SRYCD (IDX IDX2)(1:1)
                                                        =   "1"
                           MOVE    1               TO  SPA-NAI-SIJISEN
                       END-IF
                   END-PERFORM
               END-IF
             END-IF
           END-PERFORM
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
      *
           .
       30001-SIJISEN-CHK-EXT.
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
      *        登録・発行
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 4100-TOROKU-PRINT-SEC
      *        登録
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 450-TOROKU-SEC
      *    プレビュー
               WHEN    "CLICKED"       ALSO    "B01S"
                   PERFORM 4010-PREVIEW-SYORI-SEC
      *H24.4
      *        一般名称切替え
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 405-GENERIC-CHG-SEC
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
      *    入力個所のセット
           PERFORM 400-GMN-INPUT-SEC
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    入力個所のセット処理
      *****************************************************************
       400-GMN-INPUT-SEC          SECTION.
      *
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *    保険組合せ選択
               WHEN    ANY             ALSO    "MEILIST"
                   PERFORM 4001-MEILIST-SEL-SEC
           END-EVALUATE
           .
      *
       400-GMN-INPUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せ選択処理
      *****************************************************************
       4001-MEILIST-SEL-SEC          SECTION.
      *
      *    行選択
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   MAX-GMN
               IF      K08-SELECT (IDX)        =   "T"
                   MOVE    SPA-GMN-NAIBANGO (IDX)  TO  WRK-SELNUM
               END-IF
           END-PERFORM
      *
           IF      WRK-SELNUM          >   ZERO
               MOVE    ZERO            TO  IDX-Y2
      *        選択の次ぎへ
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   10
                   IF      WRK-SELNUM      =   K08-SELNUM (IDX)
      *            既にある時は、削除を解除する
                       MOVE    ZERO            TO  K08-SELNUM (IDX)
                       MOVE    IDX             TO  IDX-Y2
                       MOVE    10              TO  IDX
                   ELSE
                       IF      K08-SELNUM (IDX)    =   ZERO
                           MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM
                                                                  (IDX)
                           PERFORM 41021-SELNUM-CHK-SEC
                           IF      SPA-ERRCD       =   SPACE
                               MOVE    WRK-SELNUM      TO  K08-SELNUM
                                                                   (IDX)
                           END-IF
                           MOVE    IDX             TO  IDX-Y2
                           MOVE    10              TO  IDX
                           MOVE    "SELNUM"        TO  WRK-MCP-WIDGET
                           MOVE    IDX-Y2          TO  WRK-MCP-WIDGET
                                                                (7:2)
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
           .
      *
       4001-MEILIST-SEL-EXT.
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
      *
      *    関連処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4103-KANREI-SEC
           END-IF
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               MOVE    K08-SELNUM (IDX)    TO  SPA-GMN-SELNUM (IDX)
           END-PERFORM
      *
           MOVE    K08-USERPG          TO  SPA-GMN-USERPG-G
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
      *    同一連番がある時、１つとする
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   10
               IF      SPA-GMN-SELNUM (IDX)    NOT =   ZERO
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL       IDY     >   10
                       IF     (IDX         NOT =   IDY )  AND
                              (SPA-GMN-SELNUM (IDY)
                                           NOT =   ZERO )  AND
                              (SPA-GMN-SELNUM (IDX)
                                               =   SPA-GMN-SELNUM (IDY))
                           MOVE    ZERO        TO  SPA-GMN-SELNUM (IDY)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    ゼロずめ
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >=  10
               IF      SPA-GMN-SELNUM (IDX)    =   ZERO
                   MOVE    SPA-GMN-SELNUM (IDX + 1 )
                                               TO  SPA-GMN-SELNUM (IDX)
                   MOVE    ZERO                TO  SPA-GMN-SELNUM
                                                            (IDX + 1)
               END-IF
           END-PERFORM
      *    番号存在チェック
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX             >   10  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM (IDX)    NOT =   ZERO
                   PERFORM 41021-SELNUM-CHK-SEC
               END-IF
           END-PERFORM
      *
      *    Ｕ・Ｐ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 440-USERPG-CHK-SEC
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除選択処理
      *****************************************************************
       41021-SELNUM-CHK-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL      (IDY             >   MAX-GMN )  OR
                              (FLG-OK          =   1     )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-GMN-SELNUM (IDX)    =   SPA-NAI-BANGO (IDY)
                   MOVE    1                   TO  FLG-OK
                   IF      SPA-NAI-JIDFLG (IDY)    =   ZERO
                       MOVE    "0003"              TO  SPA-ERRCD
                       MOVE    IDX                 TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-OK              =   ZERO
               MOVE    "0002"              TO  SPA-ERRCD
               MOVE    IDX                 TO  SPA-GMN-CUR
           END-IF
           .
       41021-SELNUM-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    Ｕ・Ｐ入力　処理
      *****************************************************************
       440-USERPG-CHK-SEC              SECTION.
      *
           IF      SPA-USERPG-MAX      >   ZERO
               IF      SPA-GMN-USERPG      =   SPACE
                   MOVE    "0"                 TO  SPA-GMN-USERPG
               END-IF
           ELSE
               MOVE    SPACE               TO  SPA-GMN-USERPG
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-USERPG-MAX)  OR
                              (FLG-CHK NOT =   ZERO )
               IF      SPA-GMN-USERPG      =   SPA-GMN-USERPG-LIST (IDX)
                                                               (1:1)
                   MOVE    SPA-GMN-USERPG-LIST (IDX)
                                           TO  SPA-GMN-USERPG-G
                   MOVE    1               TO  FLG-CHK
               END-IF
           END-PERFORM
      *
           IF      SPA-USERPG-MAX      >   ZERO
               IF      FLG-CHK               =   ZERO
                   MOVE    "0007"            TO  SPA-ERRCD
                   MOVE    13                TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       440-USERPG-CHK-EXT.
           EXIT.
      *****************************************************************
      *    関連処理
      *****************************************************************
       4103-KANREI-SEC              SECTION.
      *
      *    削除区分設置
           PERFORM 41031-DELFLG-SET-SEC
      *    点数再計算
           PERFORM 41032-TENSU-KEI-SEC
      *
           .
       4103-KANREI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除区分設置処理
      *****************************************************************
       41031-DELFLG-SET-SEC              SECTION.
      *
           MOVE    ZERO                TO  FLG-DEL-ARI
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K08-MAX
               MOVE    ZERO                TO  SPA-GMN-TDELFLG (IDX)
               MOVE    ZERO                TO  SPA-NAI-DELFLG  (IDX)
               IF      SPA-GMN-NAIIDX  (IDX)   >   ZERO
                   MOVE    SPA-GMN-NAIIDX  (IDX)   TO  IDX-NAI
                   IF      SPA-NAI-JIDFLG (IDX-NAI)    =   1
                       MOVE    1               TO  SPA-GMN-TDELFLG (IDX)
                       MOVE    1               TO  FLG-DEL-ARI
                   END-IF
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >   10  )  OR
                                      (SPA-GMN-SELNUM (IDY)    =  ZERO)
                       IF      SPA-GMN-TBANGO (IDX)
                                               =   SPA-GMN-SELNUM (IDY)
                           MOVE    2               TO  SPA-GMN-TDELFLG
                                                                  (IDX)
                           MOVE    1               TO  SPA-NAI-DELFLG
                                                              (IDX-NAI)
                           MOVE    10              TO  IDY
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           .
       41031-DELFLG-SET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    点数再計算処理
      *****************************************************************
       41032-TENSU-KEI-SEC              SECTION.
      *
      *    診察料・在宅料は削除できないこととする（金額算定）
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (02)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (04)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (05)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (06)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (07)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (08)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (09)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (10)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (11)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (12)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (13)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (14)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (15)
           MOVE    ZERO                TO  SPA-GMN-HKNTEN (16)
      *H27.7
      *    点数クリア
      *??? PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   16
      *        MOVE    ZERO            TO  SPA-GMN-HKNTEN (IDX)
      *??? END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-K08-MAX
             IF      SPA-GMN-NAIIDX  (IDX)   >   ZERO
               MOVE    SPA-GMN-NAIIDX  (IDX)   TO  IDX-NAI
               IF      SPA-GMN-TDELFLG (IDX)   =   2
                   CONTINUE
               ELSE
                   MOVE    SPA-NAI-SRYKBN (IDX-NAI)    TO  WRK-SRYKBN
                   MOVE    SPA-NAI-SRYSYUKBN (IDX-NAI) TO  WRK-SRYSYUKBN
                   PERFORM 410321-SYUNOU-TENSU-SEC
                   IF      IDX-TS                  =   ZERO
      *H27.7
                                                   OR  1
                                                   OR  3
      *****************診察料は再計算なし
                       CONTINUE
                   ELSE
                       IF      SPA-NAI-HKNSFLG (IDX-NAI) =   ZERO
                           ADD     SPA-NAI-HKNTEN (IDX-NAI)
                                                   TO  SPA-GMN-HKNTEN
                                                              (IDX-TS)
                       ELSE
                           COMPUTE SPA-GMN-HKNTEN (IDX-TS)
                                                   =   SPA-GMN-HKNTEN
                                                              (IDX-TS)
                                                   -   SPA-NAI-HKNTEN
                                                              (IDX-NAI)
                       END-IF
                   END-IF
               END-IF
             END-IF
           END-PERFORM
      *    各点数
           MOVE    ZERO                TO  SPA-GMN-GOKEI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   16
               ADD     SPA-GMN-HKNTEN (IDX)    TO  SPA-GMN-GOKEI
           END-PERFORM
           .
       41032-TENSU-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納点数集計位置処理
      *****************************************************************
       410321-SYUNOU-TENSU-SEC              SECTION.
      *
           MOVE    ZERO                TO  IDX-TS
           EVALUATE    WRK-SRYKBN
      *        診察
               WHEN    "11"
               WHEN    "12"
                   MOVE    1                TO  IDX-TS
      *        指導
               WHEN    "13"
                   MOVE    2                TO  IDX-TS
      *        在宅
               WHEN    "14"
                   MOVE    3                TO  IDX-TS
      *        投薬
               WHEN    "21"   THRU  "27"
                   MOVE    4                TO  IDX-TS
      *        注射
               WHEN    "31"
               WHEN    "32"
               WHEN    "33"
                   MOVE    5                TO  IDX-TS
      *        処置
               WHEN    "40"
                   MOVE    6                TO  IDX-TS
      *        手術
               WHEN    "50"
                   MOVE    7                TO  IDX-TS
      *        麻酔
               WHEN    "54"
                   MOVE    8                TO  IDX-TS
      *        検査
               WHEN    "60"
                   MOVE    9                TO  IDX-TS
      *        病理診断
               WHEN    "64"
                   MOVE    14               TO  IDX-TS
      *        画像診断
               WHEN    "70"
                   MOVE    10               TO  IDX-TS
               WHEN    "80"
                   EVALUATE    WRK-SRYSYUKBN
      *            リハビリ
                   WHEN    "800"
                       MOVE    11               TO  IDX-TS
      *            自賠責金額
                   WHEN    "809"
                       MOVE    11               TO  IDX-TS
      *            処方せん料
                   WHEN    "820"
                       MOVE    4                TO  IDX-TS
      *            精神科
                   WHEN    "830"
                       MOVE    12               TO  IDX-TS
      *            放射線治療
                   WHEN    "840"
                       MOVE    13               TO  IDX-TS
      *            療養担当手当て
                   WHEN    "850"
                       MOVE    16               TO  IDX-TS
      *            入院料（外来）
                   WHEN    "890"
                       MOVE    15               TO  IDX-TS
      *            その他
                   WHEN    OTHER
                       MOVE    11               TO  IDX-TS
                   END-EVALUATE
      *            食事
               WHEN    "90"
               WHEN    "92"
                   MOVE    15               TO  IDX-TS
           END-EVALUATE
           .
       410321-SYUNOU-TENSU-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           PERFORM 2101-PARA02-DELETE-SEC
      *    一時データクリア
           PERFORM 4501-PARA-DELETE-SEC
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
      **** MOVE    "JOIN"              TO  MCP-PUTTYPE.
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *
      *****************************************************************
      *    確定　処理
      *****************************************************************
       450-TOROKU-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    入院更新処理
           IF      SPA-HZN-MOTOPG      =   "K02N"
               PERFORM 4502-NYUKOUSIN-SYORI-SEC
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *   包括分入力処理
          IF      SPA-HKNCOMBINUM     =   "9999"
               PERFORM 4503-HOUKATU-SYORI-SEC
               GO      TO      450-TOROKU-EXT
           END-IF
      *
      *    更新処理
           PERFORM 4501-KOUSIN-SYORI-SEC
           IF      SPA-ERRCD       NOT =   SPACE
               MOVE    99              TO  SPA-GMN-CUR
               GO      TO     450-TOROKU-EXT
           END-IF
      *
           PERFORM 2101-PARA02-DELETE-SEC
      *
           MOVE    "K03"               TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    "K03"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       450-TOROKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    登録・発行　処理
      *****************************************************************
       4100-TOROKU-PRINT-SEC             SECTION.
      *
      *    入力チェック処理
           PERFORM 410-INPUT-CHK-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               PERFORM 41001-K081-SYORI-SEC
           END-IF
      *
           .
       4100-TOROKU-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    更新処理
      *****************************************************************
       4501-KOUSIN-SYORI-SEC             SECTION.
      *
      *    自賠責の時、システム管理検索
           IF      (SPA-HKNKBN         =   1   )  AND
                   (SPA-RSI-HKNKBN     =   "4"
                                       OR  "5"
      *             第三者行為追加
                                       OR  "6"  ) 
               PERFORM 4500-RSI-SYSKANRI-SEC
           END-IF
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  FLG-SYOSIN-OK
      *    薬剤情報提供料算定有無
           MOVE    ZERO                TO  SPA-YAKJYO-ARI
      *
      *ver.4.8
      *
      *    ワーク診療行為編集
           IF      SPA-ZAIJUNKBN-FLG   =   1
      *        入力順
               PERFORM 3101-PARA-WKSRYACT-HEN-SEC
           ELSE
      *        診療区分順
               PERFORM 45011-WKSRYACT-PARA2-SEC
           END-IF
      *    収納テーブル編集
           PERFORM 3101-PARA-SYUNOU-HEN-SEC
      *
      *    一時データクリア
           PERFORM 4501-PARA-DELETE-SEC
      *
      *
      *    追加
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    OUTPUT              PARA-FILE
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   5
      *        IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
      *            MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
      *            WRITE                       PARA-REC
      *            IF      STS-PARA        NOT =  ZERO
      *                MOVE    "0051"             TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      *****END-PERFORM
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *    収納更新
           PERFORM 45012-SYUNOU-SYORI-SEC
      *    パラメタへ
           MOVE    ZERO                TO  PARA-EDANUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
      *v4.8    TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "K02"               TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  PARA-DATA-REC
      *
               PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
      *
           .
       4501-KOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分順編集処理
      *****************************************************************
       4501-PARA-DELETE-SEC             SECTION.
      *
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
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    "SYUNOU"            TO  PARA-FILEMEI
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del3"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       4501-PARA-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分順編集処理
      *****************************************************************
       45011-WKSRYACT-PARA2-SEC             SECTION.
      *
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  LNK-WKSRYACT-MAX
           MOVE    ZERO                TO  IDX
      *
           MOVE    SPACE               TO  PARA-REC
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K08"               TO  PARA-GYOUMUID
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
               ADD     1                   TO  IDX
               MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
                                                                (IDX)
               IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
                   MOVE    SPA-HKNCOMBINUM TO  LNK-WKSRY-HKNCOMBI(IDX)
               END-IF
               IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
                   MOVE    SPA-SRYKA       TO  LNK-WKSRY-SRYKA   (IDX)
               END-IF
               MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  WRK-MAX
      *
           .
       45011-WKSRYACT-PARA2-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為更新処理
      *****************************************************************
       45011-SRYACT-SYORI-SEC             SECTION.
      *
           MOVE    SPACE               TO  WRK-HKNCOMBI
           MOVE    SPACE               TO  WRK-SRYKA
           MOVE    ZERO                TO  CNT-ZAI
           MOVE    ZERO                TO  WRK-EDANUM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   MAX-GMN
                   IF      LNK-WKSRY-ZAINUM (IDX)  =   SPA-NAI-ZAINUM
                                                                 (IDY)
                       IF      SPA-NAI-DELFLG (IDY)    =   1
                           MOVE    1               TO  FLG-DEL
                       END-IF
                       MOVE    MAX-GMN             TO  IDY
                   END-IF
               END-PERFORM
               IF      FLG-DEL             =   ZERO
      *            初診料算定判定
                   PERFORM 450112-SYOSIN-CHK-SEC
      *            薬剤情報提供料判定
                   PERFORM 450113-YAKUJO-CHK-SEC
      *
      *            剤数チェック
                   PERFORM 450114-ZAICONUNT-SEC
      *
      *            ADD     1                   TO  IDX2
      ***          MOVE    "WKSRYACT"          TO  PARA-FILEMEI
      *            MOVE    IDX2                TO  PARA-EDANUM
      *            MOVE    LNK-WKSRYACT-REC (IDX) TO  WRK-WKSRYACT-REC
      *            MOVE    WRK-WKSRYACT-REC    TO  PARA-DATA-REC
      *            WRITE                       PARA-REC
      *            IF      STS-PARA        NOT =  ZERO
      *                MOVE    "0051"             TO  SPA-ERRCD
      ****         END-IF
      *v4.8        TBL_PARAに保存
                   ADD    1                    TO  WRK-EDANUM
                   MOVE    LNK-WKSRYACT-REC (IDX)  TO  WRK-WKSRYACT-REC
      *
                   INITIALIZE                      PARA-REC
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    "K02"               TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   MOVE    WRK-EDANUM          TO  PARA-EDANUM
                   MOVE    WRK-WKSRYACT-REC    TO  PARA-DATA-REC
      *
                   PERFORM 30010-PARA-DBINSERT-SEC
      *
               ELSE
                   MOVE    SPACE               TO  LNK-WKSRYACT-REC(IDX)
                   INITIALIZE                      LNK-WKSRYACT-REC(IDX)
               END-IF
           END-PERFORM
      *
           .
       45011-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    剤数チェック処理
      *****************************************************************
       450114-ZAICONUNT-SEC             SECTION.
      *
      *    入院で調剤料は対象外
           IF     (SPA-NYUGAIKBN       =   1  )  AND
                  (LNK-WKSRY-SRYKBN (IDX)  =   "24"
                                           OR  "26" )
               IF      LNK-WKSRY-AUTOKBN (IDX 1 )  =   "3"
                   GO      TO      450114-ZAICONUNT-EXT
               END-IF
           END-IF
      *
           IF      (WRK-HKNCOMBI       =   SPACE )  AND
                   (WRK-SRYKA          =   SPACE )
               MOVE    LNK-WKSRY-HKNCOMBI(IDX)     TO  WRK-HKNCOMBI
               MOVE    LNK-WKSRY-SRYKA   (IDX)     TO  WRK-SRYKA
               MOVE    ZERO                        TO  WRK-ZAINUM
               MOVE    ZERO                        TO  CNT-ZAI
           END-IF
           IF     (LNK-WKSRY-HKNCOMBI(IDX) =   WRK-HKNCOMBI )  AND
                  (LNK-WKSRY-SRYKA   (IDX) =   WRK-SRYKA    )
               IF      (LNK-WKSRY-ZAINUM  (IDX) =   WRK-ZAINUM   )
                   CONTINUE
               ELSE
                   MOVE    LNK-WKSRY-ZAINUM  (IDX)     TO  WRK-ZAINUM
                   ADD     1                   TO  CNT-ZAI
                   IF      CNT-ZAI             >   135
                       MOVE    "0006"              TO  SPA-ERRCD
                   END-IF
               END-IF
           ELSE
               MOVE    LNK-WKSRY-HKNCOMBI(IDX)     TO  WRK-HKNCOMBI
               MOVE    LNK-WKSRY-SRYKA   (IDX)     TO  WRK-SRYKA
               MOVE    ZERO                        TO  WRK-ZAINUM
               MOVE    ZERO                        TO  CNT-ZAI
           END-IF
           .
       450114-ZAICONUNT-EXT.
           EXIT.
      *****************************************************************
      *    初診料算定判定処理
      *****************************************************************
       450112-SYOSIN-CHK-SEC             SECTION.
      *
           IF      LNK-WKSRY-SRYKBN(IDX)   =   "11"  OR  "13"
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   5
                   EVALUATE    LNK-WKSRY-SRYCD  (IDX IDY)
      *                初診料
                       WHEN    "111000110"
                       WHEN    "111003610"
                       WHEN    "111700110"
                       WHEN    "111700210"
                       WHEN    "113003510"
                       WHEN    "113003710"
                       WHEN    "101110010"
      *                ダミー追加
                       WHEN    "099110001"
      *                紹介状なし追加
                       WHEN    "111012510"
      *H27.1           妥結率５割以下
                       WHEN    "111012710"
      *H28.4
                       WHEN    "113019710"
                       WHEN    "113019910"
      *H29.2           同一日２科目
                       WHEN    "111011810"
                       WHEN    "111012610"
                       WHEN    "111012810"
      *R02.4
      *               初診料（新型コロナウイルス感染症・臨時）
                       WHEN    "111013850"
                           MOVE    1             TO  FLG-SYOSIN-OK
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       450112-SYOSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報提供料判定処理
      *****************************************************************
       450113-YAKUJO-CHK-SEC             SECTION.
      *
           IF      LNK-WKSRY-SRYKBN(IDX)   =   "13"
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       IDY     >   5
                   EVALUATE    LNK-WKSRY-SRYCD  (IDX IDY)
      *                薬剤情報提供料
                       WHEN    "120002370"
                           MOVE    1               TO  SPA-YAKJYO-ARI
      *                後期高齢者薬剤情報提供料（老人手帳記載あり）
                       WHEN    "113701310"
                           MOVE    2               TO  SPA-YAKJYO-ARI
      *                後期高齢者診察料
                       WHEN    "113702110"
                           MOVE    3               TO  SPA-YAKJYO-ARI
                   END-EVALUATE
               END-PERFORM
           END-IF
           .
       450113-YAKUJO-CHK-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       45012-SYUNOU-SYORI-SEC             SECTION.
      *
      *    初期設定処理
           PERFORM 450121-GAIFTN-SYOKI-SEC
      *
      *!!!!!!!!!!!!!!
      *H27.7
      *サブプロ対応
           IF      SPA-SRYYMD          >=  "20151001"
               PERFORM 45012-SYUNOU-SYORI02-SEC
               GO      TO      45012-SYUNOU-SYORI-EXT
           END-IF
      *H27.7
      *
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-AREA
           INITIALIZE                  LNK-SYUNOU2-MAX
      *    福岡公費用
           MOVE    ZERO                TO  FLG-SYU40
      *
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  WRK-HKNCOMBI
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)   TO  WRK-SRYKA
                   PERFORM VARYING     IDH     FROM    1   BY  1
                           UNTIL      (IDH     >   15  )  OR
                                      (IDH     >   LNK-SYUNOU-MAX)
                       IF      (LNK-SYU-HKNCOMBINUM (IDH)
                                               =   WRK-HKNCOMBI ) AND
                               (LNK-SYU-SRYKA (IDH)
                                               =   WRK-SRYKA    )
                           PERFORM 45013-SYUNOU-HENSYU-SEC
                       END-IF
                   END-PERFORM
               END-PERFORM
               IF     (SPA-TBL-SRYKA (IDH1 2)  NOT =   SPACE)
      *            複数科合計追加
                   MOVE    ZERO            TO  WRK-SRYKA
                   PERFORM 45014-SYUNOU-ADD-SEC
               END-IF
           END-PERFORM
      *    複数科まとめ集計フラグ
           MOVE    SPA-KAGRPFLG        TO  LNK-S01-KAGRPFLG
      *
      *    負担金計算処理
           CALL    "ORCSGAIFTN"        USING
                                       MCPAREA
                                       LNK-ORCS01-REC
                                       LNK-SYUNOU2-AREA
                                       SPA-AREA
      *    現物給付メッセージフラグ
           MOVE    LNK-S01-GENBUTU-MSG-FLG     TO  SPA-FTNHEN-KBN
      *
           IF      SPA-KAGRPFLG        =   "0"
      *        複数科まとめ集計の差額計算
               PERFORM 45016-SYUNOU-SAGAKU-SEC
           ELSE
      *        複数科まとめ集計をしない場合、科合計を計算
               PERFORM 45015-SYUNOU-GOUKEI-SEC
           END-IF
      *
      *!!! MOVE    ZERO                TO  PARA-EDANUM
      *    PERFORM VARYING     IDX     FROM    1   BY  1
      *            UNTIL       IDX     >   LNK-SYUNOU2-MAX
      *        ADD     1                   TO  PARA-EDANUM
      *        MOVE    "SYUNOU"            TO  PARA-FILEMEI
      *        MOVE    LNK-SYUNOU2-REC(IDX)
      *                                    TO  PARA-DATA-REC
      *        WRITE                       PARA-REC
      *!!! END-PERFORM
      *
           .
       45012-SYUNOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期設定処理
      *****************************************************************
       450121-GAIFTN-SYOKI-SEC             SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  IDX-Y
           INITIALIZE                  WRK-CONTCHK-TBL
      *
      *    継続の時、前回までの内容をセット
           IF     (SPA-CONTKBN         NOT =   SPACE AND "0")  AND
                  (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
               PERFORM 45051-YAKFTN-MAESET-SEC
           END-IF
           IF     (SPA-CONTKBN             =   "1"  OR  "2" )  AND
                  (SPA-DOUJI-DENPNUM   NOT =   ZERO  )
               MOVE    WRK-IDX-Y           TO  IDX-Y
           ELSE
               INITIALIZE                  LNK-ORCS01-REC
               MOVE    ZERO                TO  IDX-Y
           END-IF
      *
           MOVE    ZERO                TO  IDY
           ADD     1                   TO  IDX-Y
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           MOVE    ZERO                TO  WRK-FUNSAI-CNT
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  FLG-FUNSAI
                   MOVE    ZERO            TO  WRK-FUNSAI-CNT
               END-IF
      *
      ****     IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
      *               (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
      *           OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
      ****            (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
               IF     (LNK-WKSRY-HKNCOMBI (IDX)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDX)    =   "H"   )
      *           薬評以外
                   OR (LNK-WKSRY-ZAIKBN   (IDX)    =   1    )
                   CONTINUE
               ELSE
      *            診療内容編集（薬剤と服用方法)（未使用）
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )
                       PERFORM 45051-ORCS01-SET-SEC
      *                特定公費（診察料未算定扱い）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409905"
                           MOVE    "1"         TO  LNK-S01-SINSATUFLG2
                       END-IF
      *                特定公費（時間外受診）判定
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409907"
                           MOVE    "1"         TO  LNK-S01-JIKANGAI
                       END-IF
      *H30.1
      *                第三者行為分計算優先フラグ
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099999931"
                           MOVE    "1"     TO  LNK-S01-DAISANYUSENFLG
                       END-IF
      *H30.11
      *                初診料算定フラグ２（地方公費用）
                       IF      LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   "099409906"
                           MOVE    "1"     TO  LNK-S01-SYOSINFLG2
                       END-IF
      *
                   END-PERFORM
               END-IF
      *        診察料算定フラグ（地方公費用）
               IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "12"
                                               OR  "13"
                                               OR  "14"
                   MOVE    "1"                 TO  LNK-S01-SINSATUFLG
               END-IF
           END-PERFORM
      *
      *    剤ごとに点数を入れ直す
           PERFORM VARYING     IDY     FROM    40 BY  -1
                   UNTIL       IDY     <   1
      *
               IF      LNK-S01-SRYKBN (IDX-Y IDY)  NOT =   SPACE
                   IF     (IDY                     =   40    )  OR
                          (LNK-S01-ZAIBAN (IDX-Y IDY)  NOT =
                                       LNK-S01-ZAIBAN (IDX-Y IDY + 1))
                       MOVE    LNK-S01-TENSUU (IDX-Y IDY)
                                               TO  WRK-S01-TENSU
                   END-IF
                   MOVE    WRK-S01-TENSU       TO  LNK-S01-TENSUU
                                                       (IDX-Y IDY)
      *
               END-IF
           END-PERFORM
      *    訂正処理
           IF      SPA-SYORIFLG        =   1
               MOVE    "1"             TO  LNK-S01-SAIKEISAN-KBN
           END-IF
      *Ｈ１４．１０以降
      *    高齢者（在宅総合・在宅末期）算定患者区分
           IF     ( SPA-SRYYMD         <   "20021001")  OR
                  ((SPA-NETAKIRI-ARI   =   ZERO )  AND
                   (SPA-NETAKIRI-ARI2  =   ZERO )  AND
                   (SPA-ZAIMATU-ARI    =   ZERO )  AND
                   (SPA-ZAIMATU-ARI2   =   ZERO )  AND
      *            Ｈ１８．４追加
                   (SPA-ZAIKANRI-ARI   =   ZERO )  AND
                   (SPA-ZAIKANRI-ARI2  =   ZERO )
      *            Ｈ２０．４追加（特定施設入居時医学総合管理料）
              AND  (SPA-IGAKUKAN-ARI   =   ZERO )
              AND  (SPA-IGAKUKAN-ARI2  =   ZERO )  )
               CONTINUE
           ELSE
      *        保険算定年齢が７０歳以上か、老人保険の時
               INITIALIZE                  ORCSAGECHKAREA
               MOVE    SPA-HOSPNUM         TO  AGECHK-HOSPNUM
               MOVE    SPA-PTID            TO  AGECHK-PTID
               MOVE    SPA-SRYYMD          TO  AGECHK-SRYYMD
               MOVE    SPA-BIRTHDAY        TO  AGECHK-BIRTHDAY
               MOVE    SPA-NYUGAIKBN       TO  AGECHK-NYUGAIKBN
               CALL    "ORCSAGECHK"        USING
                                           ORCSAGECHKAREA
                                           SPA-AREA
               IF     (AGECHK-NENREI       >=   70  )  OR
                      (SPA-NAI-ROUJIN      =    1   )
                   MOVE    "1"             TO
                                           LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
      *        Ｈ１９．４から年令に関係なくすべて対象
               IF     ( SPA-SRYYMD         >=  "20070401")
                   MOVE    "1"             TO
                                           LNK-S01-KOUREI-ZAITAKU-KBN
               END-IF
           END-IF
      *    初診料算定フラグ（地方公費用）
           IF      FLG-SYOSIN-OK       =   1
               MOVE    "1"             TO  LNK-S01-SYOSINFLG
           END-IF
      *
           .
       450121-GAIFTN-SYOKI-EXT.
           EXIT.
      *!!!!!!!!!!!!!!!!!11
      *H27.7
      *****************************************************************
      *    収納更新処理（サブプロ）
      *****************************************************************
       45012-SYUNOU-SYORI02-SEC             SECTION.
      *
           INITIALIZE                  ORCSCSYUNOUGAREA
           MOVE    "K08"               TO  ORCSCSYU-SYORIPG
      *    初期設定編集　済み
           MOVE    SPACE               TO  ORCSCSYU-SYORIKBN
           PERFORM VARYING   IDH1      FROM    1   BY  1
                   UNTIL    (IDH1      >   15  )   OR
                            (SPA-TBL-HKNCOMBI(IDH1)    =   SPACE)
               MOVE    SPA-TBL-HKNCOMBI(IDH1)     TO  ORCSCSYU-HKNCOMBI
                                                                (IDH1)
               PERFORM VARYING   IDH2      FROM   1   BY  1
                       UNTIL    (IDH2      >   10  )  OR
                                (SPA-TBL-SRYKA (IDH1 IDH2) =   SPACE )
                   MOVE    SPA-TBL-SRYKA (IDH1 IDH2)
                                               TO  ORCSCSYU-SRYKA
                                                           (IDH1 IDH2)
               END-PERFORM
           END-PERFORM
      *
           INITIALIZE                  PARA-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
      *        包括分は対象外
               IF     (LNK-WKSRY-HKNCOMBI (IDX)    =   "9999")
                   OR (LNK-WKSRY-CONTKBN  (IDX)    =   "H"   )
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF     (LNK-WKSRY-INPUTCD(IDX 1)(1:1)     =   "#"
                                                         OR  "$")
                   MOVE    1               TO  FLG-DEL
               END-IF
      *        削除分は対象外
               IF     (LNK-WKSRY-ZAINUM (IDX)  =   ZERO   )
                 AND  (LNK-WKSRY-SRYYMD (IDX)  =   SPACE  )
                   MOVE    1               TO  FLG-DEL
               END-IF
               IF      FLG-DEL             =   ZERO
                   ADD     1                   TO  IDX2
                   MOVE    LNK-WKSRYACT-REC (IDX)
                                               TO  PARA-WKSRYACT-REC
                                                               (IDX2)
                   MOVE    IDX2                TO  ORCSCSYU-WKSRYACT-MAX
               END-IF
           END-PERFORM
      *
           MOVE    SPACE               TO  LNK-SYUNOU2-AREA
           MOVE    LNK-SYUNOU-AREA     TO  LNK-SYUNOU2-AREA
           CALL    "ORCSCSYUNOUG"      USING
                                       ORCSCSYUNOUGAREA
                                       LNK-ORCS01-REC
                                       SPA-AREA
                                       LNK-SYUNOU2-AREA
                                       PARA-WKSRYACT-AREA
           MOVE    ORCSCSYU-SYUNOU-MAX TO  LNK-SYUNOU2-MAX
      *
      *H30.6
      *    収納オーバー
           IF      ORCSCSYU-ERRCD      =   9
               MOVE    "0013"          TO  SPA-ERRCD
           END-IF
      *    金額桁あふれ
           IF      ORCSCSYU-ERRCD      =   11
      *        請求額
               MOVE    "0011"          TO  SPA-ERRCD
           END-IF
           IF      ORCSCSYU-ERRCD      =   12
      *        請求額
               MOVE    "0012"          TO  SPA-ERRCD
           END-IF
      *H30.6
      *    収納請求金額チェック処理
           PERFORM 45017-SYUNOU-GOKCHK-SEC
      *
      *    現物給付メッセージフラグ
           MOVE    LNK-S01-GENBUTU-MSG-FLG     TO  SPA-FTNHEN-KBN
      *
           IF      SPA-KAGRPFLG        =   "0"
      *        複数科まとめ集計の差額計算
               PERFORM 45016-SYUNOU-SAGAKU-SEC
           ELSE
      *        複数科まとめ集計をしない場合、科合計を計算
               PERFORM 45015-SYUNOU-GOUKEI-SEC
           END-IF
           .
       45012-SYUNOU-SYORI02-EXT.
           EXIT.
      *!!!!!!!!!!!!!!!!!11
      *H27.7
      *
      *****************************************************************
      *    収納更新処理
      *****************************************************************
       45013-SYUNOU-HENSYU-SEC             SECTION.
      *
           MOVE    LNK-SYUNOU-REC (IDH)    TO  SYUNOU-REC
           INITIALIZE                  WRK-SUM-AREA
      *
           MOVE    ZERO                TO  SYU-HKNTEN(02)
           MOVE    ZERO                TO  SYU-HKNTEN(04)
           MOVE    ZERO                TO  SYU-HKNTEN(05)
           MOVE    ZERO                TO  SYU-HKNTEN(06)
           MOVE    ZERO                TO  SYU-HKNTEN(07)
           MOVE    ZERO                TO  SYU-HKNTEN(08)
           MOVE    ZERO                TO  SYU-HKNTEN(09)
           MOVE    ZERO                TO  SYU-HKNTEN(10)
           MOVE    ZERO                TO  SYU-HKNTEN(11)
           MOVE    ZERO                TO  SYU-HKNTEN(12)
           MOVE    ZERO                TO  SYU-HKNTEN(13)
           MOVE    ZERO                TO  SYU-HKNTEN(14)
           MOVE    ZERO                TO  SYU-HKNTEN(15)
           MOVE    ZERO                TO  SYU-HKNTEN(16)
           MOVE    ZERO                TO  SYU-SHOHOU-SAI
      *    各点数
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >  MAX-GMN
               IF     (SPA-NAI-HKNCOMBI(IDY)   =   LNK-SYU-HKNCOMBINUM
                                                             (IDH)) AND
                     ((LNK-SYU-SRYKA   (IDH)   =   ZERO      )  OR
                      (SPA-NAI-SRYKA   (IDY)   =   LNK-SYU-SRYKA
                                                             (IDH))) AND
                      (SPA-NAI-DELFLG (IDY)    NOT =   1  )
                   PERFORM 450120-HKNTEN-KEI-SEC
               END-IF
           END-PERFORM
           MOVE    ZERO                TO  SYU-HKNTEN(17)
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   16
      *        保険点数
               IF      SUM-SYU-HKNTEN(IDX)     <   ZERO
                   MOVE    ZERO                TO  SUM-SYU-HKNTEN (IDX)
                   MOVE    "0010"              TO  SPA-ERRCD
               END-IF
               IF      IDX                     =   1
                                               OR  3
      *            診察料と在宅は再計算なし（剤削除なし）
                   CONTINUE
               ELSE
                   MOVE    SUM-SYU-HKNTEN(IDX) TO  SYU-HKNTEN(IDX)
               END-IF
      *        保険合計点数
               ADD     SYU-HKNTEN(IDX)         TO  SYU-HKNTEN(17)
           END-PERFORM
      *
           ADD     1                   TO  LNK-SYUNOU2-MAX
           MOVE    ZERO                TO  IDX-S1
      *
      *    薬剤一部負担金計算処理
           PERFORM 4505-YAKFTN-KEI-SEC
      *
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
           .
       45013-SYUNOU-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       45014-SYUNOU-ADD-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-HKNCOMBINUM(IDS1)
                                           =   WRK-HKNCOMBI
                   IF      SYU-PTID        =   ZERO
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                           TO  SYUNOU-REC
                   ELSE
                       MOVE    LNK-SYUNOU2-REC (IDS1)
                                               TO  WRK-SYUNOU-REC
                       PERFORM 450141-SYUNOU-ADD-HEN-SEC
                   END-IF
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SYU-SRYKA
           ADD     1                   TO  LNK-SYUNOU2-MAX
           MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC
                                                   (LNK-SYUNOU2-MAX)
      *
           .
       45014-SYUNOU-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納更新・科合計追加処理
      *****************************************************************
       450141-SYUNOU-ADD-HEN-SEC       SECTION.
      *
           ADD     WRK-SYU-TAX-TAISHOU TO  SYU-TAX-TAISHOU
           ADD     WRK-SYU-TAX-MONEY   TO  SYU-TAX-MONEY
           ADD     WRK-SYU-SKYGK       TO  SYU-SKYGK
           ADD     WRK-SYU-HKNTEKMONEY TO  SYU-HKNTEKMONEY
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   17
               ADD    WRK-SYU-HKNTEN (IDS2)    TO  SYU-HKNTEN(IDS2)
               ADD    WRK-SYU-MONEY (IDS2)     TO  SYU-MONEY (IDS2)
               ADD    WRK-SYU-TGMONEY (IDS2)   TO  SYU-TGMONEY(IDS2)
               ADD    WRK-SYU-TGMONEY-TAX (IDS2)
                                               TO  SYU-TGMONEY-TAX(IDS2)
           END-PERFORM
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   10
               ADD    WRK-SYU-JIHI-TAXNASI (IDS2)
                                           TO  SYU-JIHI-TAXNASI(IDS2)
               ADD    WRK-SYU-JIHI-TAXARI (IDS2)
                                           TO  SYU-JIHI-TAXARI(IDS2)
           END-PERFORM
      *    処方せん料再掲
           ADD     WRK-SYU-SHOHOU-SAI      TO  SYU-SHOHOU-SAI
           ADD     WRK-SYU-JIHI-TOTAL      TO  SYU-JIHI-TOTAL
           ADD     WRK-SYU-JIHI-TOTAL-TAX  TO  SYU-JIHI-TOTAL-TAX
           ADD     WRK-SYU-JIHI-TAX        TO  SYU-JIHI-TAX
           ADD     WRK-SYU-RJN-FTN         TO  SYU-RJN-FTN
           ADD     WRK-SYU-KOH-FTN         TO  SYU-KOH-FTN
           ADD     WRK-SYU-KOH-FTN-ENTANI  TO  SYU-KOH-FTN-ENTANI
           ADD     WRK-SYU-YKZ-FTN         TO  SYU-YKZ-FTN
      *調整金
           ADD     WRK-SYU-CHOSEI          TO  SYU-CHOSEI
           ADD     WRK-SYU-CHOSEI1         TO  SYU-CHOSEI1
           ADD     WRK-SYU-CHOSEI2         TO  SYU-CHOSEI2
      *労災保険
           ADD     WRK-SYU-RSISHOSHIN-MONEY
                                           TO  SYU-RSISHOSHIN-MONEY
           ADD     WRK-SYU-RSISAISHIN-MONEY
                                           TO  SYU-RSISAISHIN-MONEY
           ADD     WRK-SYU-RSISDO-MONEY    TO  SYU-RSISDO-MONEY
           ADD     WRK-SYU-RSIETC-MONEY    TO  SYU-RSIETC-MONEY
           ADD     WRK-SYU-RSI-TAX-SAI     TO  SYU-RSI-TAX-SAI
           ADD     WRK-SYU-RSI-TOTAL       TO  SYU-RSI-TOTAL
           ADD     WRK-SYU-RSIJIBAI-SKYMONEY
                                           TO  SYU-RSIJIBAI-SKYMONEY
      *請求金額−消費税（再掲）
           ADD     WRK-SYU-SKYMONEY-TAX-SAI TO  SYU-SKYMONEY-TAX-SAI
      *請求金額
           ADD     WRK-SYU-SKYMONEY        TO  SYU-SKYMONEY
      *入金合計額
           ADD     WRK-SYU-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL 
      *診療区分別給付外点数
           PERFORM VARYING     IDS2    FROM    1   BY  1
                   UNTIL       IDS2    >   6
               ADD    WRK-SYU-KYUFUGAI-TEN (IDS2)
                                       TO  SYU-KYUFUGAI-TEN(IDS2)
           END-PERFORM
      *
           .
       450141-SYUNOU-ADD-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ集計の差額計算処理
      *****************************************************************
       45016-SYUNOU-SAGAKU-SEC             SECTION.
      *
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    ZERO                TO  WRK-SKYMONEY
                   MOVE    LNK-SYUNOU2-REC (IDS1)  TO  SYUNOU-REC
                   PERFORM VARYING     IDS2    FROM    1   BY  1
                           UNTIL       IDS2    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           ADD     LNK-SYU2-SKYMONEY(IDS2)
                                               TO  WRK-SKYMONEY
                       END-IF
                   END-PERFORM
                   COMPUTE SYU-GRP-SGKMONEY    =   SYU-SKYMONEY
                                               -   WRK-SKYMONEY
                   MOVE    SYUNOU-REC          TO  LNK-SYUNOU2-REC(IDS1)
      *            収納に差額編集、請求額変更
                   MOVE    ZERO                TO  IDS3
                   MOVE    SYU-GRP-SGKMONEY    TO  WRK-GRP-SGKMONEY
                   PERFORM VARYING     IDS2    FROM    LNK-SYUNOU2-MAX
                                                       BY  -1
                           UNTIL      (IDS2    <   1   )  OR
                                      (IDS3    >   ZERO )
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS2) 
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS2)
                                           NOT =   ZERO  )
                           IF     (SYU-GRP-SGKMONEY    >   ZERO ) OR
                                  (LNK-SYU2-SKYMONEY(IDS2)
                                               >=  WRK-GRP-SGKMONEY)
                               MOVE    IDS2        TO  IDS3
                           END-IF
                       END-IF
                   END-PERFORM
                   IF      IDS3                >   ZERO
                       MOVE    SYU-GRP-SGKMONEY
                                              TO  LNK-SYU2-GRP-SGKMONEY
                                                                (IDS3)
                       COMPUTE LNK-SYU2-SKYMONEY(IDS3)
                                               =   LNK-SYU2-SKYMONEY
                                                                (IDS3)
                                               +   SYU-GRP-SGKMONEY
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       45016-SYUNOU-SAGAKU-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ集計なしの収納合計作成処理
      *****************************************************************
       45015-SYUNOU-GOUKEI-SEC             SECTION.
      *
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   MOVE    LNK-SYU2-HKNCOMBINUM(IDS1)
                                               TO  WRK-HKNCOMBI
                   MOVE    SPACE               TO  SYUNOU-REC
                   INITIALIZE                  SYUNOU-REC
                   PERFORM VARYING     IDS3    FROM    1   BY  1
                           UNTIL       IDS3    >   LNK-SYUNOU2-MAX
                       IF     (LNK-SYU2-HKNCOMBINUM(IDS3)
                                               =   WRK-HKNCOMBI ) AND
                              (LNK-SYU2-SRYKA (IDS3)
                                           NOT =   ZERO  )
                           IF      SYU-PTID        =   ZERO
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  SYUNOU-REC
                               MOVE    ZERO        TO  SYU-SRYKA
                           ELSE
                               MOVE    LNK-SYUNOU2-REC (IDS3)
                                                   TO  WRK-SYUNOU-REC
                               PERFORM 450141-SYUNOU-ADD-HEN-SEC
                           END-IF
                       END-IF
                   END-PERFORM
                   MOVE    SYUNOU-REC      TO  LNK-SYUNOU2-REC (IDS1)
               END-IF
           END-PERFORM
      *
           .
       45015-SYUNOU-GOUKEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納請求金額チェック処理
      *****************************************************************
       45017-SYUNOU-GOKCHK-SEC        SECTION.
      *
          MOVE    SPACE               TO  SYUNOU-REC
          INITIALIZE                  SYUNOU-REC
          PERFORM VARYING     IDS1    FROM    1   BY  1
                   UNTIL       IDS1    >   LNK-SYUNOU2-MAX
               IF      LNK-SYU2-SRYKA (IDS1)   =   ZERO
                   CONTINUE
               ELSE
      *            請求金額の桁あふれチェック
                   IF     (LNK-SYU2-SKYMONEY(IDS1)
                                       +   SYU-SKYMONEY)
                                       >   9999999
                       MOVE    "0012"         TO  SPA-ERRCD
                   END-IF
                   ADD    LNK-SYU2-SKYMONEY(IDS1)
                                               TO  SYU-SKYMONEY
               END-IF
           END-PERFORM
      *
           .
       45017-SYUNOU-GOKCHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       450120-HKNTEN-KEI-SEC             SECTION.
      *
           MOVE    SPA-NAI-SRYKBN (IDY)    TO  WRK-SRYKBN
           MOVE    SPA-NAI-SRYSYUKBN (IDY) TO  WRK-SRYSYUKBN
      *    集計位置決定
           PERFORM 410321-SYUNOU-TENSU-SEC
           IF      IDX-TS                  >   ZERO
               IF      SPA-NAI-HKNSFLG (IDY)   =   ZERO
                   ADD     SPA-NAI-HKNTEN (IDY)
                                           TO  SUM-SYU-HKNTEN (IDX-TS)
               ELSE
                   COMPUTE SUM-SYU-HKNTEN (IDX-TS)
                                           =   SUM-SYU-HKNTEN (IDX-TS)
                                           -   SPA-NAI-HKNTEN (IDY)
               END-IF
      *        処方せん料
               IF      SPA-NAI-SRYSYUKBN (IDY) =   "820"
      *            処方せん料再掲
                   ADD     SPA-NAI-HKNTEN(IDY)
                                               TO  SYU-SHOHOU-SAI
               END-IF
           END-IF
      *
           .
       450120-HKNTEN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤一部負担金計算処理
      *****************************************************************
       4505-YAKFTN-KEI-SEC             SECTION.
      *
           MOVE    ZERO                TO  WRK-LNK-S01-SYUTEN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
      *
               IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                      (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
                  OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                      (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
      *           包括剤対象外
                  OR  (LNK-WKSRY-CONTKBN (IDX)     =   "H"   )
                   CONTINUE
               ELSE
      *            自賠責の時、技術点を集計する
                   IF      (SPA-HKNKBN         =   1   )  AND
                           (SPA-RSI-HKNKBN     =   "4"
                                               OR  "5"
      *                    第三者行為追加
                                               OR  "6"  ) 
      ************   AND   (LNK-WKSRY-SYUTEN1 (IDX)    >   ZERO )
                       PERFORM 45052-JIBAI-SYORI-SEC
                   END-IF
      *            公害保険の時、技術点を集計する
                   IF      (SPA-HKNKBN         =   2   )  AND
                           (LNK-WKSRY-SYUTEN1 (IDX)    NOT =  ZERO )
                       PERFORM 45052-KOGAI-SYORI-SEC
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-LNK-S01-SYUTEN  TO  LNK-S01-SYUTEN
                                                   (LNK-SYUNOU2-MAX)
      *
      *    船員保険で初診料算定時
           IF     (SYU-SYUHKNNUM       =   "002" )  AND
                  (SYU-HKNTEN (01)     >   ZERO  )  AND
                  (SPA-SYOSIN          =   1     )
               PERFORM 45053-SENINFLG-SET-SEC
           END-IF
      *
      *福岡県公費対応
      *    福岡県で、一般保険算定時
           IF     (SPA-PREFNUM         =   40   )  AND
                  (SPA-HKNKBN          =   ZERO )
               PERFORM 45054-PREFNUM-40-SEC
           END-IF
      *
      *test
      *    DISPLAY "K08 S01-SYUTEN:" LNK-S01-SYUTEN(LNK-SYUNOU2-MAX)
      *       ",SENINFLG:" LNK-S01-SENINFLG (LNK-SYUNOU2-MAX)
      *       ",KYUKYU:" LNK-S01-KYUKYU (LNK-SYUNOU2-MAX)
      *       ",RYOYO:" LNK-S01-RYOYO (LNK-SYUNOU2-MAX)
      *    DISPLAY "K08 S01-SINDAN:" LNK-S01-SINDAN(LNK-SYUNOU2-MAX)
      *       ",MEISAI:" LNK-S01-MEISAI (LNK-SYUNOU2-MAX)
      *       ",SRYCD1:" LNK-S01-SONOTA-SRYCD (LNK-SYUNOU2-MAX 1)
      *       ",SRYCD2:" LNK-S01-SONOTA-SRYCD (LNK-SYUNOU2-MAX 2)
      *TEST
           .
       4505-YAKFTN-KEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用セット処理
      *****************************************************************
       45051-ORCS01-SET-SEC            SECTION.
      *
      *    粉砕の時最初の１剤のみとする
           IF      LNK-WKSRY-SRYCD(IDX IDS)    =   "099209901"
               ADD     1                   TO  WRK-FUNSAI-CNT
               MOVE    1                   TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
           IF     (FLG-FUNSAI          =   1   )  AND
                  (WRK-FUNSAI-CNT      >   1   )
               MOVE    ZERO                TO  FLG-FUNSAI
               GO      TO      45051-ORCS01-SET-EXT
           END-IF
      *
           MOVE    ZERO                TO  FLG-FUNSAI
           IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "6"  ) OR
                  (LNK-WKSRY-SRYCD(IDX IDS)(1:3) =   "001")
      *        院内薬剤のみとする
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX) TO
                                                   WRK-CHK-SRYSYUKBN
               IF      WRK-CHK-SRYSYUKBN       =   SPACE
                   EVALUATE    LNK-WKSRY-SRYKBN (IDX)
                       WHEN    "21"
                           MOVE    "210"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "22"
                           MOVE    "220"     TO  WRK-CHK-SRYSYUKBN
                       WHEN    "23"
                           MOVE    "230"     TO  WRK-CHK-SRYSYUKBN
                   END-EVALUATE
               END-IF
               IF     (WRK-CHK-SRYSYUKBN           =   "211"  OR
                                                       "221"  OR
                                                       "231"  OR
                                                       "291"  OR
                                                       "294" )  OR
                     ((SPA-INGAIKBN            NOT =   "1"   ) AND
                      (WRK-CHK-SRYSYUKBN           =   "210"  OR
                                                       "220"  OR
                                                       "230"  OR
                                                       "290"  OR
                                                       "293" ))
                   ADD     1                   TO  IDY
                   IF      IDY                 >   40
                       GO      TO      45051-ORCS01-SET-EXT
                   END-IF
                   MOVE    LNK-WKSRY-SRYKBN(IDX)
                                                   TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAINUM (IDX)
                                                   TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                                   TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDS)
                                                   TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                   MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                                   TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
               END-IF
           END-IF
      *
           .
       45051-ORCS01-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    自賠責の時、技術点を集計処理
      *****************************************************************
       45052-JIBAI-SYORI-SEC          SECTION.
      *
      *    診察料と診断料等の時、診療コードを判定する
           MOVE    ZERO                TO  WRK-SYUTEN1
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"  OR  "12"  OR
                                               "13"  OR  "80"
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
      *
                   MOVE    ZERO                TO  FLG-RSIFLG
                   MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            労災の金額算定分の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYCD (IDX IDS)(1:3)
                                               =   "101" ) AND
                          (TNS-TENSIKIBETU     =   1    )
                       MOVE    1                   TO  FLG-RSIFLG
      *H26.2
      *                労災金額％加算対応
                       PERFORM 450521-RSI-SKASAN-SEC
                   END-IF
      *            自賠責で証明料等の時、点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591" OR
                                                           "09592" OR
                                                           "09593" OR
                                                           "09594" )
                       MOVE    1                   TO  FLG-RSIFLG
                   END-IF
      *            自賠責金額入力
                   IF      LNK-WKSRY-SRYSYUKBN(IDX)    =   "809"
                       MOVE    LNK-WKSRY-JIHIMONEY(IDX IDS)
                                                   TO  TNS-TEN
                   END-IF
      *
                   IF      FLG-RSIFLG          =   1
                       COMPUTE WRK-TEN         =   TNS-TEN   /   10
                       ADD     WRK-TEN         TO  WRK-SYUTEN1
                   END-IF
      *            自賠責金額集計
                   IF      LNK-WKSRY-SRYKBN(IDX)       =   "80"
      *                自賠責診断書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-SINDAN
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
      *                自賠責明細書料集計
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09592"
                           COMPUTE WRK-SINDAN  =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                           ADD     WRK-SINDAN      TO  LNK-S01-MEISAI
                                                      (LNK-SYUNOU2-MAX)
                       END-IF
      *                自賠責その他のコード編集
                       IF      LNK-WKSRY-SRYCD (IDX IDS)(1:5)
                                                       =   "09591"
                                                       OR  "09592"
                                                       OR  "09593"
                           ADD     1               TO  IDX-S1
                           IF      IDX-S1          <=  20
                               MOVE    LNK-WKSRY-SRYCD (IDX IDS)
                                           TO   LNK-S01-SONOTA-SRYCD
                                                (LNK-SYUNOU2-MAX IDX-S1)
                               MOVE    LNK-WKSRY-ZAIKAIKEI (IDX)
                                           TO   LNK-S01-SONOTA-ZAIKAISU
                                                (LNK-SYUNOU2-MAX IDX-S1)
                               MOVE    TNS-TEN
                                           TO   LNK-S01-SONOTA-TANKA
                                                (LNK-SYUNOU2-MAX IDX-S1)
                           END-IF
                       END-IF
                   END-IF
      *            療養担当手当の点数をゼロとする
                   IF     (LNK-WKSRY-SRYKBN(IDX)       =   "80")  AND
                          (LNK-WKSRY-SRYCD (IDX IDS)   =   "199000610")
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                       ADD     WRK-SINDAN      TO  LNK-S01-RYOYO
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *H25.7
      *            救急医療管理加算点数
                   IF     (SPA-SRYYMD                  >=  "20130701")
                     AND  (LNK-WKSRY-SRYKBN(IDX)       =   "80"      )
                     AND  (LNK-WKSRY-SRYCD (IDX IDS)   =   "101800890")
      *                自賠責療養担当手当点数
                       COMPUTE WRK-SINDAN      =   TNS-TEN
                                               *   LNK-WKSRY-ZAIKAIKEI
                                                               (IDX)
                       MOVE    WRK-SINDAN      TO  LNK-S01-KYUKYU
                                                      (LNK-SYUNOU2-MAX)
                   END-IF
      *
               END-PERFORM
           END-IF
      *
      *    薬剤料逓減は対象外
           IF      LNK-WKSRY-SRYCD (IDX 01)    =   "630010002"
               GO      TO      45052-JIBAI-SYORI-EXT
           END-IF
      *H26.10
      *    薬剤料逓減は対象外
           IF     (LNK-WKSRY-SRYCD (IDX 01)(1:1)   =   "6" )
             AND  (LNK-WKSRYP-PLUSKBN (01)     =   1   )
               GO      TO      45052-JIBAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    腰部固定帯加算を器材点数として対象外とする
      *    自賠責固定帯加算等取扱を判定する
           IF     (SYS-4001-KOTEIKBN    =   "2"  OR   SPACE )  AND
                  (LNK-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK-WKSRY-SRYCD(IDX IDS)     =   "140037490"
                                                       OR  "140040110"
                                                       OR  "150266970"
      *                    頸部固定帯加算
                                                       OR  "140052610"
                       MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *
      *    自賠責手技点合計
           IF      LNK-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (LNK-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1  )
                                           *   LNK-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       45052-JIBAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    労災金額％加算再計算対応処理
      *****************************************************************
       450521-RSI-SKASAN-SEC               SECTION.
      *
           MOVE    TNS-TENSU-REC       TO  TNSWK-TENSU-REC
           MOVE    ZERO                TO  FLG-RSIKSN
           PERFORM VARYING     IDR-2   FROM    IDS   BY  1
                   UNTIL      (IDR-2         >   5   )  OR
                              (LNK-WKSRY-SRYCD(IDX IDR-2)
                                              =   SPACE )
               IF     (LNK-WKSRY-SRYCD (IDX IDR-2)(1:3)
                                               =   "101" )
                   MOVE    LNK-WKSRY-SRYCD (IDX IDR-2)   TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
                   IF      TNS-TENSIKIBETU     =   5
                                               OR  6
                       MOVE    1               TO  FLG-RSIKSN
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-RSIKSN          =   1
      *        ％加算ありは、剤点数を対象とする
               ADD     LNK-WKSRY-SYUTEN1 (IDX) TO  WRK-SYUTEN1
               MOVE    ZERO                TO  FLG-RSIFLG
           END-IF
           MOVE    TNSWK-TENSU-REC     TO  TNS-TENSU-REC
           .
       450521-RSI-SKASAN-EXT.
           EXIT.
      *
      *****************************************************************
      *    公害保険の時、技術点を集計処理
      *****************************************************************
       45052-KOGAI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SYUTEN1
      *
      *    薬剤料逓減は対象外
           IF      LNK-WKSRY-SRYCD (IDX 01)    =   "630010002"
               GO      TO      45052-KOGAI-SYORI-EXT
           END-IF
      *H26.10
      *    薬剤料逓減は対象外
           IF     (LNK-WKSRY-SRYCD (IDX 01)(1:1)   =   "6" )
             AND  (LNK-WKSRYP-PLUSKBN (01)     =   1  )
               GO      TO      45052-KOGAI-SYORI-EXT
           END-IF
      *
      *    画像診断の時、フィルム点数を対象外とする
           IF      LNK-WKSRY-SRYKBN (IDX)  =   "70"
               MOVE    LNK-WKSRY-SYUTEN2 (IDX)     TO  WRK-SYUTEN1
           END-IF
      *
      *    公害固有のコードを対象外とする
           IF     (LNK-WKSRY-SRYKBN(IDX)(1:1)  NOT =   "9"  )
               PERFORM VARYING     IDS     FROM    1   BY  1
                       UNTIL      (IDS         >   5   )  OR
                                  (LNK-WKSRY-SRYCD(IDX IDS)
                                              =   SPACE )
                   IF     LNK-WKSRY-SRYCD(IDX IDS)(1:3)
                                                   =   "102"
                       MOVE    LNK-WKSRY-SRYCD (IDX IDS)   TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                       ADD     TNS-TEN         TO  WRK-SYUTEN1
                   END-IF
               END-PERFORM
           END-IF
      *    自賠責手技点合計
           IF      LNK-WKSRY-SYUTEN1 (IDX) <   WRK-SYUTEN1
               CONTINUE
           ELSE
               COMPUTE WRK-SYUTEN1-KEI     =  (LNK-WKSRY-SYUTEN1 (IDX)
                                           -   WRK-SYUTEN1  )
                                           *   LNK-WKSRY-ZAIKAIKEI
                                                             (IDX)
               IF      LNK-WKSRY-ZAIKBN (IDX) =   2
      *            減点分
                   COMPUTE WRK-SYUTEN1-KEI     =   WRK-SYUTEN1-KEI
                                               *   -1
               END-IF
               COMPUTE WRK-LNK-S01-SYUTEN  =   WRK-LNK-S01-SYUTEN
                                           +   WRK-SYUTEN1-KEI
           END-IF
      *
           .
       45052-KOGAI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    船員保険で初診料算定時セット処理
      *****************************************************************
       45053-SENINFLG-SET-SEC            SECTION.
      *
      *    保険組合マスター編集
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM        TO  COMB-HOSPNUM
           MOVE    SPA-PTID           TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM    TO  COMB-HKNCOMBINUM
      *
           MOVE    HKNCOMBI-REC        TO  MCPDATA-REC
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_hkncombi"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-HKNCOMBI-READ-SEC
           ELSE
               MOVE    1               TO  FLG-HKNCOMBI
               INITIALIZE                  HKNCOMBI-REC
           END-IF
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-HKNCOMBI        =   ZERO
      *        本人で通勤災害
               IF     (COMB-HONKZKKBN      =   "1"  )  AND
                      (COMB-HOJOKBN        =   "3"  OR
                                               "C"  OR
                                               "F"  OR
                                               "I"   )
                   MOVE    "1"             TO  LNK-S01-SENINFLG
                                                      (LNK-SYUNOU2-MAX)
               END-IF
           END-IF
           .
       45053-SENINFLG-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    福岡県公費対応処理
      *****************************************************************
       45054-PREFNUM-40-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDX-SYU
           MOVE    ZERO                TO  IDX-SYU2
           INITIALIZE                  ORCSSYU40AREA
           MOVE    "1"                 TO  ORCS40-KBN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   LNK-WKSRYACT-MAX )  OR
                              (IDX-SYU >=  5                )
      *
           IF     (LNK-WKSRY-SRYYMD (IDX)      =   SPACE ) OR
                  (LNK-WKSRY-HKNCOMBI(IDX) NOT =   SYU-HKNCOMBINUM)
              OR  ((SYU-SRYKA              NOT =   ZERO  )  AND
                  (LNK-WKSRY-SRYKA   (IDX) NOT =   SYU-SRYKA  ))
               CONTINUE
           ELSE
      *        剤毎にクリアする
               IF     (IDX                 >   1    )  AND
                      (LNK-WKSRY-ZAINUM (IDX) NOT =
                                           LNK-WKSRY-ZAINUM (IDX - 1))
                   MOVE    ZERO            TO  IDX-SYU2
               END-IF
      *
      *        初診・往診、小児科外来診察料の初診時
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11"
                                               OR  "14" )  OR
                     ((SPA-SYONIKA-ARI         =   1    )  AND
                      (SPA-SYOSIN              =   1    )  AND
                      (LNK-WKSRY-SRYKBN (IDX)  =   "13"
                                               OR  "99"  ))
      *            連番が１の時、テーブル編集
                   IF      LNK-WKSRY-RENNUM (IDX)  =   1
                       ADD     1                   TO  IDX-SYU
                       MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  ORCS40-SRYKBN
                                                           (IDX-SYU)
                   END-IF
                   MOVE    LNK-WKSRY-ZAITENKEI (IDX)   TO  ORCS40-ZAITEN
                                                           (IDX-SYU)
      *            診療内容編集
                   PERFORM VARYING     IDS     FROM    1   BY  1
                           UNTIL      (IDS         >   5   )  OR
                                      (LNK-WKSRY-SRYCD(IDX IDS)
                                                   =   SPACE )  OR
                                      (IDX-SYU2    >=  5     )
                       IF     (LNK-WKSRY-SRYCD(IDX IDS)(1:1) =   "1") OR
                              (LNK-WKSRY-SRYCD(IDX IDS) =   "099114001")
                            ADD     1              TO  IDX-SYU2
                            MOVE    LNK-WKSRY-SRYCD(IDX IDS)
                                                   TO  ORCS40-SRYCD
                                                     (IDX-SYU IDX-SYU2)
                       END-IF
                   END-PERFORM
      *
               END-IF
      *
               END-IF
           END-PERFORM
      *test
      ***  DISPLAY "K08 IDX-SYU:" IDX-SYU
      *
      *    対象があった場合
           IF      IDX-SYU             >   ZERO
      *        給付対象外点数編集
               CALL    "ORCSSYU40"     USING
                                       SPA-AREA
                                       ORCSSYU40AREA
                                       SYUNOU-REC
      *test
      *    DISPLAY "K08 FLG-SYU40:" FLG-SYU40
      *       ",SYU-KYUFUGAI-GOKEI-TEN:" SYU-KYUFUGAI-GOKEI-TEN
      *       ",SYU-KYUFUGAI-SHOSHIN-TEN:" SYU-KYUFUGAI-SHOSHIN-TEN
      *test
      *        初診料と同日初診がある時、初診料のみ
               IF      FLG-SYU40           =   1
                   COMPUTE SYU-KYUFUGAI-GOKEI-TEN
                                           =   SYU-KYUFUGAI-GOKEI-TEN
                                           -   SYU-KYUFUGAI-SHOSHIN-TEN
                   MOVE    ZERO            TO  SYU-KYUFUGAI-SHOSHIN-TEN
               END-IF
               IF      SYU-KYUFUGAI-SHOSHIN-TEN    >   ZERO
                   MOVE    1               TO  FLG-SYU40
               END-IF
           ELSE
      *        診療区分別給付外点数クリア
               INITIALIZE              SYU-KYUFUGAI
           END-IF
      *
           .
       45054-PREFNUM-40-EXT.
           EXIT.
      *
      *****************************************************************
      *    一部負担額計算用診療行為コードセット処理
      *****************************************************************
       45051-YAKFTN-MAESET-SEC            SECTION.
      *
           INITIALIZE                  LNK-ORCS01-REC
           MOVE    ZERO                TO  WRK-IDX-Y
           MOVE    ZERO                TO  IDX-Y
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *****MOVE    SPA-DOUJI-RENNUM    TO  JYURRK-RENNUM
           IF      SPA-SYORIFLG        =   ZERO
               MOVE    SPA-DOUJI-RENNUM    TO  JYURRK-RENNUM
           ELSE
               MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           MOVE    ZERO                TO  FLG-CHK
           PERFORM
               UNTIL    FLG-JYURRK         =   1
            IF    ((SPA-SYORIFLG           =   1    ) AND
      **********    SPA-DOUJI-RENNUM    NOT =   JYURRK-DOUJI-RENNUM) OR
                   (SPA-DOUJI-RENNUM       >    JYURRK-DOUJI-RENNUM)) OR
                   (SPA-SYORIFLG           =   ZERO      )
               IF      JYURRK-EDANUM       =   1
                   MOVE    ZERO                TO  FLG-CHK
               END-IF
      *
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX                 >   15  )  OR
                                  (JYURRK-ZAINUM(IDX)  =   SPACE)
      *            診療会計マスタ読み込み
                   MOVE    SPACE               TO  SRYACCT-REC
                   INITIALIZE                      SRYACCT-REC
                   MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
                   MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
                   MOVE    SPA-PTID            TO  ACCT-PTID
                   MOVE    SPA-SRYKA           TO  ACCT-SRYKA
                   MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
                   MOVE    JYURRK-ZAINUM(IDX)  TO  ACCT-ZAINUM
      *
                   MOVE    SRYACCT-REC         TO  MCPDATA-REC
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   ELSE
                       MOVE    1                   TO  FLG-SRYACCT
                   END-IF
                   PERFORM UNTIL    FLG-SRYACCT    =   1
      *
                       IF    ((ACCT-SRYKBN(1:1)    =   "2"  )  OR
                              (ACCT-SRYKBN         =   "80" ))
                         AND  (ACCT-ZAITEN         >   ZERO )
      *                    診療行為マスター編集
                           PERFORM 450511-YAKFTN-SRYACT-SEC
                       END-IF
      *
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key5"              TO  MCP-PATHNAME
                       PERFORM 940-SRYACCT-READ-SEC
                   END-PERFORM
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
               END-PERFORM
           END-IF
      *
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    IDX-Y               TO  WRK-IDX-Y
           .
       45051-YAKFTN-MAESET-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター編集処理
      *****************************************************************
       450511-YAKFTN-SRYACT-SEC            SECTION.
      *
           COMPUTE IDX-Y3              =   JYURRK-RENNUM
                                       +   1
      *ver.4.8
      **** IF      IDX-Y3              >   4
      *        MOVE    4               TO  IDX-Y3
      **** END-IF
           MOVE    ZERO                TO  WRK-ACCT-KAISU
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-Y2
           MOVE    ACCT-DAY (IDX-Y3 IDX-Y2)    TO  WRK-ACCT-KAISU
      *   診療行為マスター編集
           IF      FLG-CHK             =   ZERO
               ADD     1                   TO  IDX-Y
               MOVE    ZERO                TO  IDY
               MOVE    1                   TO  FLG-CHK
           END-IF
      *
      *    MOVE    ZERO                TO  IDY
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
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
           PERFORM UNTIL      FLG-SRYACT   =   1
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5   )  OR
                                  (SRY-SRYCD(IDX2)     =   SPACE )
                             OR   (IDY         >=  40   )
      *            薬剤一部負担金用
                   IF     (ACCT-SRYKBN             =   "21"  OR
                                                       "22"  OR
                                                       "23" )   AND
                         ((SRY-SRYCD (IDX2)(1:1)   =   "6" )  OR
                          (SRY-SRYCD (IDX2)(1:3)   =   "001" ) )
                       ADD     1                   TO  IDY
                       MOVE    ACCT-SRYKBN         TO  LNK-S01-SRYKBN
                                                          (IDX-Y IDY)
                       MOVE    ACCT-ZAINUM         TO  LNK-S01-ZAIBAN
                                                          (IDX-Y IDY)
                       MOVE    SRY-SRYCD (IDX2)    TO  LNK-S01-SRYCD
                                                          (IDX-Y IDY)
                       MOVE    SRY-SRYSURYO(IDX2)  TO  LNK-S01-SURYOU
                                                          (IDX-Y IDY)
                       MOVE    WRK-ACCT-KAISU      TO  LNK-S01-KAISUU
                                                          (IDX-Y IDY)
                       MOVE    ACCT-ZAITEN         TO  LNK-S01-TENSUU
                                                          (IDX-Y IDY)
                   END-IF
      *
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
       450511-YAKFTN-SRYACT-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院更新処理
      *****************************************************************
       4502-NYUKOUSIN-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
      *
      *ver.4.8
      *    ワーク診療行為編集
           IF      SPA-ZAIJUNKBN-FLG   =   1
      *        入力順
               PERFORM 3101-PARA-WKSRYACT-HEN-SEC
           ELSE
      *        診療区分順
               PERFORM 45011-WKSRYACT-PARA2-SEC
           END-IF
      *
      *    削除
           IF      (SPA-SYORIFLG       =   1  )  AND
                   (LNK-WKSRYACT-MAX   =   ZERO)
               IF      FLG-KAKU-FLG        =   ZERO
                   MOVE    "0101"              TO  SPA-KIDCD
                   GO      TO     4502-NYUKOUSIN-SYORI-EXT
               END-IF
           END-IF
      *
      *    追加
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    OUTPUT              PARA-FILE
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   5
      *        IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
      *            MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
      *            WRITE                       PARA-REC
      *            IF      STS-PARA        NOT =  ZERO
      *                MOVE    "0051"             TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      **** END-PERFORM
      *
      *    一時データクリア
           PERFORM 4501-PARA-DELETE-SEC
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *
      **** CLOSE                       PARA-FILE
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4502-NYUKOUSIN-SYORI-EXT
           END-IF
      *
      *    入院更新処理
           INITIALIZE                  ORCSCNYUINAREA
           MOVE    "1"                 TO  ORCSCNYUIN-KBN
      *    退院日
           MOVE    SPA-K02N-TAIINYMD   TO  ORCSCNYUIN-TAIINYMD
      *    削除
           IF      (SPA-SYORIFLG       =   1   )  AND
                   (LNK-WKSRYACT-MAX   =   ZERO)
               MOVE    "3"                 TO  ORCSCNYUIN-KBN
           END-IF
      *
           CALL    "ORCSCNYUIN"        USING
                                       SPA-AREA
                                       ORCSCNYUINAREA
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4502-NYUKOUSIN-SYORI-EXT
           END-IF
      *
      *    入院指示せん発行
           IF      FLG-SIJISEN         =   1
               PERFORM 45900-SIJISEN-SYORI-SEC
           END-IF
      *    警告エラー
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-ERRMSG2     NOT =   SPACE)  AND
                      (SPA-KEI-FLG         =   ZERO )
                   MOVE    "K001"          TO  SPA-ERRCD
                   MOVE    1               TO  SPA-KEI-FLG
                END-IF
           END-IF
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-KAKU-FLG        =   1
                   MOVE    "JOIN"              TO  WRK-MCP-PUTTYPE
               ELSE
                   MOVE    "CHANGE"            TO  WRK-MCP-PUTTYPE
               END-IF
               PERFORM 45021-END-SYORI-SEC
           END-IF
           .
       4502-NYUKOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院終了処理
      *****************************************************************
       45021-END-SYORI-SEC             SECTION.
      *
      *    感染症送信一時退避（ユーザプログラムの後で起動）
           IF      SPA-DATAINFECTIONKBN    =   1
               INITIALIZE  WRK-INF-AREA
               MOVE    SPA-HOSPNUM         TO  WRK-INF-HOSPNUM
               MOVE    SPA-HOSPID          TO  WRK-INF-HOSPID
               MOVE    SPA-SYORIFLG        TO  WRK-INF-SYORIFLG
               MOVE    SPA-NYUGAIKBN       TO  WRK-INF-NYUGAIKBN
               MOVE    SPA-PTID            TO  WRK-INF-PTID
               MOVE    SPA-SRYYMD          TO  WRK-INF-SRYYMD
               MOVE    SPA-SRYKA           TO  WRK-INF-SRYKA
               MOVE    SPA-BEDFLG          TO  WRK-INF-BEDFLG
           END-IF
      *
      *    ユーザプログラム起動
           PERFORM 4907-USERPG-SEC
      *
      *    感染症送信
           IF      SPA-DATAINFECTIONKBN    =   1
               MOVE    ZERO                TO  FLG-INFEC-DLT
               PERFORM 4908-INFECTION-SEND-SEC
           END-IF
      *
           IF      FLG-END             =   ZERO
      *        入院終了処理
               PERFORM 450211-END-SYORI-SEC
           END-IF
           .
       45021-END-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院終了処理
      *****************************************************************
       450211-END-SYORI-SEC             SECTION.
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K02N"              TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    WRK-MCP-PUTTYPE     TO  MCP-PUTTYPE
           MOVE    "K02N"              TO  MCP-WINDOW
           PERFORM 900-PUT-WINDOW
           MOVE    1                   TO  FLG-END
      *
           .
       450211-END-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ユーザプログラム起動処理
      *****************************************************************
       4907-USERPG-SEC  SECTION.
      *
           IF       SPA-SYORIFLG       =   1
               IF      LNK-WKSRYACT-MAX    =   ZERO
      *        削除
                   MOVE    3               TO  SPA-SYORI-FLG
               ELSE
      *        更新
                   MOVE    2               TO  SPA-SYORI-FLG
               END-IF
           ELSE
      *        追加
               MOVE    1               TO  SPA-SYORI-FLG
           END-IF
           IF      SPA-GMN-USERPG      =   "1"
      *        ユーザプログラム実行画面表示へ
               PERFORM 49071-XD01-SYORI-SEC
           ELSE
               IF      SPA-NAI-GYOUMU-ARI  =   "1"
      *            起動するユーザプログラムある時、実行
                   INITIALIZE                      ORCSUSERCHKAREA
                   MOVE    "K08"               TO  ORCSUSERCHK-GMNPG
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
           PERFORM 210-PARA-DEL-SEC
      *
           MOVE    SPACE               TO  LINKAREA
      *    共通キー編集
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
           MOVE    SPA-AREA            TO  LNK-KYOUTU
      *
      *    共通のキーをクリアする
           MOVE    SPACE               TO  SPA-ERRMSG
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "K02N"              TO  SPA-SAKIPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPACE               TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "XD01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-MCP-PUTTYPE     TO  MCP-PUTTYPE
           MOVE    "XD01"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       49071-XD01-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    感染症送信処理
      *****************************************************************
       4908-INFECTION-SEND-SEC         SECTION.
      *
           MOVE    SPACE               TO  SHELLTBL
           MOVE    INFECTION-FILE      TO  SHELLTBL-NAME
           MOVE    "K08"               TO  SHELLTBL-ARG1
           MOVE    WRK-INF-HOSPNUM     TO  SHELLTBL-ARG2
           MOVE    WRK-INF-HOSPID      TO  SHELLTBL-ARG3
           MOVE    WRK-INF-SYORIFLG    TO  SHELLTBL-ARG4
           IF      FLG-INFEC-DLT       =   1
               MOVE    "2"                 TO  SHELLTBL-ARG4
           END-IF
           MOVE    WRK-INF-NYUGAIKBN   TO  SHELLTBL-ARG5
           MOVE    WRK-INF-PTID        TO  WRK-PTID
           MOVE    WRK-PTID            TO  SHELLTBL-ARG6
           MOVE    WRK-INF-SRYYMD      TO  SHELLTBL-ARG7
           MOVE    WRK-INF-SRYKA       TO  SHELLTBL-ARG8
           MOVE    WRK-INF-BEDFLG      TO  SHELLTBL-ARG9
           MOVE    SHELLTBL            TO  MCPDATA-REC
           MOVE    "SHELL"             TO  MCP-FUNC
           MOVE    "shell"             TO  MCP-TABLE
           MOVE    "daily"             TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           .
       4908-INFECTION-SEND-EXT.
           EXIT.
      *
      *****************************************************************
      *    一時データ削除処理
      *****************************************************************
       2101-PARA02-DELETE-SEC               SECTION.
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID02
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM02
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME02         TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K08"               TO  PARA-GYOUMUID
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
           .
       2101-PARA02-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
           PERFORM 2101-PARA02-DELETE-SEC
      *
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "K02"               TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
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
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID1
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM1
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME1          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      *                                ORCSFDELAREA
      *
      *    MOVE    ZERO                TO  IF-RESULT
      *    MOVE    FILE-NAME2          TO  IF-FILENAME
      *    CALL    "orcfiledel"        USING
      **                               ORCSFDELAREA
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    包括分入力更新（外来）処理
      *****************************************************************
       4503-HOUKATU-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
      *
      *    MOVE    ZERO                TO  FLG-PARA
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *    INITIALIZE                  LNK-WKSRYACT-AREA
      *    MOVE    SPACE               TO  SYUNOU-REC
      *    MOVE    ZERO                TO  IDX-KOUI
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *    INITIALIZE                  HZN-PARA-AREA
      *    MOVE    SPACE               TO  LNK-SYUNOU-AREA
      *    INITIALIZE                  LNK-SYUNOU-AREA
      *    MOVE    ZERO                TO  LNK-SYUNOU-MAX
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *            UNTIL       FLG-PARA    =    1
      *        EVALUATE    PARA-FILEMEI
      *            ワーク診療行
      *            WHEN    "WKSRYACT"
      *                ADD     1                   TO  IDX
      *                登録順
      *                IF      SPA-ZAIJUNKBN-FLG   =   1
      *                    MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
      *                                                         (IDX)
      *                    IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
      *                        MOVE    SPA-HKNCOMBINUM TO
      *                                         LNK-WKSRY-HKNCOMBI(IDX)
      *                    END-IF
      *                    IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
      *                        MOVE    SPA-SRYKA       TO
      *                                         LNK-WKSRY-SRYKA   (IDX)
      *                    END-IF
      *                    MOVE    IDX             TO  LNK-WKSRYACT-MAX
      *                END-IF
      *            収納
      *            WHEN    "SYUNOU"
      *                ADD     1                   TO  LNK-SYUNOU-MAX
      *                IF      LNK-SYUNOU-MAX      <=  15
      *                    MOVE    PARA-DATA-REC   TO  LNK-SYUNOU-REC
      *                                                (LNK-SYUNOU-MAX)
      *                END-IF
      *            その他
      *            WHEN    OTHER
      *                ADD     1                   TO  IDY
      *                MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
      *        END-EVALUATE
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *    MOVE    IDX                 TO  WRK-MAX
      *
      **** CLOSE                       PARA-FILE
      *
      *    診療区分順
      *    IF      SPA-ZAIJUNKBN-FLG   =   1
      *        CONTINUE
      *    ELSE
      *        PERFORM 45011-WKSRYACT-PARA2-SEC
      **** END-IF
      *
      *ver.4.8
      *
      *    ワーク診療行為編集
           IF      SPA-ZAIJUNKBN-FLG   =   1
      *        入力順
               PERFORM 3101-PARA-WKSRYACT-HEN-SEC
           ELSE
      *        診療区分順
               PERFORM 45011-WKSRYACT-PARA2-SEC
           END-IF
      *    収納テーブル編集
           PERFORM 3101-PARA-SYUNOU-HEN-SEC
      *
      *    確認メッセージ
           IF      FLG-KAKU-FLG        =   ZERO
               IF      (SPA-SYORIFLG       =   1  )  AND
                       (LNK-WKSRYACT-MAX   =   ZERO)
                   MOVE    "0103"              TO  SPA-KIDCD
               ELSE
                   MOVE    "0102"              TO  SPA-KIDCD
               END-IF
               GO      TO     4503-HOUKATU-SYORI-EXT
           END-IF
      *    追加
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    OUTPUT              PARA-FILE
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   5
      *        IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
      *            MOVE    HZN-PARA-REC(IDY)   TO  PARA-REC
      *            WRITE                       PARA-REC
      *            IF      STS-PARA        NOT =  ZERO
      *                MOVE    "0051"             TO  SPA-ERRCD
      *            END-IF
      *        END-IF
      *    END-PERFORM
      *
      *
      *    一時データクリア
           PERFORM 4501-PARA-DELETE-SEC
      *
      *    診療行為更新
           PERFORM 45011-SRYACT-SYORI-SEC
      *
      **** CLOSE                       PARA-FILE
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4503-HOUKATU-SYORI-EXT
           END-IF
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
           IF      LNK-WKSRYACT-MAX   =   ZERO
      *        削除
               MOVE    "2"                 TO  ORCSCGAIRAI-KBN
               MOVE    1                   TO  FLG-INFEC-DLT
           ELSE
      *        新規、更新
               MOVE    "1"                 TO  ORCSCGAIRAI-KBN
               MOVE    ZERO                TO  FLG-INFEC-DLT
           END-IF
      *    収納情報
           MOVE    LNK-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC(IDX) TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
           END-PERFORM
      *
           CALL    "ORCSCGAIRAI"       USING
                                       SPA-AREA
                                       ORCSCGAIRAIAREA
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO     4503-HOUKATU-SYORI-EXT
           END-IF
      *
      *    感染症送信
           IF      SPA-DATAINFECTIONKBN    =   1
               PERFORM 4908-INFECTION-SEND-SEC
           END-IF
      *
      *    排他制御解除処理
           PERFORM 990-LOCK-OUT-SEC
      *
           PERFORM 210-PARA-DEL-SEC
      *
           INITIALIZE                      SPA-KYOTUKEY
           INITIALIZE                      SPA-K01KYOUTU
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    "K02"               TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           IF      FLG-KAKU-FLG        =   1
               MOVE    "JOIN"              TO  MCP-PUTTYPE
           END-IF
      *
           MOVE    "K02"               TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4503-HOUKATU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為クリア処理
      *****************************************************************
       4900-WKSRYACT-CLEAR-SEC             SECTION.
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
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-WKSRYACT-READ-SEC
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
           PERFORM
               UNTIL   FLG-WKSRYACT    =   1
               MOVE    ZERO                TO  WKSRY-MOD-FLG
      *
               MOVE    WKSRYACT-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
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
      *    入院指示せん指示画面へ（Ｋ０８１）
      *****************************************************************
       41001-K081-SYORI-SEC             SECTION.
      *
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-K081            TO  SPA-ETCKYOUTU-AREA
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-K08             TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGK081"          USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-FREE            TO  SPA-K08
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
      *
           MOVE    "K081"              TO  SPA-SAKIPG
           MOVE    "K08"               TO  SPA-MOTOPG
      *
           MOVE    "K08"               TO  SPA-ETC-MOTOPG
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           MOVE    SPACE               TO  LINKAREA
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "K081"              TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       41001-K081-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院指示せん発行処理
      *****************************************************************
       45900-SIJISEN-SYORI-SEC             SECTION.
      *
      *    入院指示せんオーダーデータ作成
           INITIALIZE                  ORCSORDERPRTAREA
           MOVE    "K08"               TO  LNK-ORDERPRT-PG
      *    パラメタより
           MOVE    "2"                 TO  LNK-ORDERPRT-KBN
      *
           IF      SPA-K081GMN-SYOHOU  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SYOHOU
           END-IF
           IF      SPA-K081GMN-CHUSYA  =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-CHUSYA
           END-IF
           IF      SPA-K081GMN-SIJISEN =   "1"
               MOVE    1                   TO  LNK-ORDERPRT-SIJISEN
           END-IF
           IF      SPA-K081GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  LNK-ORDERPRT-DRCD(1:1)
               MOVE    SPA-K081GMN-DRCD    TO  LNK-ORDERPRT-DRCD(2:4)
           END-IF
      *    同日再入院区分
           MOVE    SPA-K02N-DOUKBN     TO  LNK-ORDERPRT-DOUJITSUKBN
           CALL    "ORCSORDERPRT"      USING
                                       ORCSORDERPRTAREA
                                       SPA-AREA
           IF      LNK-ORDERPRT-RCD    NOT =   ZERO
               MOVE    "1200"              TO  SPA-ERRCD
           END-IF
      *
      *    帳票プログラム取得
           PERFORM 459001-PRTNUM-HEN-SEC
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF    ((SPA-CLIENT-PRT-FLG  =   2      )
             OR   (SPA-PRTCONF         =   "1"    ))
             AND  (SPA-ERRCD           =   SPACE  )
      *        クライアント印刷制御情報取得処理
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
      *    入院処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SYOHOU  =   "1"    )
               INITIALIZE              ORCHC501AREA
               MOVE    SPA-PTNUM           TO  ORCHC501-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC501-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC501-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC501-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC501-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC501-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC501-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK1KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC501-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "01"                TO  SPA-PRT-FLG
               CALL    WRK-SYOHOU-PG       USING
                                           SPA-AREA
                                           ORCHC501AREA
               IF      ORCHC501-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    注射処方箋印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-CHUSYA  =   "1"    )
               INITIALIZE                  ORCHC502AREA
               MOVE    SPA-PTNUM           TO  ORCHC502-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC502-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC502-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC502-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC502-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC502-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC502-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK2KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC502-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "02"                TO  SPA-PRT-FLG
               CALL    WRK-CHUSYA-PG       USING
                                           SPA-AREA
                                           ORCHC502AREA
               IF      ORCHC502-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *    指示せん印刷
           IF     (SPA-ERRCD           =   SPACE  )  AND
                  (SPA-K081GMN-SIJISEN =   "1"    )
               INITIALIZE              ORCHC503AREA
               MOVE    SPA-PTNUM           TO  ORCHC503-PTNUM
               MOVE    SPA-SRYYMD          TO  ORCHC503-SRYYMD
               MOVE    SPA-SYSYMD          TO  ORCHC503-SYSYMD
               MOVE    SPA-SRYKA           TO  ORCHC503-SRYKA
               MOVE    SPA-HKNCOMBINUM     TO  ORCHC503-HKNCOMBI
               MOVE    LNK-ORDERPRT-PRINTYMD   TO  ORCHC503-PRINTYMD
               MOVE    LNK-ORDERPRT-PRINTHMS   TO  ORCHC503-PRINTHMS
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >   4
                   IF      SPA-K081GMN-CHK3KBN (IDX)   =   "1"
                       MOVE    1               TO  ORCHC503-CHKPRT(IDX)
                   END-IF
               END-PERFORM
               MOVE    "03"                TO  SPA-PRT-FLG
               CALL    WRK-SIJISEN-PG      USING
                                           SPA-AREA
                                           ORCHC503AREA
               IF      ORCHC503-RC         NOT =   ZERO
                   MOVE    "1201"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           MOVE    SPACE               TO  SPA-PRT-FLG
      *
           .
       45900-SIJISEN-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    帳票プログラム取得処理
      *****************************************************************
       459001-PRTNUM-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SYOHOU-PG
           MOVE    SPACE               TO  WRK-CHUSYA-PG
           MOVE    SPACE               TO  WRK-SIJISEN-PG
      *    入院処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000005"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1202"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
           END-IF
      *    注射処方せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000006"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1203"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-CHUSYA-PG
           END-IF
      *    指示せん
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000007"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1034-REC
           IF      ORCSPRTNM-RC    NOT =   ZERO
               MOVE    "1204"              TO  SPA-ERRCD
           ELSE
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-SIJISEN-PG
           END-IF
           .
       459001-PRTNUM-HEN-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      **   INITIALIZE                  KDMY01
      **   MOVE    SPACE               TO  KDMY01-PANDAPRINT1
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      **   MOVE    "入院処方箋"        TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "注射処方箋"        TO  ORCSPRTCTRL-TITLE-I(02)
      **   MOVE    "入院指示箋"        TO  ORCSPRTCTRL-TITLE-I(03)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      ***  MOVE    ORCSPRTCTRL-OUT     TO  KDMY01-PANDAPRINT1
      *
      ***  MOVE    1                   TO  FLG-CLIENT-OK
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
      *
      *    MOVE    "_KDMY01"           TO  MCP-WINDOW
      *    MOVE    "NEW"               TO  MCP-PUTTYPE
      *
      *    PERFORM 900-PUT-WINDOW
      *    .
      *600-CLIENT-DMY-EXT.
      ***  EXIT.
      *
      *H24.4
      *****************************************************************
      *    一般名称切替え処理
      *****************************************************************
       405-GENERIC-CHG-SEC              SECTION.
      *
           IF      SPA-NAI-GENEINIT    =   1
               MOVE    ZERO            TO  SPA-NAI-GENEINIT
           ELSE
               MOVE    1               TO  SPA-NAI-GENEINIT
           END-IF
      *    院外処方名称設定
           PERFORM 4051-MEISYO-HEN-SEC
           .
       405-GENERIC-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    院外処方名称設定処理
      *****************************************************************
       4051-MEISYO-HEN-SEC          SECTION.
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K08-MAX
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =   "-"
                                                   OR  "#"
                                                   OR  "="
                   CONTINUE
               ELSE
                   IF      SPA-NAI-GENEINIT    =   1
      *                銘柄名記載
                       IF      SPA-GMN-ME-MEISYO (IDX)    NOT =   SPACE
                           MOVE    SPA-GMN-ME-MEISYO (IDX)
                                               TO  SPA-GMN-TMEISYO2(IDX)
                           INSPECT SPA-GMN-TMEISYO2(IDX)   REPLACING
                                               ALL "  "  BY  "　"
                       END-IF
                    ELSE
      *                一般名記載
                       IF      SPA-GMN-GE-MEISYO (IDX)    NOT =   SPACE
                           MOVE    SPA-GMN-GE-MEISYO (IDX)
                                               TO  SPA-GMN-TMEISYO2(IDX)
                           INSPECT SPA-GMN-TMEISYO2(IDX)   REPLACING
                                               ALL "  "  BY  "　"
                       END-IF
                    END-IF
                END-IF
           END-PERFORM
           .
       4051-MEISYO-HEN-EXT.
           EXIT.
      *H24.4
      *
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4010-PREVIEW-SYORI-SEC          SECTION.
      *
           IF      (SPA-HKNCOMBINUM    =    "9999" )
      **********OR (SPA-HKNKBN         =    1      )
               MOVE    "9001"          TO  SPA-ERRCD
           ELSE
               IF      (SPA-SYORIFLG       =   ZERO)  AND
                       (SPA-K08-MAX        =   ZERO)
                   MOVE    "9002"          TO  SPA-ERRCD
               END-IF
           END-IF
      *@!!
      ***  ４月のレセプト適用までエラー
      *    IF      SPA-ERRCD           =   SPACE
      *        IF      SPA-SRYYMD          >=  "20080401"
      *            MOVE    "9009"          TO  SPA-ERRCD
      *        END-IF
      ***  END-IF
      *@
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-NYUGAIKBN       =   "1"
                   PERFORM 40101-PREVIEW-NYU-SEC
               ELSE
                   PERFORM 40102-PREVIEW-GAI-SEC
               END-IF
      *!!!
      *        第三者行為の時、保険確認
               IF      (SPA-HKNKBN         =   1   )  AND
                       (SPA-RSI-HKNKBN     =  "6"  ) 
                  AND  (SPA-ERRCD          =   SPACE)
                   MOVE    "1001"          TO  SPA-KIDCD
                   GO      TO      4010-PREVIEW-SYORI-EXT
               END-IF
      *!!!
               IF      SPA-ERRCD           =   SPACE
      *            プレビュー処理
                   PERFORM 4130-PREVIEW-SEC
               ELSE
                   IF      SPA-ERRCD       =   "0006"
                       MOVE    "9004"          TO  SPA-ERRCD
                   ELSE
                       MOVE    "9003"          TO  SPA-ERRCD
                   END-IF
               END-IF
           END-IF
           .
       4010-PREVIEW-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    外来プレビュー前処理
      *****************************************************************
       40102-PREVIEW-GAI-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
           MOVE    ZERO                TO  FLG-SYOSIN-OK
      *
      *    MOVE    ZERO                TO  FLG-PARA
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *    INITIALIZE                  LNK-WKSRYACT-AREA
      *    MOVE    SPACE               TO  SYUNOU-REC
      *    MOVE    ZERO                TO  IDX-KOUI
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *    INITIALIZE                  HZN-PARA-AREA
      *    MOVE    SPACE               TO  LNK-SYUNOU-AREA
      *    INITIALIZE                  LNK-SYUNOU-AREA
      *    MOVE    ZERO                TO  LNK-SYUNOU-MAX
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *            UNTIL       FLG-PARA    =    1
      *        EVALUATE    PARA-FILEMEI
      *            ワーク診療行
      *            WHEN    "WKSRYACT"
      *                ADD     1                   TO  IDX
      *                MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
      *                                                         (IDX)
      *                IF      LNK-WKSRY-HKNCOMBI(IDX) =   ZERO
      *                    MOVE    SPA-HKNCOMBINUM TO
      *                                         LNK-WKSRY-HKNCOMBI(IDX)
      *                END-IF
      *                IF      LNK-WKSRY-SRYKA   (IDX) =   SPACE
      *                    MOVE    SPA-SRYKA       TO
      *                                         LNK-WKSRY-SRYKA   (IDX)
      *                END-IF
      *                MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *            収納
      *            WHEN    "SYUNOU"
      *                MOVE    PARA-DATA-REC       TO  SYUNOU-REC
      *                IF     (SYU-HKNCOMBINUM     NOT =   "9999" )
      *                    ADD     1               TO  LNK-SYUNOU-MAX
      *                    IF      LNK-SYUNOU-MAX      <=  15
      *                      MOVE    PARA-DATA-REC   TO  LNK-SYUNOU-REC
      *                                                (LNK-SYUNOU-MAX)
      *                    END-IF
      *                END-IF
      *            その他
      *            WHEN    OTHER
      *                ADD     1                   TO  IDY
      *                MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
      *        END-EVALUATE
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *    MOVE    IDX                 TO  WRK-MAX
      *
      **** CLOSE                       PARA-FILE
      *ver.4.8
      *
      *    ワーク診療行為編集
           PERFORM 3101-PARA-WKSRYACT-HEN-SEC
      *    収納テーブル編集
           PERFORM 3101-PARA-SYUNOU-HEN-SEC
      *
      *    追加
      *    MOVE    SPA-TERMID          TO  FILE-TERMIDPRV
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUMPRV
      *    OPEN    OUTPUT              PARA-PREVIEW
      *
      *    データ削除処理
           PERFORM 401011-PRVPARA-DELETE-SEC
      *
      *    診療行為更新（プレビュー）
           PERFORM 401011-SRYACT-PREVIEW-SEC
      *    収納更新
           PERFORM 45012-SYUNOU-SYORI-SEC
      *
      *    MOVE    ZERO                TO  PARA-EDANUMPRV
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU2-MAX
      *        ADD     1                   TO  PARA-EDANUMPRV
      *        MOVE    "SYUNOU"            TO  PARA-FILEMEIPRV
      *        MOVE    LNK-SYUNOU2-REC(IDX)
      *                                    TO  PARA-DATA-RECPRV
      *        WRITE                       PARA-RECPRV
      *v4.8    TBL_PARAに保存
               INITIALIZE                      PARA-REC
               MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
               MOVE    "PVK8"              TO  PARA-GYOUMUID
               MOVE    SPA-TERMID          TO  PARA-TERMID
               MOVE    "SYUNOU"            TO  PARA-FILEMEI
               MOVE    IDX                 TO  PARA-EDANUM
               MOVE    LNK-SYUNOU2-REC(IDX)
                                           TO  PARA-DATA-REC
      *
               PERFORM 30010-PARA-DBINSERT-SEC
      *
           END-PERFORM
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   5
      *        IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
      *            MOVE    HZN-PARA-REC(IDY)   TO  PARA-RECPRV
      *            WRITE                       PARA-RECPRV
      *        END-IF
      *    END-PERFORM
      *
      *****CLOSE                       PARA-PREVIEW
      *
      *    外来更新処理
           INITIALIZE                  ORCSCGAIRAIAREA
           IF      LNK-WKSRYACT-MAX   =   ZERO
      *        削除
               MOVE    "2"                 TO  ORCSCGAIRAI-KBN
           ELSE
      *        新規、更新
               MOVE    "1"                 TO  ORCSCGAIRAI-KBN
           END-IF
      *    収納情報
           MOVE    LNK-SYUNOU-MAX      TO  ORCSCGAIRAI-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-SYUNOU-MAX
               MOVE    LNK-SYUNOU-REC(IDX) TO  ORCSCGAIRAI-SYUNOU-REC
                                                               (IDX)
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               CALL    "ORCSCGAIRAIPRV"    USING
                                           SPA-AREA
                                           ORCSCGAIRAIAREA
           END-IF
      *
      *    データ削除処理
           PERFORM 401011-PRVPARA-DELETE-SEC
      *
           .
       40102-PREVIEW-GAI-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー一時データ削除処理
      *****************************************************************
       401011-PRVPARA-DELETE-SEC          SECTION.
      *
           INITIALIZE                      PARA-REC
           MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
           MOVE    "PVK8"              TO  PARA-GYOUMUID
           MOVE    SPA-TERMID          TO  PARA-TERMID
           MOVE    PARA-REC            TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       401011-PRVPARA-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    診療行為更新（プレビュー）処理
      *****************************************************************
       401011-SRYACT-PREVIEW-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-EDANUM
      *    診療行為更新
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   LNK-WKSRYACT-MAX
               MOVE    ZERO                TO  FLG-DEL
      *        包括分は対象外
               IF     (LNK-WKSRY-HKNCOMBI (IDX)    =   "9999")
      *H25.1   包括剤は対象
      *??          OR (LNK-WKSRY-CONTKBN  (IDX)    =   "H"   )
                   MOVE    1               TO  FLG-DEL
               END-IF
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   MAX-GMN )
                               OR (FLG-DEL =   1       )
                   IF      LNK-WKSRY-ZAINUM (IDX)  =   SPA-NAI-ZAINUM
                                                                 (IDY)
                       IF      SPA-NAI-DELFLG (IDY)    =   1
                           MOVE    1               TO  FLG-DEL
                       END-IF
                       MOVE    MAX-GMN             TO  IDY
                   END-IF
               END-PERFORM
               IF      FLG-DEL             =   ZERO
      *            初診料算定判定
                   PERFORM 450112-SYOSIN-CHK-SEC
      *            剤数チェック
                   PERFORM 450114-ZAICONUNT-SEC
      *
                   ADD     1                   TO  IDX2
      ***          MOVE    "WKSRYACT"          TO  PARA-FILEMEIPRV
      *            MOVE    IDX2                TO  PARA-EDANUMPRV
      *            MOVE    LNK-WKSRYACT-REC (IDX)
      *                                        TO  PARA-DATA-RECPRV
      ****         WRITE                       PARA-RECPRV
      *
                   ADD    1                    TO  WRK-EDANUM
                   INITIALIZE                      PARA-REC
                   MOVE    SPA-HOSPNUM         TO  PARA-HOSPNUM
                   MOVE    "PVK8"              TO  PARA-GYOUMUID
                   MOVE    SPA-TERMID          TO  PARA-TERMID
                   MOVE    "WKSRYACT"          TO  PARA-FILEMEI
                   MOVE    WRK-EDANUM          TO  PARA-EDANUM
                   MOVE    LNK-WKSRYACT-REC (IDX)
                                               TO  PARA-DATA-REC
      *
                   PERFORM 30010-PARA-DBINSERT-SEC
               ELSE
                   MOVE    SPACE               TO  LNK-WKSRYACT-REC(IDX)
                   INITIALIZE                      LNK-WKSRYACT-REC(IDX)
               END-IF
           END-PERFORM
      *
           .
       401011-SRYACT-PREVIEW-EXT.
           EXIT.
      *****************************************************************
      *    入院プレビュー前処理
      *****************************************************************
       40101-PREVIEW-NYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX
           MOVE    ZERO                TO  IDY
           MOVE    ZERO                TO  IDG
      *
      *    MOVE    ZERO                TO  FLG-PARA
      *    MOVE    SPACE               TO  LNK-WKSRYACT-AREA
      *    INITIALIZE                  LNK-WKSRYACT-AREA
      *    MOVE    SPACE               TO  SYUNOU-REC
      *    MOVE    ZERO                TO  IDX-KOUI
      *    MOVE    ZERO                TO  LNK-WKSRYACT-MAX
      *    INITIALIZE                  HZN-PARA-AREA
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *    END-IF
      *    PERFORM
      *            UNTIL       FLG-PARA    =    1
      *        EVALUATE    PARA-FILEMEI
      *            ワーク診療行
      *            WHEN    "WKSRYACT"
      *                ADD     1                   TO  IDX
      *                MOVE    PARA-DATA-REC       TO  LNK-WKSRYACT-REC
      *                                                         (IDX)
      *                MOVE    IDX                 TO  LNK-WKSRYACT-MAX
      *            その他
      *            WHEN    OTHER
      *                ADD     1                   TO  IDY
      *                MOVE    PARA-REC            TO  HZN-PARA-REC(IDY)
      *        END-EVALUATE
      *
      *        PERFORM 980-PARA-READ-SEC
      *    END-PERFORM
      *    MOVE    IDX                 TO  WRK-MAX
      *
      **** CLOSE                       PARA-FILE
      *
      *ver.4.8
      *    ワーク診療行為編集
           PERFORM 3101-PARA-WKSRYACT-HEN-SEC
      *
      *    データ削除処理
           PERFORM 401011-PRVPARA-DELETE-SEC
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMIDPRV
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUMPRV
      *    OPEN    OUTPUT              PARA-PREVIEW
      *
      *    PERFORM VARYING     IDY     FROM    1   BY  1
      *            UNTIL       IDY     >   5
      *        IF      HZN-PARA-DATA-REC (IDY) NOT =   SPACE
      *            MOVE    HZN-PARA-REC(IDY)   TO  PARA-RECPRV
      *            WRITE                       PARA-RECPRV
      *        END-IF
      **** END-PERFORM
      *
      *    診療行為更新（プレビュー）
           PERFORM 401011-SRYACT-PREVIEW-SEC
      *
      *****CLOSE                       PARA-PREVIEW
      *
           IF      SPA-ERRCD           =   SPACE
      *    入院更新処理
               INITIALIZE                  ORCSCNYUINAREA
               MOVE    "1"                 TO  ORCSCNYUIN-KBN
      *        退院日
               MOVE    SPA-K02N-TAIINYMD   TO  ORCSCNYUIN-TAIINYMD
               CALL    "ORCSCNYUINPRV"     USING
                                           SPA-AREA
                                           ORCSCNYUINAREA
           END-IF
      *
      *    データ削除処理
           PERFORM 401011-PRVPARA-DELETE-SEC
      *
      *
           .
       40101-PREVIEW-NYU-EXT.
           EXIT.
      *****************************************************************
      *    プレビュー処理
      *****************************************************************
       4130-PREVIEW-SEC          SECTION.
      *
           PERFORM 41301-PREVIEW-KICK-SEC
      *
           MOVE    SPACE               TO  LINKAREA
           MOVE    SPA-KYOUTU          TO  LNK-COMMON
           MOVE    SPA-K08             TO  LNK-SCRSPA
      *
      *    画面移行用のパラメタ変更
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "K08"               TO  SPA-MOTOPG3
           MOVE    "XC01"              TO  SPA-SAKIPG
      *
           MOVE    WRK-CONS-PRTKANRI-TBL-KEY
                                       TO  SPA-PRTKANRI-TBL-KEY
           MOVE    SPA-PTID            TO  LNK-XC01-TBL-GROUP
           MOVE    0                   TO  LNK-XC01-SHORI-RENNUM
           MOVE    1                   TO  LNK-XC01-RENNUM
           MOVE    1                   TO  LNK-XC01-ST-PAGE
           MOVE    99999               TO  LNK-XC01-ED-PAGE
           MOVE    "1"                 TO  LNK-XC01-MODE
           MOVE    WRK-PRV-HKNKBN      TO  LNK-XC01-HKNKBN
           MOVE    LNK-XC01-AREA       TO  SPA-WORK
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE   "JOIN"               TO  MCP-PUTTYPE
           MOVE   "XC01   "            TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       4130-PREVIEW-EXT.
           EXIT.
      *****************************************************************
      *    プレビューバッチ起動処理
      *****************************************************************
       41301-PREVIEW-KICK-SEC    SECTION.
      *
           MOVE    SPA-HKNKBN          TO  WRK-PRV-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  WRK-PRV-RSI-HKNKBN
           IF      SPA-RSI-HKNKBN      =   "4"
      *D                               OR  "5"
               MOVE    "2"                 TO  WRK-PRV-HKNKBN
           END-IF
      *    公害
           IF      SPA-HKNKBN          =   "2"
               MOVE    "3"                 TO  WRK-PRV-HKNKBN
           END-IF
      *H25.10
      *    第三者行為
           IF     (SPA-HKNKBN          =   "1"  )  AND
                  (SPA-RSI-HKNKBN      =   "6"  )
               IF      (SPA-KID1-FLG        =   "OK" )
                   MOVE    "4"                 TO  WRK-PRV-HKNKBN
               ELSE
      *            健保
                   MOVE    "0"                 TO  WRK-PRV-HKNKBN
                   MOVE    "0"                 TO  WRK-PRV-RSI-HKNKBN
               END-IF
           END-IF
      *
      *    レセプトプレビュー区分
           MOVE    SPA-NAI-RESPRVFLG   TO  WRK-PRV-KBN
      *ver.4.7
      *    業務区分追加
           MOVE    "3"                 TO  WRK-GYOUMUKBN
           CALL    "ORCSRECEARGMK"     USING
                                       "1"
                                       SPA-PTID
                                       SPA-SRYYMD(1:6)
                                       SPA-NYUGAIKBN
                                       WRK-PRV-HKNKBN
                                       WRK-PRV-RSI-HKNKBN
                                       WRK-PRV-KBN
                                       WRK-GYOUMUKBN
                                       SPA-AREA
      *
           .
       41301-PREVIEW-KICK-EXT.
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
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "K08"               TO  MCP-WINDOW.
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
           IF     (FLG-GMN-INIT        =   1   )  OR
                  (SPA-K08-MAX         =   ZERO )
               MOVE    ZERO                TO  WRK-K08-ROW
           ELSE
               MOVE    K08-ROW             TO  WRK-K08-ROW
           END-IF
      *
           MOVE    SPACE               TO  K08
           INITIALIZE                      K08
      *
      *    コラムリストの位置指定
           MOVE    ZERO                TO  K08-ROW
           MOVE    ZERO                TO  K08-ROWATTR
           IF      WRK-K08-ROW     NOT =   ZERO
               MOVE    WRK-K08-ROW         TO  K08-ROW
           END-IF
      *
      *
           MOVE    SPA-PTNUM           TO  K08-PTNUM
           MOVE    SPA-KANANAME        TO  K08-KANANAME
           MOVE    SPA-NAME            TO  K08-NAME
           MOVE    SPA-SEX             TO  K08-SEX
           MOVE    SPA-HKNCOMBIMEI-60
                                       TO  K08-HKNKUMI
           MOVE    SPA-FTNRATEMEI      TO  K08-RATE
           MOVE    SPA-SRYYMDW         TO  K08-SRYYMD
           MOVE    SPA-BIRTHDAYW       TO  K08-BIRTHDAY
           MOVE    SPA-SRYKAMEI        TO  K08-SRYKA
      *
           MOVE    SPA-NENREI-HEN      TO  K08-NENREI
      *
      *    処理
           EVALUATE    SPA-SYORIFLG
               WHEN    0
                   MOVE    SPACE           TO  K08-SYORIMEI-VALUE
               WHEN    1
                   MOVE    "red"           TO  K08-SYORIMEI-STYLE
                   MOVE    "［訂　正］"    TO  K08-SYORIMEI-VALUE
           END-EVALUATE
      *
      *
           MOVE    SPA-GMN-GOKEI       TO  WRK-GOKEI-Z3
           MOVE    WRK-GOKEI-X3        TO  K08-GOKEITEN
           MOVE    SPA-GMN-LASTYMD     TO  K08-LASTYMD
           MOVE    SPA-GMN-SYOSINYMD   TO  K08-SYOSINYMD
      *
           MOVE    SPA-GMN-MISYUMONEY  TO  WRK-GOKEI-Z3
           MOVE    WRK-GOKEI-X3        TO  K08-MISYUMONEY
      *
      *    累計点数
      *        訂正の時、”＊”
      *    IF      SPA-SYORIFLG        =   1
      *        MOVE    ALL "*"             TO  K08-ZAITENTOTAL
      **** ELSE
      *    累計点数（入院のみ）
           IF      SPA-NYUGAIKBN       =   "2"
               COMPUTE WRK-GOKEI-Z2        =   SPA-GMN-GOKEI
                                           +   SPA-ZAITENTOTAL
               MOVE    WRK-GOKEI-Z2        TO  K08-ZAITENTOTAL
           END-IF
      *
      *
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   SPA-K08-MAX
      *********MOVE    SPA-GMN-TMEISYO (IDX)   TO  K08-ITEM(IDX)
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "-"
                   MOVE    ALL "―"                TO  K08-MEISAI (IDX)
               ELSE
                   MOVE    SPA-GMN-TMEISYO (IDX)   TO  K08-MEISAI (IDX)
               END-IF
      *        番号
               IF      SPA-GMN-TBANGO  (IDX)   NOT =   ZERO
                   MOVE    SPA-GMN-TBANGO  (IDX)   TO  WRK-ZZ
                   MOVE    WRK-ZZ                  TO  K08-BANGO (IDX)
               END-IF
      *        削除フラグ
               EVALUATE    SPA-GMN-TDELFLG (IDX)
                   WHEN    0
                       MOVE    SPACE           TO  K08-DELFLG (IDX)
                   WHEN    1
                       MOVE    "◎"            TO  K08-DELFLG (IDX)
                   WHEN    2
                       MOVE    "削"            TO  K08-DELFLG (IDX)
      *R01.9
                       MOVE    "#FFCC99"       TO  K08-MEI-BGCOLOR
                                                             (IDX)
      *
               END-EVALUATE
      *
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "-"
                   MOVE    ALL "-"                 TO  K08-BANGO (IDX)
                   MOVE    ALL "-"                 TO  K08-DELFLG(IDX)
               END-IF
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "#"
                   MOVE    ALL "#"                 TO  K08-BANGO (IDX)
                   MOVE    ALL "#"                 TO  K08-DELFLG(IDX)
               END-IF
               IF      SPA-GMN-TMEISYO (IDX)(1:1)  =  "="
                   MOVE    ALL "="                 TO  K08-BANGO (IDX)
                   MOVE    ALL "="                 TO  K08-DELFLG(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-K08-MAX         TO  K08-COUNT
           MOVE    "user-font"         TO  K08-MEI-STYLE
      *
      *    各点数
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   16
               IF      SPA-GMN-HKNTEN  (IDX)   >=  ZERO
                   MOVE    SPA-GMN-HKNTEN  (IDX)   TO  WRK-Z9
                   MOVE    WRK-Z9                  TO  WRK-Z9-X
               ELSE
                   MOVE    SPA-GMN-HKNTEN  (IDX)   TO  WRK-Z92
                   MOVE    WRK-Z92                 TO  WRK-Z9-X
               END-IF
               EVALUATE    IDX
                   WHEN    1
                       MOVE    WRK-Z9-X        TO  K08-SSUHKNTEN
                   WHEN    2
                       MOVE    WRK-Z9-X          TO  K08-SDOHKNTEN
                   WHEN    3
                       MOVE    WRK-Z9-X          TO  K08-ZTKHKNTEN
                   WHEN    4
                       MOVE    WRK-Z9-X          TO  K08-TYKHKNTEN
                   WHEN    5
                       MOVE    WRK-Z9-X          TO  K08-CSYHKNTEN
                   WHEN    6
                       MOVE    WRK-Z9-X          TO  K08-SYCHKNTEN
                   WHEN    7
                       MOVE    WRK-Z9-X          TO  K08-SJTHKNTEN
                   WHEN    8
                       MOVE    WRK-Z9-X          TO  K08-MSIHKNTEN
                   WHEN    9
                       MOVE    WRK-Z9-X          TO  K08-KNSHKNTEN
                   WHEN    10
                       MOVE    WRK-Z9-X          TO  K08-GZUHKNTEN
                   WHEN    11
                       MOVE    WRK-Z9-X          TO  K08-ETCHKNTEN
                   WHEN    12
                       MOVE    WRK-Z9-X          TO  K08-SSNHKNTEN
                   WHEN    13
                       MOVE    WRK-Z9-X          TO  K08-HOUHKNTEN
                   WHEN    14
                       MOVE    WRK-Z9-X          TO  K08-BYRHKNTEN
                   WHEN    15
                       MOVE    WRK-Z9-X          TO  K08-NYUHKNTEN
                       IF      SPA-GMN-HKNTEN  (IDX) NOT =   ZERO
                           MOVE    "入院料等"        TO  K08-SRYMEI14
                       END-IF
                   WHEN    16
                       MOVE    WRK-Z9-X          TO  K08-RYOHKNTEN
                       IF      SPA-GMN-HKNTEN  (IDX) NOT =   ZERO
                           MOVE    "療養担当"        TO  K08-SRYMEI15
                       END-IF
               END-EVALUATE
           END-PERFORM
      *    選択
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX         >   10
               MOVE    SPA-GMN-SELNUM  (IDX)   TO  K08-SELNUM (IDX)
           END-PERFORM
      *
      *    メッセージ
           MOVE    SPA-GMN-MSGDATA     TO  K08-MSGDATA-VALUE
           IF      SPA-GMN-MSGDATA     =   SPACE
               MOVE    "black"             TO  K08-MSGDATA-STYLE
           ELSE
               MOVE    "red"               TO  K08-MSGDATA-STYLE
           END-IF
      *
      *    登録・発行
           MOVE    SPACE               TO  K08-B10-LABEL
           MOVE    WIDGET-INSENSITIVE  TO  K08-B10-STATE
           IF      SPA-NYUGAIKBN       =   "1"
               MOVE    SPA-ETCKYOUTU-AREA  TO  SPA-K081
               IF     (SPA-NAI-SIJISENKBN  =   1  )  AND
                     ((SPA-NAI-SYOHOU      =   1 )  OR
                      (SPA-NAI-CHUSYA      =   1 )  OR
                      (SPA-NAI-SIJISEN     =   1 ))
      *            入院指示せん発行あり
                   MOVE    "登録・発行"        TO  K08-B10-LABEL
                   MOVE    WIDGET-NORMAL       TO  K08-B10-STATE
               END-IF
           END-IF
      *    Ｕ・Ｐ
           MOVE    SPA-GMN-USERPG-G    TO  K08-USERPG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-USERPG-MAX
               MOVE    SPA-GMN-USERPG-LIST (IDX)
                                           TO  K08-USERPG-ITEM(IDX)
           END-PERFORM
           MOVE    SPA-USERPG-MAX      TO  K08-USERPG-COUNT
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR         =   ZERO
               IF      WRK-MCP-WIDGET(1:6)     =   "SELNUM"
                   MOVE    WRK-MCP-WIDGET(7:2)     TO  IDX-Y2
                   IF      SPA-GMN-SELNUM (IDX-Y2) =   ZERO
                       MOVE    11                  TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           IF      SPA-GMN-CUR         =   ZERO
               PERFORM VARYING     IDX-Y2  FROM    1   BY  1
                       UNTIL       IDX-Y2  >   10
                   IF      SPA-GMN-SELNUM (IDX-Y2) =   ZERO
                       MOVE    IDX-Y2              TO  SPA-GMN-CUR
                       MOVE    10                  TO  IDX-Y2
                   END-IF
               END-PERFORM
               IF      SPA-GMN-CUR         =   ZERO
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM01"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM02"        TO  MCP-WIDGET
               WHEN    03
                   MOVE    "SELNUM03"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "SELNUM04"        TO  MCP-WIDGET
               WHEN    05
                   MOVE    "SELNUM05"        TO  MCP-WIDGET
               WHEN    06
                   MOVE    "SELNUM06"        TO  MCP-WIDGET
               WHEN    07
                   MOVE    "SELNUM07"        TO  MCP-WIDGET
               WHEN    08
                   MOVE    "SELNUM08"        TO  MCP-WIDGET
               WHEN    09
                   MOVE    "SELNUM09"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "SELNUM10"        TO  MCP-WIDGET
               WHEN    11
                   MOVE    "B12"             TO  MCP-WIDGET
               WHEN    13
                   MOVE    "USERPG"          TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B01"             TO  MCP-WIDGET
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
           MOVE    SPACE               TO  SPA-ERRMSG
      *
           EVALUATE    SPA-ERRCD
               WHEN    "0001"
                   MOVE    "入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "剤番号がありません"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "自動算定分ではありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "削除できません。"
                                       TO  SPA-ERRMSG(27:)
               WHEN    "0005"
                   STRING  "【ＤＢ更新がエラーになりました。"
                           "ＳＥに連絡して下さい。】"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *VER.4.5
               WHEN    "0105"
                   STRING  "この診療科の当日の受診が９回登録済です。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0010"
                   STRING   "保険適用点数がマイナスになります。"
                            "収納の登録ができません。"
                                       DELIMITED  BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *VER.4.5
               WHEN    "0006"
                   STRING  "１日の受診履歴が１３５剤以上になります。"
                           "剤を減らして入力して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0007"
                   MOVE    "Ｕ・Ｐがありません。"
                                       TO  SPA-ERRMSG
      *!
      *H30.6
               WHEN    "0011"
                   STRING  "負担割合計算額が７桁を超えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "今回請求金額が７桁を超えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "作成する収納が上限件数を超えます。"
                                       DELIMITED   BY  SIZE
                                       INTO    SPA-ERRMSG
                   END-STRING
      *!
      *!
               WHEN    "1000"
                   MOVE    "６００行以上になります。"
                                       TO  SPA-ERRMSG
                   MOVE    "６００行以上は表示しません。"
                                       TO  SPA-ERRMSG(25:)
               WHEN    "1036"
                   MOVE    "受診履歴の伝票番号がゼロです。"
                                       TO  SPA-ERRMSG
                   MOVE    "訂正できません。"
                                       TO  SPA-ERRMSG(31:)
      *!
               WHEN    "1200"
                   MOVE    "入院帳票オーダマスタ更新エラー。"
                                       TO  SPA-ERRMSG
               WHEN    "1201"
                   MOVE    "入院帳票が印刷エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "1202"
                   MOVE    "入院処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1203"
                   MOVE    "注射処方箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(35:)
               WHEN    "1204"
                   MOVE    "指示箋プログラムがありません。"
                                       TO  SPA-ERRMSG
                   MOVE    "システム管理を確認して下さい。"
                                       TO  SPA-ERRMSG(31:)
      *
               WHEN    "9000"
                   MOVE    "【算定履歴の更新ができませんでした】"
                                       TO  SPA-ERRMSG
               WHEN    "0051"
                   STRING  "中間データに不整合が発生しました。"
                           "再度、診療行為へ戻って下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "9001"
                   MOVE    "プレビューができない保険組合せです。"
                                       TO  SPA-ERRMSG
               WHEN    "9002"
                   MOVE    "診療内容がありません。"
                                       TO  SPA-ERRMSG
               WHEN    "9003"
                   MOVE    "プレビューデータ作成が失敗しました。"
                                       TO  SPA-ERRMSG
               WHEN    "9004"
                   STRING  "１日の受診履歴が１３５剤以上になります。"
                           "プレビューデータ作成が失敗しました。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *@@
               WHEN    "9009"
                   STRING  "レセプトの４月改正は未対応です。"
                           "４月診療以降のプレビューはできません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *@@
      *!
               WHEN    "K001"
                   MOVE    SPA-ERRMSG2     TO  SPA-ERRMSG
      *D           MOVE    SPACE           TO  SPA-ERRMSG2
                   MOVE    SPA-ERRCD       TO  SPA-ERRMSG2
      *!
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           IF     (SPA-ERRMSG2     NOT =   SPACE  )  AND
                  (SPA-ERRCD       NOT =   "K001")
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
           MOVE    "K08"               TO  SPA-MOTOPG
           MOVE    "KERR"              TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE.
           MOVE    "KERR"              TO  MCP-WINDOW.
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    エラーメッセージ表示２処理
      *****************************************************************
       5101-KERR2-SET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KERR2
           INITIALIZE                      KERR2
           MOVE    SPA-ERRCD           TO  KERR2-ERRCODE
           MOVE    SPA-ERRMSG          TO  KERR2-ERRMSG
           MOVE    SPA-ERRMSG2         TO  KERR2-ERRMSG2(1:80)
      *
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "K08"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KIDSET-SEC              SECTION.
      *
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
                   STRING  "該当の受診履歴を削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "0102"
                   MOVE    "包括分入力を登録します。"
                                       TO  WRK-KIDMSG
               WHEN    "0103"
                   STRING  "包括分入力の受診履歴を削除します。"
                           "よろしいですか？"
                                       DELIMITED  BY  SIZE
                                       INTO  WRK-KIDMSG
                   END-STRING
               WHEN    "1001"
                   STRING  "第三者行為の保険組合せです。"
                           "ＯＫで第三者をＮＯで健保の処理を行います。"
                                       DELIMITED  BY  SIZE
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
           EVALUATE    SPA-KIDCD
               WHEN    "0101"
               WHEN    "1001"
                   MOVE    "ＮＯ"            TO  KID1-B01
           END-EVALUATE
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "K08"               TO  SPA-MOTOPG
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
      *****************************************************************
      *    システム管理（労災情報）検索処理
      *****************************************************************
       4500-RSI-SYSKANRI-SEC         SECTION.
      *
      *    システム管理検索（労災情報）
           MOVE    SPACE               TO  SYS-4001-REC
           INITIALIZE                  SYS-4001-REC
           MOVE    "4001"              TO  SYS-4001-KANRICD
           MOVE    "*"                 TO  SYS-4001-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-4001-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-4001-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-4001-HOSPNUM
           MOVE    SYS-4001-REC        TO  MCPDATA-REC
      *
      *    システム管理検索
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-4001-REC
               INITIALIZE                  SYS-4001-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    MCPDATA-REC         TO  SYS-4001-REC
           ELSE
               INITIALIZE                  SYS-4001-REC
           END-IF
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4500-RSI-SYSKANRI-EXT.
           EXIT.
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
      *!
      *    検査正式名称
           IF      SPA-FORMALFLG       =   "1"
               IF     (TNS-SRYCD(1:1)      =   "1") AND
                      (TNS-SRYKBN          =   "60"
                                           OR  "64"  )  AND
                      (TNS-FORMALNAME  NOT =   SPACE )  AND
                      (TNS-FORMALNAME  NOT =   TNS-NAME )
                   MOVE    TNS-FORMALNAME  TO  TNS-NAME
               END-IF
           END-IF
      *R02.2
      *    附加情報のユーザー単位設定
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7"  )
              AND (FLG-TENSU           =   ZERO )
               PERFORM 9101-TENSUPLUS-READ-SEC
      *        ユーザー単位
               IF     (FLG-TENSUPLUS       =   ZERO)
                  AND (TNSP-USERTANICD NOT =   SPACE)
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           END-IF
      *
           .
       910-TENSU-READ-EXT.
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
      *
      *****************************************************************
      *    パラメタデータ順読み
      *****************************************************************
      *980-PARA02-READ-SEC         SECTION.
      *
      *    READ    PARA-FILE02
      *        AT  END
      *            MOVE    1           TO  FLG-PARA
      *        NOT AT  END
      *            MOVE    ZERO        TO  FLG-PARA
      *    END-READ
      *
      *    .
      *980-PARA02-READ-EXT.
      *    EXIT.
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
       930-INPUTCD-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       970-JYURRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  JYURRK-REC
               MOVE    ZERO            TO  FLG-JYURRK
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスタ読み込み
      *****************************************************************
       940-SRYACCT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
      *
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
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       900-SRYACT-NEXT-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
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
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
      *
           MOVE    "tbl_hkncombi"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  HKNCOMBI-REC
               MOVE    ZERO            TO  FLG-HKNCOMBI
           ELSE
               INITIALIZE                  HKNCOMBI-REC
               MOVE    1               TO  FLG-HKNCOMBI
           END-IF
      *
           .
       910-HKNCOMBI-READ-EXT.
           EXIT.
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    ZERO                TO  FLG-SYSKANRI
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
      *
           .
       960-SYSKANRI-READ-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為データ次読込
      *****************************************************************
       970-WKSRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
      *
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
      *H24.4
      *****************************************************************
      *    システム管理キー検索処理
      *****************************************************************
       900-SYSYKANRI-KEY-READ-SEC          SECTION.
      *
      *    システム管理検索
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       900-SYSYKANRI-KEY-READ-EXT.
           EXIT.
      *H24.4
      *
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9101-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOSTYMD
           MOVE    SPA-SRYYMD          TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_tensuplus"     TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
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
           PERFORM 990-DBCLOSE-SEC
           .
       9101-TENSUPLUS-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報検索処理
      *****************************************************************
       9102-GENERICNAME-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  GENERICNAME-REC
           INITIALIZE                      GENERICNAME-REC
           MOVE    TNS-YAKKAKJNCD      TO  GENERIC-YAKKAKJNCD
      **** MOVE    SPA-HOSPNUM         TO  GENERIC-HOSPNUM
      *
           MOVE    GENERICNAME-REC     TO  MCPDATA-REC
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_genericname"   TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  GENERICNAME-REC
                   MOVE    ZERO                TO  FLG-GENERICNAME
               ELSE
                   INITIALIZE                  GENERICNAME-REC
                   MOVE    1                   TO  FLG-GENERICNAME
               END-IF
           ELSE
               INITIALIZE                      GENERICNAME-REC
               MOVE    1                   TO  FLG-GENERICNAME
           END-IF
           MOVE    "tbl_genericname"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       9102-GENERICNAME-READ-EXT.
           EXIT.
      *H24.4
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
      *    ＤＢＣＬＯＳＥ処理
      *****************************************************************
       990-DBCLOSE-SEC         SECTION.
      *
           MOVE    "DBCLOSECURSOR"      TO  MCP-FUNC
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       990-DBCLOSE-EXT.
           EXIT.
      *
      *v.4.8
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
      *    排他制御解除処理
      *****************************************************************
       990-LOCK-OUT-SEC         SECTION.
      *
      *    ワーク診療行為クリア
           PERFORM 4900-WKSRYACT-CLEAR-SEC
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
