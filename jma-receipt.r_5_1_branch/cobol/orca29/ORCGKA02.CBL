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
       PROGRAM-ID.                 ORCGKA02.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 処方せん・薬剤情報再印刷
      *  コンポーネント名  : 入院投薬帳票印刷（ＫＡ０２）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  08/08/29    NACL-多々納   新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/09/XX  '0086'コメントコード対応
      *  04.05.00    NACL-多々納  10/01/XX  治験対応
      *  04.06.00    NACL-多々納  11/02/22  用法コメント対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.07.00    NACL-多々納  12/01/10  クライアント印刷対応
      *  04.07.00    NACL-多々納  12/09/03  在宅器材・商品名対応
      *  04.07.00    NACL-多々納  13/07/XX  ＱＲ患者個別設定対応
      *  04.08.00    NACL-多々納  14/08/11  クライアント印刷変更
      *  04.08.00    NACL-多々納  16/05/XX  一般名表示対応
      *  04.08.00    NACL-多々納  16/11/XX   用法コード数量追加
      *  05.00.00    NACL-多々納  17/05/XX  API(GINBEE)対応
      *  05.00.00    NACL-多々納  20/03/XX  Ｒ０２年４月改正対応
      *****************************************************************
      *
       ENVIRONMENT                 DIVISION.
       CONFIGURATION               SECTION.
       INPUT-OUTPUT                SECTION.
       FILE-CONTROL.
      *
      *    選択内容データ
      *    SELECT  SPAPRT-FILE     ASSIGN  FILE-NAME1
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPAPRT.
      *
      *    ＳＰＡデータ
      *    SELECT  SPASCR-FILE     ASSIGN  FILE-NAME2
      *                            ORGANIZATION    IS  SEQUENTIAL
      *                            FILE    STATUS  IS  STS-SPASCR.
      *
       DATA                        DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  SPAPRT-FILE.
      *01  SPAPRT-REC              PIC X(10000).
      *
      *    ＳＰＡパラメタデータ
      *FD  SPASCR-FILE.
      *01  SPA-DATA-REC            PIC X(10000).
      *
       WORKING-STORAGE             SECTION.
      *
      * ＳＰＡデータ
      *01  FILE-NAME1.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "KA02PRT".
      *    03  FILE-HOSPNUM1       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID1        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(04)   VALUE   ".TXT".
      *    パラメタファイル名
      *01  FILE-NAME2.
      *    03  FILLER              PIC X(09)   VALUE   "/var/tmp/".
      *    03  FILLER              PIC X(07)   VALUE   "KA02SPA".
      *    03  FILE-HOSPNUM2       PIC 9(02)   VALUE   ZERO.
      *    03  FILE-TERMID2        PIC X(64)   VALUE   SPACE.
      *    03  FILLER              PIC X(06)   VALUE   ".TXT".
      *
      *    画面色等
           COPY    "ENUM-VALUE".
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *
           COPY    "KA01COMMON-SPA".
      *
      *    画面スパ領域
           COPY    "KA02SCR-SPA".
      *
      *    画面スパ領域２
           COPY    "KA02SCR-SPA2".
      *
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-SPAPRT          PIC X(02).
      *    03  STS-SPASCR          PIC X(02).
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-SPASCR          PIC 9(01).
           03  FLG-SPAPRT          PIC 9(01).
           03  FLG-END             PIC 9(01).
           03  FLG-END2            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-PTNYUINRRK      PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
           03  FLG-PTCONF          PIC 9(01).
           03  FLG-SPATMP          PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-KOUSIN          PIC 9(01).
           03  FLG-ZAITAKU         PIC 9(01).
           03  FLG-PRINT           PIC 9(01).
           03  FLG-SP              PIC 9(01).
           03  FLG-YAKUZAI         PIC 9(01).
           03  FLG-HENKOU          PIC 9(01).
           03  FLG-KANSAN          PIC 9(01).
      *    クライアント印刷有
           03  FLG-CLIENT-OK       PIC 9(01).
      *
           03  FLG-FUKA            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDZ                 PIC 9(04).
           03  IDG                 PIC 9(04).
           03  IDG-K               PIC 9(04).
           03  IDM                 PIC 9(04).
           03  IDX-T               PIC 9(04).
           03  IDX-J               PIC 9(04).
           03  IDX-J2              PIC 9(04).
           03  IDX-S               PIC 9(04).
           03  IDX-R               PIC 9(04).
           03  IDX-B               PIC 9(04).
           03  IDX-G               PIC 9(04).
           03  IDX-W               PIC 9(04).
           03  IDX-W2              PIC 9(04).
           03  IDK1                PIC 9(04).
           03  IDK2                PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX3                PIC 9(04).
      *
           03  IDX-KOUI             PIC 9(04).
           03  IDX-KAI              PIC 9(04).
      *
           03  IDX11               PIC 9(01).
           03  IDX2                PIC 9(04).
           03  IDX-S2              PIC 9(02).
           03  IDX-STR             PIC 9(04).
      *
           03  IDX-Z               PIC 9(04).
      *
           03  IDXP                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SELNUM              PIC 9(03).
           03  WRK-SELNUM2             PIC 9(03).
           03  WRK-SELNUM-T            PIC 9(03).
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-ZAIKAISU        PIC 9(08).
           03  WRK-EDANUM          PIC 9(02).
           03  WRK-SRYYMD          PIC X(08).
           03  WRK-YUKOSTYMD       PIC X(08).
           03  WRK-DAY-TBL.
               05  WRK-DAY-T           PIC 9(03)   OCCURS   31.
      *
           03  WRK-NYUINYMD            PIC X(08).
           03  WRK-TAIINYMD            PIC X(08).
           03  WRK-DRCD                PIC X(05).
      *
          03  WRK-ZAITEN           PIC 9(07).
          03  WRK-KAISU-X.
               05  WRK-KAISU       PIC Z(08).
           03  WRK-TENSU-X.
               05  WRK-TENSU       PIC Z(08).
      *
      *    診療行為名称
           03  WRK-SRYSYUMEI       PIC X(40).
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
      *    剤回数等画面編集用
           03  WRK-KAISU-H         PIC X(08).
           03  WRK-KAISU-J         PIC X(16).
           03  WRK-IDG             PIC 9(04).
           03  WRK-ZAIKEIJ         PIC X(80).
           03  IDJ                 PIC 9(04).
           03  IDJ1                PIC 9(01).
           03  IDJ2                PIC 9(04).
      *
           03  WRK-Z9              PIC Z,ZZZ,ZZZ.
           03  WRK-Z92             PIC --,---,---.
           03  WRK-ZZ              PIC ZZZ.
           03  WRK-ZZ9             PIC ZZZ.
      *
           03  WRK-SRYKAISUKEI         PIC 9(03).
      *
           03  WRK-TBANGO              PIC 9(03).
      *
           03  WRK-MCP-WIDGET          PIC X(68).
      *    処方せんＰＧ名
           03  WRK-SYOHOU-PG           PIC X(20).
      *    薬剤情報ＰＧ名
           03  WRK-YAKJYO-PG           PIC X(20).
      *    お薬手帳ＰＧ名
           03  WRK-OKUSURI-PG          PIC X(20).
      *    病棟・病室
           03  WRK-BTUNAME             PIC X(20).
           03  WRK-ROOMNO              PIC X(06).
           03  WRK-BRMNUM              PIC 9(06).
           03  WRK-Z6                  PIC ZZZZZ9.
      *
           03  WRK-ERRCD               PIC X(04).
      *
      *    コラムリスト位置
           03  WRK-KA02-RRKLIST-ROW    PIC S9(09).
           03  WRK-KA02-MEILIST-ROW    PIC S9(09).
           03  FLG-GMN-INIT1           PIC  9(01).
           03  FLG-GMN-INIT2           PIC  9(01).
      *    換算値数量表示用
           03  WRK-TSURYO              PIC X(30).
           03  WRK-G-SURYO             PIC X(30).
           03  IDM2                    PIC 9(04).
           03  IDM3                    PIC 9(04).
           03  WRK-KANSURYO            PIC X(12).
           03  WRK-SHO                 PIC 9(04).
           03  WRK-AMARI               PIC 9(04).
           03  WRK-KANZANTANINAME      PIC X(08).
           03  WRK-KANSURYO-9          PIC 9(05)V9(05).
      *    用法テーブル
           03  WRK-YOHOU-G.
               05  WRK-YOHOU-TBL       OCCURS  10.
                   07  WRK-YOHOU           PIC X(09).
                   07  WRK-YOHOU-KBN       PIC X(01).
           03  WRK-YSRYCD              PIC X(09).
           03  IDXY                    PIC 9(04).
      *
      *    商品名単位名称
           03  WRK-SYO-TANINAME        PIC X(12).
      *    商品名コード
           03  WRK-SHO-SRYCD           PIC X(09).
      *
      *    ＱＲ設定区分
           03  WRK-QRKBN               PIC X(01).
           03  WRK-CSVKBN              PIC X(01).
           03  WRK-PRTKBN              PIC X(01).
      *
      *H30.12
           03  WRK-INPUT-SURYO         PIC 9(03).
      *
      *   年齢表示
       01  WRK-NENREI-G.
           03  WRK-NENREI-Z        PIC ZZ9.
           03  WRK-NENREI-MEI      PIC X(04).
       01  WRK-NENREI-GR           REDEFINES   WRK-NENREI-G.
           03  WRK-NENREI-ZN       PIC Z9.
           03  WRK-NENREI-MEIN     PIC X(08).
      *
      *
       01  WRK-HENSYU-AREA.
           03  WRK-HENYMDG         PIC X(09).
           03  WRK-HENYMD.
               05  WRK-HGO         PIC X(01).
               05  WRK-HYY         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HMM         PIC Z9.
               05  FILLER          PIC X(01)   VALUE  ".".
               05  WRK-HDD         PIC Z9.
      *画面行ＭＡＸ
       01  MAX-GMN                 PIC 9(04)   VALUE   600.
      *
      *    ＳＰＡ保存
           COPY    "COMMON-SPA"    REPLACING
                                   //SPA-// BY //HZN-SPA-//.
      *
      *    診療種別名テーブル
           COPY    "CPORCSSRYSYU.INC".
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK5001.INC".
      *
           COPY    "CPSK1005.INC".
      *    職員情報
           COPY    "CPSK1010.INC".
      *    帳票ＰＧ
           COPY    "CPSK1032.INC".
      *    診療行為管理情報 
           COPY    "CPSK1038.INC".
      *
           COPY    "CPSK1030.INC".
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    入力コードマスタ−
       01  INPUTCD-REC.
           COPY    "CPINPUTCD.INC".
      *
      *    診療行為マスタ−
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
      *
      *    診療会計附加情報マスタ−
       01  SRYACCTPLUS-REC.
           COPY    "CPSRYACCTPLUS.INC".
      *
      *    患者コメント
       01  PTCOM-REC.
           COPY    "CPPTCOM.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *    受診履歴
       01  HZN-JYURRK-REC.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //HZN-JYURRK-//.
      *    受診履歴
       01  WRK-JYURRK-TBL-G.
          02  WRK-JYURRK-REC         OCCURS   9.
           COPY    "CPJYURRK.INC"    REPLACING
                                     //JYURRK-// BY //WRK-JYURRK-//.
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   50.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    患者個別設定
       01  PTCONF-REC.
           COPY    "CPPTCONF.INC".
      *
      *TEST
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
           COPY    "CPORCSNUM.INC".
      *
      *    画面日付変換サブ
           COPY    "CPORCSGDAY.INC".
      *
      *    全角変換サブ
           COPY    "CPORCSKANACHK.INC".
      *    全角・半角チェック
      **** COPY    "CPORCXKANACONV.INC".
      *    保険年齢・負担割合算定サブ
           COPY    "CPORCSAGECHK.INC".
      *
      *    ドクター編集変換サブ
           COPY    "CPORCSDRSET.INC".
      *
      *    保険組合せセット領域
           COPY    "CPORCSHKNSET.INC".
      *
      *    処方箋印刷パラメタ（Ａ５）
           COPY    "CPORCHC19.INC".
      *
      *    入院薬情報印刷パラメタ
           COPY    "CPORCSCHN30.INC".
      *
      *    お薬手帳発行サブパラメタ
      **** COPY    "CPORCSCH62.INC".
      *    入院お薬手帳発行パラメタ
           COPY    "CPORCHCN62.INC".
      *
      *    入院院外処方せんパラメタ領域
           COPY    "CPORCSNYUINSHO.INC".
      *
      *    表示名称編集パラメタ領域
           COPY    "CPORCSMEIHEN.INC".
      *H28.5
      *    院外処方名編集パラメタ
           COPY    "CPORCSCGENNAME.INC".
      *
      *    ファイル削除パラメタ
           COPY    "CPORCSFDEL.INC".
      *
      *    オンライン帳票名・出力先プリンタ名取得パラ
           COPY    "CPORCSPRTNM.INC".
      *
      *    クライアント印刷制御
           COPY    "CPORCSPRTCTRL.INC".
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
           COPY    "KA01.INC".
           COPY    "KA02.INC".
           COPY    "KA021.INC".
           COPY    "KAERR.INC".
           COPY    "KAID1.INC".
           COPY    "KAID2.INC".
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
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-KA01KYOUTU
           MOVE    SPA-FREE            TO  SPA-KA02
      *
      *    画面ＳＰＡ読み込み
           PERFORM 1002-KA02SPA-SET-SEC
      *
           MOVE    SPACE               TO  SPA-ERRCD
           MOVE    ZERO                TO  FLG-END
           EVALUATE    MCP-STATUS      ALSO    MCP-EVENT
               WHEN    "LINK"          ALSO    ANY
                   PERFORM 100-INIT-SEC
      *    画面遷移
               WHEN     "PUTG"           ALSO   "CLICKED"
                   PERFORM 200-GMNSENI
      *    入力
               WHEN      OTHER
                   PERFORM 200-ENTRY
           END-EVALUATE
      *
      *    画面遷移がない時、画面表示へ
           IF      FLG-END             =   ZERO
               PERFORM 500-SET-SCREEN
           END-IF
      *    画面ＳＰＡ書込み
           IF      FLG-END2            =   ZERO
               PERFORM 1003-KA02SPA-OUT-SEC
           END-IF
      *
      *    クライアント印刷
      *    IF      FLG-CLIENT-OK       =   1
      *        PERFORM 600-CLIENT-DMY-SEC
      *    END-IF
      *
           MOVE    SPA-KA01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KA02            TO  SPA-FREE
      *
           .
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    ＳＰＡデータＲＥＡＤ処理
      *****************************************************************
       1002-KA02SPA-SET-SEC             SECTION.
      *
           MOVE    SPACE               TO  SPA-KA02-DATA
           INITIALIZE                      SPA-KA02-DATA
      *
           MOVE    ZERO                TO  SPA-PARA-MAX
           MOVE    ZERO                TO  SPA-WKSRY-MAX
      *
      *    tbl_spa_tmp を使用する
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL  (FLG-SPATMP          =   1   )
                 OR   (IDX                 >=  100 )
               ADD    1                    TO  IDX
               MOVE   SPATMP-DATA-REC      TO  SPA-NAI-WKSRYREC
                                                       (IDX)
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           END-PERFORM
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-WKSRY-MAX
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPANAI"            TO  SPATMP-FILEMEI
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           ELSE
               INITIALIZE                  SPATMP-REC
               MOVE    1               TO  FLG-SPATMP
           END-IF
           MOVE    ZERO                TO  IDX
           PERFORM
               UNTIL  (FLG-SPATMP          =   1   )
                 OR   (IDX                 >=  100 )
               ADD    1                    TO  IDX
               MOVE   SPATMP-DATA-REC      TO  SPA-PARA-ZAI-REC
                                                       (IDX)
      *
               MOVE    "tbl_spa_tmp"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 990-SPATMP-READ-SEC
           END-PERFORM
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-SPATMP-DBCLOSE-SEC
      *
           MOVE    IDX                 TO  SPA-PARA-MAX
      *
           .
       1002-KA02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡデータＷＲＩＴＥ処理
      *****************************************************************
       1003-KA02SPA-OUT-SEC             SECTION.
      *
      *    ＳＰＡ一時保存テーブル削除
           PERFORM 10031-SPATMP-DELETE-SEC
      *
      *    SPA-GMN-TBL保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPAGMN"            TO  SPATMP-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   100  )  OR
                              (SPA-WKSRY-ZAINUM (IDX)  =   ZERO)
      *
               MOVE    SPA-NAI-WKSRYREC (IDX)  TO  SPATMP-DATA-REC
               MOVE    IDX                     TO  SPATMP-EDANUM
      *
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "KT02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               END-IF
           END-PERFORM
      *
      *    SPA-GMN-TBL保存
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
           MOVE    "SPANAI"            TO  SPATMP-FILEMEI
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   100  )  OR
                              (SPA-PARA-ZAINUM (IDX)  =   ZERO)
      *
               MOVE    SPA-PARA-ZAI-REC (IDX)  TO  SPATMP-DATA-REC
               MOVE    IDX                     TO  SPATMP-EDANUM
      *
               MOVE    SPATMP-REC          TO  MCPDATA2-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-SPATMP-CALL-SEC
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "KT02 SPATMP INSERT ERR:"  MCP-RC
                           ",KEY:" SPATMP-KEY
               END-IF
           END-PERFORM
      *
      *
           .
       1003-KA02SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    ＳＰＡ一時保存テーブル削除処理
      *****************************************************************
       10031-SPATMP-DELETE-SEC                SECTION.
      *
           INITIALIZE                      SPATMP-REC
           MOVE    SPA-HOSPNUM         TO  SPATMP-HOSPNUM
           MOVE    "KA02"              TO  SPATMP-GYOUMUID
           MOVE    SPA-TERMID          TO  SPATMP-TERMID
      *
           MOVE    SPATMP-REC          TO  MCPDATA2-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "del2"              TO  MCP-PATHNAME
           PERFORM 910-SPATMP-CALL-SEC
      *
           .
       10031-SPATMP-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期　処理
      *****************************************************************
       100-INIT-SEC                SECTION.
      *
      *    エラー画面より
           IF      SPA-MOTOPG          =   "KAERR"
               MOVE    SPACE               TO  SPA-MOTOPG
               PERFORM 5001-MAPCUR-SEC
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
               MOVE    SPACE               TO  LNK-KYOUTU
      *
               MOVE   SPACE                TO  MCP-PUTTYPE
               MOVE   "KA02"               TO  MCP-WINDOW
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
           IF      LNK-KYOUTU      NOT =   SPACE
               MOVE    LNK-KYOUTU          TO  SPA-AREA
           END-IF
      *
      *    内部画面より
           EVALUATE    SPA-MOTOPG
               WHEN    "KAID1"
      *            確認画面より
                   PERFORM 3002-KAID1-SET-SEC
               WHEN    "KAID2"
      *            確認画面より
                   PERFORM 3003-KAID2-SET-SEC
               WHEN    "KA021"
      *            選択確認画面より
                   MOVE    1               TO  SPA-GMN-CUR
               WHEN    OTHER
                   PERFORM 3001-SPA-SET-SEC
           END-EVALUATE
      *
           .
       300-SCREEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    初期画面編集処理
      *****************************************************************
       3001-SPA-SET-SEC             SECTION.
      *
           INITIALIZE                  SPA-KA02
           INITIALIZE                  SPA-KA01KYOUTU
      *
           MOVE    SPA-MOTOPG          TO  SPA-HZN-MOTOPG
      *
      *    診療日
           IF      SPA-SRYYMD          =   SPACE
               MOVE    SPA-SYSYMD          TO  SPA-SRYYMD
               MOVE    SPA-SYSYMDWH        TO  SPA-SRYYMDW
               MOVE    SPA-SYSYMDWH        TO  SPA-GMN-SRYYMD
           ELSE
               MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-SRYYMD
           END-IF
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *    交付日
           MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-KOUFUYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-KOUFUYMD
      *    入院
           MOVE    "1"                 TO  SPA-NYUGAIKBN
      *    患者情報検索
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 900-PTINF-READ-SEC
      *
           MOVE    SPA-PTNUM           TO  SPA-GMN-PTNUM
           MOVE    PTINF-KANANAME      TO  SPA-GMN-KANANAME
           MOVE    PTINF-NAME          TO  SPA-GMN-NAME
           EVALUATE    PTINF-SEX
               WHEN    "1"
                   MOVE    "男"                TO  SPA-GMN-SEX
               WHEN    "2"
                   MOVE    "女"                TO  SPA-GMN-SEX
           END-EVALUATE
      *
           MOVE    PTINF-BIRTHDAY      TO  SPA-BIRTHDAY
           MOVE    PTINF-BIRTHDAY      TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-BIRTHDAY
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
      *    画面表示用
           IF      AGECHK-NENREI2-YY   =   ZERO
               MOVE    AGECHK-NENREI2-MM   TO  WRK-NENREI-ZN
               MOVE    "ヶ月"              TO  WRK-NENREI-MEIN
           ELSE
               MOVE    AGECHK-NENREI2-YY   TO  WRK-NENREI-Z
               MOVE    "才"                TO  WRK-NENREI-MEI
           END-IF
           MOVE    WRK-NENREI-G        TO  SPA-GMN-NENREI
      *
      *    診療科初期編集
           MOVE    SPACE               TO  SYS-KEY
           MOVE    SPACE               TO  SYS-1005-REC
           INITIALIZE                  SYS-1005-REC
           MOVE    "1005"              TO  SYS-1005-KANRICD
           MOVE    SPA-SRYYMD          TO  SYS-1005-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1005-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-1005-HOSPNUM
           MOVE    SYS-1005-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYS-1005-REC
               INITIALIZE                  SYS-1005-REC
               MOVE    1               TO  FLG-SYSKANRI
           END-IF
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       (IDX            >   99   )  OR
                               (FLG-SYSKANRI   =   1    )
               MOVE    MCPDATA-REC           TO  SYS-1005-REC
               MOVE    SYS-1005-KBNCD (1:2)  TO  SPA-GMN-TSRYKA   (IDX)
               MOVE    SYS-1005-SRYKANAME1   TO  SPA-GMN-TSRYKAMEI(IDX)
               MOVE    IDX                   TO  SPA-SRYKA-MAX
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    保険組合せ
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKNCOMBI-MAX
      *
           PERFORM VARYING     IDX     FROM   1   BY  1
                   UNTIL      (IDX     >   ORCSHKN-HKN-MAX )
                           OR (IDX     >   30   )
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX)
               MOVE    IDX             TO  SPA-HKNCOMBI-MAX
           END-PERFORM
      *
      *    病棟・病室編集
           PERFORM 41041-BTUNAME-HEN-SEC
      *H28.5
           PERFORM 30002-SYS1030-SET-SEC
      *
      *    受診履歴編集
      *    院内分を表示
           MOVE    1                   TO  SPA-NAI-INNAI
           PERFORM 30011-JYURRK-HENSYU-SEC
           IF      SPA-JYURRK-MAX      >   ZERO
      *       １件目を対象として内容編集
               MOVE    1               TO  SPA-GMN-SELNUM1
               PERFORM 30012-MEISAI-SYORI-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    91                  TO  SPA-GMN-CUR
           END-IF
      *
           .
       3001-SPA-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    入退院日編集処理
      *****************************************************************
       30011-NYUINRRK-HEN-SEC          SECTION.
      *
      *    入退院日
           MOVE    SPACE               TO  WRK-NYUINYMD
           MOVE    SPACE               TO  WRK-TAIINYMD
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    SPACE               TO  WRK-TAIINYMD
           MOVE    SPACE               TO  WRK-NYUINYMD
           MOVE    SPACE               TO  WRK-DRCD
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )
      *        転入日
               IF      PTNYUINRRK-TENNYUYMD    <=  SPA-NAI-SRYYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  SPA-NAI-SRYYMD)
      *                入院対象処理
                       IF     (PTNYUINRRK-TAIINYMD
                                                   =   SPA-NAI-SRYYMD)
                         OR   (PTNYUINRRK-TENSTUYMD
                                                   >   SPA-NAI-SRYYMD)
                           MOVE    1               TO  FLG-OK
      *                    入院期間
                           MOVE    PTNYUINRRK-NYUINYMD TO  WRK-NYUINYMD
                           MOVE    PTNYUINRRK-TAIINYMD TO  WRK-TAIINYMD
      *                    担当医１（主治医）
                           MOVE    PTNYUINRRK-DRCD1    TO  WRK-DRCD
                       END-IF
      *                同日再入院分
                       IF     (SPA-NAI-DOUKBN      =   "1"  )
                          AND (PTNYUINRRK-NYUINYMD
                                               NOT =   SPA-NAI-SRYYMD )
                           MOVE    ZERO            TO  FLG-OK
                       END-IF
                   END-IF
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key34"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key34"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    入院日
           MOVE    WRK-NYUINYMD        TO  SPA-NAI-NYUINYMD
           MOVE    SPA-NAI-NYUINYMD    TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-NYUINYMD
      *    退院日
           MOVE    WRK-TAIINYMD        TO  SPA-NAI-TAIINYMD
           IF      SPA-NAI-TAIINYMD    =   SPACE
               MOVE    ALL "9"             TO  SPA-NAI-TAIINYMD
               MOVE    "-"                 TO  SPA-GMN-NH1
               MOVE    SPACE               TO  SPA-GMN-TAIINYMD
           ELSE
               MOVE    "-"                 TO  SPA-GMN-NH1
               MOVE    SPA-NAI-TAIINYMD    TO  WRK-SYMD
               PERFORM 800-SEIWA-HEN-SEC
               MOVE    WRK-HENYMDG         TO  SPA-GMN-TAIINYMD
           END-IF
           MOVE    WRK-DRCD            TO  SPA-NAI-DRCD
           .
       30011-NYUINRRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    一般名記載区分取得
      *****************************************************************
       30002-SYS1030-SET-SEC          SECTION.
      *
           MOVE    SPACE               TO  SPA-NAI-KOUHATUKA
      *
           MOVE    SPACE               TO  SYS-1030-REC
           INITIALIZE                  SYS-1030-REC
           MOVE    "1030"              TO  SYS-1030-KANRICD
           MOVE    "*"                 TO  SYS-1030-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1030-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1030-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1030-HOSPNUM
           MOVE    SYS-1030-REC       TO  SYSKANRI-REC
           PERFORM 910-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1030-REC
               MOVE    SYS-1030-KOUHATUKA  TO  SPA-NAI-KOUHATUKA
           END-IF
      *
           MOVE    SPACE               TO  SPA-NAI-GENERICFLG
      *
           MOVE    SPACE               TO  SYS-1038-REC
           INITIALIZE                  SYS-1038-REC
           MOVE    "1038"              TO  SYS-1038-KANRICD
           MOVE    "*"                 TO  SYS-1038-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-1038-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-1038-EDYUKYMD
           MOVE    SPA-HOSPNUM         TO  SYS-1038-HOSPNUM
           MOVE    SYS-1038-REC       TO  SYSKANRI-REC
           PERFORM 910-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-1038-REC
      *        一般名記載
               IF      SYS-1038-GENERICFLG     =   "2"
                   MOVE    "1"             TO  SPA-NAI-GENERICFLG
               END-IF
           END-IF
           .
       30002-SYS1030-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴編集処理
      *****************************************************************
       30011-JYURRK-HENSYU-SEC          SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT1
           INITIALIZE                  SPA-GMN-RRKTBL
           INITIALIZE                  SPA-NAI-RRKTBL
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                  JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  IDX
           INITIALIZE                  WRK-JYURRK-TBL-G
           PERFORM
                   UNTIL   (SPA-JYURRK-MAX >=  100  )  OR
                           (FLG-JYURRK     =   1    )
      *H25.2
      *        レセ電移行分は対象外
             IF      JYURRK-SRYKA (1:1)    =   "A"
               CONTINUE
             ELSE
      *
      *        院内処方は包括保険を対象とする
               IF     (SPA-NAI-INNAI       =   2  )  AND
                      (JYURRK-HKNCOMBINUM  =   "9999" )
                   CONTINUE
               ELSE
      *            伝票番号毎にテーブル編集
                   IF     (IDX             >   ZERO )  AND
                          (JYURRK-EDANUM   =   1    )
      *                編集処理
                       PERFORM 300111-JYURRK-CHK-SEC
                       MOVE    ZERO                TO  IDX
                       INITIALIZE                  WRK-JYURRK-TBL-G
                   END-IF
                   ADD     1               TO  IDX
                   MOVE    JYURRK-REC      TO  WRK-JYURRK-REC (IDX)
               END-IF
      *
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
      *    編集処理
           IF      IDX             >   ZERO
                PERFORM 300111-JYURRK-CHK-SEC
           END-IF
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       30011-JYURRK-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴チェック・編集処理
      *****************************************************************
       300111-JYURRK-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDX-J   FROM    1   BY  1
                   UNTIL      (IDX-J   >   IDX     )   OR
                              (FLG-OK  =   1       )
               MOVE    WRK-JYURRK-REC (IDX-J)  TO  HZN-JYURRK-REC
      *
               IF      SPA-NAI-INNAI       =   1
                   IF     (HZN-JYURRK-SRYKBN2  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN3  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN4  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN5  =   "01" )
                   OR     (HZN-JYURRK-SRYKBN1  =   "01" )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               ELSE
                   IF     (HZN-JYURRK-SRYKBN1  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN2  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN3  =   "01" )  OR
                          (HZN-JYURRK-SRYKBN4  =   "01" )
                       MOVE    1               TO  FLG-CHK
                   END-IF
               END-IF
               IF      FLG-CHK         =   1
                   PERFORM VARYING     IDX-J2  FROM    1   BY  1
                           UNTIL      (IDX-J2  >   15  )   OR
                                      (FLG-OK  =   1       )
                       IF      HZN-JYURRK-ZAINUM(IDX-J2)
                                                   NOT =   ZERO
                           MOVE    HZN-JYURRK-ZAINUM(IDX-J2)
                                                       TO  WRK-ZAINUM
                           PERFORM 300112-SRYACCT-CHK-SEC
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
           IF     FLG-OK               =   1
               IF      SPA-JYURRK-MAX      <    100
                   PERFORM 300113-JYURRK-HEN-SEC
               END-IF
           END-IF
      *
           .
       300111-JYURRK-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    院内・院外薬剤剤判定処理
      *****************************************************************
       300112-SRYACCT-CHK-SEC         SECTION.
      *
           MOVE    SPACE               TO  SRYACT-REC
           INITIALIZE                      SRYACT-REC
           MOVE    HZN-JYURRK-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    HZN-JYURRK-NYUGAIKBN    TO  SRY-NYUGAIKBN
           MOVE    HZN-JYURRK-PTID         TO  SRY-PTID
           MOVE    HZN-JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    HZN-JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    WRK-ZAINUM          TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           IF      SPA-NAI-INNAI       =   1
      *         院内
               IF     (SRY-SRYKBN      =   "31"
                                       OR  "32"
                                       OR  "33"
                                       OR  "34"
                                       OR  "21"
                                       OR  "22"
                                       OR  "23"
                                       OR  "14")
                   IF      SRY-SRYSYUKBN   =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "295"
                                           OR  "298"
                       MOVE    1               TO  FLG-SRYACT
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           ELSE
      *        院外
               IF      SRY-SRYSYUKBN   =   "212"
                                       OR  "222"
                                       OR  "232"
                                       OR  "292"
                                       OR  "295"
                                       OR  "298"
                                       OR  "148"
                                       OR  "149"
                   CONTINUE
                   MOVE    1               TO  FLG-OK
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           END-IF
           PERFORM
                   UNTIL  (FLG-SRYACT      =   1  )  OR
                          (FLG-OK          =   1  )
      *        薬剤・器材判定
               PERFORM VARYING     IDX-S   FROM    1   BY  1
                       UNTIL      (IDX-S   >   5   )  OR
                                  (SRY-SRYCD (IDX-S)  =   SPACE)
                   IF     (SRY-SRYCD(IDX-S)(1:1)   =   "6" 
                                                   OR  "7"  )  OR
                          (SRY-SRYCD(IDX-S)(1:3)   =   "059" )
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       300112-SRYACCT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面編集処理
      *****************************************************************
       300113-JYURRK-HEN-SEC          SECTION.
      *
           MOVE    WRK-JYURRK-REC (1)  TO  HZN-JYURRK-REC
      *
           ADD     1                   TO  SPA-JYURRK-MAX
           MOVE    SPA-JYURRK-MAX      TO  IDX-J
           MOVE    IDX-J               TO  SPA-GMN-NUM  (IDX-J)
      *    連番
           MOVE    HZN-JYURRK-RENNUM   TO  SPA-NAI-RENNUM(IDX-J)
      *    連番
           MOVE    HZN-JYURRK-DOUJI-RENNUM
                                       TO  SPA-NAI-DOUJI-RENNUM(IDX-J)
      *    伝票番号
           MOVE    HZN-JYURRK-DENPNUM  TO  SPA-NAI-RRK-DENPNUM(IDX-J)
      *    診察日
           MOVE    HZN-JYURRK-SRYYMD   TO  SPA-NAI-RRKYMD(IDX-J)
           MOVE    HZN-JYURRK-SRYYMD   TO  WRK-SYMD
           PERFORM 800-SEIWA-HEN-SEC
           MOVE    WRK-HENYMDG         TO  SPA-GMN-RRKYMD (IDX-J)
      *    保険組合せ
           MOVE    HZN-JYURRK-HKNCOMBINUM
                                       TO  SPA-NAI-RRK-HKNCOMBI(IDX-J)
           MOVE    HZN-JYURRK-SRYKA    TO  SPA-NAI-RRK-SRYKA (IDX-J)
           MOVE    HZN-JYURRK-DRCD     TO  SPA-NAI-RRK-DRCD  (IDX-J)
      *    同日再診
           IF     (HZN-JYURRK-RENNUM        >   1 ) OR
                  (HZN-JYURRK-DOUJI-RENNUM  >  ZERO )
               MOVE    "("                 TO  SPA-GMN-RRKYMD (IDX-J)
                                                               (10:1)
               MOVE    HZN-JYURRK-RENNUM   TO  SPA-GMN-RRKYMD (IDX-J)
                                                              (11:1)
               MOVE    ")"                 TO  SPA-GMN-RRKYMD (IDX-J)
                                                              (12:1)
               IF      HZN-JYURRK-DOUJI-RENNUM NOT =   ZERO
                   MOVE    HZN-JYURRK-DOUJI-RENNUM 
                                           TO  SPA-GMN-RRKYMD (IDX-J)
                                                              (13:1)
               END-IF
           END-IF
      *    保険組合せ
           MOVE    SPA-NAI-RRK-HKNCOMBI(IDX-J)
                                           TO  SPA-GMN-RRKHKNCOMBI
                                                              (IDX-J)
           IF      HZN-JYURRK-HKNKBN       =   "1"
               MOVE    "労"                TO  SPA-GMN-RRKHKNCOMBI
                                                              (IDX-J)
           END-IF
           IF      HZN-JYURRK-HKNKBN       =   "2"
               MOVE    "自"                TO  SPA-GMN-RRKHKNCOMBI
                                                              (IDX-J)
           END-IF
      *    科名
           MOVE    HZN-JYURRK-SRYKA    TO  SPA-GMN-RRKSRYKA(IDX-J)
           PERFORM VARYING IDY     FROM    1   BY  1
                   UNTIL   IDY         >   SPA-SRYKA-MAX
               IF      HZN-JYURRK-SRYKA    =   SPA-GMN-TSRYKA (IDY)
                   MOVE    SPA-GMN-TSRYKAMEI (IDY)
                                           TO  SPA-GMN-RRKSRYKA(IDX-J)
                   MOVE    SPA-SRYKA-MAX   TO  IDY
               END-IF
           END-PERFORM
           .
       300113-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    明細内容表示処理
      *****************************************************************
       30012-MEISAI-SYORI-SEC         SECTION.
      *
           MOVE    1                   TO  FLG-GMN-INIT2
           MOVE    ZERO                TO  SPA-KEIFLG2
      *
      *    診療種別テーブルセット
           CALL    "ORCSSRYSYU"        USING
                                       MSYU-DATA-REC
      *
           MOVE    SPA-GMN-SELNUM1     TO  IDX
      *    診療日等編集
           PERFORM 300121-HEAD-HEN-SEC
      *
           INITIALIZE                  SPA-GMN-MEISAI-AREA
           INITIALIZE                  SPA-NAI-WKSRY-TBL
           MOVE    ZERO                TO  SPA-WKSRY-MAX
           MOVE    ZERO                TO  SPA-KEIFLG
      *
      *    受診歴を検索する
           INITIALIZE                  JYURRK-KEY
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      *
           MOVE    SPA-NAI-RRK-SRYKA (IDX)
                                       TO  JYURRK-SRYKA
           MOVE    SPA-NAI-RRKYMD (IDX)
                                       TO  JYURRK-SRYYMD
           MOVE    SPA-NAI-RENNUM (IDX)
                                       TO  JYURRK-RENNUM
           MOVE    SPA-NAI-RRK-HKNCOMBI (IDX)
                                       TO  JYURRK-HKNCOMBINUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key19"             TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           ELSE
               INITIALIZE                  JYURRK-REC
               MOVE    1               TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL   (FLG-JYURRK     =   1 )
                       OR  (SPA-WKSRY-MAX  >=  100 )
                       OR  (IDG            >=  MAX-GMN )
               PERFORM VARYING     IDX-J   FROM    1   BY  1
                       UNTIL      (IDX-J           >   15  )
                   IF      JYURRK-ZAINUM (IDX-J)   NOT =   ZERO
                       PERFORM 41011-ZAISET-HEN-SEC
                   END-IF
               END-PERFORM
      *
               MOVE    "key19"              TO  MCP-PATHNAME
               PERFORM 910-JYURRK-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key19"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    最大件数
           IF      IDG                 >   MAX-GMN
               MOVE    MAX-GMN             TO  SPA-MEISAI-MAX
           ELSE
               MOVE    IDG                 TO  SPA-MEISAI-MAX
           END-IF
      *
      *    すべての行を選択可能とする
           MOVE    ZERO                TO  WRK-TBANGO
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-MEISAI-MAX
               IF      SPA-GMN-TMEISYO(IDX)(1:3)   NOT =   "---"
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-TBANGO
                   END-IF
                   MOVE    WRK-TBANGO      TO  SPA-NAI-TBANGO2(IDX)
               END-IF
           END-PERFORM
      *
      *H28.5
      *    一般名記載
           IF     (SPA-NAI-INNAI       =   2       )
              AND (SPA-NAI-SRYYMD      >=  "20160401")
               PERFORM 300129-MEISYO-CHG-SEC
           END-IF
      *
          .
       30012-MEISAI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療日等編集処理
      *****************************************************************
       300121-HEAD-HEN-SEC           SECTION.
      *
           MOVE    SPA-GMN-RRKYMD (IDX)    TO  SPA-GMN-SRYYMD
           MOVE    SPA-NAI-RRKYMD (IDX)    TO  SPA-NAI-SRYYMD
      *    診療科
           MOVE    SPA-GMN-RRKSRYKA(IDX)   TO  SPA-GMN-SRYKAMEI
      *    同日入院区分
           IF      SPA-NAI-RENNUM (IDX)    =   2
               MOVE    "1"                     TO  SPA-NAI-DOUKBN
           ELSE
               MOVE    SPACE                   TO  SPA-NAI-DOUKBN
           END-IF
      *    選択登録がない時、内容を編集する
           IF      SPA-PARA-MAX        =   ZERO
               PERFORM 300121-INIT-HENSYU-SEC
           END-IF
      *    保険組合せ
           MOVE    SPACE               TO  SPA-GMN-HKNCOMBI-G
           MOVE    SPA-NAI-RRK-HKNCOMBI(IDX)
                                       TO  SPA-GMN-HKNCOMBINUM
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-HKNCOMBI-MAX
               IF      SPA-GMN-HKNCOMBINUM =   SPA-GMN-THKNCOMBINUM
                                                   (IDY)
                   MOVE    SPA-GMN-HKNCOMBI-COMB(IDY)
                                           TO  SPA-GMN-HKNCOMBI-G
                   MOVE    SPA-HKNCOMBI-MAX    TO  IDY
                   MOVE    1                   TO  FLG-CHK
               END-IF
           END-PERFORM
           .
       300121-HEAD-HEN-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴初期編集処理
      *****************************************************************
       300121-INIT-HENSYU-SEC           SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           MOVE    SPA-NAI-SRYYMD      TO  SPA-SRYYMD
      *    保険組合せ
           INITIALIZE                  ORCSHKNSETAREA
           MOVE    "1"                 TO  ORCSHKN-KBN
           MOVE    "1"                 TO  ORCSHKN-KBN2
           CALL    "ORCSHKNSET"        USING
                                       SPA-AREA
                                       ORCSHKNSETAREA
           MOVE    ORCSHKN-HKN-MAX     TO  SPA-HKNCOMBI-MAX
           MOVE    WRK-SRYYMD          TO  SPA-SRYYMD
      *
           PERFORM VARYING     IDX-T     FROM   1   BY  1
                   UNTIL      (IDX-T     >   ORCSHKN-HKN-MAX )
                           OR (IDX-T     >   30   )
               MOVE    ORCSHKN-GMN-HKNCOMBINUM (IDX-T)
                                       TO  SPA-GMN-THKNCOMBINUM (IDX-T)
               MOVE    ORCSHKN-GMN-HKNCOMBIMEI (IDX-T)
                                       TO  SPA-GMN-THKNCOMBIMEI (IDX-T)
               MOVE    IDX-T           TO  SPA-HKNCOMBI-MAX
           END-PERFORM
      *    入退院日編集
           PERFORM 30011-NYUINRRK-HEN-SEC
      *    ドクター
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-NAI-SRYYMD      TO  ORCSDR-SYSYMD
           MOVE    "1"                 TO  ORCSDR-KBN
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-COMB-G
           MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
      *
           MOVE    SPACE               TO  SPA-GMN-DRCD-G
           MOVE    SPA-NAI-DRCD(2:4)   TO  SPA-GMN-DRCD
           MOVE    ZERO                TO  FLG-CHK
           PERFORM VARYING     IDY     FROM    1   BY  1
                   UNTIL       IDY     >   SPA-DRCD-MAX
               IF      SPA-GMN-DRCD    =   SPA-GMN-TDRCD
                                                   (IDY)
                   MOVE    SPA-GMN-DRCD-COMB (IDY)
                                           TO  SPA-GMN-DRCD-G
                   MOVE    SPA-DRCD-MAX    TO  IDY
               END-IF
           END-PERFORM
      *    交付日
      *    退院済みの時、退院日とする
           IF      SPA-NAI-TAIINYMD        =   "99999999"
               MOVE    SPA-SRYYMD          TO  SPA-NAI-KOUFUYMD
               MOVE    SPA-SRYYMDW         TO  SPA-GMN-KOUFUYMD
           ELSE
               MOVE    SPA-NAI-TAIINYMD    TO  SPA-NAI-KOUFUYMD
               MOVE    SPA-GMN-TAIINYMD    TO  SPA-GMN-KOUFUYMD
           END-IF
           MOVE    SPA-NAI-KOUFUYMD       TO  SPA-MAE-KOUFUYMD
           .
       300121-INIT-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    診療行為検索処理
      *****************************************************************
       41011-ZAISET-HEN-SEC           SECTION.
      *
      *    ワーク診療行為のレイアウトへ編集後、画面用編集へ
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
           MOVE    ZERO                TO  IDX-KAI
      *    対象日の位置セット
           MOVE    JYURRK-SRYYMD(7:2)  TO  IDX-S2
           MOVE    JYURRK-SRYYMD       TO  WRK-SRYYMD
      *
           INITIALIZE                  SRYACT-REC
           MOVE    SPA-HOSPNUM         TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    SPA-PTID            TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    JYURRK-ZAINUM(IDX-J)
                                       TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           MOVE    ZERO                TO  WRK-ZAINUM
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ZAITAKU
           MOVE    ZERO                TO  FLG-YAKUZAI
      *    対象判定
           IF      SPA-NAI-INNAI       =   1
      *         院内（投薬・注射）
               IF     (SRY-SRYKBN      =   "31"
                                       OR  "32"
                                       OR  "33"
                                       OR  "34"
                                       OR  "21"
                                       OR  "22"
                                       OR  "23"
                                       OR  "14"
                                       OR  "98" )
                   IF      SRY-SRYSYUKBN   =   "212"
                                           OR  "222"
                                           OR  "232"
                                           OR  "292"
                                           OR  "295"
                                           OR  "298"
                                           OR  "982"
                                           OR  "148"
                                           OR  "149"
                       MOVE    1               TO  FLG-SRYACT
                   END-IF
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           ELSE
      *        院外（院外処方）
               IF      SRY-SRYSYUKBN   =   "212"
                                       OR  "222"
                                       OR  "232"
                                       OR  "292"
                                       OR  "295"
                                       OR  "298"
                                       OR  "148"
                                       OR  "149"
                                       OR  "982"
                   CONTINUE
               ELSE
                   MOVE    1               TO  FLG-SRYACT
               END-IF
           END-IF
           PERFORM
                   UNTIL   FLG-SRYACT      =   1
      *        診療会計検索編集
               PERFORM 41012-SRYACT-HEN-SEC
      *
               IF      IDY                 >   ZERO
                   ADD     1               TO  IDX-KOUI
                   MOVE    WKSRYACT-REC    TO  LNK-WKSRYACT-REC
                                                          (IDX-KOUI)
               END-IF
      *
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 930-SRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF     (IDX-KOUI        >   ZERO ) AND
                  (FLG-OK          =   1    ) AND
                  (SPA-WKSRY-MAX   <   100  )
      *H28.5
      *            一般名編集（院外分）
                   IF     (SPA-NAI-INNAI       =   2   )
                      AND (WRK-SRYYMD          >=  "20160401")
                       PERFORM 410112-GENERIC-HEN-SEC
                   END-IF
      *
      *TEST
      *        剤毎に処理を行う
               PERFORM 3101-WKSRYACT-HEN-SEC
           END-IF
           .
       41011-ZAISET-HEN-EXT.
           EXIT.
      *TEST
      *H28.5
      *****************************************************************
      *     一般名編集処理
      *****************************************************************
       410112-GENERIC-HEN-SEC              SECTION.
      *
           INITIALIZE                  ORCSCGENNAMEAREA
           MOVE    "0"                 TO  ORCSCGENN-SYORIKBN
           MOVE    "0"                 TO  ORCSCGENN-SYORIKBN2
           MOVE    WRK-SRYYMD          TO  ORCSCGENN-SRYYMD
      *    後発医薬品への変更可署名(SYS-1030-KOUHATUKA)
           MOVE    SPA-NAI-KOUHATUKA   TO  ORCSCGENN-1030-KOUHATUKA
      *    後発品変更不可（処方毎）有
      *    IF      WRK-FUKA-INPUT      =   "1"
      *        MOVE    "1"             TO  ORCSCGENN-KOUHATU-IN
      *    END-IF
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                          OR  (IDX-Z   >=  50 )
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                              OR  (IDX-Z       >=  50 )
                   ADD     1                   TO  IDX-Z
                   MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                           TO  ORCSCGENN-SRYCD (IDX-Z)
                   MOVE    IDX-Z           TO  ORCSCGENN-TBLMAX
               END-PERFORM
           END-PERFORM
      *
           CALL    "ORCSCGENNAME"      USING    ORCSCGENNAMEAREA
                                                SPA-AREA
      *
           MOVE    ZERO                TO  IDX-Z
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI )
                          OR  (IDX-Z   >=  50 )
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                              OR  (IDX-Z       >=  50 )
                   ADD     1                   TO  IDX-Z
                   IF     (ORCSCGENN-SRYCD (IDX-Z)
                                               =   LNK-WKSRY-SRYCD
                                                         (IDX IDZ))
                   AND    (ORCSCGENN-SRYCD (IDX-Z)(1:1)  =   "6"  )
                       IF     (ORCSCGENN-NAME-KBN (IDX-Z)  =   "1"
                                                           OR  "3")
                          AND (ORCSCGENN-HEN-NAME(IDX-Z) NOT =   SPACE)
      *                    一般名
                           MOVE    ORCSCGENN-HEN-NAME (IDX-Z)
                                           TO  LNK-WKSRY-INPUTCOMENT
                                                            (IDX IDZ)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       410112-GENERIC-HEN-EXT.
           EXIT.
      *TEST
      *
      *****************************************************************
      *    診療行為マスタを作成する 
      *****************************************************************
       41012-SRYACT-HEN-SEC           SECTION.
      *
           IF      SRY-RENNUM          =   1
               PERFORM 410121-SRYACCT-SYORI-SEC
           END-IF
      *    回数ゼロは対象外とする
           IF      WRK-ZAIKAISU        =   ZERO
      *       薬評は対象外
              OR  (ACCT-ZAIKBN         =   1    )
      *H28.9
      *       持参薬は対象外
              OR  (ACCT-ZAIKBN         =   3    )
              OR  (ACCT-ZAIKBN         =   4    )
               CONTINUE
           ELSE
               PERFORM 410121-WKSRYACT-HEN-SEC
           END-IF
      *
           .
       41012-SRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計編集処理 
      *****************************************************************
       410121-SRYACCT-SYORI-SEC              SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAIKAISU
           MOVE    ZERO                TO  WRK-DAY-TBL
      *    剤回数取得
           INITIALIZE                      SRYACCT-REC
           MOVE    SRY-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SRY-PTID            TO  ACCT-PTID
           MOVE    SRY-SRYKA           TO  ACCT-SRYKA
           MOVE    SRY-SRYYM           TO  ACCT-SRYYM
           MOVE    SRY-SRYKBN          TO  ACCT-SRYKBN
           MOVE    SRY-ZAINUM          TO  ACCT-ZAINUM
      *    主キー読み
           PERFORM 940-SRYACCT-READ-SEC
           IF      FLG-SRYACCT         =   ZERO
      *    回数決定
      *        同日再診の最終回セット
      *ver.4.8
      *        EVALUATE    JYURRK-RENNUM
      *            WHEN    1
      *                MOVE    ACCT-DAY (2 IDX-S2)    TO  WRK-ZAIKAISU
      *            WHEN    2
      *                MOVE    ACCT-DAY (3 IDX-S2)    TO  WRK-ZAIKAISU
      *            WHEN    OTHER
      *                MOVE    ACCT-DAY (4 IDX-S2)    TO  WRK-ZAIKAISU
      ****     END-EVALUATE
      *
               MOVE    ACCT-DAY (JYURRK-RENNUM + 1 IDX-S2)
                                               TO  WRK-ZAIKAISU
      *
               MOVE    ACCT-DAY-TBL (1)    TO  WRK-DAY-TBL
           END-IF
      *ver.4.7
           IF      ACCT-ZAIREQFLG      =   "1"
      *         附加情報あり
                PERFORM 4101212-SRYACCTPLUS-READ-SEC
      *         対象分の合計点数計算
                IF      ACCTP-PLUSKBN        =   1
      *            減点（マイナス）は対象外
                   MOVE    ZERO              TO  WRK-ZAIKAISU
                END-IF
           END-IF
      *
           INITIALIZE                  WKSRYACT-REC
      *
           .
       410121-SRYACCT-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    ワーク診療行為レイアウト編集処理 
      *****************************************************************
       410121-WKSRYACT-HEN-SEC              SECTION.
      *
           INITIALIZE                  WKSRYACT-REC
           MOVE    SRY-HOSPNUM         TO  WKSRY-HOSPNUM
           MOVE    SRY-NYUGAIKBN       TO  WKSRY-NYUGAIKBN
           MOVE    SRY-PTID            TO  WKSRY-PTID
           MOVE    SRY-SRYKA           TO  WKSRY-SRYKA
           MOVE    SRY-SRYYM           TO  WKSRY-SRYYMD(1:6)
           MOVE    SRY-SRYKBN          TO  WKSRY-SRYKBN
           MOVE    SRY-ZAINUM          TO  WKSRY-ZAINUM
           MOVE    SRY-RENNUM          TO  WKSRY-RENNUM
           MOVE    SRY-SRYSYUKBN       TO  WKSRY-SRYSYUKBN
           MOVE    SRY-JIHIMONEYTOTAL  TO  WKSRY-JIHIMONEYTOTAL
      *剤回数計
           MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAIKEI
      *
      *    剤点数計
           MOVE    ACCT-ZAITEN         TO  WKSRY-ZAITENKEI
      *保険組合
           MOVE    ACCT-HKNCOMBI       TO  WKSRY-HKNCOMBI
      *
           MOVE    ZERO                TO  IDX-W
           MOVE    ZERO                TO  IDY
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   5   )   OR
                              (SRY-SRYCD (IDX)     =   SPACE )
               IF      (SRY-SRYCD (IDX)(1:1)   =   "6"
                                               OR  "8" ) OR
                       (SRY-SRYCD (IDX)(1:3)   =   "059" 
                                               OR  "058"
                                               OR  "001"
                                               OR  "008" )
      *                用量割合・粉砕
                   OR  (SRY-SRYCD (IDX)(1:5)   =   "09920")
      *                在宅器材
                   OR ((SRY-SRYKBN             =   "14"  ) AND
                       (SRY-SRYCD (IDX)(1:1)   =   "7"   ))
      *
                   MOVE    1                   TO  FLG-OK
      *
                   ADD     1                   TO  IDY
                   MOVE    SRY-SRYCD (IDX)     TO  WKSRY-SRYCD    (IDY)
                   MOVE    SRY-SRYSURYO (IDX)  TO  WKSRY-SRYSURYO (IDY)
                   MOVE    SRY-SRYKAISU (IDX)  TO  WKSRY-SRYKAISU (IDY)
                   MOVE    SRY-AUTOKBN  (IDX)  TO  WKSRY-AUTOKBN  (IDY)
      *!!
      *            器材商品名対応
                   IF      SRY-MEISKYFLG (IDX) =   "1"
                       MOVE    "T"             TO  WKSRY-AUTOKBN  (IDY)
                   END-IF
      *ver.4.5
      *            換算値
                   MOVE    SRY-KANSURYO (IDX)  TO  WKSRY-KANSURYO (IDY)
      *
      *!!
                   MOVE    SRY-INPUTNUM (IDX)  TO  WKSRY-INPUTNUM (IDY)
                   MOVE    SRY-JIHIMONEY(IDX)  TO  WKSRY-JIHIMONEY(IDY)
                   IF      SRY-INPUTNUM (IDX)  NOT =   ZERO
                       PERFORM 490111-PTCOM-SET-SEC
                   END-IF
                   MOVE    IDY                 TO  IDX-W
               END-IF
               IF      (SRY-SRYCD (IDX)(1:1)   =   "6"
                                               OR  "7"  )
                   OR  (SRY-SRYCD (IDX)(1:3)   =   "059")
                   MOVE    1                   TO  FLG-YAKUZAI
               END-IF
           END-PERFORM
      *
           IF      IDX-W               >   ZERO
               MOVE    WRK-ZAIKAISU        TO  WKSRY-ZAIKAISU (IDX-W)
           END-IF
      *
           .
       410121-WKSRYACT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    患者コメントから編集する 
      *****************************************************************
       490111-PTCOM-SET-SEC              SECTION.
      *
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
           MOVE    SRY-PTID            TO  PTCOM-PTID
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
           MOVE    SRY-SRYCD (IDX)     TO  PTCOM-SRYCD
           MOVE    SRY-INPUTNUM(IDX)   TO  PTCOM-RENNUM
      *主キーで検索
           PERFORM 950-PTCOM-READ-SEC
           IF      FLG-PTCOM           =   ZERO
               MOVE    PTCOM-INPUTCOMENT   TO  WKSRY-INPUTCOMENT (IDY)
               MOVE    PTCOM-INPUTCHI (1)  TO  WKSRY-INPUTCHI (IDY 1)
               MOVE    PTCOM-INPUTCHI (2)  TO  WKSRY-INPUTCHI (IDY 2)
               MOVE    PTCOM-INPUTCHI (3)  TO  WKSRY-INPUTCHI (IDY 3)
               MOVE    PTCOM-INPUTCHI (4)  TO  WKSRY-INPUTCHI (IDY 4)
               MOVE    PTCOM-INPUTCHI (5)  TO  WKSRY-INPUTCHI (IDY 5)
           END-IF
           .
       490111-PTCOM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療行為内容編集処理
      *****************************************************************
       3101-WKSRYACT-HEN-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  IDZ
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
      *
           ADD     1                   TO  SPA-WKSRY-MAX
           MOVE    JYURRK-ZAINUM(IDX-J)    TO  SPA-WKSRY-ZAINUM
                                                    (SPA-WKSRY-MAX)
           MOVE    ZERO                TO  IDX-W
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI  )
      *        診療種別
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SPA-WKSRY-SRYKBN
                                                    (SPA-WKSRY-MAX)
               MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                           TO  SPA-WKSRY-SRYSYUKBN
                                                    (SPA-WKSRY-MAX)
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)
                                           TO  SPA-WKSRY-ZAITENKEI
                                                    (SPA-WKSRY-MAX)
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                           TO  SPA-WKSRY-ZAIKAIKEI
                                                    (SPA-WKSRY-MAX)
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                       TO  WRK-SRYKAISUKEI
      *        算定日
               MOVE    WRK-DAY-TBL     TO  SPA-WKSRY-DAY-G
                                                    (SPA-WKSRY-MAX)
      *        剤内１行目
               IF      IDX                 =   1
                   ADD     1                   TO  IDG
                   IF      IDG                 <=  MAX-GMN
                       PERFORM 31011-GYO1-HEN-SEC
                   END-IF
               END-IF
      *
      *        剤明細編集
               PERFORM VARYING     IDZ     FROM    1   BY  1
                       UNTIL      (IDZ         >   5   )  OR
                                  (IDG         >=  MAX-GMN )  OR
                                  (IDX-W       >=  50  )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDZ) =  SPACE)
                   ADD     1                   TO  IDX-W
                   PERFORM 31012-ZAIMEI-HEN-SEC
               END-PERFORM
           END-PERFORM
      *
      *     ＊日数　編集 （最終行に追加）
            PERFORM 31013-KAISU-HEN-SEC
            IF      SPA-GMN-TZAIKEI (IDG)   =   SPACE
                                            OR  ALL "　"
                CONTINUE
            ELSE
                ADD    1                    TO  IDG
                MOVE    SPA-WKSRY-MAX       TO  SPA-NAI-TBANGO    (IDG)
            END-IF
            MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI (IDG)
            MOVE    1                   TO  SPA-NAI-KAIFLG  (IDG)
      *
           INSPECT SPA-GMN-TZAIKEI (IDG)   REPLACING
                                               ALL "  "  BY  "　"
      *
           MOVE    SPA-WKSRY-MAX       TO  SPA-NAI-TBANGO    (IDG)
      *
      *    剤終了編集
           ADD     1                   TO  IDG
           IF      IDG                 <=  MAX-GMN
               MOVE    ALL "-"             TO  SPA-GMN-TMEISYO (IDG)
           END-IF
      *
            .
       3101-WKSRYACT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤内１行目  編集処理
      *****************************************************************
       31011-GYO1-HEN-SEC                  SECTION.
      *
      *    診療種別区分名称、診療行為区分セット
           SET     MSYU-IDX            TO  1
           SEARCH      MSYU-TBL-OCC    VARYING   MSYU-IDX
                   AT   END
                       MOVE    SPACE       TO  WRK-SRYSYUMEI
                   WHEN    LNK-WKSRY-SRYSYUKBN(IDX) =   MSYU-SRYSYUKBN
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
                   WHEN    LNK-WKSRY-SRYKBN(IDX)
                                           =   MSYU-SRYKBN(MSYU-IDX)
                       MOVE    MSYU-SRYMEI (MSYU-IDX)
                                               TO  WRK-SRYSYUMEI
               END-SEARCH
           END-IF
      *
           IF      LNK-WKSRY-SRYSYUKBN (IDX)   =   SPACE
               MOVE    SPACE           TO  SPA-GMN-TMEISYO (IDG)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           ELSE
               MOVE    "."             TO  SPA-GMN-TMEISYO (IDG)(1:1)
               MOVE    LNK-WKSRY-SRYSYUKBN (IDX)
                                       TO  SPA-GMN-TMEISYO (IDG)(2:3)
               MOVE    WRK-SRYSYUMEI   TO  SPA-GMN-TMEISYO (IDG)(7:)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO  (IDG)   REPLACING
                                            ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI0   (IDG)   REPLACING
                                            ALL "  "  BY  "　"
      *
      *    番号
           MOVE    SPA-WKSRY-MAX        TO  SPA-GMN-TBANGO    (IDG)
           MOVE    SPA-WKSRY-MAX        TO  SPA-NAI-TBANGO    (IDG)
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
           MOVE    SPA-WKSRY-MAX       TO  SPA-NAI-TBANGO    (IDG)
      *    ＳＰＡへ
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                       TO  SPA-WKSRY-SRYCD
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                       TO  SPA-WKSRY-SRYCD
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)
                                       TO  SPA-WKSRY-SRYSURYO
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-SRYKAISU (IDX IDZ)
                                       TO  SPA-WKSRY-SRYKAISU
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-SRYKAISU (IDX IDZ)
                                       TO  SPA-WKSRY-SRYKAISU
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-ZAIKAISU (IDX IDZ)
                                       TO  SPA-WKSRY-ZAIKAISU
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-INPUTNUM(IDX IDZ)
                                       TO  SPA-WKSRY-INPUTNUM
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-AUTOKBN(IDX IDZ)
                                       TO  SPA-WKSRY-AUTOKBN
                                                   (SPA-WKSRY-MAX IDX-W)
           MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                       TO  SPA-WKSRY-INPUTCOMENT
                                                   (SPA-WKSRY-MAX IDX-W)
      *H28.5
      *    一般名
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6" )
              AND (LNK-WKSRY-INPUTCOMENT(IDX IDZ)  NOT =   SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                       TO  SPA-WKSRY-GENE-NAME
                                                   (SPA-WKSRY-MAX IDX-W)
               MOVE    SPACE           TO  SPA-WKSRY-INPUTCOMENT
                                                   (SPA-WKSRY-MAX IDX-W)
           END-IF
      *
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)
                                       TO  SPA-NAI-TSRYCD (IDG)
           MOVE    IDX-W               TO  SPA-NAI-TWNUM (IDG)
      *
      *    診療コードより診療行為名称を取得
           MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOSTYMD
           MOVE    WRK-SRYYMD          TO  TNS-YUKOEDYMD
           PERFORM 910-TENSU-READ-SEC
           IF      FLG-TENSU           =   1
               MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-NAME
           END-IF
           MOVE    TNS-NAME            TO  SPA-WKSRY-NAME
                                                   (SPA-WKSRY-MAX IDX-W)
      *    行為名称
           MOVE    TNS-NAME            TO  SPA-GMN-TMEISYO2(IDG)
           MOVE    TNS-NAME            TO  WRK-MEISYO
      *    薬剤服用コメントの時、再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)(1:3)  =   "001"
                PERFORM 3100121-FUKIYO-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *    後発医薬品変更再編集
           IF      LNK-WKSRY-SRYCD (IDX IDZ)   =   "099209903"
                                               OR  "099209905"
                                               OR  "099209906"
      *H24.4 改正
                                               OR  "099209907"
                                               OR  "099209908"
      *H24.4 改正
                PERFORM 3100122-KOUHATU-MEISYO-SEC
                MOVE    WRK-MEISYO    TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *
      *    コメントの時、内容設定
           IF    ((LNK-WKSRY-SRYCD   (IDX IDZ)(1:1)  =   "8"   ) OR
                  (LNK-WKSRY-SRYCD   (IDX IDZ)(1:3)  =   "008" ))  AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  WRK-MEISYO
           END-IF
      *H30.4
           IF     (LNK-WKSRY-SRYCD   (IDX IDZ)(1:7)  =   "0992081" )
            AND   (LNK-WKSRY-INPUTCOMENT(IDX IDZ) NOT =  SPACE )
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  SPA-GMN-TMEISYO2(IDG)
               MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDZ)
                                           TO  WRK-MEISYO
           END-IF
      *
           MOVE    WRK-MEISYO              TO  SPA-WKSRY-NAME
                                                   (SPA-WKSRY-MAX IDX-W)
      *    用法コード編集
           IF      LNK-WKSRY-SRYCD  (IDX IDZ)(1:3) =   "001"
      *        コメント区分
               MOVE    TNS-KENTAICOMMENT   TO  SPA-WKSRY-KENTAICOMMENT
                                                   (SPA-WKSRY-MAX IDX-W)
      *H28.11  数量
               MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDZ)
                                       TO  SPA-WKSRY-INPUTCHI-G
                                                   (SPA-WKSRY-MAX IDX-W)
      *        服用時点設定判定
               IF    ((TNS-SSTKIJUNCD(1)   =   ZERO )  AND
                      (TNS-SSTKIJUNCD(2)   =   ZERO )  AND
                      (TNS-SSTKIJUNCD(3)   =   ZERO )  AND
                      (TNS-SSTKIJUNCD(4)   =   ZERO )  AND
                      (TNS-SSTKIJUNCD(5)   =   ZERO ))
                   MOVE    ZERO                TO  SPA-WKSRY-YSETKBN
                                                   (SPA-WKSRY-MAX IDX-W)
               ELSE
                   MOVE    1                   TO  SPA-WKSRY-YSETKBN
                                                   (SPA-WKSRY-MAX IDX-W)
               END-IF
           END-IF
      *
      *H30.4
      *    分割調剤
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:7)  =   "0992081")
               MOVE    LNK-WKSRY-INPUTCHI-G (IDX IDZ)
                                       TO  SPA-WKSRY-INPUTCHI-G
                                                   (SPA-WKSRY-MAX IDX-W)
           END-IF
      *!
      *    用量割合再編集
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:7)  =   "0992000")  AND
                  (LNK-WKSRY-SRYSURYO(IDX IDZ) NOT =   ZERO     )
                PERFORM 3005-INPUT-MEISYO3-SEC
                MOVE    WRK-MEISYO         TO  SPA-GMN-TMEISYO2(IDG)
           END-IF
      *!
      *    器材商品名の器材コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:1)   =   "7"  )  AND
                  (LNK-WKSRY-AUTOKBN (IDX IDZ)     =   "T"  )
                PERFORM 3005-INPUT-MEISYO4-SEC
                MOVE    WRK-MEISYO        TO  SPA-GMN-TMEISYO2(IDG)
      *ver.4.7
      *        商品名コードの単位へ
               IF      WRK-SYO-TANINAME    NOT =   SPACE
                   MOVE    WRK-SYO-TANINAME    TO  TNS-TANINAME
                   MOVE    SPACE               TO  WRK-SYO-TANINAME
               END-IF
           END-IF
      *    器材商品名コード
           IF     (LNK-WKSRY-SRYCD(IDX IDZ)(1:3)   =   "058"  )
               MOVE    TNS-TANINAME        TO  WRK-SYO-TANINAME
           END-IF
      *ver.4.7
      *!
      *
      *    数量（薬剤、器材のとき）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)   =   "6"  OR  "7" )
               OR (LNK-WKSRY-SRYCD (IDX IDZ)(1:3)   =   "059"  )
               MOVE    LNK-WKSRY-SRYSURYO (IDX IDZ)  TO  WRK-SURYO
               PERFORM 310121-SURYO-HEN-SEC
               MOVE    WRK-SURYO-J         TO  SPA-GMN-TSURYOU (IDG)
               MOVE    TNS-TANINAME        TO  SPA-GMN-TTANI1  (IDG)
           END-IF
      *
      *    交付日期間外の点数マスタ判定
           IF     (FLG-TENSU           =   ZERO  )
               IF     (TNS-YUKOSTYMD       <=  SPA-NAI-KOUFUYMD ) AND
                      (TNS-YUKOEDYMD       >=  SPA-NAI-KOUFUYMD )
                   CONTINUE
               ELSE
                   MOVE    LNK-WKSRY-SRYCD (IDX IDZ)   TO  TNS-SRYCD
                   MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOSTYMD
                   MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOEDYMD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    1               TO  SPA-WKSRY-ERRKBN
                                                   (SPA-WKSRY-MAX IDX-W)
                       MOVE    1               TO  SPA-NAI-ERRFLG  (IDG)
                   END-IF
               END-IF
           ELSE
               MOVE    1               TO  SPA-WKSRY-ERRKBN
                                                   (SPA-WKSRY-MAX IDX-W)
               MOVE    1               TO  SPA-NAI-ERRFLG  (IDG)
           END-IF
      *
           INSPECT SPA-GMN-TMEISYO2(IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TSURYOU (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANMEI (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TTANI1  (IDG) REPLACING  ALL "  "  BY  "　"
           INSPECT SPA-GMN-TYOBI1  (IDG) REPLACING  ALL "  "  BY  "　"
      *ver.4.5
      *    換算値数量（薬剤）
           IF     (LNK-WKSRY-SRYCD (IDX IDZ)(1:1)  =   "6"   )
               PERFORM 980-TENSUPLUS-READ-SEC
      *
               IF     (TNSP-KANZANTANINAME =   SPACE )  AND
                      (TNSP-KANZANCHI      =   1     )
                   MOVE    ZERO                TO  TNSP-KANZANCHI
               END-IF
               MOVE    TNSP-KANZANCHI      TO  SPA-WKSRY-KANZANCHI
                                                   (SPA-WKSRY-MAX IDX-W)
               MOVE    TNSP-KANZANTANINAME TO  SPA-WKSRY-KANZANTANINAME
                                                   (SPA-WKSRY-MAX IDX-W)
      *        換算値数量設定あり
               IF      TNSP-KANZANCHI       >  ZERO
                   IF      LNK-WKSRY-SRYSURYO (IDX IDZ)
                                            =  (TNSP-KANZANCHI
                                            *  LNK-WKSRY-KANSURYO
                                                          (IDX IDZ) )
                       MOVE    LNK-WKSRY-KANSURYO (IDX IDZ)
                                               TO  SPA-WKSRY-KANSURYO
                                                 (SPA-WKSRY-MAX IDX-W)
                   ELSE
                       MOVE    ZERO         TO  SPA-WKSRY-KANSURYO
                                                 (SPA-WKSRY-MAX IDX-W)
                   END-IF
                   PERFORM 310129-KANSURYO-HEN-SEC
               ELSE
                   MOVE    ZERO         TO  SPA-WKSRY-KANSURYO
                                                  (SPA-WKSRY-MAX IDX-W)
               END-IF
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
       31012-ZAIMEI-HEN-EXT.
           EXIT.
      *ver.4.5
      *****************************************************************
      *   換算値数量編集処理
      *****************************************************************
       310129-KANSURYO-HEN-SEC                  SECTION.
      *
      *    次行へ編集
           ADD    1                    TO  IDG
           MOVE    SPA-WKSRY-MAX       TO  SPA-NAI-TBANGO    (IDG)
      *
           MOVE    SPA-WKSRY-KANSURYO (SPA-WKSRY-MAX IDX-W)
                                       TO  WRK-SURYO
           MOVE    SPA-WKSRY-KANZANTANINAME (SPA-WKSRY-MAX IDX-W)
                                       TO  WRK-KANZANTANINAME
           PERFORM 3101291-KANSURYO-HENKAN-SEC
      *
           INSPECT SPA-GMN-TMEISYO(IDG) REPLACING
                                               ALL "  "  BY  "　"
      *
           .
       310129-KANSURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   換算値数量編集処理
      *****************************************************************
       3101291-KANSURYO-HENKAN-SEC                  SECTION.
      *
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    1                   TO  IDM
           MOVE    1                   TO  IDM2
           MOVE    SPACE               TO  WRK-TSURYO
           PERFORM
                   UNTIL       IDM     >   22
               IF      WRK-SURYO-J(IDM:1)    =   SPACE
                   ADD     1               TO  IDM
               ELSE
                   IF      WRK-SURYO-J(IDM:2)    =   "　"
                       ADD     2               TO  IDM
                   ELSE
                       MOVE    WRK-SURYO-J(IDM:2)
                                               TO  WRK-TSURYO
                                                        (IDM2:2)
                       ADD     2               TO  IDM2
                       ADD     2               TO  IDM
                   END-IF
               END-IF
           END-PERFORM
           IF      WRK-SURYO           =   ZERO
               MOVE    SPACE               TO  WRK-TSURYO
           END-IF
      *    数量換算単位名称
           MOVE    WRK-KANZANTANINAME  TO  WRK-TSURYO  (IDM2:)
           MOVE    SPACE               TO  WRK-G-SURYO
           STRING  "（"
                   WRK-TSURYO
                   "）"                DELIMITED  BY  SPACE
                                       INTO    WRK-G-SURYO
           END-STRING
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1   BY  1
                   UNTIL      (IDM     >   30  )
                         OR   (WRK-G-SURYO (IDM:1) =   SPACE )
               ADD     1               TO  IDM2
           END-PERFORM
           COMPUTE IDM3                =   30
                                       -   IDM2
                                       +   1
           MOVE    SPACE                   TO  SPA-GMN-TKANSURYO
                                                    (IDG)
           MOVE    WRK-G-SURYO(1:IDM2)     TO  SPA-GMN-TKANSURYO
                                                    (IDG)(IDM3:IDM2)
      *
           .
       3101291-KANSURYO-HENKAN-EXT.
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
       3005-INPUT-MEISYO3-EXT.
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
      *
      *****************************************************************
      *    数量表示用編集処理
      *****************************************************************
       310121-SURYO-HEN-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    SPACE               TO  WRK-SURYO-J
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
           IF      WRK-SURYO-X     NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-SURYO-X         TO  KANACHK-MAE-INPUT
               MOVE    22                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-SURYO-J
           END-IF
           .
       310121-SURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   回数画面再編集処理
      *****************************************************************
       31013-KAISU-HEN-SEC                  SECTION.
      *
      *    回数
           MOVE    WRK-SRYKAISUKEI     TO  WRK-KAISU
           MOVE    ZERO                TO  IDK2
           MOVE    SPACE               TO  WRK-KAISU-H 
           MOVE    SPACE               TO  WRK-KAISU-J 
           PERFORM VARYING     IDK1    FROM    1   BY  1
                   UNTIL       IDK1    >   8
               IF      WRK-KAISU-X (IDK1:1)    NOT =   SPACE
                   ADD     1           TO  IDK2
                   MOVE    WRK-KAISU-X (IDK1:1)
                                       TO  WRK-KAISU-H (IDK2:1)
               END-IF
           END-PERFORM
      *
      *    日本語変換
           IF      WRK-KAISU-H     NOT =   SPACE
               INITIALIZE                  ORCSKANACHKAREA
               MOVE    "2"                 TO  KANACHK-SYORI
               MOVE    WRK-KAISU-H         TO  KANACHK-MAE-INPUT
               MOVE    40                  TO  KANACHK-MAX-CNT
               CALL    "ORCSKANACHK"       USING
                                           ORCSKANACHKAREA
               MOVE    KANACHK-OUT-INPUT(1:KANACHK-MAX)
                                           TO  WRK-KAISU-J
           END-IF
      *
           MOVE    SPACE               TO  WRK-ZAIKEIJ
           STRING  "×"                    DELIMITED   BY  SIZE
                   WRK-KAISU-J             DELIMITED   BY  SPACE
                   INTO                    WRK-ZAIKEIJ
           END-STRING
      *
           .
       31013-INPUTCD-HEN-EXT.
           EXIT.
      *
      *H28.5
      *****************************************************************
      *    一般名画面表示編集処理
      *****************************************************************
       300129-MEISYO-CHG-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-FUKA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-MEISAI-MAX
      *        後発品変更不可
               IF      SPA-NAI-TSRYCD (IDX)    =   "099209910"
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDX-W
                   MOVE    SPA-NAI-TWNUM  (IDX)    TO  IDX-W2
                   IF     (SPA-WKSRY-ZAIKAIKEI (IDX-W)
                                                   >   ZERO )
                       MOVE    1                   TO  FLG-FUKA
                   END-IF
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX             >   SPA-MEISAI-MAX
               IF      SPA-NAI-TSRYCD (IDX)(1:1)   =   "6"
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDX-W
                   MOVE    SPA-NAI-TWNUM  (IDX)    TO  IDX-W2
                   IF     (SPA-WKSRY-GENE-NAME (IDX-W IDX-W2)
                                                   NOT =   SPACE)
                     AND  (SPA-NAI-GENERICFLG      =   "1"    )
      *                一般名表示
                       MOVE    SPA-WKSRY-GENE-NAME(IDX-W IDX-W2)
                                         TO  SPA-GMN-TMEISYO2(IDX)
                       IF      FLG-FUKA      =   1
      *                銘柄名
                           MOVE    SPA-WKSRY-NAME (IDX-W IDX-W2)
                                         TO  SPA-GMN-TMEISYO2(IDX)
                       END-IF
                   ELSE
      *                銘柄名
                       MOVE    SPA-WKSRY-NAME (IDX-W IDX-W2)
                                         TO  SPA-GMN-TMEISYO2(IDX)
                   END-IF
                   INSPECT SPA-GMN-TMEISYO2(IDX)
                                   REPLACING  ALL "  "  BY  "　"
               END-IF
           END-PERFORM
      *
           .
       300129-MEISYO-CHG-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       3002-KAID1-SET-SEC          SECTION.
      *
           IF      SPA-KAID1-FLG       =   "OK"
               PERFORM 30021-KAID1-PRT-SEC
           ELSE
               IF      SPA-PRINT-SYORI     =   1
                   INITIALIZE              SPA-PARA-WKSRY-TBL
                   INITIALIZE              SPA-KA02-PRT
                   MOVE    ZERO            TO  SPA-WKSRY-MAX
               END-IF
               MOVE    1                   TO  SPA-GMN-CUR
               MOVE    SPACE               TO  SPA-KAID1CD
           END-IF
      *
           .
       3002-KAID1-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＸＤＩＤ１）ＯＫ処理
      *****************************************************************
       30021-KAID1-PRT-SEC          SECTION.
      *
      *    印刷制御情報取得サブ
           CALL    "ORCSONPRTSET"      USING   SPA-AREA
      *
           IF     (SPA-CLIENT-PRT-FLG  =   2 )
             OR   (SPA-PRTCONF         =   "1" )
      *        クライアント印刷制御情報取得処理
               PERFORM 4901-CLIENT-PRT-SEC
           END-IF
      *
           EVALUATE    SPA-KAID1CD
      *        薬剤情報印刷
               WHEN    "0101"
                   MOVE    2                TO  FLG-PRINT
                   PERFORM 4103-YAKJYO-PRINT-SEC
      *        退院時処方せん選択印刷
               WHEN    "0106"
                   MOVE    2                TO  FLG-PRINT
                   PERFORM 4102-SYOHOU-PRINT-SEC
      *        退院時処方せん印刷
               WHEN    "0105"
                   MOVE    2                TO  FLG-PRINT
                   PERFORM 451-SHOHOSEN-PRINT-SEC
      *        お薬手帳前回印刷
               WHEN    "0107"
                   MOVE    2                TO  FLG-PRINT
                   PERFORM 4104-OKUSURI-PRINT-SEC
           END-EVALUATE
           MOVE    SPACE               TO  SPA-KAID1CD
      *
           IF      (SPA-CLIENT-PRT-FLG =   1  )
      *        印刷ダミー処理
               MOVE    "99"                TO  SPA-PRT-FLG
               CALL    "ORCHCDUMMY"    USING   SPA-AREA
           END-IF
      *
           IF      SPA-ERRCD       NOT =   SPACE
               PERFORM 510-ERRSET-SEC
           END-IF
           .
       30021-KAID1-PRT-EXT.
           EXIT.
      *****************************************************************
      *    クライアント印刷前　処理
      *****************************************************************
       4901-CLIENT-PRT-SEC         SECTION.
      *
      *    クライアント印刷制御情報取得サブ
           INITIALIZE                  ORCSPRTCTRLAREA
      *    MOVE    "お薬情報"          TO  ORCSPRTCTRL-TITLE-I(01)
      *    MOVE    "処方箋"            TO  ORCSPRTCTRL-TITLE-I(02)
      *    MOVE    "お薬手帳"          TO  ORCSPRTCTRL-TITLE-I(03)
           CALL    "ORCSPRTCTRL"       USING   SPA-AREA
                                               ORCSPRTCTRLAREA
                                               MCPAREA
      *
      **   MOVE    1                   TO  FLG-CLIENT-OK
      *
           .
       4901-CLIENT-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認画面（ＫＡＩＤ２）ＯＫ処理
      *****************************************************************
       3003-KAID2-SET-SEC          SECTION.
      *
           .
       3003-KAID2-SET-EXT.
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
      *        クリア
               WHEN    "CLICKED"       ALSO    "B02"
                    PERFORM 402-CLREA-SEC
      *        交付日へ
               WHEN    "CLICKED"       ALSO    "B03"
                    PERFORM 403-KOUFUYMD-SEC
      *        院外・院内切替
               WHEN    "CLICKED"       ALSO    "B04"
                    PERFORM 404-INNAI-CHNGE-SEC
      *        選択印刷
               WHEN    "CLICKED"       ALSO    "B05"
                    PERFORM 405-SEL-PRT-SEC
      *        前頁
               WHEN    "CLICKED"       ALSO    "B06"
                   PERFORM 406-MAE-PAGE-SEC
      *        次頁
               WHEN    "CLICKED"       ALSO    "B07"
                   PERFORM 407-NEXT-PAGE-SEC
      *        変更確定
               WHEN    "CLICKED"       ALSO    "B08"
                    PERFORM 408-KAKUTEI-SEC
      *        選択登録
               WHEN    "CLICKED"       ALSO    "B09"
                    PERFORM 409-TOUROKU-SEC
      *        選択確認画面
               WHEN    "CLICKED"       ALSO    "B09S"
                    PERFORM 409-KA021-SET-SEC
      *        お薬手帳印刷
               WHEN    "CLICKED"       ALSO    "B10"
                    PERFORM 410-OKUSURI-PRT-SEC
      *        薬剤情報印刷
               WHEN    "CLICKED"       ALSO    "B11"
                    PERFORM 411-YAKUZAI-PRT-SEC
      *        処方せん印刷
               WHEN    "CLICKED"       ALSO    "B12"
                    PERFORM 412-SYOHOU-PRT-SEC
      *H28.5
      *        名称切替え
               WHEN    "CLICKED"       ALSO    "B05S"
                    PERFORM 415-GENERIC-CHG-SEC
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
           MOVE    MCP-WIDGET          TO  WRK-MCP-WIDGET
           MOVE    ZERO                TO  SPA-GMN-CUR
      *
           EVALUATE    MCP-EVENT       ALSO    MCP-WIDGET
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM1"
                   PERFORM 4001-SELNUM1-CHK-SEC
      *        選択
               WHEN    ANY             ALSO    "RRKLIST"
                   PERFORM 4003-RRKLIST-SEL-SEC
      *        選択番号
               WHEN    "ACTIVATE"      ALSO    "SELNUM2"
                   PERFORM 4004-SELNUM2-CHK-SEC
      *        選択
               WHEN    ANY             ALSO    "MEILIST"
                   PERFORM 4005-MEILIST-SEL-SEC
      *
               WHEN    OTHER
                   MOVE    ZERO            TO  FLG-KOUSIN
      *            入力チェック処理
                   PERFORM 410-INPUT-CHK-SEC
           END-EVALUATE
      *
           .
      *
       200-ENTRY-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号１チェック処理
      *****************************************************************
       4001-SELNUM1-CHK-SEC          SECTION.
      *
           MOVE    KA02-SELNUM1        TO  SPA-GMN-SELNUM1
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-JYURRK-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SELNUM1     =   SPA-GMN-NUM (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           INITIALIZE                  SPA-GMN-SELNUM2
           INITIALIZE                  SPA-GMN-INPUT-AREA
           INITIALIZE                  SPA-GMN-MEISAI-AREA
           MOVE    ZERO                TO  SPA-SEL-SYORI
           IF      FLG-OK              =   ZERO
               IF      SPA-GMN-SELNUM1     >   ZERO
                   MOVE    "0001"              TO  SPA-ERRCD
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
                   PERFORM 4021-CLREA-SELNUM01-SEC
                   MOVE    1                   TO  SPA-GMN-CUR
               END-IF
           ELSE
      *        診療科判定
               IF      SPA-PARA-SRYKA      NOT =   SPACE
                   MOVE    SPA-GMN-SELNUM1     TO  IDX
                   IF     (SPA-NAI-RRK-SRYKA(IDX)   =   SPA-PARA-SRYKA)
                   AND    (SPA-NAI-RRKYMD (IDX)
                                               >=  SPA-PARA-NYUINYMD )
                   AND    (SPA-NAI-RRKYMD (IDX)
                                               <=  SPA-PARA-TAIINYMD )
                       CONTINUE
                   ELSE
                       MOVE    "0011"              TO  SPA-ERRCD
                       MOVE    1                   TO  SPA-GMN-CUR
                   END-IF
               END-IF
               IF      SPA-ERRCD           =   SPACE
      *            内容編集
                   PERFORM 30012-MEISAI-SYORI-SEC
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4001-SELNUM1-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細リスト選択処理
      *****************************************************************
       4003-RRKLIST-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-JYURRK-MAX
               IF      KA02-RRKLIST-SELECT (IDX)   =   "T"
                   MOVE    SPA-GMN-NUM  (IDX)      TO  WRK-SELNUM
                   MOVE    SPA-JYURRK-MAX          TO  IDX
               END-IF
           END-PERFORM
      *
           IF     (WRK-SELNUM          >   ZERO )
      ****        (WRK-SELNUM      NOT =   SPA-GMN-SELNUM1)
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM1
               MOVE    SPA-GMN-SELNUM1     TO  KA02-SELNUM1
               PERFORM 4001-SELNUM1-CHK-SEC
           END-IF
      *
           .
       4003-RRKLIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択番号２チェック処理
      *****************************************************************
       4004-SELNUM2-CHK-SEC          SECTION.
      *
           MOVE    ZERO                TO  FLG-HENKOU
           IF     (KA02-SELNUM2        =   SPA-GMN-SELNUM2 )
            AND   (SPA-GMN-SELNUM2     =   ZERO            )
               MOVE    1                   TO  FLG-HENKOU
           END-IF
      *
           MOVE    KA02-SELNUM2        TO  SPA-GMN-SELNUM2
      *
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MEISAI-MAX )  OR
                              (FLG-OK  =   1   )
               IF      SPA-GMN-SELNUM2     =   SPA-GMN-TBANGO  (IDX)
                   MOVE    1               TO  FLG-OK
               END-IF
           END-PERFORM
           IF      SPA-GMN-SELNUM2     =   ZERO
               INITIALIZE                  SPA-GMN-INPUT-AREA
               IF      FLG-HENKOU          =   1
                   MOVE    1                   TO  SPA-GMN-CUR
               ELSE
                   MOVE    2                   TO  SPA-GMN-CUR
               END-IF
               MOVE    ZERO                TO  SPA-KEIFLG
           ELSE
               IF      FLG-OK              =   ZERO
                   MOVE    "0002"              TO  SPA-ERRCD
                   MOVE    2                   TO  SPA-GMN-CUR
               ELSE
      *            内容編集
                   PERFORM 40041-INPUT-HENSYU-SEC
      *            日数へ
                   MOVE    10                  TO  SPA-GMN-CUR
               END-IF
           END-IF
      *
           .
       4004-SELNUM2-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択内容編集処理
      *****************************************************************
       40041-INPUT-HENSYU-SEC          SECTION.
      *
           MOVE    ZERO                TO  IDX-W
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MEISAI-MAX )  OR
                              (IDX-W   >   ZERO      )
               IF      SPA-GMN-SELNUM2     =   SPA-GMN-TBANGO (IDX)
                   MOVE    IDX                     TO  IDG
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDX-W
               END-IF
           END-PERFORM
      *    明細選択設定
           IF     (IDX-W               >   ZERO ) AND
                  (SPA-SEL-SYORI       =   1    )
               MOVE    "T"             TO  SPA-GMN-TSEL   (IDG)
               MOVE    1               TO  SPA-WKSRY-SELFLG (IDX-W)
           END-IF
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
           MOVE    ZERO                TO  SPA-KEIFLG
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )   OR
                              (SPA-WKSRY-SRYCD (IDX-W IDX) =   SPACE)
               MOVE    SPA-WKSRY-SRYCD (IDX-W IDX)
                                       TO  SPA-NAI-SRYCD (IDX)
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)   =   "6"
                                                          OR  "7" ) OR
                      (SPA-WKSRY-SRYCD (IDX-W IDX)(1:3)   =   "059") OR
      *               用量割合再編集
                      (SPA-WKSRY-SRYCD (IDX-W IDX)(1:7)   =   "0992000")
                   MOVE    SPA-WKSRY-SRYSURYO (IDX-W IDX)
                                           TO  SPA-NAI-SURYO (IDX)
               ELSE
                   IF      SPA-WKSRY-SRYSURYO (IDX-W IDX)  >   ZERO
                       MOVE    1           TO  SPA-NAI-SURYO (IDX)
                   ELSE
                       MOVE    ZERO        TO  SPA-NAI-SURYO (IDX)
                   END-IF
               END-IF
      *!
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)   =   "6" )
                 AND  (SPA-WKSRY-KANSURYO (IDX-W IDX)  >   ZERO)
                   MOVE    SPA-WKSRY-KANSURYO (IDX-W IDX)
                                           TO  SPA-NAI-KANSURYO(IDX)
               END-IF
               MOVE    SPA-WKSRY-KANZANCHI (IDX-W IDX)
                                           TO  SPA-NAI-KANZANCHI (IDX)
      *!
      *        用量割合再編集
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:7)   =   "0992000")
                  AND (SPA-WKSRY-SRYSURYO (IDX-W IDX) NOT =   ZERO     )
                    PERFORM 41031-INPUT-MEISYO4-SEC
                    MOVE    WRK-MEISYO     TO  SPA-GMN-INPUT (IDX)
               ELSE
                   MOVE    SPA-WKSRY-NAME (IDX-W IDX)
                                           TO  SPA-GMN-INPUT (IDX)
               END-IF
      *H28.5
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)   =   "6")
                 AND  (SPA-WKSRY-GENE-NAME (IDX-W IDX)
                                               NOT =   SPACE)
                 AND  (SPA-NAI-GENERICFLG      =   "1"    )
      *            一般名表示
                   MOVE    SPA-WKSRY-GENE-NAME(IDX-W IDX)
                                         TO  SPA-GMN-INPUT (IDX)
               END-IF
      *VER.4.5
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)   =   "6")
                  AND (SPA-WKSRY-KANZANCHI (IDX-W IDX)    >   ZERO)
                    PERFORM 41031-INPUT-MEISYO5-SEC
               END-IF
      *!
               PERFORM 400411-SUURYO-HEN-SEC
      *!!
      *H30.XX
      *        分割調剤対応
               IF     (SPA-WKSRY-SRYCD (IDX-W IDX) =   "099208101" )
                 AND  (SPA-WKSRY-SRYSURYO (IDX-W IDX)  >   ZERO    )
                   MOVE    IDX                 TO  IDX-B
                   PERFORM 400412-BUNKATU-SEC
               END-IF
      *
               MOVE    IDX             TO  SPA-INPUT-MAX
           END-PERFORM
           MOVE    SPA-WKSRY-ZAIKAIKEI (IDX-W)
                                       TO  SPA-GMN-NISUU
           MOVE    SPA-WKSRY-SRYKBN    (IDX-W)
                                       TO  SPA-GMN-SRYKBN
           MOVE    SPA-WKSRY-DAY-G     (IDX-W)
                                       TO  SPA-GMN-DAY-G
           MOVE    1                   TO  SPA-INPUT-PAGE
           .
       40041-INPUT-HENSYU-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量編集処理
      *****************************************************************
       400411-SUURYO-HEN-SEC          SECTION.
      *
           MOVE    SPA-NAI-SURYO (IDX) TO  WRK-SURYO
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
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
           IF      SPA-NAI-KANSURYO (IDX)  >   ZERO
               MOVE    WRK-SURYO-X         TO  SPA-GMN-KEISURYO (IDX)
               PERFORM 404111-KANSURYO-GHEN-SEC
           ELSE
               MOVE    WRK-SURYO-X         TO  SPA-GMN-SURYO (IDX)
           END-IF
      *
           .
       400411-SUURYO-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    換算値数量編集処理
      *****************************************************************
       404111-KANSURYO-GHEN-SEC          SECTION.
      *
           MOVE    SPA-NAI-KANSURYO (IDX)  TO  WRK-SURYO
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1
      *
           MOVE    ZERO                TO  FLG-SP
           PERFORM VARYING     IDM     FROM    5   BY  -1
                   UNTIL       IDM     <   1
               IF      WRK-SURYO-S2(IDM:1) NOT =   ZERO
                   MOVE    1               TO  FLG-SP
               END-IF
               IF      FLG-SP          =   1
                   MOVE    WRK-SURYO-S2(IDM:1) TO  WRK-SURYO-Z3(IDM:1)
               END-IF
           END-PERFORM
           IF      WRK-SURYO-Z3        NOT =   SPACE
               MOVE    "."                 TO  WRK-SURYO-Z2
               MOVE    WRK-SURYO-S1        TO  WRK-SURYO-Z1-9
           END-IF
      *
           MOVE    SPACE               TO  SPA-GMN-SURYO (IDX)
           PERFORM VARYING     IDM     FROM    11  BY  -1
                   UNTIL       IDM     <   1
              IF      WRK-SURYO-X (IDM:1)  NOT =   SPACE
                  STRING  WRK-SURYO-X(1:IDM)   DELIMITED  BY  SIZE 
                          "A"                  DELIMITED  BY  SIZE
                                       INTO   SPA-GMN-SURYO (IDX)
                  END-STRING
                  MOVE    1            TO  IDM
               END-IF
           END-PERFORM
           .
       404111-KANSURYO-GHEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    分割調剤対応処理
      *****************************************************************
       400412-BUNKATU-SEC          SECTION.
      *
           MOVE    ZERO                TO  SPA-NAI-BUNKSUU
      *    分割数値変換
           PERFORM VARYING     IDXP    FROM   1   BY  1
                   UNTIL       IDXP    >   3
               IF      SPA-WKSRY-INPUTCHI(IDX-W IDX-B IDXP) =   SPACE
                   MOVE    ZERO                TO  WRK-INPUT-SURYO
               ELSE
                   INITIALIZE                      ORCSNUMAREA
                   MOVE    SPA-WKSRY-INPUTCHI(IDX-W IDX-B IDXP)
                                               TO  SNUM-INX
                   CALL    "ORCSNUM"           USING
                                               ORCSNUMAREA
                   IF     (SNUM-RC         NOT =   ZERO  )
                       MOVE    ZERO            TO  WRK-INPUT-SURYO
                   ELSE
                       MOVE    SNUM-OUTNUM     TO  WRK-INPUT-SURYO
                   END-IF
               END-IF
               ADD     WRK-INPUT-SURYO         TO  SPA-NAI-BUNKSUU
           END-PERFORM
           .
       400412-BUNKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    明細リスト選択処理
      *****************************************************************
       4005-MEILIST-SEL-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-SELNUM
           MOVE    ZERO                TO  WRK-SELNUM2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   SPA-MEISAI-MAX
              IF     (KA02-MEILIST-SELECT (IDX)   =   "T" )
                 AND (KA02-MNUM  (IDX)(1:1)  NOT  =   "-" )
                   IF      SPA-NAI-TBANGO (IDX)
                                               NOT =   SPA-GMN-SELNUM2
                       MOVE    SPA-NAI-TBANGO2(IDX)
                                                   TO  WRK-SELNUM
                   ELSE
                       MOVE    SPA-NAI-TBANGO2(IDX)
                                                   TO  WRK-SELNUM2
                   END-IF
               END-IF
           END-PERFORM
      *    明細選択設定・選択解除
           IF     (WRK-SELNUM          =   ZERO )  AND
                  (WRK-SELNUM2         =   ZERO )
               CONTINUE
           ELSE
               IF      SPA-SEL-SYORI       =   1
                   PERFORM 40051-SEL-SYORI-SEC
               END-IF
           END-IF
      *
           IF     (WRK-SELNUM          >   ZERO ) AND
                  (WRK-SELNUM      NOT =   SPA-GMN-SELNUM2)
               MOVE    WRK-SELNUM          TO  SPA-GMN-SELNUM2
               MOVE    SPA-GMN-SELNUM2     TO  KA02-SELNUM2
               PERFORM 4004-SELNUM2-CHK-SEC
           ELSE
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
      *
           .
       4005-MEILIST-SEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    選択解除処理
      *****************************************************************
       40051-SEL-SYORI-SEC          SECTION.
      *
           IF      WRK-SELNUM      =   ZERO
               MOVE    WRK-SELNUM2     TO  WRK-SELNUM-T
           ELSE
               MOVE    WRK-SELNUM      TO  WRK-SELNUM-T
           END-IF
      *
           MOVE    ZERO                TO  IDX-W
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MEISAI-MAX )  OR
                              (IDX-W   >   ZERO      )
               IF      WRK-SELNUM-T        =   SPA-GMN-TBANGO (IDX)
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDX-W
                   IF      SPA-GMN-TSEL   (IDX)    =   "T"
                       MOVE    SPACE           TO  SPA-GMN-TSEL   (IDX)
                       MOVE    ZERO            TO  SPA-WKSRY-SELFLG
                                                                 (IDX-W)
                       MOVE    ZERO            TO  WRK-SELNUM
      *                選択項目クリア
                       MOVE    WRK-SELNUM      TO  SPA-GMN-SELNUM2
                       INITIALIZE                  SPA-GMN-INPUT-AREA
                       MOVE    ZERO                TO  SPA-KEIFLG
                   ELSE
                       MOVE    "T"             TO  SPA-GMN-TSEL   (IDX)
                       MOVE    1               TO  SPA-WKSRY-SELFLG
                                                                 (IDX-W)
                   END-IF
               END-IF
           END-PERFORM
           .
       40051-SEL-SYORI-EXT.
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
           IF      FLG-KOUSIN          =   2
               PERFORM 4103-KOUSIN-CHK-SEC
           END-IF
      *
           .
       410-INPUT-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面ＳＰＡ編集処理
      *****************************************************************
       4101-GMN-SPA-SET-SEC          SECTION.
      *
           MOVE    KA02-DRCD           TO  SPA-GMN-DRCD-G
           MOVE    KA02-HKNCOMBI       TO  SPA-GMN-HKNCOMBI-G
           MOVE    KA02-KOUFUYMD       TO  SPA-GMN-KOUFUYMD
      *
           IF      SPA-INPUT-PAGE      >   ZERO
               MOVE    KA02-NISUU          TO  SPA-GMN-NISUU
               COMPUTE IDX-STR         =  (SPA-INPUT-PAGE  -  1  )
                                       *   5
                                       +   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   5   )  OR
                                  (IDX-STR     >   50  )
                   IF      SPA-GMN-INPUT (IDX-STR) =   SPACE
                       CONTINUE
                   ELSE
                       MOVE    KA02-SUURYO (IDX)
                                           TO  SPA-GMN-SURYO(IDX-STR)
                   END-IF
                   ADD     1               TO  IDX-STR
               END-PERFORM
           END-IF
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
      *    保険組合せ
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4101-HKNCOMBI-CHK-SEC
           END-IF
      *    ドクター
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-DRCD-CHK-SEC
           END-IF
      *    交付日
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4102-KOUFUYMD-CHK-SEC
           END-IF
      *
      *    剤選択時のみ
           IF      SPA-INPUT-PAGE      >   ZERO
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4103-NISUU-CHK-SEC
               END-IF
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 4103-SURYO-CHK-SEC
               END-IF
           END-IF
      *
           .
       4102-KIHON-CHK-EXT.
           EXIT.
      *****************************************************************
      *    交付日チェック処理
      *****************************************************************
       4102-KOUFUYMD-CHK-SEC          SECTION.
      *
           INITIALIZE                      ORCSGDAYAREA
           MOVE    SPA-GMN-KOUFUYMD    TO  SGDAY-INDAY
           CALL    "ORCSGDAY"          USING
                                       ORCSGDAYAREA
           IF      SGDAY-RC            =   ZERO
               MOVE    SGDAY-OTDAY         TO  SPA-GMN-KOUFUYMD
               MOVE    SGDAY-SDAY          TO  SPA-NAI-KOUFUYMD
           ELSE
               MOVE    "0007"              TO  SPA-ERRCD
               MOVE    05                  TO  SPA-GMN-CUR
           END-IF
           IF      SPA-ERRCD           =   SPACE
      *        入院期間中
               IF     (SPA-NAI-KOUFUYMD    <   SPA-NAI-NYUINYMD ) OR
                      (SPA-NAI-KOUFUYMD    >   SPA-NAI-TAIINYMD )
                   MOVE    "0008"              TO  SPA-ERRCD
                   MOVE    05                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4102-KOUFUYMD-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    ドクターチェック処理
      *****************************************************************
       4102-DRCD-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE           TO  SPA-GMN-DRCD-G
           ELSE
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-DRCD-MAX)
                               OR (FLG-OK      =   1     )
                   IF      SPA-GMN-DRCD        =   SPA-GMN-TDRCD(IDX)
                       MOVE    SPA-GMN-DRCD-COMB (IDX)
                                               TO  SPA-GMN-DRCD-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF      FLG-OK              =   ZERO
                   PERFORM 41021-DRCD-TBL-CHK-SEC
                   IF      ORCSDR-ERRCD    NOT =   ZERO
                       MOVE    "0004"          TO  SPA-ERRCD
                       MOVE    4               TO  SPA-GMN-CUR
                   END-IF
               END-IF
           END-IF
           .
       4102-DRCD-CHK-EXT.
           EXIT.
      *****************************************************************
      *    ドクター追加チェック処理
      *****************************************************************
       41021-DRCD-TBL-CHK-SEC          SECTION.
      *
           INITIALIZE                  ORCSDRSETAREA
           MOVE    SPA-NAI-SRYYMD      TO  ORCSDR-SYSYMD
           MOVE    "2"                 TO  ORCSDR-KBN
           MOVE    SPA-NAI-RRK-SRYKA(SPA-GMN-SELNUM1)
                                       TO  ORCSDR-SRYKA
           MOVE    SPA-GMN-DRCD-COMB-G TO  ORCSDR-DRCD-TBL-G
           MOVE    SPA-DRCD-MAX        TO  ORCSDR-DRCD-MAX
           MOVE    SPA-GMN-DRCD        TO  ORCSDR-IN-DRCD
           CALL    "ORCSDRSET"         USING
                                       SPA-AREA
                                       ORCSDRSETAREA
           IF      ORCSDR-ERRCD            ZERO
               MOVE    ORCSDR-DRCD-TBL-G   TO  SPA-GMN-DRCD-COMB-G
               MOVE    ORCSDR-DRCD-MAX     TO  SPA-DRCD-MAX
               MOVE    ORCSDR-IN-DRCD-G    TO  SPA-GMN-DRCD-G
           END-IF
           .
       41021-DRCD-TBL-CHK-EXT.
           EXIT.
      *****************************************************************
      *    保険組合せチェック処理
      *****************************************************************
       4101-HKNCOMBI-CHK-SEC          SECTION.
      *
           IF      SPA-GMN-HKNCOMBINUM =   SPACE
               MOVE    SPACE           TO  SPA-GMN-HKNCOMBI-G
           END-IF
           IF      SPA-GMN-SELNUM1     >   ZERO
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX         >   SPA-HKNCOMBI-MAX)
                               OR (FLG-OK      =   1     )
                   IF      SPA-GMN-HKNCOMBINUM =   SPA-GMN-THKNCOMBINUM
                                                                (IDX)
                       MOVE    SPA-GMN-HKNCOMBI-COMB (IDX)
                                               TO  SPA-GMN-HKNCOMBI-G
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
      *
               IF     (SPA-GMN-HKNCOMBINUM     =   "9999" )
                 AND  (SPA-NAI-INNAI           =   2      )
                   MOVE    "0013"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
               END-IF
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "0003"          TO  SPA-ERRCD
                   MOVE    3               TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       4101-HKNCOMBI-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    日数チェック処理
      *****************************************************************
       4103-NISUU-CHK-SEC            SECTION.
      *
      *    在宅、コメントは日数１とする
           IF      SPA-GMN-NISUU       >   1
               IF      SPA-GMN-SRYKBN      =   "14"  OR  "98"
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    10              TO  SPA-GMN-CUR
               END-IF
           END-IF
      *H30.XX
      *    分割調剤の時、日数変更不可
      *    IF     (SPA-NAI-BUNKSUU    >   ZERO )
      *       AND (SPA-GMN-NISUU      >   ZERO )
      *        IF      SPA-NAI-BUNKSUU    NOT =   SPA-GMN-NISUU
      *            IF      SPA-NAI-KEIFLG2     =   ZERO
      *                MOVE    "K014"          TO  SPA-ERRCD
      *                MOVE    10              TO  SPA-GMN-CUR
      *                MOVE    1               TO  SPA-NAI-KEIFLG2
      *            END-IF
      *        END-IF
      **** END-IF
           .
       4103-NISUU-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    数量チェック処理
      *****************************************************************
       4103-SURYO-CHK-SEC            SECTION.
      *
           MOVE    ZERO                TO  IDY
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   50  )  OR
                              (SPA-ERRCD   NOT =   SPACE )
               ADD     1               TO  IDY
               IF      IDY             >   5
                   MOVE    1               TO  IDY
               END-IF
               IF     (SPA-GMN-INPUT (IDX)     =   SPACE )  OR
                      (SPA-GMN-SURYO (IDX)     =   SPACE )
                   MOVE    ZERO           TO  SPA-NAI-SURYO(IDX)
                   MOVE    ZERO           TO  SPA-NAI-KANSURYO(IDX)
                   MOVE    SPACE          TO  SPA-GMN-KEISURYO(IDX)
               ELSE
                   PERFORM 41031-SURYO-KANCHEK-SEC
      ***          INITIALIZE                      ORCSNUMAREA
      *            MOVE    SPA-GMN-SURYO (IDX) TO  SNUM-INX
      *            CALL    "ORCSNUM"           USING
      *                                        ORCSNUMAREA
      *            IF     (SNUM-RC        NOT =   ZERO  )   OR
      *                   (SNUM-MINKBN        =   "1"   )   OR
      *                   (SNUM-SEISUU        >   5     )   OR
      *                   (SNUM-SYOSUU        >   5     )
      *                MOVE    "0005"         TO  SPA-ERRCD
      *                COMPUTE SPA-GMN-CUR    =   IDY   +   10
      *            ELSE
      *                MOVE    SNUM-OUTNUM     TO  SPA-NAI-SURYO(IDX)
      *                PERFORM 400411-SUURYO-HEN-SEC
      *****        END-IF
               END-IF
           END-PERFORM
           .
       4103-SURYO-CHK-EXT.
           EXIT.
      *****************************************************************
      *    換算数量チェック処理
      *****************************************************************
       41031-SURYO-KANCHEK-SEC            SECTION.
      *
           MOVE    SPACE               TO  WRK-SURYO-X
           MOVE    ZERO                TO  FLG-KANSAN
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1   BY  1
                   UNTIL      (IDM     >   12 )
                           OR (IDM2    >=  11 )
               IF      SPA-GMN-SURYO (IDX) (IDM:1)  =   "A"
                                                    OR  "a"
                   MOVE    1               TO  FLG-KANSAN
               ELSE
                   ADD     1               TO  IDM2
                   MOVE    SPA-GMN-SURYO (IDX) (IDM:1) 
                                           TO  WRK-SURYO-X (IDM2:1)
               END-IF
           END-PERFORM
      *
           INITIALIZE                      ORCSNUMAREA
           MOVE    WRK-SURYO-X         TO  SNUM-INX
           CALL    "ORCSNUM"           USING
                                           ORCSNUMAREA
           IF     (SNUM-RC         NOT =   ZERO  )   OR
                  (SNUM-MINKBN         =   "1"   )   OR
                  (SNUM-SEISUU         >   5     )   OR
                  (SNUM-SYOSUU         >   5     )
               MOVE    "0005"              TO  SPA-ERRCD
           ELSE
               MOVE    SNUM-OUTNUM     TO  SPA-NAI-SURYO(IDX)
           END-IF
           IF     (FLG-KANSAN          =   1   )  AND
                  (SPA-NAI-KANZANCHI (IDX) =   ZERO )
               MOVE    "0020"              TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               IF      FLG-KANSAN          =   ZERO
                   MOVE    ZERO            TO  SPA-NAI-KANSURYO(IDX)
               ELSE
                   MOVE    SPA-NAI-SURYO (IDX)
                                           TO  SPA-NAI-KANSURYO(IDX)
                   COMPUTE SPA-NAI-SURYO (IDX)
                                           =   SPA-NAI-KANSURYO (IDX)
                                           *   SPA-NAI-KANZANCHI (IDX)
               END-IF
               PERFORM 400411-SUURYO-HEN-SEC
           ELSE
               COMPUTE SPA-GMN-CUR         =   IDY   +   10
           END-IF
           .
       41031-SURYO-KANCHEK-EXT.
           EXIT.
      *****************************************************************
      *    更新前チェック処理
      *****************************************************************
       4103-KOUSIN-CHK-SEC            SECTION.
      *
           .
       4103-KOUSIN-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    戻る　処理
      *****************************************************************
       210-BACK                        SECTION.
      *
           PERFORM 210-PARA-DEL-SEC
      *
           MOVE    SPA-HZN-MOTOPG      TO  SPA-SAKIPG
           MOVE    "KA02"              TO  SPA-MOTOPG
      *
           MOVE    SPA-KYOUTU          TO  LNK-KYOUTU
      *
           MOVE    "CHANGE"            TO  MCP-PUTTYPE
           MOVE    SPA-SAKIPG          TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       210-BACK-EXT.
           EXIT.
      *****************************************************************
      *    戻るのクリア編集処理
      *****************************************************************
       210-PARA-DEL-SEC               SECTION.
      *
      *    ＳＰＡ一時保存テーブル削除
           PERFORM 10031-SPATMP-DELETE-SEC
      *
           MOVE    1                   TO  FLG-END2
      *
           .
       210-PARA-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    クリア処理
      *****************************************************************
       402-CLREA-SEC       SECTION.
      *
           IF      SPA-GMN-SELNUM2     >   ZERO
               MOVE    ZERO                TO  SPA-GMN-SELNUM2
               INITIALIZE                  SPA-GMN-INPUT-AREA
               MOVE    2                   TO  SPA-GMN-CUR
               MOVE    ZERO                TO  SPA-KEIFLG
           ELSE
      *        初期表示
               INITIALIZE                  SPA-KA02-DATA
               INITIALIZE                  SPA-PARA-WKSRY-TBL
               MOVE    ZERO                TO  SPA-WKSRY-MAX
               PERFORM 4021-CLREA-SELNUM01-SEC
               MOVE    1                   TO  SPA-GMN-CUR
      *        交付日
      *        MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-KOUFUYMD
      *        MOVE    SPA-SRYYMD          TO  SPA-NAI-KOUFUYMD
      *
      *        INITIALIZE                  SPA-KA02-DATA
      *        INITIALIZE                  SPA-PARA-WKSRY-TBL
      *        MOVE    ZERO                TO  SPA-WKSRY-MAX
      *        INITIALIZE                  SPA-GMN-MEISAI-AREA
      *        IF      SPA-JYURRK-MAX      >   ZERO
      *           １件目を対象として内容編集
      *            MOVE    1               TO  SPA-GMN-SELNUM1
      *            PERFORM 30012-MEISAI-SYORI-SEC
      *            MOVE    2                   TO  SPA-GMN-CUR
      *        ELSE
      *            MOVE    91                  TO  SPA-GMN-CUR
      ******   END-IF
           END-IF
      *
           .
       402-CLREA-EXT.
           EXIT.
      *****************************************************************
      *    選択１クリア処理
      *****************************************************************
       4021-CLREA-SELNUM01-SEC       SECTION.
      *
           INITIALIZE                  SPA-GMN-MEISAI-AREA
           MOVE    ZERO                TO  SPA-GMN-SELNUM1
           MOVE    SPACE               TO  SPA-GMN-SRYKAMEI
      *    診療日
           MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-SRYYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-SRYYMD
      *    交付日
           MOVE    SPA-SRYYMDW(1:9)    TO  SPA-GMN-KOUFUYMD
           MOVE    SPA-SRYYMD          TO  SPA-NAI-KOUFUYMD
      *    内容を編集する
           PERFORM 300121-INIT-HENSYU-SEC
      *    保険組合せは１件目
           MOVE    SPA-GMN-HKNCOMBI-COMB(1)
                                       TO  SPA-GMN-HKNCOMBI-G
           .
       4021-CLREA-SELNUM01-EXT.
           EXIT.
      *****************************************************************
      *    交付日処理
      *****************************************************************
       403-KOUFUYMD-SEC       SECTION.
      *
           MOVE    05                  TO  SPA-GMN-CUR
           .
       403-KOUFUYMD-EXT.
           EXIT.
      *****************************************************************
      *    変更確定処理
      *****************************************************************
       408-KAKUTEI-SEC       SECTION.
      *
           IF      SPA-GMN-SELNUM2     >   ZERO
      *        入力チェック処理
               MOVE    2               TO  FLG-KOUSIN
               PERFORM 410-INPUT-CHK-SEC
               IF      SPA-ERRCD       =   SPACE
      *            入力編集処理
                   PERFORM 4103-INPUT-TBLSET-SEC
               END-IF
          END-IF
      *
           .
       408-KAKUTEI-EXT.
           EXIT.
      *****************************************************************
      *    入力編集処理
      *****************************************************************
       4103-INPUT-TBLSET-SEC       SECTION.
      *
           MOVE    ZERO                TO  IDX-W
           MOVE    ZERO                TO  IDX-G
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-MEISAI-MAX )  OR
                              (IDX-W   >   ZERO      )
               IF      SPA-GMN-SELNUM2     =   SPA-GMN-TBANGO (IDX)
                   MOVE    SPA-NAI-TBANGO (IDX)    TO  IDX-W
                   MOVE    IDX                 TO  IDX-G
               END-IF
           END-PERFORM
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )   OR
                              (SPA-WKSRY-SRYCD (IDX-W IDX) =   SPACE)
               IF      SPA-WKSRY-SRYCD (IDX-W IDX)
                                           =  SPA-NAI-SRYCD (IDX)
                   MOVE    SPA-NAI-SURYO (IDX)
                                   TO  SPA-WKSRY-SRYSURYO (IDX-W IDX)
                   MOVE    SPA-NAI-KANSURYO (IDX)
                                   TO  SPA-WKSRY-KANSURYO (IDX-W IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-GMN-NISUU       TO  SPA-WKSRY-ZAIKAIKEI(IDX-W)
      *
           MOVE    ZERO                TO  IDG-K
      *    画面再編集
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   50  )   OR
                              (SPA-WKSRY-SRYCD (IDX-W IDX)
                                        =   SPACE )
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDG     FROM    IDX-G   BY  1
                       UNTIL      (IDG     >   MAX-GMN )   OR
                                  (FLG-OK  =   1   )
                   IF      SPA-WKSRY-SRYCD (IDX-W IDX)
                                       =   SPA-NAI-TSRYCD (IDG)
                       MOVE    1               TO  FLG-OK
                       MOVE    SPACE           TO  SPA-GMN-TFLG (IDG)
                       IF      SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)
                                                   =   "6"  OR  "7"
                           MOVE    SPA-WKSRY-SRYSURYO (IDX-W IDX)
                                               TO  WRK-SURYO
                           PERFORM 310121-SURYO-HEN-SEC
                           MOVE    WRK-SURYO-J
                                               TO  SPA-GMN-TSURYOU (IDG)
                           INSPECT SPA-GMN-TSURYOU (IDG)
                                   REPLACING  ALL "  "  BY  "　"
                       END-IF
      *!
      *                用量割合再編集
                       IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:7)
                                                   =   "0992000") AND
                              (SPA-WKSRY-SRYSURYO (IDX-W IDX)
                                               NOT =   ZERO     )
                           PERFORM 41031-INPUT-MEISYO4-SEC
                           MOVE    WRK-MEISYO      TO
                                                SPA-GMN-TMEISYO2(IDG)
                        END-IF
      *!
                       IF      SPA-WKSRY-SRYSURYO (IDX-W IDX)
                                               =   ZERO
                           MOVE    "削"        TO  SPA-GMN-TFLG (IDG)
                       END-IF
      *!!
      *                換算値入力
                       IF     (SPA-WKSRY-SRYCD (IDX-W IDX)(1:1)
                                                   =   "6"    )
                       AND    (SPA-WKSRY-KANZANCHI(IDX-W IDX)
                                                   >   ZERO   )
                           ADD     1               TO  IDG
                           MOVE    SPA-WKSRY-KANSURYO (IDX-W IDX)
                                                   TO  WRK-SURYO
                           MOVE    SPA-WKSRY-KANZANTANINAME (IDX-W IDX)
                                               TO  WRK-KANZANTANINAME
                           PERFORM 3101291-KANSURYO-HENKAN-SEC
                           INSPECT SPA-GMN-TMEISYO(IDG) REPLACING
                                               ALL "  "  BY  "　"
                       END-IF
      *!!
                   END-IF
               END-PERFORM
           END-PERFORM
      *    回数
           PERFORM VARYING     IDG     FROM    IDX-G   BY  1
                   UNTIL      (IDG     >   SPA-MEISAI-MAX )
               IF     (SPA-GMN-SELNUM2     =   SPA-GMN-TBANGO (IDG))
                   IF      (SPA-WKSRY-ZAIKAIKEI(IDX-W)  =   ZERO )
                       MOVE    "削"            TO  SPA-GMN-TFLG (IDG)
                   ELSE
                       MOVE    SPACE           TO  SPA-GMN-TFLG (IDG)
                   END-IF
               END-IF
               IF          SPA-NAI-KAIFLG (IDG)    =   1
                   MOVE    SPA-WKSRY-ZAIKAIKEI(IDX-W)
                                               TO   WRK-SRYKAISUKEI
                   PERFORM 31013-KAISU-HEN-SEC
                   MOVE    WRK-ZAIKEIJ         TO  SPA-GMN-TZAIKEI
                                                              (IDG)
               END-IF
               IF      SPA-GMN-TMEISYO (IDG)(1:1)  =   "-"
                   MOVE    SPA-MEISAI-MAX          TO  IDG
               END-IF
           END-PERFORM
      *
           INITIALIZE                  SPA-GMN-INPUT-AREA
           MOVE    ZERO                TO  SPA-GMN-SELNUM2
           MOVE    ZERO                TO  SPA-KEIFLG
           MOVE    2                   TO  SPA-GMN-CUR
           .
       4103-INPUT-TBLSET-EXT.
           EXIT.
      *****************************************************************
      *     用量割合再編集処理
      *****************************************************************
       41031-INPUT-MEISYO4-SEC        SECTION.
      *
           INITIALIZE                  ORCSMEIHENAREA
           MOVE    "1"                 TO  ORCSMEIHEN-KBN
           MOVE    SPA-WKSRY-SRYSURYO (IDX-W IDX)
                                       TO  ORCSMEIHEN-SURYO
           MOVE    SPA-WKSRY-SRYCD (IDX-W IDX)
                                       TO  ORCSMEIHEN-SRYCD
           MOVE    SPA-WKSRY-NAME (IDX-W IDX)
                                       TO  ORCSMEIHEN-TNS-NAME
           CALL    "ORCSMEIHEN"        USING
                                       SPA-AREA
                                       ORCSMEIHENAREA
           MOVE    ORCSMEIHEN-MEISYO   TO  WRK-MEISYO
           .
       41031-INPUT-MEISYO4-EXT.
           EXIT.
      *****************************************************************
      *     換算値編集処理
      *****************************************************************
       41031-INPUT-MEISYO5-SEC        SECTION.
      *
           MOVE    SPA-WKSRY-KANZANCHI (IDX-W IDX)
                                       TO  WRK-SURYO
           PERFORM 310121-SURYO-HEN-SEC
           MOVE    SPACE               TO  WRK-KANSURYO
           MOVE    ZERO                TO  IDM2
           PERFORM VARYING     IDM     FROM    1   BY  1
                   UNTIL       IDM     >   11
               IF      WRK-SURYO-X(IDM:1)  NOT =   SPACE
                   ADD     1               TO  IDM2
                   MOVE    WRK-SURYO-X(IDM:1)
                                           TO  WRK-KANSURYO(IDM2:1)
               END-IF
           END-PERFORM
           STRING  "["                 DELIMITED  BY  SIZE
                   WRK-KANSURYO        DELIMITED  BY  SPACE
                   "]"                 DELIMITED  BY  SIZE
                                       INTO    WRK-G-SURYO
           END-STRING
           COMPUTE IDM2                =   IDM2   +   2
           DIVIDE  IDM2        BY  2   GIVING  WRK-SHO
                                       REMAINDER WRK-AMARI
           IF      WRK-AMARI       NOT =   ZERO
               ADD     1               TO  IDM2
           END-IF
      *??  MOVE    SPA-WKSRY-NAME (IDX-W IDX)
      *                                TO  WRK-MEISYO
      *??  MOVE    WRK-MEISYO          TO  SPA-GMN-INPUT (IDX)
           COMPUTE IDM3                =   60
                                       -   IDM2
                                       +   1
           MOVE    WRK-G-SURYO (1:IDM2)
                                       TO  SPA-GMN-INPUT (IDX)
                                             (IDM3:IDM2)
           .
       41031-INPUT-MEISYO5-EXT.
           EXIT.
      *!
      *!
      *****************************************************************
      *    選択印刷処理
      *****************************************************************
       405-SEL-PRT-SEC       SECTION.
      *
           IF      SPA-SEL-SYORI       =   ZERO
               MOVE    1               TO  SPA-SEL-SYORI
           ELSE
               MOVE    ZERO            TO  SPA-SEL-SYORI
           END-IF
      *
           IF      SPA-GMN-SELNUM1     >   ZERO
      *        選択のクリア
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >    SPA-MEISAI-MAX
                   MOVE    SPACE           TO  SPA-GMN-TSEL (IDX)
               END-PERFORM
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL       IDX     >    SPA-WKSRY-MAX
                   MOVE    ZERO            TO  SPA-WKSRY-SELFLG(IDX)
               END-PERFORM
               INITIALIZE                  SPA-GMN-INPUT-AREA
               MOVE    ZERO                TO  SPA-GMN-SELNUM2
               MOVE    ZERO                TO  SPA-KEIFLG
               MOVE    2                   TO  SPA-GMN-CUR
           END-IF
           .
       405-SEL-PRT-EXT.
           EXIT.
      *****************************************************************
      *    選択内容登録処理
      *****************************************************************
       409-TOUROKU-SEC       SECTION.
      *
           IF      SPA-GMN-SELNUM1    >   ZERO
      *        選択印刷でない時、剤選択中は、エラー
               IF     (SPA-GMN-SELNUM2     >   ZERO )
                AND   (SPA-SEL-SYORI       =   ZERO )
                   MOVE    "0010"          TO  SPA-ERRCD
               ELSE
      *            受診内容をパラメタへ編集
                   PERFORM 4091-WKSRY-PARA-HEN-SEC
               END-IF
           END-IF
           .
       409-TOUROKU-EXT.
           EXIT.
      *****************************************************************
      *    受診内容パラメタ編集処理
      *****************************************************************
       4091-WKSRY-PARA-HEN-SEC       SECTION.
      *
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL      (IDX-W   >   SPA-WKSRY-MAX )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF     ( SPA-WKSRY-ZAIKAIKEI(IDX-W) =   ZERO  )
                 OR   ((SPA-SEL-SYORI              =   1    ) AND
                       (SPA-WKSRY-SELFLG (IDX-W)   =   ZERO ))
                   CONTINUE
               ELSE
      *            登録済みチェック
                   PERFORM 4091-ZAINUM-CHK-SEC
                   IF      SPA-ERRCD           =   SPACE
                       ADD     1                   TO  SPA-PARA-MAX
                       IF      SPA-PARA-MAX        >   100
                           MOVE    "0012"          TO  SPA-ERRCD
                       ELSE
                           PERFORM 40911-PARA-HENSYU-SEC
                       END-IF
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      SPA-ERRCD           =   SPACE
               IF      SPA-PRINT-SYORI     =   ZERO
                   MOVE    ZERO                TO  SPA-SEL-SYORI
                   MOVE    ZERO                TO  SPA-KEIFLG2
                   PERFORM 30012-MEISAI-SYORI-SEC
               END-IF
           END-IF
          .
       4091-WKSRY-PARA-HEN-EXT.
           EXIT.
      *****************************************************************
      *    登録済みチェック処理
      *****************************************************************
       4091-ZAINUM-CHK-SEC       SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   SPA-PARA-MAX )
                          OR  (SPA-ERRCD   NOT =   SPACE )
               IF      SPA-WKSRY-ZAINUM (IDX-W)
                                       =   SPA-PARA-ZAINUM  (IDX)
                   MOVE    "K002"          TO  SPA-ERRCD
               END-IF
           END-PERFORM
           IF      SPA-ERRCD       NOT =   SPACE
               IF      SPA-KEIFLG2         =   ZERO
                   MOVE    1               TO  SPA-KEIFLG2
               ELSE
                   MOVE    SPACE           TO  SPA-ERRCD
               END-IF
           END-IF
           .
       4091-ZAINUM-CHK-EXT.
           EXIT.
      *****************************************************************
      *    印刷内容パラメタ編集処理
      *****************************************************************
       40911-PARA-HENSYU-SEC       SECTION.
      *
           IF      SPA-PARA-MAX        =   1
               MOVE    SPA-NAI-RRK-HKNCOMBI (SPA-GMN-SELNUM1)
                                           TO  SPA-PARA-HKNCOMBI
               MOVE    SPA-NAI-RRK-SRYKA (SPA-GMN-SELNUM1)
                                           TO  SPA-PARA-SRYKA
               MOVE    SPA-NAI-RRK-DRCD (SPA-GMN-SELNUM1)
                                           TO  SPA-PARA-DRCD
               MOVE    SPA-NAI-NYUINYMD    TO  SPA-PARA-NYUINYMD
               MOVE    SPA-NAI-TAIINYMD    TO  SPA-PARA-TAIINYMD
           END-IF
           MOVE    SPA-PARA-MAX        TO  IDX1
           MOVE    SPA-WKSRY-ZAINUM (IDX-W)
                                       TO  SPA-PARA-ZAINUM  (IDX1)
           MOVE    SPA-WKSRY-ZAIKAIKEI (IDX-W)
                                       TO  SPA-PARA-ZAIKAIKEI (IDX1)
           MOVE    SPA-WKSRY-SRYSYUKBN (IDX-W)
                                       TO  SPA-PARA-SRYSYUKBN (IDX1)
           MOVE    SPA-WKSRY-SRYKBN (IDX-W)
                                       TO  SPA-PARA-SRYKBN (IDX1)
           MOVE    SPA-WKSRY-ZAITENKEI (IDX-W)
                                       TO  SPA-PARA-ZAITENKEI (IDX1)
           MOVE    ZERO            TO  IDX2
           PERFORM VARYING     IDX-W2  FROM    1   BY  1
                   UNTIL      (IDX-W2  >   50      )  OR
                              (SPA-WKSRY-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
               IF     (SPA-WKSRY-SRYSURYO (IDX-W IDX-W2)
                                                   =   ZERO )
               OR     (SPA-WKSRY-SRYCD (IDX-W IDX-W2)(1:4)
                                                   =   "0086" )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX2
                   MOVE    SPA-WKSRY-SRYCD (IDX-W IDX-W2)
                                           TO  SPA-PARA-SRYCD(IDX1 IDX2)
                   MOVE    SPA-WKSRY-SRYSURYO (IDX-W IDX-W2)
                                           TO  SPA-PARA-SRYSURYO
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-SRYKAISU (IDX-W IDX-W2)
                                           TO  SPA-PARA-SRYKAISU
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-INPUTNUM (IDX-W IDX-W2)
                                           TO  SPA-PARA-INPUTNUM
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-AUTOKBN (IDX-W IDX-W2)
                                           TO  SPA-PARA-AUTOKBN
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-INPUTCOMENT (IDX-W IDX-W2)
                                           TO  SPA-PARA-INPUTCOMENT
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-KANSURYO (IDX-W IDX-W2)
                                           TO  SPA-PARA-KANSURYO
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-KANZANCHI (IDX-W IDX-W2)
                                           TO  SPA-PARA-KANZANCHI
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-KANZANTANINAME (IDX-W IDX-W2)
                                           TO  SPA-PARA-KANZANTANINAME
                                                             (IDX1 IDX2)
      *H28.5
                   IF     (SPA-WKSRY-SRYCD (IDX-W IDX-W2)(1:1) =   "6")
                     AND  (SPA-WKSRY-GENE-NAME (IDX-W IDX-W2)
                                               NOT =   SPACE)
                     AND  (SPA-NAI-GENERICFLG      =   "1"    )
      *                一般名表示
                       MOVE    SPA-WKSRY-GENE-NAME(IDX-W IDX-W2)
                                           TO  SPA-PARA-INPUTCOMENT
                                                             (IDX1 IDX2)
                   END-IF
      *H28.11      用法コード数量
                   MOVE    SPA-WKSRY-INPUTCHI-G (IDX-W IDX-W2)
                                           TO  SPA-PARA-INPUTCHI-G
                                                             (IDX1 IDX2)
      *
                   MOVE    SPA-WKSRY-KENTAICOMMENT (IDX-W IDX-W2)
                                           TO  SPA-PARA-KENTAICOMMENT
                                                             (IDX1 IDX2)
                   MOVE    SPA-WKSRY-YSETKBN (IDX-W IDX-W2)
                                           TO  SPA-PARA-YSETKBN
                                                             (IDX1 IDX2)
      *
                   MOVE    SPA-WKSRY-ERRKBN (IDX-W IDX-W2)
                                           TO  SPA-PARA-ERRKBN
                                                             (IDX1 IDX2)
               END-IF
           END-PERFORM
           .
       40911-PARA-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    院外・院内切替登録
      *****************************************************************
       404-INNAI-CHNGE-SEC       SECTION.
      *
           IF      SPA-NAI-INNAI       =   1
               MOVE    2               TO  SPA-NAI-INNAI
           ELSE
               MOVE    1               TO  SPA-NAI-INNAI
           END-IF
           INITIALIZE                  SPA-KA02-DATA
           INITIALIZE                  SPA-PARA-WKSRY-TBL
           INITIALIZE                  SPA-GMN-INPUT-AREA
           INITIALIZE                  SPA-GMN-SELNUM1
           INITIALIZE                  SPA-GMN-SELNUM2
           INITIALIZE                  SPA-WKSRY-MAX
           INITIALIZE                  SPA-GMN-MEISAI-AREA
           INITIALIZE                  SPA-NAI-WKSRY-TBL
           INITIALIZE                  SPA-KA021
      *    受診履歴編集
           PERFORM 30011-JYURRK-HENSYU-SEC
           IF      SPA-JYURRK-MAX      >   ZERO
      *       １件目を対象として内容編集
               MOVE    1                   TO  SPA-GMN-SELNUM1
               PERFORM 30012-MEISAI-SYORI-SEC
               MOVE    2                   TO  SPA-GMN-CUR
           ELSE
               MOVE    ZERO                TO  SPA-PRINT-SYORI
               INITIALIZE                  SPA-GMN-MEISAI-AREA
               INITIALIZE                  SPA-NAI-WKSRY-TBL
               MOVE    ZERO                TO  SPA-WKSRY-MAX
               MOVE    ZERO                TO  SPA-KEIFLG
               MOVE    ZERO                TO  SPA-GMN-SELNUM1
               MOVE    91                  TO  SPA-GMN-CUR
           END-IF
           .
       404-INNAI-CHNGE-EXT.
           EXIT.
      *****************************************************************
      *    選択確認画面
      *****************************************************************
       409-KA021-SET-SEC       SECTION.
      *
           MOVE    SPACE               TO  SPA-MOTOPG
      *
           MOVE    SPA-KA01KYOUTU      TO  SPA-WORK
           MOVE    SPA-AREA            TO  SPA-COMMON
           MOVE    SPA-KA02            TO  SPA-FREE
      *
           MOVE    "LINK"              TO  MCP-STATUS
           MOVE    SPACE               TO  MCP-EVENT
      *
           CALL    "ORCGKA021"         USING
                                       MCPAREA
                                       SPAAREA
                                       LINKAREA
                                       SCRAREA.
      *
           MOVE    SPA-COMMON          TO  SPA-AREA
           MOVE    SPA-WORK            TO  SPA-KA01KYOUTU
           MOVE    SPA-FREE            TO  SPA-KA02
      *
      *    カーソル移動
           MOVE    "SELNUM"            TO  MCP-WIDGET
      *
           MOVE    "KA02"              TO  SPA-MOTOPG
           MOVE    "KA021"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KA021"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
           .
       409-KA021-SET-EXT.
           EXIT.
      *****************************************************************
      *    前頁処理
      *****************************************************************
       406-MAE-PAGE-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-KOUSIN
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-INPUT-PAGE      >   1     )
               COMPUTE SPA-INPUT-PAGE  =   SPA-INPUT-PAGE
                                       -   1
               MOVE    11                  TO  SPA-GMN-CUR
           END-IF
           .
       406-MAE-PAGE-EXT.
           EXIT.
      *****************************************************************
      *    次頁処理
      *****************************************************************
       407-NEXT-PAGE-SEC       SECTION.
      *
           MOVE    1                   TO  FLG-KOUSIN
           PERFORM 410-INPUT-CHK-SEC
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-INPUT-PAGE      >   ZERO  )
               COMPUTE IDX-STR         =   SPA-INPUT-PAGE
                                       *   5
                                       +   1
               IF     (IDX-STR         <=  50 )  AND
                      (SPA-GMN-INPUT (IDX-STR) NOT =   SPACE )
                   COMPUTE SPA-INPUT-PAGE  =   SPA-INPUT-PAGE
                                               +   1
                   MOVE    11                  TO  SPA-GMN-CUR
               END-IF
           END-IF
           .
       407-NEXT-PAGE-EXT.
           EXIT.
      *
      *****************************************************************
      *    印刷内容チェック処理
      *****************************************************************
       4104-TBL-CHK-SEC       SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  FLG-ERR
      *    点数マスタの期間チェック
           IF      SPA-SRYYMD      NOT =   SPA-NAI-KOUFUYMD
               MOVE    1               TO  FLG-OK
           END-IF
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL      (IDX-W   >   SPA-PARA-MAX )
                           OR (FLG-OK      =   1        )
                           OR (FLG-ERR     =   1        )
               PERFORM VARYING     IDX-W2  FROM    1   BY  1
                       UNTIL      (IDX-W2  >   50      )  OR
                                  (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
                               OR (FLG-ERR     =   1        )
      *            交付日の点数マスタの期間チェック
                   MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                               TO  TNS-SRYCD
                   MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOSTYMD
                   MOVE    SPA-NAI-KOUFUYMD    TO  TNS-YUKOEDYMD
                   PERFORM 910-TENSU-READ-SEC
                   IF      FLG-TENSU           =   1
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-PERFORM
           END-PERFORM
           IF      SPA-KEIFLG      =   ZERO
               IF      FLG-ERR         =   1
                   MOVE    1               TO  SPA-KEIFLG
                   MOVE    "K001"          TO  SPA-ERRCD
               END-IF
           END-IF
           .
      *
       4104-TBL-CHK-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報印刷処理
      *****************************************************************
       411-YAKUZAI-PRT-SEC       SECTION.
      *
           IF     (SPA-GMN-SELNUM1     =   ZERO )
           AND    (SPA-PARA-SRYKA      =   SPACE )
               MOVE    "0100"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-NAI-INNAI       =   2     )
               MOVE    "0102"          TO  SPA-ERRCD
           END-IF
      *    剤選択中は、エラー
           IF     (SPA-GMN-SELNUM2     >   ZERO )
           AND    (SPA-SEL-SYORI       =   ZERO )
               MOVE    "0010"          TO  SPA-ERRCD
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  FLG-KOUSIN
               PERFORM 410-INPUT-CHK-SEC
               MOVE    ZERO            TO  SPA-PRINT-SYORI
               IF      SPA-ERRCD       =   SPACE
      *            対象チェック処理
                   IF      SPA-PARA-SRYKA      =   SPACE
      *                受診内容をパラメタへ編集
                       MOVE    1                   TO  SPA-PRINT-SYORI
                       PERFORM 4091-WKSRY-PARA-HEN-SEC
                       MOVE    1                   TO  SPA-PRINT-SYORI
                   END-IF
      *            選択内容のチェック
                   PERFORM 4104-TBL-CHK-SEC
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0101"              TO  SPA-KAID1CD
           END-IF
           .
       411-YAKUZAI-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳印刷処理
      *****************************************************************
       410-OKUSURI-PRT-SEC       SECTION.
      *
           IF     (SPA-GMN-SELNUM1     =   ZERO )
           AND    (SPA-PARA-SRYKA      =   SPACE )
               MOVE    "0100"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-NAI-INNAI       =   2     )
               MOVE    "0102"          TO  SPA-ERRCD
           END-IF
      *    剤選択中は、エラー
           IF     (SPA-GMN-SELNUM2     >   ZERO )
           AND    (SPA-SEL-SYORI       =   ZERO )
               MOVE    "0010"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  FLG-KOUSIN
               PERFORM 410-INPUT-CHK-SEC
               MOVE    ZERO            TO  SPA-PRINT-SYORI
               IF      SPA-ERRCD       =   SPACE
      *            対象チェック処理
                   IF      SPA-PARA-SRYKA      =   SPACE
      *                受診内容をパラメタへ編集
                       MOVE    1                   TO  SPA-PRINT-SYORI
                       PERFORM 4091-WKSRY-PARA-HEN-SEC
                       MOVE    1                   TO  SPA-PRINT-SYORI
                   END-IF
      *            選択内容のチェック
                   PERFORM 4104-TBL-CHK-SEC
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0107"              TO  SPA-KAID1CD
           END-IF
           .
       410-OKUSURI-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん印刷処理
      *****************************************************************
       412-SYOHOU-PRT-SEC       SECTION.
      *
           IF     (SPA-GMN-SELNUM1     =   ZERO )
               MOVE    "0100"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-NAI-INNAI       =   1     )
               MOVE    "0103"          TO  SPA-ERRCD
           END-IF
           IF     (SPA-JYURRK-MAX      =   ZERO  )
               MOVE    "0104"          TO  SPA-ERRCD
           END-IF
      *    剤選択中は、エラー
           IF     (SPA-GMN-SELNUM2     >   ZERO )
           AND    (SPA-SEL-SYORI       =   ZERO )
               MOVE    "0010"          TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD           =   SPACE
               MOVE    1               TO  FLG-KOUSIN
               PERFORM 410-INPUT-CHK-SEC
               MOVE    ZERO                TO  SPA-PRINT-SYORI
               IF      SPA-ERRCD       =   SPACE
      *            対象チェック処理
                   IF      SPA-PARA-SRYKA      =   SPACE
      *                受診内容をパラメタへ編集
                       MOVE    1               TO  SPA-PRINT-SYORI
                       PERFORM 4091-WKSRY-PARA-HEN-SEC
                       MOVE    1               TO  SPA-PRINT-SYORI
                   END-IF
      *            選択内容のチェック
                   PERFORM 4104-TBL-CHK-SEC
               END-IF
           ELSE
               MOVE    1               TO  SPA-GMN-CUR
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "0106"              TO  SPA-KAID1CD
           END-IF
           .
       412-SYOHOU-PRT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん印刷処理
      *****************************************************************
       4102-SYOHOU-PRINT-SEC             SECTION.
      *
           MOVE    SPA-AREA            TO  HZN-SPA-AREA
      *
           MOVE    "2"                 TO  SPA-NYUGAIKBN
      *    帳票プログラム取得
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
               GO      TO      4102-SYOHOU-PRINT-EXT
           END-IF
           MOVE    ORCSPRTNM-PRTPG     TO  WRK-SYOHOU-PG
      *
           MOVE    SPA-NAI-KOUFUYMD    TO  SPA-SRYYMD
      *    処方箋印刷
           INITIALIZE                      ORCHC19AREA
           MOVE    "N"                 TO  ORCHC19-KBN
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           IF      SPA-GMN-DRCD        =   SPACE
               MOVE    SPACE               TO  SPA-DRCD
           ELSE
               MOVE    "1"                 TO  SPA-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  SPA-DRCD(2:4)
           END-IF
           MOVE    SPA-PARA-SRYKA      TO  SPA-SRYKA
           PERFORM 41021-ORCHC19-HENSYU-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "02"                TO  SPA-PRT-FLG
               CALL    WRK-SYOHOU-PG       USING   SPA-AREA
                                                   ORCHC19AREA
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
           MOVE    HZN-SPA-AREA        TO  SPA-AREA
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           IF      SPA-PRINT-SYORI     =   1
               INITIALIZE              SPA-PARA-WKSRY-TBL
               INITIALIZE              SPA-KA02-PRT
               MOVE    ZERO                TO  SPA-SEL-SYORI
               MOVE    ZERO                TO  SPA-KEIFLG2
               PERFORM 30012-MEISAI-SYORI-SEC
           END-IF
           .
       4102-SYOHOU-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    処方せん明細編集処理
      *****************************************************************
       41021-ORCHC19-HENSYU-SEC             SECTION.
      *
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL       IDX-W   >   SPA-PARA-MAX
               ADD     1               TO  IDX1
               MOVE    IDX1            TO  ORCHC19-ZAI-MAX
               MOVE    SPA-PARA-ZAIKAIKEI (IDX-W)
                                           TO  ORCHC19-ZAIKAIKEI (IDX1)
               MOVE    SPA-PARA-SRYSYUKBN (IDX-W)
                                           TO  ORCHC19-SRYSYUKBN (IDX1)
               MOVE    SPA-PARA-SRYKBN (IDX-W)
                                           TO  ORCHC19-SRYKBN (IDX1)
               MOVE    SPA-PARA-ZAITENKEI (IDX-W)
                                           TO  ORCHC19-ZAITENKEI (IDX1)
               MOVE    ZERO            TO  IDX2
               PERFORM VARYING     IDX-W2  FROM    1   BY  1
                       UNTIL      (IDX-W2  >   50      )  OR
                                      (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
                   IF     (SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   >   ZERO )
      *                後発医薬品への変更不可判定
                       IF    ((SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                       =   "099209903")
                         AND  (SPA-PARA-SRYSURYO (IDX-W IDX-W2 - 1)
                                                       =   ZERO   ))
                          CONTINUE
                       ELSE
                           ADD     1           TO  IDX2
                           MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                           TO  ORCHC19-SRYCD (IDX1 IDX2)
                           MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                           TO  ORCHC19-SRYSURYO
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-SRYKAISU (IDX-W IDX-W2)
                                           TO  ORCHC19-SRYKAISU
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-INPUTCOMENT (IDX-W IDX-W2)
                                           TO  ORCHC19-INPUTCOMENT
                                                             (IDX1 IDX2)
      *H30.4
                           MOVE    SPA-PARA-INPUTCHI-G  (IDX-W IDX-W2)
                                           TO  ORCHC19-INPUTCHI-G
                                                             (IDX1 IDX2)
      *H30.12
      *                    分割調剤対応
                           IF     (SPA-WKSRY-SRYCD(IDX-W IDX-W2)
                                                   =   "099208101" )
                               PERFORM 41021-ORCHC19-BUNKATU-SEC
                           END-IF
      *
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           IF      ORCHC19-ZAI-MAX     =   ZERO
               MOVE    "0101"          TO  SPA-ERRCD
           END-IF
           .
       41021-ORCHC19-HENSYU-EXT.
           EXIT.
      *H30.XX
      *****************************************************************
      *    分割調剤　編集チェック処理
      *****************************************************************
       41021-ORCHC19-BUNKATU-SEC             SECTION.
      *
      *    分割調剤
           MOVE    IDX-W2              TO  IDX-B
           PERFORM 400412-BUNKATU-SEC
      *
           IF      SPA-WKSRY-ZAIKAIKEI (IDX-W)
                                   NOT =   SPA-NAI-BUNKSUU
      *        分割調剤の回数と一致しない時は、分割なし
               MOVE    SPACE               TO  ORCHC19-INPUTCHI-G
                                                         (IDX1 IDX2)
           END-IF
           .
       41021-ORCHC19-BUNKATU-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報印刷処理
      *****************************************************************
       4103-YAKJYO-PRINT-SEC             SECTION.
      *
           MOVE    SPA-AREA            TO  HZN-SPA-AREA
      *
      *    病棟・病室編集
      *    PERFORM 41041-BTUNAME-HEN-SEC
      *
           INITIALIZE                      ORCSCHN30AREA
      *
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-PARA-SRYKA      TO  SPA-SRYKA
      *
           MOVE    "N"                 TO  ORCSHCN30-KBN
           IF      SPA-GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCSHCN30-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  ORCSHCN30-DRCD(2:4)
           END-IF
           MOVE    SPA-NAI-BTUNAME     TO  ORCSHCN30-BTUNAME
           MOVE    SPA-NAI-ROOMNO      TO  ORCSHCN30-ROOMNO
           MOVE    SPA-NAI-KOUFUYMD    TO  ORCSHCN30-SRYYMD
      *API(GINBEE)対応
      *    病棟番号
           MOVE    SPA-NAI-BTUNUM      TO  ORCSHCN30-BTUNUM
           PERFORM 41031-ORCSHCN30-HENSYU-SEC
      *
           IF      SPA-ERRCD           =   SPACE
               MOVE    "01"                TO  SPA-PRT-FLG
               CALL    "ORCSCHN30"         USING   SPA-AREA
                                                   ORCSCHN30AREA
           END-IF
           MOVE    SPA-ERRCD           TO  WRK-ERRCD
      *
           MOVE    HZN-SPA-AREA        TO  SPA-AREA
           MOVE    WRK-ERRCD           TO  SPA-ERRCD
      *
           IF      SPA-PRINT-SYORI     =   1
               INITIALIZE              SPA-PARA-WKSRY-TBL
               INITIALIZE              SPA-KA02-PRT
               MOVE    ZERO                TO  SPA-SEL-SYORI
               MOVE    ZERO                TO  SPA-KEIFLG2
               PERFORM 30012-MEISAI-SYORI-SEC
           END-IF
           .
       4103-YAKJYO-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    薬剤情報明細編集１処理
      *****************************************************************
       41031-ORCSHCN30-HENSYU-SEC        SECTION.
      *
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL       IDX-W   >   SPA-PARA-MAX
      *        投薬のみ対象
               IF     ( SPA-PARA-SRYKBN (IDX-W)   =   "98" )
                 OR   ( SPA-PARA-SRYKBN (IDX-W)   =   "31"
                                                  OR  "32"
                                                  OR  "33"
                                                  OR  "34" )
                   CONTINUE
               ELSE
                   ADD     1               TO  IDX1
                   MOVE    IDX1            TO  ORCSHCN30-ZAI-MAX
                   MOVE    SPA-PARA-ZAIKAIKEI (IDX-W)
                                           TO  ORCSHCN30-ZAIKAIKEI(IDX1)
                   MOVE    SPA-PARA-SRYSYUKBN (IDX-W)
                                           TO  ORCSHCN30-SRYSYUKBN(IDX1)
                   MOVE    SPA-PARA-SRYKBN (IDX-W)
                                           TO  ORCSHCN30-SRYKBN (IDX1)
                   MOVE    ZERO            TO  IDX2
                   PERFORM VARYING     IDX-W2  FROM    1   BY  1
                           UNTIL      (IDX-W2  >   50      )  OR
                                      (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
                       ADD     1           TO  IDX2
                       MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                           TO  ORCSHCN30-SRYCD
                                                             (IDX1 IDX2)
                       MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                           TO  ORCSHCN30-SRYSURYO
                                                             (IDX1 IDX2)
                       MOVE    SPA-PARA-KANSURYO (IDX-W IDX-W2)
                                           TO  ORCSHCN30-KANSURYO
                                                             (IDX1 IDX2)
                       MOVE    SPA-PARA-SRYKAISU (IDX-W IDX-W2)
                                           TO  ORCSHCN30-SRYKAISU
                                                             (IDX1 IDX2)
                       MOVE    SPA-PARA-ZAIKAISU (IDX-W IDX-W2)
                                           TO  ORCSHCN30-ZAIKAISU
                                                             (IDX1 IDX2)
                       MOVE    SPA-PARA-INPUTCOMENT (IDX-W IDX-W2)
                                           TO  ORCSHCN30-INPUTCOMENT
                                                             (IDX1 IDX2)
      *H28.11          用法コード数量
                       IF     (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:3)
                                               =   "001"   )
      *H30.4
                          OR  (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:7)
                                               =   "0992081"   )
                           MOVE    SPA-PARA-INPUTCHI-G (IDX-W IDX-W2)
                                           TO  ORCSHCN30-INPUTCHI-G
                                                             (IDX1 IDX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      ORCSHCN30-ZAI-MAX    =   ZERO
               MOVE    "0101"          TO  SPA-ERRCD
           END-IF
      *
           .
       41031-ORCSHCN30-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    病棟・病室編集処理
      *****************************************************************
       41041-BTUNAME-HEN-SEC           SECTION.
      *
      *    入院履歴検索
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-PTNYUINRRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (FLG-OK              =   1 )
      *        システム日付での病棟
               IF      PTNYUINRRK-TENNYUYMD    <=  SPA-SYSYMD
      *            転出日
                   IF     (PTNYUINRRK-TENSTUYMD    =   SPACE )  OR
                          (PTNYUINRRK-TENSTUYMD    NOT =   SPACE AND
                           PTNYUINRRK-TENSTUYMD    >=  SPA-SYSYMD )
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-IF
      *
               IF      FLG-OK              =   ZERO
                   MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
                   MOVE    "key36"             TO  MCP-PATHNAME
                   PERFORM 910-PTNYUINRRK-READ-SEC
               END-IF
           END-PERFORM
           IF      FLG-OK              =   ZERO
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key36"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    病棟
           MOVE    SPACE               TO  SYS-5001-REC
           INITIALIZE                  SYS-5001-REC
           MOVE    "5001"              TO  SYS-5001-KANRICD
           MOVE    PTNYUINRRK-BTUNUM   TO  SYS-5001-KBNCD
           MOVE    SPA-HOSPNUM         TO  SYS-5001-HOSPNUM
           MOVE    SPA-SYSYMD          TO  SYS-5001-STYUKYMD
           MOVE    SPA-SYSYMD          TO  SYS-5001-EDYUKYMD
      *
           MOVE    SYS-5001-REC        TO  SYSKANRI-REC
           PERFORM 910-SYSKANRI-KEYREAD-SEC
           IF      FLG-SYSKANRI        =   ZERO
               MOVE    SYSKANRI-REC        TO  SYS-5001-REC
           ELSE
               INITIALIZE                      SYS-5001-REC
           END-IF
      *
           MOVE    SYS-5001-BTU-NAME   TO  SPA-NAI-BTUNAME
      *
      *    病室番号
           IF      PTNYUINRRK-BRMNUM (3:6) IS  NUMERIC
               MOVE    PTNYUINRRK-BRMNUM (3:6)
                                           TO  WRK-BRMNUM
               MOVE    WRK-BRMNUM          TO  WRK-Z6
               MOVE    WRK-Z6              TO  SPA-NAI-ROOMNO
           ELSE
               MOVE    PTNYUINRRK-BRMNUM (3:6)
                                           TO  SPA-NAI-ROOMNO
           END-IF
      *API(GINBEE)対応
      *    病棟番号
           MOVE    PTNYUINRRK-BTUNUM   TO  SPA-NAI-BTUNUM
      *
           .
       41041-BTUNAME-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳印刷処理
      *****************************************************************
       4104-OKUSURI-PRINT-SEC             SECTION.
      *
           MOVE    SPA-AREA            TO  HZN-SPA-AREA
      *
      *    帳票プログラム取得
           INITIALIZE                      ORCSPRTNMAREA
           MOVE    "1"                 TO  ORCSPRTNM-KBN
           MOVE    "00000010"          TO  ORCSPRTNM-KBNCD
           MOVE    SPA-SRYYMD          TO  ORCSPRTNM-SRYYMD
           CALL    "ORCSPRTNM"         USING
                                       ORCSPRTNMAREA
                                       SPA-AREA
                                       SYS-1032-REC
           IF      ORCSPRTNM-RC        =   ZERO
               MOVE    ORCSPRTNM-PRTPG     TO  WRK-OKUSURI-PG
           ELSE
               MOVE    "1031"              TO  SPA-ERRCD
           END-IF
      *    病棟・病室編集
      **** PERFORM 41041-BTUNAME-HEN-SEC
      *ver.4.7
      *    患者個別設定取得
           PERFORM 41041-PTCONF-HEN-SEC
      *
           INITIALIZE                      ORCHCN62AREA
      *
           MOVE    SPA-GMN-HKNCOMBINUM TO  SPA-HKNCOMBINUM
           MOVE    SPA-PARA-SRYKA      TO  SPA-SRYKA
      *
           IF      SPA-GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCHCN62-DRCD(1:1)
               MOVE    SPA-GMN-DRCD        TO  ORCHCN62-DRCD(2:4)
           END-IF
           MOVE    SPA-NAI-KOUFUYMD    TO  ORCHCN62-SRYYMD
           MOVE    SPA-NAI-BTUNAME     TO  ORCHCN62-BTUNAME
           MOVE    SPA-NAI-ROOMNO      TO  ORCHCN62-ROOMNO
      *API(GINBEE)対応
      *    病棟番号
           MOVE    SPA-NAI-BTUNUM      TO  ORCHCN62-BTUNUM
      *
           MOVE    1                   TO  ORCHCN62-CNT
      *ver.4.7
           MOVE    WRK-QRKBN           TO  ORCHCN62-QRKBN
      *    ＣＳＶ
      *    バージョン
           MOVE    WRK-CSVKBN          TO  ORCHCN62-CSVKBN
      *
           PERFORM 41041-ORCHC62-HENSYU-SEC
           IF      SPA-ERRCD           =   SPACE
               MOVE    "03"                TO  SPA-PRT-FLG
               CALL    WRK-OKUSURI-PG      USING
                                           SPA-AREA
                                           ORCHCN62AREA
           END-IF
      *
           MOVE    SPA-ERRCD            TO  WRK-ERRCD
           MOVE    HZN-SPA-AREA         TO  SPA-AREA
           MOVE    WRK-ERRCD            TO  SPA-ERRCD
      *
           IF      SPA-PRINT-SYORI     =   1
               INITIALIZE              SPA-PARA-WKSRY-TBL
               INITIALIZE              SPA-KA02-PRT
               MOVE    ZERO                TO  SPA-SEL-SYORI
               MOVE    ZERO                TO  SPA-KEIFLG2
               PERFORM 30012-MEISAI-SYORI-SEC
           END-IF
           .
       4104-OKUSURI-PRINT-EXT.
           EXIT.
      *
      *****************************************************************
      *    お薬手帳明細編集処理
      *****************************************************************
       41041-ORCHC62-HENSYU-SEC             SECTION.
      *
      *    投薬・注射
           MOVE    ZERO                TO  IDX1
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL       IDX-W   >   SPA-PARA-MAX
               IF      SPA-PARA-SRYKBN (IDX-W) =   "21"
                                               OR  "22"
                                               OR  "23"
                                               OR  "31"
                                               OR  "32"
                                               OR  "33"
                                               OR  "14"
                   ADD     1               TO  IDX1
                   MOVE    IDX1            TO  ORCHCN62-ZAI-MAX
                   MOVE    SPA-PARA-ZAIKAIKEI (IDX-W)
                                           TO  ORCHCN62-ZAIKAIKEI (IDX1)
                   MOVE    SPA-PARA-SRYSYUKBN (IDX-W)
                                           TO  ORCHCN62-SRYSYUKBN (IDX1)
                   MOVE    SPA-PARA-SRYKBN (IDX-W)
                                           TO  ORCHCN62-SRYKBN (IDX1)
                   MOVE    SPA-PARA-ZAINUM (IDX-W)
                                           TO  ORCHCN62-ZAINUM (IDX1)
                   MOVE    ZERO            TO  IDX2
                   MOVE    ZERO            TO  IDX3
                   MOVE    ZERO                TO  IDXY
                   INITIALIZE                      WRK-YOHOU-G
      *ver.4.7
                   MOVE    SPACE               TO  WRK-SHO-SRYCD
                   PERFORM VARYING     IDX-W2  FROM    1   BY  1
                           UNTIL      (IDX-W2  >   50      )  OR
                                      (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
      *ver.4.7
      *                在宅器材の商品名
                       IF     (SPA-PARA-SRYKBN (IDX-W)  =   "14" )  AND
                              (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:3) 
                                                       =   "058")
                           MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                       TO  WRK-SHO-SRYCD
                       END-IF
      *ver.4.7
      *                薬剤・コメント・用法・のみ
                       IF     (SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   >   ZERO )
                        AND  ((SPA-PARA-SRYCD (IDX-W IDX-W2)(1:1)
                                                   =   "6"
                                                   OR  "8"   )  OR
                              (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:3)
                                                   =   "059"
                                                   OR  "001" )  OR
                              (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:4)
                                                   =   "0082"
                                                   OR  "0083"
                                                   OR  "0084"
      *??                                          OR  "0085" ))
                                                   OR  "0085" )
      *ver.4.7
      *                 在宅器材は対象
                        OR  ((SPA-PARA-SRYKBN (IDX-W)  =   "14" )  AND
                             (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:1) 
                                                       =   "7"  )) )
      *ver.4.7
      *
                           ADD     1           TO  IDX2
                           MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYCD(IDX1 IDX2)
                           MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYSURYO
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-KANSURYO (IDX-W IDX-W2)
                                           TO  ORCHCN62-KANSURYO
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-SRYKAISU (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYKAISU
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-INPUTCOMENT (IDX-W IDX-W2)
                                           TO  ORCHCN62-INPUTCOMENT
                                                             (IDX1 IDX2)
      *ver.4.7 
      *                    商品名コード対応
                           IF      SPA-PARA-SRYCD(IDX-W IDX-W2)(1:1)
                                                           =   "7"
                               MOVE    WRK-SHO-SRYCD
                                           TO  ORCHCN62-SHO-SRYCD
                                                             (IDX1 IDX2)
                           END-IF
                           MOVE    SPACE               TO  WRK-SHO-SRYCD
      *ver.4.7 
      *                    薬剤
                           IF      SPA-PARA-SRYCD (IDX-W IDX-W2)(1:1)
                                                       =   "6"
                               MOVE    IDX2                TO  IDX3
                           END-IF
                       END-IF
      *                用量割合再編集
                       IF     (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:7)
                                                   =   "0992000") AND
                              (IDX3                >   ZERO     )
                           EVALUATE    SPA-PARA-SRYCD (IDX-W IDX-W2)
      *                    朝
                           WHEN    "099200011"
                               MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   TO  ORCHCN62-YOURYOU
                                                         (IDX1 IDX3 1)
      *                    昼
                           WHEN    "099200012"
                               MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   TO  ORCHCN62-YOURYOU
                                                         (IDX1 IDX3 2)
      *                    夜
                           WHEN    "099200013"
                               MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   TO  ORCHCN62-YOURYOU
                                                         (IDX1 IDX3 3)
      *                    寝
                           WHEN    "099200014"
                               MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   TO  ORCHCN62-YOURYOU
                                                         (IDX1 IDX3 4)
      *                    起床
                           WHEN    "099200015"
                               MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   TO  ORCHCN62-YOURYOU
                                                         (IDX1 IDX3 5)
                           END-EVALUATE
                       END-IF
      *!!
      *                用法コード判定
                       IF     (SPA-PARA-SRYCD (IDX-W IDX-W2)(1:3)
                                                   =   "001"  )
                       AND    (SPA-PARA-KENTAICOMMENT(IDX-W IDX-W2)
                                                   =   "0"  OR  "1" )
                           ADD     1                   TO  IDXY
                           IF      IDXY                <=  10
                               MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                       TO  WRK-YOHOU
                                                               (IDXY)
                               MOVE    SPA-PARA-YSETKBN (IDX-W IDX-W2)
                                                       TO  WRK-YOHOU-KBN
                                                               (IDXY)
                           END-IF
                       END-IF
                   END-PERFORM
      *            用法コード設定
                   PERFORM 451-ORCHC62-YOUHO-SEC
               END-IF
           END-PERFORM
      *
      *    コメント
           PERFORM VARYING     IDX-W   FROM    1   BY  1
                   UNTIL       IDX-W   >   SPA-PARA-MAX
               IF      SPA-PARA-SRYKBN (IDX-W) =   "98"
                   ADD     1               TO  IDX1
                   MOVE    IDX1            TO  ORCHCN62-ZAI-MAX
                   MOVE    SPA-PARA-ZAIKAIKEI (IDX-W)
                                           TO  ORCHCN62-ZAIKAIKEI (IDX1)
                   MOVE    SPA-PARA-SRYSYUKBN (IDX-W)
                                           TO  ORCHCN62-SRYSYUKBN (IDX1)
                   MOVE    SPA-PARA-SRYKBN (IDX-W)
                                           TO  ORCHCN62-SRYKBN (IDX1)
                   MOVE    SPA-PARA-ZAINUM (IDX-W)
                                           TO  ORCHCN62-ZAINUM (IDX1)
                   MOVE    ZERO            TO  IDX2
                   PERFORM VARYING     IDX-W2  FROM    1   BY  1
                           UNTIL      (IDX-W2  >   50      )  OR
                                      (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   SPACE)
      *            対象外
                       IF     (SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                                   =   ZERO )  OR
                              (SPA-PARA-SRYCD (IDX-W IDX-W2)
                                                   =   "099209902" )
                           CONTINUE
                       ELSE
                           ADD     1           TO  IDX2
                           MOVE    SPA-PARA-SRYCD (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYCD(IDX1 IDX2)
                           MOVE    SPA-PARA-SRYSURYO (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYSURYO
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-SRYKAISU (IDX-W IDX-W2)
                                           TO  ORCHCN62-SRYKAISU
                                                             (IDX1 IDX2)
                           MOVE    SPA-PARA-INPUTCOMENT (IDX-W IDX-W2)
                                           TO  ORCHCN62-INPUTCOMENT
                                                             (IDX1 IDX2)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
           IF      ORCHCN62-ZAI-MAX    =   ZERO
               MOVE    "0101"              TO  SPA-ERRCD
           END-IF
      *
           .
       41041-ORCHC62-HENSYU-EXT.
           EXIT.
      *****************************************************************
      *    用法コード設定処理
      *****************************************************************
       451-ORCHC62-YOUHO-SEC              SECTION.
      *
      *    用法は最後の用法コードとする
           MOVE    SPACE               TO  WRK-YSRYCD
           PERFORM VARYING     IDXY    FROM    10  BY  -1
                   UNTIL      (IDXY        <   1      )
               IF     (WRK-YOHOU(IDXY)     NOT =   SPACE )
               AND    (WRK-YOHOU-KBN(IDXY)     =   "1"   )
                   MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   MOVE    1                   TO  IDXY
               ELSE
                   IF     (WRK-YOHOU(IDXY) NOT =   SPACE )
                   AND    (WRK-YSRYCD          =   SPACE )
                       MOVE    WRK-YOHOU(IDXY)     TO  WRK-YSRYCD
                   END-IF
               END-IF
           END-PERFORM
           MOVE    WRK-YSRYCD          TO  ORCHCN62-YSRYCD (IDX1)
           .
       451-ORCHC62-YOUHO-EXT.
           EXIT.
      *!!!
      *****************************************************************
      *    院外処方せん印刷処理
      *****************************************************************
       451-SHOHOSEN-PRINT-SEC              SECTION.
      *
           INITIALIZE                  ORCSNYUINSHOAREA
           MOVE    "3"                 TO  ORCSNYUINSHO-KBN
           MOVE    SPA-NAI-NYUINYMD    TO  ORCSNYUINSHO-NYUINYMD
           MOVE    SPA-NAI-TAIINYMD    TO  ORCSNYUINSHO-TAIINYMD
           MOVE    SPA-NAI-RRK-SRYKA (SPA-GMN-SELNUM1)
                                       TO  ORCSNYUINSHO-SRYKA (1)
           IF      SPA-GMN-DRCD    NOT =   SPACE
               MOVE    "1"                 TO  ORCSNYUINSHO-DRCD
                                                           (1)(1:1)
               MOVE    SPA-GMN-DRCD        TO  ORCSNYUINSHO-DRCD
                                                           (1)(2:4)
           END-IF
           MOVE    1                   TO  ORCSNYUINSHO-SRYKA-MAX
      *
           MOVE    "02"                TO  SPA-PRT-FLG
      *
           CALL    "ORCSNYUINSHO"      USING
                                       SPA-AREA
                                       ORCSNYUINSHOAREA
           .
       451-SHOHOSEN-PRINT-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    患者個別設定取得編集処理
      *****************************************************************
       41041-PTCONF-HEN-SEC             SECTION.
      *
      *    ＱＲ出力設定
           MOVE    SPACE               TO  PTCONF-REC
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    "MNOTEQR"           TO  PTCONF-CKEY
           PERFORM 970-PTCONF-READ-SEC
           IF      FLG-PTCONF          =   ZERO
               MOVE    PTCONF-CDATA(1:1)   TO  WRK-QRKBN
           END-IF
      *    データフォーマット区分設定
           MOVE    SPACE               TO  PTCONF-REC
           INITIALIZE                  PTCONF-REC
           MOVE    SPA-HOSPNUM         TO  PTCONF-HOSPNUM
           MOVE    SPA-PTID            TO  PTCONF-PTID
           MOVE    "MNOTEVER"          TO  PTCONF-CKEY
           PERFORM 970-PTCONF-READ-SEC
           IF      FLG-PTCONF          =   ZERO
               MOVE    PTCONF-CDATA(1:1)   TO  WRK-CSVKBN
           END-IF
           .
       41041-PTCONF-HEN-EXT.
           EXIT.
      *!!
      *H28.5
      *****************************************************************
      *    名称切替え処理
      *****************************************************************
       415-GENERIC-CHG-SEC       SECTION.
      *
           IF      SPA-NAI-GENERICFLG  =   "1"
               MOVE    SPACE               TO  SPA-NAI-GENERICFLG
           ELSE
               MOVE    "1"                 TO  SPA-NAI-GENERICFLG
           END-IF
      *H28.5
      *    一般名記載
           IF     (SPA-NAI-INNAI       =   2       )
              AND (SPA-NAI-SRYYMD      >=  "20160401")
               PERFORM 300129-MEISYO-CHG-SEC
           END-IF
          .
       415-GENERIC-CHG-EXT.
           EXIT.
      *****************************************************************
      *    西暦和暦編集処理
      *****************************************************************
       800-SEIWA-HEN-SEC             SECTION.
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
       800-SEIWA-HEN-EXT.
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
           IF      SPA-KAID1CD       NOT =   SPACE
               PERFORM 520-KA02IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
           IF      SPA-KAID2CD       NOT =   SPACE
               PERFORM 530-KA02IDSET-SEC
               GO      TO      500-SET-SCREEN-EXT
           END-IF
      *
      *
           MOVE    "CURRENT"           TO  MCP-PUTTYPE.
           MOVE    "KA02"              TO  MCP-WINDOW.
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
           IF      FLG-GMN-INIT1       =   1
               MOVE    ZERO            TO  WRK-KA02-RRKLIST-ROW
               MOVE    ZERO            TO  WRK-KA02-MEILIST-ROW
           ELSE
               MOVE    KA02-RRKLIST-ROW    TO  WRK-KA02-RRKLIST-ROW
               IF      FLG-GMN-INIT2       =   1
                   MOVE    ZERO            TO  WRK-KA02-MEILIST-ROW
               ELSE
                   MOVE    KA02-MEILIST-ROW TO  WRK-KA02-MEILIST-ROW
               END-IF
           END-IF
      *
           MOVE    SPACE               TO  KA02
      *    コラムリストの位置指定
           INITIALIZE                      KA02
           MOVE    ZERO                TO  KA02-RRKLIST-ROW
      **** MOVE    ZERO                TO  KA02-RRKLIST-ROWATTR
           MOVE    ZERO                TO  KA02-MEILIST-ROW
     ***** MOVE    ZERO                TO  KA02-MEILIST-ROWATTR
           IF      WRK-KA02-RRKLIST-ROW NOT =   ZERO
               MOVE    WRK-KA02-RRKLIST-ROW TO  KA02-RRKLIST-ROW
           END-IF
           IF      WRK-KA02-MEILIST-ROW NOT =   ZERO
               MOVE    WRK-KA02-MEILIST-ROW TO  KA02-MEILIST-ROW
           END-IF
      *
           MOVE    SPA-GMN-PTNUM       TO  KA02-PTNUM
           MOVE    SPA-GMN-NAME        TO  KA02-NAME
           MOVE    SPA-GMN-SEX         TO  KA02-SEX
           MOVE    SPA-GMN-BIRTHDAY    TO  KA02-BIRTHDAY
           MOVE    SPA-GMN-NENREI      TO  KA02-NENREI
           MOVE    SPA-GMN-NYUINKIKAN  TO  KA02-NYUINKIKAN
      *
           IF      SPA-SEL-SYORI       =   ZERO
               MOVE   SPACE                TO  KA02-SYORIMEI
           ELSE
               MOVE   "＜ 選択印刷 ＞"     TO  KA02-SYORIMEI-VALUE
               MOVE   "red"                TO  KA02-SYORIMEI-STYLE
           END-IF
      *
           MOVE    SPA-GMN-SRYYMD      TO  KA02-SRYYMD
           MOVE    SPA-GMN-KOUFUYMD    TO  KA02-KOUFUYMD
           MOVE    SPA-GMN-SRYKAMEI    TO  KA02-SRYKA
      *
           IF      SPA-NAI-INNAI       =   1
               MOVE    "【院内処方】"      TO  KA02-INNAI-VALUE
               MOVE   "black"              TO  KA02-INNAI-STYLE
           ELSE
               MOVE    "【院外処方】"      TO  KA02-INNAI-VALUE
               MOVE   "blue"               TO  KA02-INNAI-STYLE
           END-IF
      *
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX     >   SPA-JYURRK-MAX
               MOVE    SPA-GMN-NUM  (IDX)  TO  KA02-TNUM   (IDX)
               MOVE    SPA-GMN-RRKYMD(IDX)
                                           TO  KA02-TRRKYMD (IDX)
               MOVE    SPA-GMN-RRKSRYKA(IDX)
                                           TO  KA02-TRRKSRYKA   (IDX)
               MOVE    SPA-GMN-RRKHKNCOMBI(IDX)
                                           TO  KA02-TRRKHKNCOMBI   (IDX)
               IF      SPA-GMN-NUM (IDX)   =   SPA-GMN-SELNUM1
                   MOVE    "T"             TO  KA02-RRKLIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  KA02-RRKLIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-JYURRK-MAX      TO  KA02-RRKLIST-COUNT
      *
           PERFORM VARYING   IDX     FROM    1   BY  1
                   UNTIL     IDX     >   SPA-MEISAI-MAX
               IF      SPA-GMN-TMEISYO(IDX)(1:1)   =   "-"
                   MOVE    ALL "-"             TO  KA02-MNUM  (IDX)
                   MOVE    ALL "-"             TO  KA02-MSYORI(IDX)
                   MOVE    ALL "-"             TO  KA02-MDEL  (IDX)
                   MOVE    ALL "−"            TO  KA02-MEISAI(IDX)
               ELSE
                   IF      SPA-GMN-TBANGO (IDX)    NOT =   ZERO
                       MOVE    SPA-GMN-TBANGO (IDX)    TO  WRK-ZZ
                       MOVE    WRK-ZZ              TO  KA02-MNUM (IDX)
                   ELSE
                       MOVE    SPACE               TO  KA02-MNUM (IDX)
                   END-IF
                   IF      SPA-GMN-TSEL   (IDX)    =   "T"
                       MOVE    "◎"            TO  KA02-MSYORI(IDX)
                   ELSE
                       MOVE    SPACE           TO  KA02-MSYORI(IDX)
                   END-IF
                   MOVE    SPA-GMN-TFLG   (IDX)
                                           TO  KA02-MDEL   (IDX)
      *            期間外
                   IF     (SPA-GMN-TSEL (IDX)  =   SPACE )  AND
                          (SPA-NAI-ERRFLG(IDX) =   1     )
                       MOVE    "外"            TO  KA02-MSYORI (IDX)
                   END-IF
                   MOVE    SPA-GMN-TMEISYO(IDX)
                                           TO  KA02-MEISAI (IDX)
               END-IF
               IF     (SPA-GMN-TMEISYO (IDX)   NOT =   ALL "-" )  AND
                      (SPA-GMN-TBANGO (IDX)    >   ZERO  )  AND
                      (SPA-GMN-TBANGO (IDX)    =   SPA-GMN-SELNUM2)
                   MOVE    "T"             TO  KA02-MEILIST-SELECT(IDX)
               ELSE
                   MOVE    "F"             TO  KA02-MEILIST-SELECT(IDX)
               END-IF
           END-PERFORM
           MOVE    SPA-MEISAI-MAX      TO  KA02-MEILIST-COUNT
      *
           MOVE    SPA-GMN-DRCD-G      TO  KA02-DRCD
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-DRCD-MAX
               MOVE    SPA-GMN-DRCD-COMB(IDX)  TO  KA02-DRCD-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-DRCD-MAX        TO  KA02-DRCD-COUNT
      *
           MOVE    SPA-GMN-HKNCOMBI-G  TO  KA02-HKNCOMBI
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   SPA-HKNCOMBI-MAX
               MOVE    SPA-GMN-HKNCOMBI-COMB(IDX)
                                           TO  KA02-HKNCOMBI-ITEM
                                                                  (IDX)
           END-PERFORM
           MOVE    SPA-HKNCOMBI-MAX    TO  KA02-HKNCOMBI-COUNT
      *
           MOVE    SPA-GMN-SELNUM1     TO  KA02-SELNUM1
           MOVE    SPA-GMN-SELNUM2     TO  KA02-SELNUM2
      *
           MOVE    SPA-GMN-NISUU       TO  KA02-NISUU
      *
           IF      SPA-INPUT-PAGE      >   ZERO
               COMPUTE IDX-STR         =  (SPA-INPUT-PAGE -  1)
                                       *  5    +   1
               PERFORM   VARYING   IDX     FROM    1   BY  1
                         UNTIL    (IDX     >   5   )   OR
                                  (IDX-STR >   50  )
                   MOVE    SPA-GMN-SURYO (IDX-STR)
                                           TO   KA02-SUURYO(IDX)
                   MOVE    SPA-GMN-KEISURYO (IDX-STR)
                                           TO   KA02-KEISUURYO(IDX)
                   EVALUATE    IDX
                       WHEN    1
                           MOVE    SPA-GMN-INPUT (IDX-STR)
                                                   TO  KA02-INPUT01
                       WHEN    2
                           MOVE    SPA-GMN-INPUT (IDX-STR)
                                                   TO  KA02-INPUT02
                       WHEN    3
                           MOVE    SPA-GMN-INPUT (IDX-STR)
                                                   TO  KA02-INPUT03
                       WHEN    4
                           MOVE    SPA-GMN-INPUT (IDX-STR)
                                                   TO  KA02-INPUT04
                       WHEN    5
                           MOVE    SPA-GMN-INPUT (IDX-STR)
                                                   TO  KA02-INPUT05
                   END-EVALUATE
                   ADD     1               TO  IDX-STR
               END-PERFORM
           END-IF
           PERFORM   VARYING   IDX     FROM    1   BY  1
                     UNTIL     IDX     >   31
               MOVE    SPA-GMN-DAY (IDX)   TO  WRK-ZZ9
               IF      WRK-ZZ9(1:1)        =   SPACE
                   MOVE    WRK-ZZ9(2:2)        TO  KA02-DAY (IDX)(1:2)
               ELSE
                   MOVE    WRK-ZZ9             TO  KA02-DAY (IDX)
               END-IF
           END-PERFORM
      *
           IF      SPA-NAI-INNAI       =   1
               MOVE    "院外処方"          TO  KA02-B04-LABEL
           ELSE
               MOVE    "院内処方"          TO  KA02-B04-LABEL
           END-IF
           IF      SPA-PARA-SRYKA      =   SPACE
               MOVE    SPACE               TO  KA02-B09S-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  KA02-B09S-STATE
           ELSE
               MOVE    "選択確認"          TO  KA02-B09S-LABEL
               MOVE    WIDGET-NORMAL       TO  KA02-B09S-STATE
           END-IF
      *H28.5
      *    一般名記載
           IF     (SPA-NAI-INNAI       =   2       )
              AND (SPA-NAI-SRYYMD      >=  "20160401")
               MOVE    "名称切替"          TO  KA02-B05S-LABEL
               MOVE    WIDGET-NORMAL       TO  KA02-B05S-STATE
           ELSE
               MOVE    SPACE               TO  KA02-B05S-LABEL
               MOVE    WIDGET-INSENSITIVE  TO  KA02-B05S-STATE
           END-IF
      *
      *    非活性処理
           PERFORM 510-GSRAUTH-SEC
      *
           PERFORM 5001-MAPCUR-SEC
           .
       500-GMNHEN-EXT.
           EXIT.
      *****************************************************************
      *    非活性処理
      *****************************************************************
       510-GSRAUTH-SEC             SECTION.
      *
           .
       51O-GSRAUTH-EXT.
           EXIT.
      *
      *****************************************************************
      *    画面カーソルセット処理
      *****************************************************************
       5001-MAPCUR-SEC             SECTION.
      *
           IF      SPA-GMN-CUR     =   ZERO
               PERFORM 50011-CUR-SET-SEC
           END-IF
      *
           EVALUATE    SPA-GMN-CUR
               WHEN    01
                   MOVE    "SELNUM1"        TO  MCP-WIDGET
               WHEN    02
                   MOVE    "SELNUM2"         TO  MCP-WIDGET
               WHEN    03
                   MOVE    "HKNCOMBI"        TO  MCP-WIDGET
               WHEN    04
                   MOVE    "DRCD"            TO  MCP-WIDGET
               WHEN    05
                   MOVE    "KOUFUYMD"        TO  MCP-WIDGET
               WHEN    10
                   MOVE    "NISUU"           TO  MCP-WIDGET
               WHEN    11
                   MOVE    "SUURYO1"         TO  MCP-WIDGET
               WHEN    12
                   MOVE    "SUURYO2"         TO  MCP-WIDGET
               WHEN    13
                   MOVE    "SUURYO3"         TO  MCP-WIDGET
               WHEN    14
                   MOVE    "SUURYO4"         TO  MCP-WIDGET
               WHEN    15
                   MOVE    "SUURYO5"         TO  MCP-WIDGET
               WHEN    20
                   MOVE    "B08"             TO  MCP-WIDGET
               WHEN    91
                   MOVE    "B01"             TO  MCP-WIDGET
               WHEN    98
                   MOVE    "B11"             TO  MCP-WIDGET
               WHEN    99
                   MOVE    "B12"             TO  MCP-WIDGET
           END-EVALUATE
      *
           .
       5001-MAPCUR-EXT.
           EXIT.
      *
      *****************************************************************
      *    カーソル編集処理
      *****************************************************************
       50011-CUR-SET-SEC           SECTION.
      *
           EVALUATE    WRK-MCP-WIDGET
               WHEN    "SELNUM1"
                   MOVE    2               TO  SPA-GMN-CUR
               WHEN    "SELNUM2"
                   MOVE    05              TO  SPA-GMN-CUR
               WHEN    "HKNCOMBI"
                   MOVE    4               TO  SPA-GMN-CUR
               WHEN    "DRCD"
                   IF      SPA-NAI-INNAI   =   1
                       MOVE    98              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    99              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "KOUFUYMD"
                   IF      SPA-NAI-INNAI   =   1
                       MOVE    98              TO  SPA-GMN-CUR
                   ELSE
                       MOVE    99              TO  SPA-GMN-CUR
                   END-IF
               WHEN    "NISUU"
                   MOVE    11              TO  SPA-GMN-CUR
               WHEN    OTHER
                   IF      WRK-MCP-WIDGET(1:6) =   "SUURYO"
                       MOVE    WRK-MCP-WIDGET(7:1) TO  IDX11
                       COMPUTE SPA-GMN-CUR     =   IDX11   +   10
                       IF     (IDX11               <   5 ) AND
                              (SPA-GMN-INPUT (IDX11 + 1)
                                                   NOT =   SPACE)
                           ADD     1               TO  SPA-GMN-CUR
                       ELSE
                           MOVE    20              TO  SPA-GMN-CUR
                       END-IF
                   END-IF
           END-EVALUATE
      *
           .
       50011-CUR-SET-EXT.
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
                   MOVE    "受診履歴番号入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0002"
                   MOVE    "明細番号選択エラ−"
                                       TO  SPA-ERRMSG
               WHEN    "0003"
                   MOVE    "保険組合せ入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0004"
                   MOVE    "ドクター入力エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0005"
                   MOVE    "数量数値エラー"
                                       TO  SPA-ERRMSG
               WHEN    "0006"
                   MOVE    "日数は１で入力して下さい。"
                                       TO  SPA-ERRMSG
               WHEN    "0007"
                   MOVE    "交付日が暦日エラーです。"
                                       TO  SPA-ERRMSG
               WHEN    "0008"
                   MOVE    "交付日が入院期間中でありません。"
                                       TO  SPA-ERRMSG
               WHEN    "0010"
                   STRING  "数量・日数の変更中です。"
                           "変更確定を押下して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0011"
                   STRING  "選択登録されている診療科・入院期間が"
                           "同じ受診履歴を選択して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0012"
                   STRING  "登録されている剤が１００剤以上になります。"
                           "これ以上登録できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0013"
                   STRING  "院外処方では包括分入力保険の選択は"
                           "できません。他の保険組合せを選択して下さい"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
      *H30.XX
               WHEN    "K014"
                   STRING  "警告！"
                           "分割調剤の合計日数と日数が違います。"
                           "分割調剤は反映しません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K002"
                   STRING  "警告！！"
                           "今回登録する剤番号に既に選択登録済みの"
                           "剤番号があります。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0100"
                   MOVE    "履歴番号が選択されていません。"
                                       TO  SPA-ERRMSG
               WHEN    "0101"
                   MOVE    "明細がありません。印刷しません。"
                                       TO  SPA-ERRMSG
               WHEN    "0102"
                   STRING  "お薬手帳・薬剤情報は"
                           "院内処方を表示して印刷して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0103"
                   STRING  "退院時院外処方せんは"
                           "院外処方を表示して印刷して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "0104"
                   STRING  "院外処方の登録がありません。"
                           "印刷できません。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1030"
                   STRING  "処方せんプログラムがありません。"
                           "システム管理を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "1031"
                   STRING  "薬剤情報印刷プログラムがありません。"
                           "システム管理を確認して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    "K001"
                   STRING  "警告！交付日で期間外のコードがあります。"
                           "空白で印刷されます。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-ERRCD   TO  SPA-ERRMSG
           END-EVALUATE
      *
           MOVE    SPACE               TO  KAERR
           MOVE    SPA-ERRCD           TO  KAERR-ERRCODE
           MOVE    SPA-ERRMSG          TO  KAERR-ERRMSG
           MOVE    "B01"               TO  MCP-WIDGET
      *
           MOVE    "KA02"              TO  SPA-MOTOPG
           MOVE    "KAERR"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KAERR"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW
      *
           MOVE    1                   TO  FLG-END
      *
           .
       510-ERRSET-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       520-KA02IDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KAID1
           INITIALIZE                      KAID1
           EVALUATE    SPA-KAID1CD
               WHEN    "0101"
                   STRING  "交付日で薬剤情報を印刷します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    "0106"
                   STRING  "交付日で退院時院外処方せんを印刷します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    "0105"
                   STRING  "選択登録がありません。"
                           "対象の科で退院時院外処方せんを印刷します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    "0107"
                   STRING  "交付日でお薬手帳を印刷します。"
                           "よろしいですか？"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID1-ID1MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KAID1CD
                                       TO  KAID1-ID1MSG
           END-EVALUATE
      *
           MOVE    SPA-KAID1CD         TO  KAID1-ID1CODE
      *
           MOVE    SPACE               TO  SPA-KAID1-FLG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "KA02"              TO  SPA-MOTOPG
           MOVE    "KAID1"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KAID1"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       520-KA02IDSET-EXT.
           EXIT.
      *
      *****************************************************************
      *    確認メッセージ表示理
      *****************************************************************
       530-KA02IDSET-SEC              SECTION.
      *
           MOVE    SPACE               TO  KAID2
           INITIALIZE                      KAID2
           EVALUATE    SPA-KAID2CD
               WHEN    "0101"
                   STRING  "診療日を交付日として再印刷します。"
                                           DELIMITED  BY  SIZE
                           "再印刷する帳票を選択して下さい。"
                                           DELIMITED  BY  SIZE
                                       INTO   KAID2-ID2MSG
                   END-STRING
               WHEN    OTHER
                   MOVE    SPA-KAID2CD
                                       TO  KAID2-ID2MSG
           END-EVALUATE
      *
           MOVE    SPA-KAID2CD         TO  KAID2-ID2CODE
      *
           MOVE    SPACE               TO  SPA-KAID2-FLG
      *
           MOVE    "B12"               TO  MCP-WIDGET
      *
           MOVE    "KA02"              TO  SPA-MOTOPG
           MOVE    "KAID2"             TO  SPA-SAKIPG
      *
           MOVE    "NEW"               TO  MCP-PUTTYPE
           MOVE    "KAID2"             TO  MCP-WINDOW
      *
           PERFORM 900-PUT-WINDOW.
      *
           MOVE    1                   TO  FLG-END
      *
           .
       530-KA02IDSET-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報検索処理
      *****************************************************************
       4101212-SRYACCTPLUS-READ-SEC       SECTION.
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
       4101212-SRYACCTPLUS-READ-EXT.
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
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC     TO  PTINF-REC
                   MOVE    ZERO            TO  FLG-PTINF
               ELSE
                   INITIALIZE                  PTINF-REC
                   MOVE    1               TO  FLG-PTINF
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
      *    システム管理読込処理
      *****************************************************************
       900-SYSKANRI-READ-SEC         SECTION.
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
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
      *
      *****************************************************************
      *    システム管理読込処理
      *****************************************************************
       910-SYSKANRI-KEYREAD-SEC   SECTION.
      *
           MOVE    SYSKANRI-REC        TO  MCPDATA-REC
      *
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 900-SYSKANRI-READ-SEC
               IF      FLG-SYSKANRI        =   ZERO
                   MOVE    MCPDATA-REC         TO  SYSKANRI-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       910-SYSKANRI-KEYREAD-EXT.
           EXIT.
      *
      *****************************************************************
      *    点数マスター読込
      *****************************************************************
       910-TENSU-READ-SEC         SECTION.
      *
           MOVE    TNS-YUKOSTYMD       TO  WRK-YUKOSTYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNS-HOSPNUM
           MOVE    TNS-TENSU-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensu"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
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
      *R02.2
      *    ユーザー単位編集
           IF     (TNS-SRYCD(1:1)      =   "6"
                                       OR  "7" )
               PERFORM 980-TENSUPLUS-READ-SEC
      *        ユーザー単位設定
               IF     (FLG-TENSUPLUS       =   ZERO  )
                  AND (TNSP-USERTANICD NOT =   SPACE )
                    MOVE    TNSP-USERTANICD    TO  TNS-TANICD
                    MOVE    TNSP-USERTANINAME  TO  TNS-TANINAME
               END-IF
           END-IF
      *
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       930-SRYACT-READ-SEC         SECTION.
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SRYACT-REC
               MOVE    ZERO            TO  FLG-SRYACT
           ELSE
               INITIALIZE                  SRYACT-REC
               MOVE    1               TO  FLG-SRYACT
           END-IF
      *
           .
       930-SRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療会計マスター読込（主キー）
      *****************************************************************
       940-SRYACCT-READ-SEC           SECTION.
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SRYACCT-REC
                   MOVE    ZERO                TO  FLG-SRYACCT
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
                   INITIALIZE                  SRYACCT-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
               INITIALIZE                  SRYACCT-REC
           END-IF
      *
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-SRYACCT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスタ読み込み
      *****************************************************************
       950-PTCOM-READ-SEC           SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCOM-REC
                   MOVE    ZERO                TO  FLG-PTCOM
               ELSE
                   MOVE    1                   TO  FLG-PTCOM
                   INITIALIZE                  PTCOM-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-PTCOM
               INITIALIZE                  PTCOM-REC
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴マスター読込
      *****************************************************************
       910-JYURRK-READ-SEC         SECTION.
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
       910-JYURRK-READ-EXT.
           EXIT.
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
      *
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
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
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       980-TENSUPLUS-READ-SEC          SECTION.
      *
           MOVE    SPACE               TO  TENSUPLUS-REC
           INITIALIZE                      TENSUPLUS-REC
           MOVE    TNS-SRYCD           TO  TNSP-SRYCD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOSTYMD
           MOVE    WRK-YUKOSTYMD       TO  TNSP-YUKOEDYMD
      *
           MOVE    SPA-HOSPNUM         TO  TNSP-HOSPNUM
           MOVE    TENSUPLUS-REC       TO  MCPDATA-REC
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  TENSUPLUS-REC
                   MOVE    ZERO                TO  FLG-TENSUPLUS
               ELSE
                   INITIALIZE                  TENSUPLUS-REC
                   MOVE    1                   TO  FLG-TENSUPLUS
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       980-TENSUPLUS-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者個別設定テーブル読込処理
      *****************************************************************
       970-PTCONF-READ-SEC         SECTION.
      *
           MOVE    PTCONF-REC          TO  MCPDATA-REC
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  PTCONF-REC
                   MOVE    ZERO                TO  FLG-PTCONF
               ELSE
                   INITIALIZE                  PTCONF-REC
                   MOVE    1                   TO  FLG-PTCONF
               END-IF
           ELSE
               INITIALIZE                  PTCONF-REC
               MOVE    1                   TO  FLG-PTCONF
           END-IF
      *
           MOVE    "tbl_ptconf"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       970-PTCONF-READ-EXT.
           EXIT.
      *
      *TEST
      *****************************************************************
      *    一時保存テーブル読み込み
      *****************************************************************
       990-SPATMP-READ-SEC        SECTION.
      *
           MOVE    "DBFETCH"       TO  MCP-FUNC
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
      *TEST
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
       990-DBCLOSE-SEC           SECTION.
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
           MOVE   "PUTWINDOW"          TO  MCP-FUNC.
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           .
       900-PUT-WINDOW-EXT.
           EXIT.
      *
