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
       PROGRAM-ID.                 ORCSCNYUIN.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 入院用マスタ更新処理(K08で使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  02/10/18    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  02/12/17  診療種別の判定追加
      *  01.00.02    NACL-多々納  03/04/01  翌月調剤料の算定修正
      *  01.00.03    NACL-多々納  03/05/12  剤チェックをサブへ
      *  01.00.04    NACL-多々納  04/11/11  調剤料算定判定の追加
      *  01.00.05    NACL-多々納  05/04/22  レセ電対応
      *  01.00.06    NACL-多々納  05/11/28  MONFUNC 対応
      *  01.00.07    NACL-多々納  06/03/06  主科設定追加
      *  03.03.00    NACL-多々納  06/10/27  削除時の受診履歴変更追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/XX  公務災害・公害対応
      *                                     器材商品名コード対応
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      *  04.03.00    NACL-多々納  08/08/01  入院調剤料保険期間チェック対応
      *  04.05.00    NACL-多々納  09/10/XX  換算値・関連コメント対応
      *  04.07.00    NACL-多々納  11/08/XX  算定履歴付加テーブル削除対応
      *  04.07.00    NACL-多々納  11/10/XX  同日再入院対応
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      *  04.08.00    NACL-多々納  13/07/XX  第三者行為対応
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *****************************************************************
      *
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
       DATA                    DIVISION.
       FILE                        SECTION.
      *
      *    パラメタデータ
      *FD  PARA-FILE.
      *01  PARA-REC.
      *    03  PARA-KEY.
      *        05  PARA-FILEMEI         PIC X(08).
      *        05  PARA-EDABAN          PIC 9(03).
      *    03  PARA-DATA-REC            PIC X(60000).
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
      *    フラグ領域
      *01  STS-AREA.
      *    03  STS-PARA            PIC X(02).
      *
      *    フラグ領域
       01  FLG-AREA.
           03  FLG-END             PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-HKNCOMBI        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-PTNYUINRRK        PIC 9(01).
      *
           03  FLG-SINKI           PIC 9(01).
           03  FLG-SYORI           PIC 9(01).
           03  FLG-OK              PIC 9(01).
           03  FLG-ARI             PIC 9(01).
      *
           03  FLG-ERR             PIC 9(01).
           03  FLG-TOKTEI          PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
           03  FLG-DAY             PIC 9(01).
      *
           03  FLG-CHOZAI          PIC 9(01).
      *
           03  FLG-TOUYAKU         PIC 9(01).
      *    内分泌負荷試験
           03  FLG-NAIBUN                PIC 9(01).
      *
           03  FLG-DEL             PIC 9(01).
           03  FLG-RENNUM          PIC 9(01).
           03  FLG-DOUJITU         PIC 9(01).
           03  FLG-OK2             PIC 9(01).
      *    処理有
           03  FLG-SYORI-ARI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
      *
           03  IDX-KAI1            PIC 9(04).
           03  IDX-KAI2            PIC 9(02).
           03  IDX-KAI3            PIC 9(04).
           03  IDX-KAI4            PIC 9(04).
           03  IDX-DAY             PIC 9(02).
           03  IDX-SIN             PIC 9(04).
           03  IDX-SIN2            PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-1               PIC 9(02).
           03  IDX-2               PIC 9(02).
           03  IDX-3               PIC 9(02).
      *
           03  IDX-K               PIC 9(02).
           03  IDX-D               PIC 9(02).
           03  IDX-A               PIC 9(04).
      *
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
      *
      *    一時領域
       01  WRK-AREA.
           03  WRK-SYMD.
               05  WRK-SYY         PIC 9(04).
               05  WRK-SMM         PIC 9(02).
               05  WRK-SDD         PIC 9(02).
           03  WRK-WYMD.
               05  WRK-WGO         PIC X(01).
               05  WRK-WYY         PIC 9(02).
               05  WRK-WMM         PIC 9(02).
               05  WRK-WDD         PIC 9(02).
      *
           03  ORCSCNYUIN-ERRMSG   PIC X(80).
      *    診療行為計算用
           03  WRK-TENSU           PIC 9(08).
           03  WRK-ZAITEN          PIC 9(08).
           03  WRK-SRYKAISUKEI     PIC 9(03).
           03  WRK-SRYSURYOKEI     PIC 9(07)V9(05).
           03  WRK-MEISAISU        PIC 9(07).
           03  WRK-SRYCD           PIC X(09).
           03  WRK-SRYCD9          PIC 9(09).
           03  WRK-SRYCDKEI        PIC 9(14).
           03  WRK-SRYYMD.
               05  WRK-SRYYY           PIC 9(04).
               05  WRK-SRYMM           PIC 9(02).
               05  WRK-SRYDD           PIC 9(02).
           03  WRK-LST-SRYYMD.
               05  WRK-LST-YYMM.
                   07  WRK-LST-YY      PIC 9(04).
                   07  WRK-LST-MM      PIC 9(02).
               05  WRK-LST-DD          PIC 9(02).
           03  WRK-STR-YMD.
               05  WRK-STR-YYMM.
                   07  WRK-STR-YY      PIC 9(04).
                   07  WRK-STR-MM      PIC 9(02).
               05  WRK-STR-DD          PIC 9(02).
      *
           03  WRK-ZAINUM          PIC 9(08).
           03  WRK-H-ZAINUM        PIC 9(08).
      *    診療会計用集計
           03  WRK-WKSRY-AREA.
               05  WRK-SYUTEN1             PIC  9(07).
               05  WRK-SYUTEN2             PIC  9(07).
               05  WRK-YKZTEN              PIC  9(07).
               05  WRK-KIZAITEN            PIC  9(07).
               05  WRK-JIHIMONEY           PIC  9(08).
      *    回数
           03  WRK-KAISU           PIC 9(08).
           03  WRK-SRYSURYO        PIC 9(07)V9(05).
      *
           03  WRK-JYURRK-RENNUM           PIC 9(01).
           03  WRK-JYURRK-EDANUM           PIC 9(01).
           03  WRK-JYURRK-DOUJI-RENNUM     PIC 9(01).
           03  WRK-KAIKEI-RENNUM           PIC  9(03).
      *
           03  WRK-PATH                    PIC X(64).
           03  WRK-MEIBAN                  PIC 9(03).
      *
      *    診療種別
           03  WRK-SRYSYUKBN              PIC X(03).
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
      *    末日
           03  WRK-MATU                    PIC 9(02).
           03  WRK-NEXT-SRYYMD             PIC X(08).
      *
      *    受診時間
           03  WRK-TIME1.
               05  WRK-TIME11          PIC 9(08).
           03  WRK-TIME2.
               05  WRK-TIME22          PIC 9(04).
      *    保険組合せ保存
           03  WRK-HZN-HKNCOMBINUM     PIC X(04).
           03  WRK-HZN-HKNKBN          PIC X(01).
      *
           03  WRK-SRYKA               PIC X(02).
      *
           03  WRK-RENNUM-KEY          PIC 9(01).
           03  WRK-RENNUM              PIC 9(01).
           03  WRK-DOUJI-RENNUM-KEY    PIC 9(01).
           03  WRK-CNT-DOUJI           PIC 9(01).
      *
           03  WRK-HKNERR              PIC X(01).
      *
      *    診療行為内容比較用テーブル
      *01  TBL-AREA.
      *    03  TBL-MAE-AREA.
      *        05  TBL-MAE-REC     OCCURS   50.
      *            07  TBL-MAE-SRYCD       PIC  X(09).
      *            07  TBL-MAE-SRYSURYO    PIC  9(05)V9(05).
      *            07  TBL-MAE-SRYKAISU    PIC  9(03).
      *            07  TBL-MAE-INPUTCOMENT PIC  X(40).
      *            07  TBL-MAE-ATAI-G.
      *                09  TBL-MAE-ATAI    PIC  X(08)  OCCURS  3.
      *    03  TBL-ATO-AREA.
      *        05  TBL-ATO-REC     OCCURS   50.
      *            07  TBL-ATO-SRYCD       PIC  X(09).
      *            07  TBL-ATO-SRYSURYO    PIC  9(05)V9(05).
      *            07  TBL-ATO-SRYKAISU    PIC  9(03).
      *            07  TBL-ATO-INPUTCOMENT PIC  X(40).
      *            07  TBL-ATO-ATAI-G.
      *                09  TBL-ATO-ATAI    PIC  X(08)  OCCURS  3.
      *
      *    剤テーブル領域
       01  WRK-TBL-ZAINUM-AREA.
           03  WRK-TBL-ZAINUM-G            OCCURS   150.
               05  WRK-TBL-ZAINUM          PIC 9(08).
           03  WRK-TBL-IDX                 PIC 9(04).
           03  FLG-ZAINUM                  PIC 9(01).
      *
       01  TBL-KAISUU-AREA.
           03  TBL-KAISUU-OCCU         OCCURS   10.
               05  TBL-KAISU           PIC 9(03).
               05  TBL-DAY             PIC 9(02)
                                       OCCURS   31.
      *翌月用テーブル
       01  TBL2-KAISUU-AREA.
           03  TBL2-KAISUU-OCCU        OCCURS   10.
               05  TBL2-KAISU          PIC 9(03).
               05  TBL2-DAY            PIC 9(02)
                                       OCCURS   31.
      *当月用保存
       01  HZN-KAISUU-AREA.
           03  HZN-KAISUU-OCCU         OCCURS   10.
               05  HZN-KAISU           PIC 9(03).
               05  HZN-DAY             PIC 9(02)
                                       OCCURS   31.
      *
      *    入院取消診療科
       01  TBL-NYU-SRYKA-AREA.
           03  TBL-NYU-SRYKA           PIC X(02)   OCCURS   10.
      *
      *!!!!
      *    リハビリ開始日訂正前情報
       01  TBL-RIHADEL-AREA.
           03  TBL-RIHADEL-G               OCCURS   10.
               05  TBL-RIHA-SRYCD          PIC X(09).
               05  TBL-RIHA-SRYKA          PIC X(02).
               05  TBL-RIHA-HKNCOMBI       PIC X(04).
               05  TBL-RIHA-HKNKBN         PIC X(01).
           03  IDX-RIH                 PIC 9(02).
           03  IDX-R2                  PIC 9(02).
      *!!!!
      *
      *****************************************************************
      *    ファイルレイアウト
      *****************************************************************
      *
      *    システム管理マスタ
           COPY    "CPSYSKANRI.INC".
      *
           COPY    "CPSK0041.INC".
      *
           COPY    "CPSK1013.INC".
      *
      *    保険組み合わせ
       01  HKNCOMBI-REC.
           COPY    "CPHKNCOMBI.INC".
      *
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    診療行為マスター
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
      *    受診履歴
       01  JYURRK-REC.
           COPY    "CPJYURRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    患者入院履歴
       01  PTNYUINRRK-REC.
           COPY    "CPPTNYUINRRK.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   300.
           COPY    "CPWKSRYACT.INC"  REPLACING
                                     //WKSRY-// BY //LNK-WKSRY-//.
      *    附加情報を追加
           03  LNK-WKSRYACTPLUS-REC.
           COPY    "CPWKSRYACTPLUS.INC"  REPLACING
                                     //WKSRYP-// BY //LNK-WKSRYP-//.
       01  LNK-WKSRYACT-MAX           PIC 9(04).
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
      *    受診履歴
       01  TBL-JYURRK-AREA.
           02  TBL-JYURRK-REC        OCCURS   10.
           COPY    "CPJYURRK.INC"  REPLACING
                                     //JYURRK-// BY //TBL-JYURRK-//.
      *
      *v4.8
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
      *
      *    診療行為入力項目変換パラメタ
      *    COPY    "CPORCSC97.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    入院調剤料更新サブ（退院時）
           COPY    "CPORCSCHOZAI.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    ＳＰＡパラメタ（内分泌負荷試験包括点数チェック用のDAMMY)
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      **** COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *
      *    一括日変換パラメタ
           COPY    "CPORCSNDAYCHG.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *
      **** COPY    "ORCA-DBPATH".
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCNYUIN.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCNYUINAREA
           .
      *
      *****************************************************************
      *    主　　処理
      *****************************************************************
       000-PROC-SEC                SECTION.
      *
           INITIALIZE                  FLG-AREA
           INITIALIZE                  IDX-AREA
           INITIALIZE                  WRK-AREA
           INITIALIZE                  TBL-RIHADEL-AREA
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    SPACE               TO  ORCSCNYUIN-ERRMSG
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    ORCSCNYUIN-KBN
      *        診療行為作成
               WHEN    "1"
      *        診療行為削除（訂正削除）
               WHEN    "3"
                   PERFORM 100-SAKUSEI-SYORI-SEC
      *        退院処理（診療会計削除）
               WHEN    "2"
                   PERFORM 200-TAIIN-SYORI-SEC
           END-EVALUATE
      *
           IF     (ORCSCNYUIN-ERRMSG   NOT =   SPACE ) AND
                  (SPA-ERRCD           NOT =   SPACE )
               MOVE    ORCSCNYUIN-ERRMSG  TO  SPA-ERRMSG2
           END-IF
      *
      *    調剤料保険組合せ期間外作成
           IF     (SPA-ERRCD           =   SPACE )
               IF      WRK-HKNERR          =   "1"
                   STRING  "警告！！"
                           "保険組合せ有効期間外の入院調剤料が"
                           "あります。"
                           "会計照会で保険変更して下さい。"
                                       DELIMITED  BY  SIZE
                                       INTO  SPA-ERRMSG2
                   END-STRING
               END-IF
           END-IF
      *
           MOVE    SPA-K01KYOUTU       TO  SPA-WORK
      *
           .
       000-PROC-EXT.
      *
           EXIT    PROGRAM
           .
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       100-SAKUSEI-SYORI-SEC           SECTION.
      *
           MOVE    SPA-WORK            TO  SPA-K01KYOUTU
           MOVE    ZERO                TO  FLG-TOUYAKU
           INITIALIZE                      TBL-NYU-SRYKA-AREA
      *
      *    剤番号テーブル
           INITIALIZE                  WRK-TBL-ZAINUM-AREA
      *
      *    訂正時、受診履歴等の削除
           IF      SPA-SYORIFLG         =   1
               PERFORM 300-TEISEI-SYORI-SEC
           END-IF
      *
      *    診療行為更新
           MOVE    ZERO                TO  FLG-DEL
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           INITIALIZE                  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
      *
      *    更新用ＤＢ読込み
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *
      *    IF      STS-PARA           =  ZERO
      *        PERFORM 980-PARA-READ-SEC
      *    ELSE
      *        MOVE    1               TO  FLG-PARA 
      *****END-IF
      *ver.4.8
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
      *
           PERFORM     UNTIL      (FLG-PARA    =    1 )  OR
                                  (SPA-ERRCD   NOT =   SPACE )
      *!!!
      *        対象有
                MOVE    1                   TO  FLG-SYORI-ARI
      *!!!
               MOVE    PARA-DATA-REC       TO  WRK-WKSRYACT-REC
               MOVE    1                   TO  FLG-DEL
      *
               IF     (IDX-KOUI        >   ZERO  )  AND
                      (WRK-WKSRY-ZAINUM     NOT =
                                           LNK-WKSRY-ZAINUM (IDX-KOUI))
      *            剤毎に処理を行う
                   PERFORM 100-SRYACT-KOU-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 200-JYURRK-SYORI-SEC
                   END-IF
                   MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                   MOVE    ZERO            TO  IDX-KOUI
               END-IF
               ADD     1               TO  IDX-KOUI
               MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                            (IDX-KOUI)
      *
               MOVE    "tbl_para"          TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-PARA-READ-SEC
           END-PERFORM
           MOVE    "tbl_para"          TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      SPA-ERRCD           =   SPACE
      *    剤毎に処理を行う
              IF      IDX-KOUI        >   ZERO
                   PERFORM 100-SRYACT-KOU-SEC
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 200-JYURRK-SYORI-SEC
                   END-IF
               END-IF
           END-IF
      *
      *!!!!
           IF     (SPA-SYORIFLG        =   1  )
             AND  (SPA-ERRCD           =   SPACE )
      *        リハビリ開始日の履歴付加削除
               PERFORM 45005-SANTEIPLUS-DEL-SEC
           END-IF
      *!!!!
      *
      *    削除の時、受診履歴の連番更新
           IF      SPA-ERRCD           =   SPACE
               IF     (FLG-DEL             =   ZERO )
                 OR  ((SPA-OLD-SRYKA   NOT =   SPACE  )  AND
                      (SPA-SRYKA       NOT =   SPA-OLD-SRYKA))
                   PERFORM 4500115-TEISEI-RENNUM-SEC
               END-IF
           END-IF
      *!!!
      *    明細の登録なしはＤＢ異常終了とする
           IF    ((ORCSCNYUIN-KBN      =   "1"  )  AND
                  (FLG-SYORI-ARI       =   ZERO ))
             OR   (SPA-ERRCD       NOT =   SPACE)
                   MOVE    SPACE               TO  MCPDATA-REC
                   MOVE    "DBSELECT"          TO  MCP-FUNC
                   MOVE    "tbl_syskanri"      TO  MCP-TABLE
                   MOVE    "abort"             TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      SPA-ERRCD           =   SPACE
                       MOVE    "0051"              TO  SPA-ERRCD
                   END-IF
           END-IF
      *!!!
      *
      *    包括まとめの時、以下対象外
           IF     (SPA-ERRCD           =   SPACE ) AND
                  (SPA-HKNCOMBINUM     =   "9999")
      *        ワーク診療行為マスタ−が存在した時、削除する
               PERFORM 400-WKSRYACT-DEL-SEC
           END-IF
      *
           IF     (SPA-ERRCD       NOT =   SPACE ) OR 
                  (SPA-HKNCOMBINUM     =   "9999")
               CONTINUE
           ELSE
               PERFORM 1001-SAKUSEI-SYORI2-SEC
           END-IF
      *    主科設定
           IF     (SPA-ERRCD       NOT =   SPACE ) OR 
                  (SPA-HKNCOMBINUM     =   "9999")
               CONTINUE
           ELSE
               PERFORM 1009-ORCSSYUKAMAIN-SEC
           END-IF
      *
           .
       100-SAKUSEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       1009-ORCSSYUKAMAIN-SEC             SECTION.
      *
           INITIALIZE                      ORCSSYUACCAREA
           MOVE    "03"                TO  LNK-SYUACC-I-KBN
      *    診療年月
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-SYUACC-I-SRYYM
           CALL    "ORCSSYUKAMAIN"     USING
                                       SPA-AREA
                                       ORCSSYUACCAREA
           .
      *
       1009-ORCSSYUKAMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為作成処理
      *****************************************************************
       1001-SAKUSEI-SYORI2-SEC         SECTION.
      *
      *    当月の調剤料を算定しなおす
           IF     FLG-TOUYAKU          =   1
               INITIALIZE                  ORCSCHOZAIAREA
               MOVE    "1"                 TO  ORCSCHO-KBN
               MOVE    SPA-PTID            TO  ORCSCHO-PTID
               MOVE    SPA-SRYYMD(1:6)     TO  ORCSCHO-SRYYM
               CALL    "ORCSCHOZAI"        USING
                                           SPA-AREA
                                           ORCSCHOZAIAREA
               IF      ORCSCHO-HKNERR      NOT =   ZERO
                   MOVE    ORCSCHO-HKNERR          TO  WRK-HKNERR
               END-IF
      *
      *        来月の調剤料を算定しなおす
               PERFORM 600-NEXT-CHOZAI-SEC
           END-IF
      *
      *    診療科履歴変更
           IF      SPA-ERRCD           =   SPACE
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
               IF      ORCSCNYUIN-TAIINYMD =   SPACE OR  ALL "9"
                   CONTINUE
               ELSE
                   MOVE    ORCSCNYUIN-TAIINYMD TO  SKARRK-LASTYMD
               END-IF
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "ORCSKARRK ERR "
                           SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
               END-IF
               IF     (SPA-SYORIFLG        =   1  )  AND
                      (SPA-OLD-SRYKA   NOT =   SPACE )  AND
                      (SPA-OLD-SRYKA   NOT =   SPA-SRYKA )
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
                   MOVE    SPA-OLD-SRYKA       TO  SKARRK-SRYKA
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
                   IF      SKARRK-RC       NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                       STRING  "ORCSKARRK ERR "
                               SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                               INTO  ORCSCNYUIN-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-IF
      *
      *****退院日がある時、最終受診日を編集する
      *    IF      ORCSCNYUIN-TAIINYMD =   SPACE OR  ALL "9"
      *        CONTINUE
      *    ELSE
      *        MOVE    SPA-SRYKA           TO  TBL-NYU-SRYKA (01)
      *        PERFORM 500-SRYKARRK-UPD-SEC
      *****END-IF
      *
      *    ワーク診療行為マスタ−が存在した時、削除する
           PERFORM 400-WKSRYACT-DEL-SEC
      *
      *    受付が有った時、受診済とする
           IF      SPA-SYORIFLG         =   ZERO
               PERFORM 410-UKETUKE-SYORI-SEC
           END-IF
      *
           IF     (SPA-ERRCD           =   SPACE )  AND
                  (FLG-NAIBUN          =   1     )
      *        内分泌負荷試験包括点数チェック
               INITIALIZE                  ORCSSANFUKAAREA
               INITIALIZE                  SPA-SCR-REC
               MOVE    3                   TO  LNK-SANFUKA-SYORI
               MOVE    "K02"               TO  LNK-SANFUKA-PG
               MOVE    SPA-SRYYMD          TO  LNK-SANFUKA-SRYYMD
               CALL    "ORCSSANFUKA"       USING
                                           ORCSSANFUKAAREA
                                           SPA-AREA
                                           SPA-SCR-REC
               IF      LNK-SANFUKA-ERRMSG  NOT =   SPACE
                   MOVE    LNK-SANFUKA-ERRMSG  TO  SPA-ERRMSG2
               END-IF
           END-IF
      *
           .
       1001-SAKUSEI-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    来月の調剤料算定処理
      *****************************************************************
       600-NEXT-CHOZAI-SEC            SECTION.
      *
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           COMPUTE WRK-SRYMM           =   WRK-SRYMM   +   1
           IF      WRK-SRYMM           >   12
               COMPUTE WRK-SRYYY           =   WRK-SRYYY   +   1
               MOVE    1                   TO  WRK-SRYMM
           END-IF
      *    保険が来月も対象であること
           MOVE    SPACE               TO  HKNCOMBI-REC
           INITIALIZE                  HKNCOMBI-REC
           MOVE    SPA-HOSPNUM         TO  COMB-HOSPNUM
           MOVE    SPA-PTID            TO  COMB-PTID
           MOVE    SPA-HKNCOMBINUM     TO  COMB-HKNCOMBINUM
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
      *    対象なし
           IF     (FLG-HKNCOMBI        =   1  )  OR
                  (COMB-TEKEDYMD(1:6)  <   WRK-SRYYMD (1:6))
               GO      TO      600-NEXT-CHOZAI-EXT
           END-IF
      *
           INITIALIZE                  ORCSCHOZAIAREA
           MOVE    "1"                 TO  ORCSCHO-KBN
           MOVE    SPA-PTID            TO  ORCSCHO-PTID
           MOVE    WRK-SRYYMD(1:6)     TO  ORCSCHO-SRYYM
           CALL    "ORCSCHOZAI"        USING
                                       SPA-AREA
                                       ORCSCHOZAIAREA
           IF      ORCSCHO-HKNERR      NOT =   ZERO
               MOVE    ORCSCHO-HKNERR          TO  WRK-HKNERR
           END-IF
      *
           .
       600-NEXT-CHOZAI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター更新処理
      *****************************************************************
       100-SRYACT-KOU-SEC            SECTION.
      *
      *    回数・日付決定
           INITIALIZE                      TBL-KAISUU-AREA
           INITIALIZE                      TBL2-KAISUU-AREA
           MOVE    ZERO                TO  IDX-KAI1
           MOVE    ZERO                TO  IDX-KAI2
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL     ( IDY     >   5   )  OR
                                 ((LNK-WKSRY-SRYCD  (IDX IDY) =  SPACE)
                            AND   (LNK-WKSRY-INPUTCD(IDX IDY) =  SPACE))
                   IF      LNK-WKSRY-INPUTCD(IDX IDY)(1:1)
                                               =   "*"
                       PERFORM 1001-KAISUU-SYORI-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *    ’＊’の行がなかった時、１回の診療日とする
           IF      TBL-KAISU (01)          =   ZERO
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI (IDX-KOUI)
                                           TO  TBL-KAISU (01)
               IF      TBL-KAISU (01)      =   ZERO
                   MOVE    1               TO  TBL-KAISU (01)
               END-IF
               MOVE    SPA-SRYYMD(7:2)     TO  IDX-KAI2
               MOVE    1                   TO  TBL-DAY   (01 IDX-KAI2)
           END-IF
      *!!!
      *    同日再入院日判定
           IF     (SPA-SRYYMD(1:6)     =   SPA-K02N-NYUINYMD (1:6))
             AND  (SPA-K02N-DOUDAY     >   ZERO           )
               MOVE    SPA-K02N-DOUDAY     TO  IDX-1
           ELSE
               MOVE    ZERO                TO  IDX-1
           END-IF
      *!!!
      *    算定日が重複した時、前の回数とする
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL       IDX-KAI1    >   10
               PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                       UNTIL       IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2) >   ZERO
                       COMPUTE IDX         =   IDX-KAI1    +  1
                       PERFORM VARYING     IDY    FROM   IDX  BY  1 
                                UNTIL      IDY    >   10
                           MOVE    ZERO           TO  TBL-DAY
                                                      (IDY IDX-KAI2)
                       END-PERFORM
                   END-IF
      *!!!
                   IF     (TBL-DAY (IDX-KAI1 IDX-KAI2) >   ZERO )
                     AND  (IDX-1               =   IDX-KAI2  )
                       MOVE    2               TO  TBL-DAY
                                                  (IDX-KAI1 IDX-KAI2) 
                   END-IF
      *!!!
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-SINKI
      *    診療会計マスター処理
           PERFORM 1002-SRYACCT-SYORI-SEC
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               GO      TO      100-SRYACT-KOU-EXT
           END-IF
      *
      *    診療行為マスター処理 
           IF     (FLG-SINKI           =   1     )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 4501-SRYACT-SYORI-SEC
           END-IF
      *!!
      *    診療会計附加情報処理 
           IF     (FLG-SINKI           =   1 )  AND
                  (LNK-WKSRYP-ADDKBN(01)   =   "1"  )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 4503-SRYACCTPLUS-SYORI-SEC
           END-IF
      *!!
      *
      *    算定履歴更新処理
           IF     (SPA-ERRCD           =   SPACE )
               PERFORM 4502-SANTEI-SYORI-SEC
           END-IF
      *
      *    翌月がある時、翌月分を作成する
           IF     (TBL2-KAISUU-AREA    NOT =   ZERO )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 1000-NEXT-SRYACT-KOU-SEC
           END-IF
           .
       100-SRYACT-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *     回数決定処理
      *****************************************************************
       1001-KAISUU-SYORI-SEC              SECTION.
      *
      *
      *    INITIALIZE                  ORCSC97AREA
      *    MOVE    SPACE               TO  SPA-SCR-REC
      *    ＰＧ名
      *    MOVE    "K02N"              TO  LNK-SC97-PG
      *    MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
      *                                TO  LNK-SC97-INPUTCD
      *    CALL    "ORCSC97"           USING
      *                                SPA-AREA
      *                                ORCSC97AREA
      *                                SPA-SCR-REC
      *
           INITIALIZE                      ORCSNDAYCHGAREA
           MOVE    SPA-SRYYMD          TO  ORCSNDAYCHG-SRYYMD
           MOVE    LNK-WKSRY-INPUTCD(IDX IDY)
                                       TO  ORCSNDAYCHG-INPUTCD
           MOVE    54                  TO  ORCSNDAYCHG-KETA
           CALL    "ORCSNDAYCHG"       USING
                                       SPA-AREA
                                       ORCSNDAYCHGAREA
           MOVE    ORCSNDAYCHG-ERRCD   TO  SPA-ERRCD
      *
           IF      SPA-ERRCD       NOT =   SPACE
               GO      TO      1001-KAISUU-SYORI-EXT
           END-IF
      *
      *    自動発生で、回数が１以上の時、入院調剤料とする
           IF     (LNK-WKSRY-AUTOKBN (01 01)   =   "3" ) AND
                  (LNK-WKSRY-SRYKBN  (01)      =   "24" OR "26")
               MOVE    1                   TO  FLG-CHOZAI
               PERFORM 10011-NISUU-SYORI-SEC
               GO      TO      1001-KAISUU-SYORI-EXT
           END-IF
      *
      *    回数
           IF      ORCSNDAYCHG-KAISU   =   ZERO
               MOVE    1               TO  ORCSNDAYCHG-KAISU
           END-IF
      *    外用薬の時、回数は１回とする
           IF      LNK-WKSRY-SRYKBN (01)   =   "23"
               MOVE    1               TO  ORCSNDAYCHG-KAISU
           END-IF
           MOVE    ZERO                TO  IDX-KAI1
           MOVE    ZERO                TO  IDX-KAI2
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL      (IDX2        >   10   )  OR
                              (IDX-KAI1    >   ZERO )
               IF      TBL-KAISU (IDX2)    =   ZERO
                   MOVE    IDX2                TO  IDX-KAI1
                   MOVE    ORCSNDAYCHG-KAISU   TO  TBL-KAISU (IDX-KAI1)
               ELSE
                   IF      ORCSNDAYCHG-KAISU   =   TBL-KAISU (IDX2)
                       MOVE    IDX2            TO  IDX-KAI1
                   END-IF
               END-IF
           END-PERFORM
      *
      *    算定日
           MOVE    ZERO                TO  FLG-DAY
           PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                   UNTIL       IDX-KAI2    >   31
               IF      ORCSNDAYCHG-NYU-DAY (IDX-KAI2) NOT =   ZERO
                   MOVE    1               TO  TBL-DAY
                                                   (IDX-KAI1 IDX-KAI2)
                   MOVE    1               TO  FLG-DAY
               END-IF
           END-PERFORM
           IF      FLG-DAY             =   ZERO
               MOVE    SPA-SRYYMD(7:2) TO  IDX-2
               MOVE    1               TO  TBL-DAY(IDX-KAI1 IDX-2)
           END-IF
           .
       1001-KAISUU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     日数分作成処理
      *****************************************************************
       10011-NISUU-SYORI-SEC              SECTION.
      *
      *    診療年月の末日算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY7-AREA
           MOVE    "71"                TO  LNK-DAY7-IRAI
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-DAY7-YM
           CALL    "ORCSDAY"           USING
                                           STS-AREA-DAY
                                           LNK-DAY7-AREA
           MOVE    LNK-DAY7-KOYOMI(7:2)    TO  WRK-MATU
      *
      *    算定開始日を設定する
           IF      ORCSNDAYCHG-NYUIN-KAISU =   ZERO
               MOVE    SPA-SRYYMD(7:2) TO  IDX-2
               MOVE    1               TO  ORCSNDAYCHG-NYU-DAY (IDX-2)
           END-IF
      *    １日１回とする
           MOVE    1                   TO  IDX-KAI1
           MOVE    1                   TO  TBL-KAISU (IDX-KAI1)
      *    回数
           IF      ORCSNDAYCHG-KAISU   =   ZERO
               MOVE    1               TO  ORCSNDAYCHG-KAISU
           END-IF
      *
      *    算定日
           PERFORM VARYING     IDX-2       FROM    1   BY  1
                   UNTIL       IDX-2       >   31
               IF      ORCSNDAYCHG-NYU-DAY (IDX-2) NOT =   ZERO
                   PERFORM VARYING  IDX-KAI3    FROM    1   BY  1
                           UNTIL    IDX-KAI3    >   ORCSNDAYCHG-KAISU
                       COMPUTE IDX-KAI4    =   IDX-2
                                           +   IDX-KAI3
                                           -   1
                       IF      IDX-KAI4    >   WRK-MATU
      *                    翌月へ
                           COMPUTE IDX-KAI4    =   IDX-KAI4
                                               -   WRK-MATU
                           IF      IDX-KAI4    >   31
                               MOVE    31          TO  IDX-KAI4
                           END-IF
                           MOVE    1           TO  TBL2-DAY
                                                   (IDX-KAI1 IDX-KAI4)
                           MOVE    1           TO  TBL2-KAISU
                                                   (IDX-KAI1)
                       ELSE
                           MOVE    1           TO  TBL-DAY
                                                   (IDX-KAI1 IDX-KAI4)
                       END-IF
                   END-PERFORM
               END-IF
           END-PERFORM
      *
      *    既に他科で算定済みの時、作成しない
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    SPA-SRYYMD(1:6)     TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)
                                       TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL        FLG-SRYACCT         =   1
               PERFORM VARYING     IDX-2       FROM    1   BY  1
                       UNTIL       IDX-2       >   31
                   IF      ACCT-DAY (1 IDX-2)  >   ZERO
                       MOVE    ZERO            TO  TBL-DAY (1 IDX-2)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    次月がある時、次月の判定
           IF      TBL2-KAISUU-AREA    NOT =   ZERO
               GO      TO      10011-KAISUU-SYORI-EXT
           END-IF
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           COMPUTE WRK-SMM             =   WRK-SMM   +   1
           IF      WRK-SMM             >   12
               ADD     1               TO  WRK-SYY
               MOVE    1               TO  WRK-SMM
           END-IF
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    SPA-HOSPNUM         TO  ACCT-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
           MOVE    SPA-PTID            TO  ACCT-PTID
           MOVE    WRK-SYMD (1:6)      TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)
                                       TO  ACCT-SRYKBN
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           PERFORM
               UNTIL        FLG-SRYACCT         =   1
               PERFORM VARYING     IDX-2       FROM    1   BY  1
                       UNTIL       IDX-2       >   31
                   IF      ACCT-DAY (1 IDX-2)  >   ZERO
                       MOVE    ZERO            TO  TBL2-DAY (1 IDX-2)
                   END-IF
               END-PERFORM
      *
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           END-PERFORM
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       10011-KAISUU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスター処理
      *****************************************************************
       1002-SRYACCT-SYORI-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-SRYSURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-SRYCDKEI
           INITIALIZE                      WRK-WKSRY-AREA
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        手技料１
               MOVE    LNK-WKSRY-SYUTEN1  (IDX)    TO  WRK-SYUTEN1
      *        手技料２
               MOVE    LNK-WKSRY-SYUTEN2  (IDX)    TO  WRK-SYUTEN2
      *        薬剤
               MOVE    LNK-WKSRY-YKZTEN  (IDX)     TO  WRK-YKZTEN
      *        器材
               MOVE    LNK-WKSRY-KIZAITEN (IDX)    TO  WRK-KIZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      *        自費
               IF      LNK-WKSRY-JIHIMONEYTOTAL (IDX)  NOT =   ZERO
                   MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX) 
                                                   TO  WRK-JIHIMONEY
               END-IF
      *        投薬判定
               IF    (LNK-WKSRY-SRYKBN (IDX)       =   "21"  OR
                                                       "22"  OR
                                                       "23"  )  AND
                     (LNK-WKSRY-SRYSYUKBN (IDX)(3:1)
                                               NOT =   "4"   )  AND
                     (LNK-WKSRY-ZAITENKEI (IDX)    >   ZERO  )
                   MOVE    1                   TO  FLG-TOUYAKU
               END-IF
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
                   IF      LNK-WKSRY-SRYCD (IDX IDY)     NUMERIC
                       MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                               TO  WRK-SRYCD9
                       ADD     WRK-SRYCD9      TO  WRK-SRYCDKEI
                       ADD     LNK-WKSRY-SRYSURYO (IDX IDY)
                                               TO  WRK-SRYSURYOKEI
                       ADD     1               TO  WRK-MEISAISU
                   ELSE
                       IF      LNK-WKSRY-SRYCD(IDX IDY)(1:1) =  "S"
                           ADD     1               TO  WRK-MEISAISU
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *    剤点数がゼロの時、自費を点数へ
      ***  IF      WRK-ZAITEN          =   ZERO
           IF    ( LNK-WKSRY-SRYKBN(01)    =   "95"
                                           OR  "96" )
               MOVE   WRK-JIHIMONEY        TO  WRK-ZAITEN
           END-IF
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO
               GO      TO      1002-SRYACCT-SYORI-EXT
           END-IF 
      *
      *    診療会計剤チェックサブ
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    LNK-WKSRY-SRYKA(01)     TO  ORCSCZAICHK-SRYKA
           MOVE    SPA-SRYYMD(1:6)         TO  ORCSCZAICHK-SRYYM
           MOVE    LNK-WKSRY-SRYKBN(01)    TO  ORCSCZAICHK-SRYKBN
           MOVE    LNK-WKSRY-SRYSYUKBN(01) TO  ORCSCZAICHK-SRYSYUKBN
           MOVE    SPA-HKNCOMBINUM         TO  ORCSCZAICHK-HKNCOMBI
           MOVE    WRK-SRYCDKEI            TO  ORCSCZAICHK-SRYCDTOTAL
           MOVE    WRK-ZAITEN              TO  ORCSCZAICHK-ZAITEN
           MOVE    WRK-MEISAISU            TO  ORCSCZAICHK-MEISAISU
           MOVE    WRK-SRYSURYOKEI         TO  ORCSCZAICHK-SURYOUTOTAL
           MOVE    WRK-SRYKAISUKEI         TO  ORCSCZAICHK-ZAIKAISU
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (1)   =   "H"
               MOVE    "1"                 TO  ORCSCZAICHK-JIHOKBN
           END-IF
           MOVE    ZERO                    TO  IDX-ATO
           PERFORM VARYING     IDX1    FROM    1   BY  1
                   UNTIL       IDX1        >   IDX-KOUI
               PERFORM VARYING     IDX2    FROM    1   BY  1
                       UNTIL      (IDX2        >   5          )  OR
                                  (IDX-ATO     >=  50         )  OR
                                  (LNK-WKSRY-SRYCD(IDX1 IDX2) =  SPACE)
                   ADD     1                   TO  IDX-ATO
                   MOVE    LNK-WKSRY-SRYCD (IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYCD
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYSURYO
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-SRYKAISU(IDX1 IDX2)
                                               TO  ORCSCZAICHK-SRYKAISU
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-INPUTCOMENT(IDX1 IDX2)
                                           TO  ORCSCZAICHK-INPUTCOMENT
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-INPUTCHI-G(IDX1 IDX2)
                                           TO  ORCSCZAICHK-INPUTCHI-G
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-KANSURYO(IDX1 IDX2)
                                               TO  ORCSCZAICHK-KANSURYO
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-INPUTKBN(IDX1 IDX2)
                                               TO  ORCSCZAICHK-INPUTKBN
                                                             (IDX-ATO)
                   MOVE    LNK-WKSRY-JIHIMONEY(IDX1 IDX2)
                                               TO  ORCSCZAICHK-JIHIMONEY
                                                             (IDX-ATO)
               END-PERFORM
           END-PERFORM
      *
           CALL    "ORCSCZAICHK"       USING   SPA-AREA
                                               ORCSCZAICHKAREA
           MOVE    ORCSCZAICHK-ZAINUM  TO  WRK-ZAINUM
      *
           IF      WRK-ZAINUM          =   ZERO
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      1002-SRYACCT-SYORI-EXT
               END-IF
           END-IF
      *    診療会計マスター更新用読み込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-WKSRY-HOSPNUM (01)  TO  ACCT-HOSPNUM
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCT-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCT-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCT-SRYKA
      *****MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
           MOVE    SPA-SRYYMD(1:6)         TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)   TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM              TO  ACCT-ZAINUM
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 930-SRYACCT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCT
           END-IF
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SRYACCT         =   1
      *        診療会計マスター新規編集処理
               PERFORM 45015-SRYACCT-SRY-SEC
      *        診療会計マスター編集処理
               PERFORM 45014-SRYACCT-HEN-SEC
      *        更新日設定
               MOVE    SMCNDATE-YMD        TO  ACCT-CREYMD
               MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
               MOVE    SPA-OPID            TO  ACCT-OPID
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "SYRACCT INS ERR:" MCP-RC
                            ",KEY:" ACCT-KEY
                   STRING  "SYRACCT INS ERR KEY : "
                           ACCT-KEY      DELIMITED  BY  SIZE
                                   INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
      *        新規のときのみ、診療行為マスターを追加する
               MOVE    1                   TO  FLG-SINKI
           ELSE
      *        診療会計マスター編集処理
               PERFORM 45014-SRYACCT-HEN-SEC
      *
      *        更新日設定
               MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
               MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
               MOVE    SPA-OPID            TO  ACCT-OPID
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "SYRACCT UPDTE ERR:" MCP-RC
                            ",KEY:" ACCT-KEY
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "SYRACCT UPD ERR KEY : "
                           ACCT-KEY      DELIMITED  BY  SIZE
                                   INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
               END-IF   
               MOVE    ZERO                TO  FLG-SINKI
           END-IF
      *
           .
      *
       1002-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *   剤番号取得処理
      *****************************************************************
       45013-ZAINUM-SET-SEC          SECTION.
      *
      *    患者マスターより、最終剤番号を再番し、患者マスターを更新する
           INITIALIZE                      PTINF-REC
           MOVE    SPA-HOSPNUM         TO  PTINF-HOSPNUM
           MOVE    SPA-PTID            TO  PTINF-PTID
           PERFORM 940-PTINF-READ-SEC
           IF      FLG-PTINF           =   1
               MOVE    "0002"          TO  SPA-ERRCD
               DISPLAY "K08 ZAINUM-SET ERR:" SPA-PTID
               STRING  "PTINF INV ERR SPA-PTID : "
                       PTINF-PTID      DELIMITED  BY  SIZE
                                   INTO  ORCSCNYUIN-ERRMSG
               END-STRING
               GO      TO      45013-ZAINUM-SET-EXT
           END-IF
      *
      *    最大剤番号
           IF      PTINF-MAXZAINUM     =   ALL "9"
               MOVE    ZERO                TO  PTINF-MAXZAINUM
           END-IF
           ADD     1                   TO  PTINF-MAXZAINUM
           MOVE    PTINF-MAXZAINUM     TO  WRK-ZAINUM
      *
           MOVE    SMCNDATE-YMD        TO  PTINF-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTINF-UPHMS
      *
           MOVE    SPA-OPID            TO  PTINF-OPID
      *
           MOVE    PTINF-REC           TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "PTINF UPDTE ERR:"   MCP-RC
                            ",KEY:" PTINF-KEY
               MOVE    "0005"              TO  SPA-ERRCD
               STRING  "PTINF UPD ERR KEY : "
                       PTINF-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCNYUIN-ERRMSG
               END-STRING
           END-IF 
      *
           .
      *
       45013-ZAINUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター新規編集処理
      *****************************************************************
       45015-SRYACCT-SRY-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCT-REC
           INITIALIZE                      SRYACCT-REC
           MOVE    LNK-WKSRY-HOSPNUM (01)  TO  ACCT-HOSPNUM
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCT-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCT-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCT-SRYKA
      *****MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
           MOVE    SPA-SRYYMD (1:6)        TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)   TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    SPA-HKNCOMBINUM     TO  ACCT-HKNCOMBI
      *    剤点数
           MOVE    WRK-ZAITEN          TO  ACCT-ZAITEN
      *    診療コード計
           MOVE    WRK-SRYCDKEI        TO  ACCT-SRYCDTOTAL
      *    数量計
           MOVE    WRK-SRYSURYOKEI     TO  ACCT-SURYOUTOTAL
      *    明細数
           MOVE    WRK-MEISAISU        TO  ACCT-MEISAISU
      *    手技点数１
           MOVE    WRK-SYUTEN1         TO  ACCT-SYUTEN1
      *    手技点数２
           MOVE    WRK-SYUTEN2         TO  ACCT-SYUTEN2
      *    薬剤点数
           MOVE    WRK-YKZTEN          TO  ACCT-YKZTEN
      *    器材点数
           MOVE    WRK-KIZAITEN        TO  ACCT-KIZAITEN
      *!!
      *    附加情報あり
           IF      LNK-WKSRYP-ADDKBN(01)   =   "1"
      *        剤請求フラグ
               MOVE    "1"                 TO  ACCT-ZAIREQFLG
           END-IF
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (1)   =   "H"
               MOVE    "1"                 TO  ACCT-JIHOKBN
           ELSE
               MOVE    SPACE               TO  ACCT-JIHOKBN
           END-IF
      *    薬評・器評、減点分
           MOVE    LNK-WKSRY-ZAIKBN (1)    TO  ACCT-ZAIKBN
           .
      *
       45015-SRYACCT-SRY-EXT.
           EXIT.
      *
      *****************************************************************
      *   診療会計マスター編集処理
      *****************************************************************
       45014-SRYACCT-HEN-SEC          SECTION.
      *
      *    回数セット
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL       (IDX-KAI1   >   10  )   OR
                               (TBL-KAISU(IDX-KAI1)    =   ZERO )
               PERFORM VARYING      IDX-KAI2    FROM    1   BY  1
                       UNTIL        IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2)  NOT =   ZERO
      *                回数セット
                       ADD     TBL-KAISU(IDX-KAI1)
                                           TO  ACCT-DAY (1 IDX-KAI2)
      *!!!!
      *                同日再入院分
                       EVALUATE   TBL-DAY (IDX-KAI1 IDX-KAI2)
                       WHEN    2
                           MOVE    3           TO  IDX-1
                       WHEN    OTHER
                           MOVE    2           TO  IDX-1
                       END-EVALUATE
                       ADD     TBL-KAISU(IDX-KAI1)
                                           TO  ACCT-DAY (IDX-1 IDX-KAI2)
      *!!
      *                自動算定分は集計しない
                       IF      LNK-WKSRY-AUTOKBN(01 01)  =   "3"
                           MOVE    TBL-KAISU(IDX-KAI1)
                                           TO  ACCT-DAY (1 IDX-KAI2)
                           MOVE    TBL-KAISU(IDX-KAI1)
                                           TO  ACCT-DAY (IDX-1 IDX-KAI2)
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *    剤回数
           MOVE    ZERO                TO  ACCT-ZAIKAISU
           PERFORM VARYING     IDX2    FROM    1   BY  1
                   UNTIL       IDX2    >   31
               MOVE    ZERO                TO  ACCT-DAY (1 IDX2)
      *****    COMPUTE ACCT-DAY (1 IDX2)   =   ACCT-DAY (2 IDX2)
      *                                    +   ACCT-DAY (3 IDX2)
      ****                                 +   ACCT-DAY (4 IDX2)
      *ver.4.8
               PERFORM VARYING     IDX-A   FROM    2   BY  1
                       UNTIL       IDX-A   >   10
                   COMPUTE ACCT-DAY (1 IDX2)   =   ACCT-DAY (1 IDX2)
                                               +   ACCT-DAY (IDX-A IDX2)
               END-PERFORM
               ADD     ACCT-DAY (1 IDX2)   TO  ACCT-ZAIKAISU
           END-PERFORM
      *
           .
      *
       45014-SRYACCT-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療行為マスター処理
      *****************************************************************
       4501-SRYACT-SYORI-SEC          SECTION.
      *
      *
           MOVE    ZERO                TO  WRK-MEIBAN
      *    新規分のみ、作成する
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
               INITIALIZE                          SRYACT-REC
               MOVE    LNK-WKSRY-HOSPNUM  (IDX) TO  SRY-HOSPNUM
               MOVE    LNK-WKSRY-NYUGAIKBN (IDX)   TO  SRY-NYUGAIKBN
               MOVE    LNK-WKSRY-PTID   (IDX)  TO  SRY-PTID
               MOVE    LNK-WKSRY-SRYKA  (IDX)  TO  SRY-SRYKA
      *********MOVE    LNK-WKSRY-SRYYMD (IDX)(1:6)
               MOVE    SPA-SRYYMD      (1:6)
                                               TO  SRY-SRYYM
      *        剤番号
               MOVE    WRK-ZAINUM              TO  SRY-ZAINUM
      *        連番号
               MOVE    IDX                     TO  SRY-RENNUM
               MOVE    LNK-WKSRY-SRYSYUKBN(IDX)
                                               TO  SRY-SRYSYUKBN
               MOVE    LNK-WKSRY-SRYKBN (IDX)  TO  SRY-SRYKBN
               MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX)
                                               TO  SRY-JIHIMONEYTOTAL
               MOVE    ZERO                    TO  IDX-SIN2
               PERFORM VARYING IDX-SIN     FROM    1   BY  1
                       UNTIL   IDX-SIN     >   5
               IF      LNK-WKSRY-SRYCD (IDX IDX-SIN)   =   SPACE
                   CONTINUE
               ELSE
                   ADD     1                   TO  IDX-SIN2
                   MOVE    LNK-WKSRY-SRYCD (IDX IDX-SIN)
                                               TO  SRY-SRYCD (IDX-SIN2)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDX-SIN)
                                               TO  SRY-SRYSURYO
                                                            (IDX-SIN2)
                   MOVE    LNK-WKSRY-SRYKAISU(IDX IDX-SIN)
                                               TO  SRY-SRYKAISU
                                                            (IDX-SIN2)
      *!!
      *            明細請求区分
                   IF      LNK-WKSRY-AUTOKBN(IDX IDX-SIN)  =   "T"
                       MOVE    "1"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN2)
                       MOVE    SPACE           TO  LNK-WKSRY-AUTOKBN
                                                         (IDX IDX-SIN)
                   END-IF
      *!!
                   MOVE    LNK-WKSRY-AUTOKBN(IDX IDX-SIN)
                                               TO  SRY-AUTOKBN
                                                            (IDX-SIN2)
                   MOVE    LNK-WKSRY-JIHIMONEY (IDX IDX-SIN)
                                               TO  SRY-JIHIMONEY
                                                            (IDX-SIN2)
      *            換算値数量
                   MOVE    LNK-WKSRY-KANSURYO(IDX IDX-SIN)
                                               TO  SRY-KANSURYO
                                                            (IDX-SIN2)
                   EVALUATE    LNK-WKSRY-INPUTKBN(IDX IDX-SIN)
      *            関連コメント指示
                       WHEN    "1"
                           MOVE    "2"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN2)
      *            多剤投与指示
                       WHEN    "2"
                           MOVE    "3"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN2)
                   END-EVALUATE
      *            患者コメント作成
                   IF     (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:1)  =  "8")
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:3)  =  "008")
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:3)  =  "001")
      *H30.4
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:7)
                                                           =  "0992081")
                       PERFORM 45011-PTCOM-SYROI-SEC
                   END-IF
               END-IF
               END-PERFORM
      *
               IF     IDX-SIN2             >   ZERO
      *
               MOVE    SMCNDATE-YMD        TO  SRY-CREYMD
               MOVE    SMCNDATE-YMD        TO  SRY-UPYMD
               MOVE    SMCNDATE-HMS        TO  SRY-UPHMS
      *
               MOVE    SPA-OPID            TO  SRY-OPID
      *
               MOVE    SRYACT-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K08 SRYACT INS ERR:" SRY-KEY  "."
                   DISPLAY "MCP-RC:" MCP-RC ":" 
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "SRYACT INS ERR KEY : "
                           SRY-KEY       DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
               END-IF
      *
               END-IF
      *
           END-PERFORM
           .
      *
       4501-SRYACT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメント作成処理
      *****************************************************************
       45011-PTCOM-SYROI-SEC           SECTION.
      *
      *    入力があれば作成
           IF     (LNK-WKSRY-INPUTCHI-G (IDX IDX-SIN)  =  SPACE ) AND
                  (LNK-WKSRY-INPUTCOMENT(IDX IDX-SIN)  =  SPACE )
               GO      TO      45011-PTCOM-SYROI-EXT
           END-IF
      *
           ADD     1                   TO  WRK-MEIBAN
           INITIALIZE                  PTCOM-REC
      *医療機関ＩＤ
           MOVE    SRY-HOSPNUM         TO  PTCOM-HOSPNUM
      *患者ＩＤ
           MOVE    SRY-PTID            TO  PTCOM-PTID
      *剤番号
           MOVE    SRY-ZAINUM          TO  PTCOM-ZAINUM
      *診療コード
           MOVE    LNK-WKSRY-SRYCD (IDX IDX-SIN)
                                       TO  PTCOM-SRYCD
      *コメント枝番番号
           MOVE    WRK-MEIBAN          TO  PTCOM-RENNUM
      *入力コメント
           MOVE    LNK-WKSRY-INPUTCOMENT(IDX IDX-SIN)
                                       TO  PTCOM-INPUTCOMENT
      *入力値
           MOVE    LNK-WKSRY-INPUTCHI-G  (IDX IDX-SIN)
                                       TO  PTCOM-INPUTCHI-AREA
      *
      *
           MOVE    SMCNDATE-YMD        TO  PTCOM-CREYMD
           MOVE    SMCNDATE-YMD        TO  PTCOM-UPYMD
           MOVE    SMCNDATE-HMS        TO  PTCOM-UPHMS
      *
           MOVE    SPA-OPID            TO  PTCOM-OPID
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K08 PTCOM INS ERR:" PTCOM-KEY
                            ",MCP-RC:" MCP-RC
               MOVE    "0005"          TO  SPA-ERRCD
               STRING  "PTCOM INS ERR KEY : "
                       PTCOM-KEY       DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
               END-STRING
           END-IF
      *
      *    明細番号
           MOVE    WRK-MEIBAN          TO  SRY-INPUTNUM  (IDX-SIN2)
      *
           .
       45011-PTCOM-SYROI-EXT.
           EXIT.
      *
      *****************************************************************
      *     算定履歴更新処理
      *****************************************************************
       4502-SANTEI-SYORI-SEC              SECTION.
      *
           IF      SPA-HKNCOMBINUM     =   "9999"
               GO      TO      4502-SANTEI-SYORI-EXT
           END-IF
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF    ((LNK-WKSRY-SRYCD (IDX IDY)(1:1)  =   "1" )
      *                システム予約コード
                       OR (LNK-WKSRY-SRYCD (IDX IDY)(1:3)  =   "099" ))
      *            自費の時、履歴なし
                   AND   ( LNK-WKSRY-SRYKBN(IDX)   NOT =   "95"  AND
                                                           "96" )
                       MOVE    LNK-WKSRY-SRYCD (IDX IDY)
                                                   TO  WRK-SRYCD
                       MOVE    LNK-WKSRY-SRYSURYO (IDX IDY)
                                                   TO  WRK-SRYSURYO
      *
                       MOVE    WRK-SRYCD           TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                算定用の診療コードへ変更
                       PERFORM 45020-SANTEI-SRYCD-SEC
                       IF      LNK-WKSRY-SRYCD (IDX IDY)
                                               NOT =   WRK-SRYCD
                           MOVE    WRK-SRYCD           TO  TNS-SRYCD
                           PERFORM 910-TENSU-READ-SEC
                       END-IF
                       IF      TNS-SANTEIRRKKBN    NOT =   ZERO
      *                    特定薬剤治療管理の初回日があれば編集
                           MOVE    1               TO  FLG-TOKTEI
                           MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)
                                                   TO  WRK-KAISU
                           PERFORM 45021-SANTEI-SYORI-SEC
                       END-IF
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4502-SANTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       45021-SANTEI-SYORI-SEC              SECTION.
      *
           PERFORM VARYING     IDX-KAI1    FROM    1  BY  1
                   UNTIL      (IDX-KAI1    >   10  )  OR
                              (TBL-KAISU (IDX-KAI1)   =   ZERO)
               PERFORM VARYING     IDX-KAI2    FROM    1  BY  1
                       UNTIL       IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2) NOT =   ZERO
                       MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                       MOVE    IDX-KAI2            TO  WRK-SRYDD
                       MOVE    TBL-KAISU (IDX-KAI1)    TO  WRK-KAISU
                       PERFORM 45021-SANTEI-UPD-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       45021-SANTEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       45021-SANTEI-UPD-SEC                SECTION.
      *
           IF      WRK-SRYYMD          =   SPACE
               MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           END-IF
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "1"                 TO  SSANTEI-SYORI
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    WRK-SRYYMD          TO  SSANTEI-SRYYMD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           IF     (FLG-TOKTEI          =   1   )  OR
                  (SPA-TOKTEI-YMD  NOT =   SPACE)
               MOVE    SPA-TOKTEI-YMD  TO  SSANTEI-SYOKAIYMD
           END-IF
           MOVE    WRK-KAISU           TO  SSANTEI-KAISU
      *    数量
      *    １日上限で単位が分以外の時、数量編集
           IF      (WRK-SRYSURYO       >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    WRK-SRYSURYO        TO  SSANTEI-SRYSURYO
           END-IF
      *    内分泌負荷試験用
           IF      LNK-WKSRYP-MSANTEITFLG(IDX)   =   1
               MOVE    LNK-WKSRYP-MSANTEITEN (IDX)
                                        TO  SSANTEI-MSANTEITEN
               MOVE    LNK-WKSRYP-MSANTEITENKBN (IDX)
                                        TO  SSANTEI-MSANTEITENKBN
               MOVE    1                TO  FLG-NAIBUN
           END-IF
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *
      *!!!!
      *    訂正前のリハビリ開始日判定
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING IDX-R2      FROM    1   BY  1
                       UNTIL  (IDX-R2      >   IDX-RIH )
                   IF     (TBL-RIHA-SRYCD(IDX-R2)  =   WRK-SRYCD )
                       MOVE    SPACE       TO  TBL-RIHA-SRYCD(IDX-R2)
                   END-IF
               END-PERFORM
           END-IF
      *!!!!
           .
       45021-SANTEI-UPD-EXT.
           EXIT.
      *!!
      *****************************************************************
      *    診療会計附加情報処理
      *****************************************************************
       4503-SRYACCTPLUS-SYORI-SEC              SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *    連番号
           MOVE    1                   TO  ACCTP-RENNUM
      *    点数区分
           MOVE    LNK-WKSRYP-PLUSKBN (1)
                                       TO  ACCTP-PLUSKBN
      *    労災の円
           MOVE    LNK-WKSRYP-RSIKINGAKU(1)
                                       TO  ACCTP-RSIKINGAKU
      *
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL       IDW     >   5
               MOVE    LNK-WKSRYP-SRYCD (1 IDW)
                                           TO  ACCTP-SRYCD (IDW)
               MOVE    LNK-WKSRYP-SYUTEN (1 IDW)
                                           TO  ACCTP-SYUTEN (IDW)
               MOVE    LNK-WKSRYP-YKZTEN (1 IDW)
                                           TO  ACCTP-YKZTEN (IDW)
               MOVE    LNK-WKSRYP-KIZAITEN (1 IDW)
                                           TO  ACCTP-KIZAITEN (IDW)
               MOVE    LNK-WKSRYP-FIRTEN (1 IDW)
                                           TO  ACCTP-FIRTEN (IDW)
           END-PERFORM
      *    酸素点数
           MOVE    LNK-WKSRYP-SANSOTEN (1) TO  ACCTP-SANSOTEN
      *    窒素点数
           MOVE    LNK-WKSRYP-CHISOTEN (1) TO  ACCTP-CHISOTEN
      *    院外処方点数
           MOVE    LNK-WKSRYP-INGAITEN (1) TO  ACCTP-INGAITEN
      *
           PERFORM 4503-SRYACCTPLUS-INS-SEC
           .
       4503-SRYACCTPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加情報追加処理
      *****************************************************************
       4503-SRYACCTPLUS-INS-SEC              SECTION.
      *
           MOVE    SMCNDATE-YMD        TO  ACCTP-CREYMD
           MOVE    SMCNDATE-YMD        TO  ACCTP-UPYMD
           MOVE    SMCNDATE-HMS        TO  ACCTP-UPHMS
      *
           MOVE    SPA-OPID            TO  ACCTP-OPID
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
      *
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K08 SRYACCTPLUS INS ERR:" ACCTP-KEY  "."
               DISPLAY "MCP-RC:" MCP-RC ":" 
               MOVE    "0005"          TO  SPA-ERRCD
               STRING  "SRYACCTPLUS INS ERR KEY : "
                       ACCTP-KEY       DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
               END-STRING
           END-IF
      *
           .
       4503-SRYACCTPLUS-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    翌月分診療行為マスター更新処理
      *****************************************************************
       1000-NEXT-SRYACT-KOU-SEC          SECTION.
      *
      *    診療年月の翌月算定
           INITIALIZE                      STS-AREA-DAY
           INITIALIZE                      LNK-DAY6-AREA
           MOVE    "61"                TO  LNK-DAY6-IRAI
           MOVE    SPA-SRYYMD          TO  LNK-DAY6-KIJUN
           MOVE    "2"                 TO  LNK-DAY6-ZOGENPTN
           MOVE    1                   TO  LNK-DAY6-ZOGEN
           CALL    "ORCSDAY"           USING   STS-AREA-DAY
                                               LNK-DAY6-AREA
           MOVE    LNK-DAY6-KOYOMI     TO  WRK-NEXT-SRYYMD
           MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
           MOVE    WRK-NEXT-SRYYMD     TO  SPA-SRYYMD
      *
      *    回数・日付決定
      *    当月用保存
           MOVE    TBL-KAISUU-AREA     TO  HZN-KAISUU-AREA
           MOVE    TBL2-KAISUU-AREA    TO  TBL-KAISUU-AREA
      *
           MOVE    ZERO                TO  FLG-SINKI
      *    診療会計マスター処理
           PERFORM 1002-SRYACCT-SYORI-SEC
      *
      *    診療コードがない時、処理をしない
      *    IF      WRK-MEISAISU        =   ZERO
      *        GO      TO      100-SRYACT-KOU-EXT
      *    END-IF
      *
      *    診療行為マスター処理 
           IF      FLG-SINKI           =   1
               PERFORM 4501-SRYACT-SYORI-SEC
           END-IF
      *!!
      *    診療会計附加情報処理 
           IF     (FLG-SINKI           =   1 )  AND
                  (LNK-WKSRYP-ADDKBN(01)   =   "1"  )
               PERFORM 4503-SRYACCTPLUS-SYORI-SEC
           END-IF
      *!!
      *
      *    算定履歴更新処理
           PERFORM 4502-SANTEI-SYORI-SEC
      *
      *    当月分を元へ
           MOVE    WRK-SRYYMD          TO  SPA-SRYYMD
           MOVE    HZN-KAISUU-AREA     TO  TBL-KAISUU-AREA
      *
           .
       1000-NEXT-SRYACT-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理
      *****************************************************************
       200-JYURRK-SYORI-SEC          SECTION.
      *
      *    入院の調剤料・麻毒で自動算定の時、受診履歴に追加しない
           IF      (LNK-WKSRY-SRYKBN (1)   =   "24"  OR  "26" )  AND
                   (LNK-WKSRY-AUTOKBN(1 1) =   "3"   )
               GO      TO      200-JYURRK-SYORI-EXT
           END-IF
           IF      (WRK-ZAINUM         =   ZERO ) 
               GO      TO      200-JYURRK-SYORI-EXT
           END-IF
      *
      *    当月
           PERFORM VARYING     IDX-KAI1    FROM    1   BY  1
                   UNTIL      (IDX-KAI1    >   10  )  OR
                              (TBL-KAISU (IDX-KAI1)    =   ZERO )
                          OR  (SPA-ERRCD       NOT =   SPACE )
               PERFORM VARYING     IDX-KAI2    FROM    1   BY  1
                       UNTIL       IDX-KAI2    >   31
                   IF      TBL-DAY (IDX-KAI1 IDX-KAI2) NOT =   ZERO
      *                受診履歴作成
                       MOVE    SPA-SRYYMD          TO  WRK-SRYYMD
                       MOVE    IDX-KAI2            TO  WRK-SRYDD
      *!!!!
      *                同日再入院日判定
                       IF     (SPA-SRYYMD(1:6)     =   SPA-K02N-NYUINYMD
                                                                 (1:6))
                       AND    (IDX-KAI2            =   SPA-K02N-DOUDAY)
                           MOVE    2               TO  WRK-RENNUM
                       ELSE
                           MOVE    1               TO  WRK-RENNUM
                       END-IF
      *!!!!
                       PERFORM 2001-JYURRK-SAKUSEI-SEC
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       200-JYURRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理
      *****************************************************************
       2001-JYURRK-SAKUSEI-SEC          SECTION.
      *
      *    受診履歴の同日作成分カウント
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           IF      SPA-DOUJI-RENNUM    NOT =   ZERO
               MOVE    SPA-DOUJI-RENNUM    TO  WRK-JYURRK-RENNUM
           END-IF
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
      **** MOVE    1                   TO  JYURRK-RENNUM
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
      *    １日連番１とする
      *    MOVE    "JYURRK-KEY5"       TO  WRK-PATH
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-CNT-DOUJI
           PERFORM
                   UNTIL      (FLG-JYURRK      =   1 )  OR
                              (FLG-OK          =   1 )
      *        同じ保険を対象とする
               IF      JYURRK-HKNCOMBINUM  =   SPA-HKNCOMBINUM
                   MOVE    JYURRK-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
                   MOVE    1               TO  FLG-OK
               ELSE
                   MOVE    JYURRK-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
                   ADD     1               TO  WRK-JYURRK-DOUJI-RENNUM
                   IF      JYURRK-EDANUM   =   1
                       ADD     1           TO  WRK-CNT-DOUJI
                   END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    訂正で科・保険組合せ変更がない時、前の同日連番
           IF     (SPA-SYORIFLG        =   1  )  AND
                  (SPA-SRYKA           =   SPA-OLD-SRYKA )
      *         訂正日のみ
           AND    (SPA-SRYYMD          =   WRK-SRYYMD    )
      *        同じ保険組合せがない時、訂正前の連番
               IF      FLG-OK              =   ZERO
                   MOVE    SPA-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
               END-IF
      *        他の保険組合せがない時、ゼロ
               IF      WRK-CNT-DOUJI       =   ZERO
                   MOVE    ZERO            TO  WRK-JYURRK-DOUJI-RENNUM
               END-IF
           END-IF
      *
           IF      FLG-OK              =   ZERO
               MOVE    1                   TO  WRK-JYURRK-EDANUM
      *        受診履歴新規
               PERFORM 20011-JYURRK-SYOKI-SEC
           ELSE
      *        受診履歴更新
               PERFORM 20012-JYURRK-KOUSIN-SEC
           END-IF
      *
           .
       2001-JYURRK-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴更新処理
      *****************************************************************
       20012-JYURRK-KOUSIN-SEC          SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
      ***  MOVE    1                   TO  JYURRK-RENNUM
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
      *    １日連番１とする
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  FLG-ARI
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      (WRK-JYURRK-DOUJI-RENNUM    =
                                            JYURRK-DOUJI-RENNUM ) AND
                       (JYURRK-HKNCOMBINUM     =   SPA-HKNCOMBINUM)
      *            診療区分編集
                   IF      JYURRK-EDANUM       =   1
                       PERFORM 200121-JYURRK-SRYKBN-SEC
                   END-IF
                   MOVE    JYURRK-EDANUM       TO  WRK-JYURRK-EDANUM
                   PERFORM VARYING     IDX     FROM    1   BY  1
                           UNTIL      (IDX     >   15  )  OR
                                      (FLG-ARI NOT =   ZERO )
                       IF      JYURRK-ZAINUM (IDX) =   WRK-ZAINUM
                           MOVE    1               TO  FLG-ARI
                       ELSE
                           IF      JYURRK-ZAINUM(IDX)  =   ZERO
                               MOVE    WRK-ZAINUM      TO  JYURRK-ZAINUM
                                                                   (IDX)
                               MOVE    2               TO  FLG-ARI
                           END-IF
                       END-IF
                   END-PERFORM
      *
                   IF     (FLG-ARI             =   2    )  OR
                          (JYURRK-EDANUM       =   1    )
      *                受診履歴更新処理
                       PERFORM 200122-JYURRK-UPD-SEC
                   END-IF
               END-IF
      *
               IF      FLG-ARI             =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-ARI             =   ZERO
      *        枝番
               IF      WRK-JYURRK-EDANUM   =   9
                   MOVE    "0006"          TO  SPA-ERRCD
                   MOVE    ZERO            TO  WRK-JYURRK-EDANUM
               END-IF
      *        受診履歴新規（ＤＢ更新をキャンセルさせる為エラーでも更新）
               ADD     1                   TO  WRK-JYURRK-EDANUM
               PERFORM 20011-JYURRK-SYOKI-SEC
           END-IF
      *
           .
       20012-JYURRK-KOUSIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療区分編集処理
      *****************************************************************
       200121-JYURRK-SRYKBN-SEC          SECTION.
      *
      *    診療区分領域の編集
           EVALUATE    LNK-WKSRY-SRYKBN (1)
               WHEN   "11"
               WHEN   "12"
               WHEN   "13"
               WHEN   "14"
                   MOVE    "01"        TO  JYURRK-SRYKBN1
               WHEN   "21"
                   MOVE    "01"        TO  JYURRK-SRYKBN2
               WHEN   "22"
                   MOVE    "01"        TO  JYURRK-SRYKBN3
               WHEN   "23"
                   MOVE    "01"        TO  JYURRK-SRYKBN4
               WHEN   "31"
               WHEN   "32"
               WHEN   "33"
                   MOVE    "01"        TO  JYURRK-SRYKBN5
               WHEN   "40"
                   MOVE    "01"        TO  JYURRK-SRYKBN6
               WHEN   "50"
                   MOVE    "01"        TO  JYURRK-SRYKBN7
               WHEN   "54"
                   MOVE    "01"        TO  JYURRK-SRYKBN8
               WHEN   "60"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "64"
                   MOVE    "01"        TO  JYURRK-SRYKBN9
               WHEN   "70"
                   MOVE    "01"        TO  JYURRK-SRYKBN10
               WHEN   "80"
                   MOVE    "01"        TO  JYURRK-SRYKBN11
      *****    WHEN   "93"
      *        WHEN   "95"
      *        WHEN   "96"
      *        WHEN   "99"
      *****        MOVE    "01"        TO  JYURRK-SRYKBN11
           END-EVALUATE
           .
       200121-JYURRK-SRYKBN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴更新処理
      *****************************************************************
       200122-JYURRK-UPD-SEC          SECTION.
      *
      *    受診履歴更新処理
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBUPDATE"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               DISPLAY "K08 JYURRK-UPD ERR:" MCP-RC 
                        ",KEY:" JYURRK-KEY "."
               STRING  "JYURRK UPD ERR KEY : "
                       JYURRK-KEY      DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
               END-STRING
           END-IF
      *
           .
       200122-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（初期）
      *****************************************************************
       20011-JYURRK-SYOKI-SEC          SECTION.
      *
      *    受診履歴レコードの作成
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYYMD          TO  JYURRK-SRYYMD
      *****MOVE    1                   TO  JYURRK-RENNUM
           MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
           MOVE    WRK-JYURRK-DOUJI-RENNUM
                                       TO  JYURRK-DOUJI-RENNUM
           MOVE    WRK-JYURRK-EDANUM   TO  JYURRK-EDANUM
      *    会計連番
           MOVE    1                   TO  JYURRK-KAIKEI-RENNUM
      *
           MOVE    SPA-DRCD            TO  JYURRK-DRCD
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           MOVE    ZERO                TO  JYURRK-DENPNUM
      *
      *    労災用に保険区分をセットする
           MOVE    SPA-HKNKBN          TO  JYURRK-HKNKBN
           IF      SPA-HKNKBN          =   1
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  JYURRK-HKNKBN
                   WHEN    "4"
      *                自賠責
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "4"                 TO  JYURRK-HKNKBN
                       ELSE
                           MOVE    "2"                 TO  JYURRK-HKNKBN
                       END-IF
                   WHEN    "5"
      *                公務災害
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "5"                 TO  JYURRK-HKNKBN
                       ELSE
                           MOVE    "1"                 TO  JYURRK-HKNKBN
                       END-IF
      *
                   WHEN    "6"
      *                自賠責（第三者行為）
                       MOVE    "7"             TO  JYURRK-HKNKBN
               END-EVALUATE
           END-IF
      *    公害保険
           IF      SPA-HKNKBN          =   2
               MOVE    "6"                 TO  JYURRK-HKNKBN
           END-IF
      *    診療区分領域の編集
           PERFORM 200121-JYURRK-SRYKBN-SEC
      *    剤番号
           MOVE    WRK-ZAINUM          TO  JYURRK-ZAINUM (1)
      *
           MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
           MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
           MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
           MOVE    SPA-OPID            TO  JYURRK-OPID
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "JYURRK INS ERR KEY : "
                           JYURRK-KEY      DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
               END-IF
               DISPLAY "K08 JYURRK INS ERR:" MCP-RC 
                        ",KEY:" JYURRK-KEY "."
           END-IF
      *
           .
       20011-JYURRK-SYOKI-EXT.
           EXIT.
      *****************************************************************
      *    訂正時、受診履歴等の削除処理
      *****************************************************************
       300-TEISEI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           IF      SPA-OLD-SRYKA       =   SPACE
               MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           ELSE
               MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           END-IF
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF     (SPA-DOUJI-RENNUM    =   JYURRK-DOUJI-RENNUM) AND
                      (SPA-RENNUM          =   JYURRK-RENNUM      ) AND
                      (SPA-DENPNUM         =   ZERO               )
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                               WRK-JYURRK-DOUJI-RENNUM
      *
      *            診療会計マスタの診療回数をクリアする
                   PERFORM 4500112-TEISEI-SRYACCT-SEC
      *
      *            会計連番
                   MOVE    JYURRK-KAIKEI-RENNUM
                                               TO  WRK-KAIKEI-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
                   MOVE    SPA-OPID            TO  JYURRK-OPID
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key4"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key4"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    受診履歴削除
           PERFORM 4500114-TEISEI-JYURRKDEL-SEC
      *    受診履歴の連番更新
           IF      SPA-ERRCD           =   SPACE
               IF     (SPA-SRYKA           =   SPA-OLD-SRYKA)
               AND    (SPA-OLD-HKNCOMBI    NOT =   SPA-HKNCOMBINUM  )
                   PERFORM 3009-TEISEI-JYURRK-SEC
               END-IF
           END-IF
      *
           .
       300-TEISEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       4500112-TEISEI-SRYACCT-SEC          SECTION.
      *
      *    保険組合せを保存する
      *!   MOVE    SPA-HKNCOMBINUM     TO  WRK-HZN-HKNCOMBINUM
      *!   MOVE    SPA-HKNKBN          TO  WRK-HZN-HKNKBN
      *!   MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
      *!   MOVE    JYURRK-HKNKBN       TO  SPA-HKNKBN
      *
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )  OR
                              (JYURRK-ZAINUM (IDX) =   ZERO )
      *
               INITIALIZE                      SRYACCT-REC
               MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
               MOVE    JYURRK-NYUGAIKBN    TO  ACCT-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  ACCT-PTID
               MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
               PERFORM
                   UNTIL   (FLG-SRYACCT        =   1    ) OR
                           (FLG-OK             =   1    )
      ***        投薬関係の有無
      *          IF      ACCT-SRYKBN(1:1)      =   "2"
      *              MOVE    1                 TO  FLG-TOUYAKU
      **         END-IF
      *
      *          入院調剤料と麻毒については後で一括変更するのでそのまま
      *          IF      ACCT-SRYKBN             =   "24"  OR "26"
      *                CONTINUE
      ****       ELSE
      *
                   MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
      *ver.4.8
      *****        EVALUATE    JYURRK-RENNUM
      *                WHEN    1
      *                    MOVE    2               TO  IDX-1
      *                WHEN    2
      *                    MOVE    3               TO  IDX-1
      *                WHEN    OTHER
      *                    MOVE    4               TO  IDX-1
      *****        END-EVALUATE
                   COMPUTE IDX-1               =   JYURRK-RENNUM +   1
      *
      *            算定履歴の削除処理
                   MOVE    ACCT-DAY (IDX-1 IDX-2)  TO  WRK-KAISU
                   PERFORM 4500113-TEISEI-SANTEI-SEC
      *
      *            剤回数　変更
                   MOVE    ZERO                TO  ACCT-DAY
                                                       (IDX-1 IDX-2)
      *            剤回数編集
                   PERFORM 450140-ZAIKAISU-HEN-SEC
      *            回数がゼロの時、削除
                   IF      ACCT-ZAIKAISU       =   ZERO
                       PERFORM 45001121-ZAINUM-DELETE-SEC
                   ELSE
      *
                       MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                       MOVE    SPA-OPID            TO  ACCT-OPID
      *
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "K08 SRYACCT UPD ERR:" MCP-RC
                           MOVE    "0005"          TO  SPA-ERRCD
                           STRING  "SRYACCT UPD ERR KEY : "
                                   ACCT-KEY      DELIMITED  BY  SIZE
                                            INTO  ORCSCNYUIN-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
      *
      ******      END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-NEXT-SEC
               END-PERFORM
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
      *!   保険組合せを戻す
      *!   MOVE    WRK-HZN-HKNCOMBINUM     TO  SPA-HKNCOMBINUM
      *!   MOVE    WRK-HZN-HKNKBN          TO  SPA-HKNKBN
      *
           .
       4500112-TEISEI-SRYACCT-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤回数編集処理
      *****************************************************************
       450140-ZAIKAISU-HEN-SEC          SECTION.
      *
      **** COMPUTE ACCT-DAY (1 IDX-2)  =   ACCT-DAY (2 IDX-2)
      *                                +   ACCT-DAY (3 IDX-2)
      ****                             +   ACCT-DAY (4 IDX-2)
      *ver.4.8
           MOVE    ZERO                TO  ACCT-DAY (1 IDX-2)
           PERFORM VARYING     IDX-A   FROM    2   BY  1
                   UNTIL       IDX-A   >   10
               COMPUTE ACCT-DAY (1 IDX-2)  =   ACCT-DAY (1 IDX-2)
                                           +   ACCT-DAY (IDX-A IDX-2)
           END-PERFORM
      *
           MOVE    ZERO                TO  ACCT-ZAIKAISU
           PERFORM VARYING     IDX-D   FROM    1   BY  1
                   UNTIL       IDX-D   >   31
              IF      ACCT-DAY (1 IDX-D)   >   ZERO
      *            剤回数集計
                   COMPUTE ACCT-ZAIKAISU       =   ACCT-ZAIKAISU
                                               +   ACCT-DAY
                                                       (1  IDX-D)
               END-IF
           END-PERFORM
           .
       450140-ZAIKAISU-HEN-EXT.
      *
      *****************************************************************
      *    剤削除処理
      *****************************************************************
       45001121-ZAINUM-DELETE-SEC          SECTION.
      *
           INITIALIZE                  SRYACT-REC
           MOVE    ACCT-HOSPNUM        TO  SRY-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  SRY-NYUGAIKBN
           MOVE    ACCT-PTID           TO  SRY-PTID
           MOVE    ACCT-SRYKA          TO  SRY-SRYKA
           MOVE    ACCT-SRYYM          TO  SRY-SRYYM
           MOVE    ACCT-ZAINUM         TO  SRY-ZAINUM
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
                DISPLAY "K08 SRYACT DEL ERR:" MCP-RC
                        ",KEY:" SRY-KEY
                MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *    コメント
           INITIALIZE                  PTCOM-REC
           MOVE    ACCT-HOSPNUM        TO  PTCOM-HOSPNUM
           MOVE    ACCT-PTID           TO  PTCOM-PTID
           MOVE    ACCT-ZAINUM         TO  PTCOM-ZAINUM
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "del11"             TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K08  PTCOM  DEL ERR:" MCP-RC
                     ",KEY:" PTCOM-KEY
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           MOVE    SRYACCT-REC         TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacct"       TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K08 SRYACCT DEL ERR:" MCP-RC
                        ",KEY:" ACCT-KEY
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
      *    附加情報削除
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    ACCT-HOSPNUM        TO  ACCTP-HOSPNUM
           MOVE    ACCT-NYUGAIKBN      TO  ACCTP-NYUGAIKBN
           MOVE    ACCT-PTID           TO  ACCTP-PTID
           MOVE    ACCT-SRYKA          TO  ACCTP-SRYKA
           MOVE    ACCT-SRYYM          TO  ACCTP-SRYYM
           MOVE    ACCT-ZAINUM         TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "DBDELETE"          TO  MCP-FUNC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "del2"              TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "K08  SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
               MOVE    "0005"          TO  SPA-ERRCD
           END-IF
      *
           .
       45001121-ZAINUM-DELETE-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴の回数クリア処理
      *****************************************************************
       4500113-TEISEI-SANTEI-SEC          SECTION.
      *
           INITIALIZE                      SRYACT-REC
           MOVE    JYURRK-HOSPNUM      TO  SRY-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SRY-NYUGAIKBN
           MOVE    JYURRK-PTID         TO  SRY-PTID
           MOVE    JYURRK-SRYKA        TO  SRY-SRYKA
           MOVE    JYURRK-SRYYMD(1:6)  TO  SRY-SRYYM
           MOVE    JYURRK-ZAINUM (IDX) TO  SRY-ZAINUM
      *
           MOVE    SRYACT-REC          TO  MCPDATA-REC
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 915-SRYACT-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACT
           END-IF
      *    投薬関係の有無
           IF       FLG-SRYACT         =   ZERO
               IF      (ACCT-SRYKBN        =   "21"
                                           OR  "22"
                                           OR  "23" )
                   IF      (SRY-SRYSYUKBN(3:1)   NOT =   "4" )
                       MOVE    1                 TO  FLG-TOUYAKU
                   END-IF
               END-IF
           END-IF
           PERFORM
                   UNTIL   FLG-SRYACT          =   1
      *
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (SRY-SRYCD (IDY) =   SPACE)
      *
      *        手技料の時、算定歴を判定する
               IF     (SRY-SRYCD (IDY)(1:1)    =   "1"   )
      *            システム予約コード
                   OR (SRY-SRYCD (IDY)(1:3)    =   "099")
                   MOVE    SRY-SRYCD (IDY)     TO  WRK-SRYCD
                   MOVE    SRY-SRYSURYO (IDY)  TO  WRK-SRYSURYO
      *
                   MOVE    WRK-SRYCD           TO  TNS-SRYCD
                   PERFORM 910-TENSU-READ-SEC
      *            算定用の診療コードへ変更
                   PERFORM 45020-SANTEI-SRYCD-SEC
                   IF      SRY-SRYCD (IDY) NOT =   WRK-SRYCD
                       MOVE    WRK-SRYCD           TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
                   END-IF
                   IF      TNS-SANTEIRRKKBN    NOT =   ZERO
                       PERFORM 45001131-SANTEI-DEL-SEC
                   END-IF
               END-IF
      *
               END-PERFORM
      *
               MOVE    "tbl_sryact"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 915-SRYACT-NEXT-SEC
      *
           END-PERFORM
      *
           MOVE    "tbl_sryact"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4500113-TEISEI-SANTEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    算定履歴回数減処理
      *****************************************************************
       45001131-SANTEI-DEL-SEC          SECTION.
      *
           INITIALIZE                  ORCSSANTEIAREA
           MOVE    "3"                 TO  SSANTEI-SYORI
      *    削除（算定履歴付加削除）
           IF     (ORCSCNYUIN-KBN      =   "2"
                                       OR  "3" )
               MOVE    "1"                 TO  SSANTEI-SYORI2
           END-IF
           MOVE    "K03"               TO  SSANTEI-PG
           MOVE    WRK-SRYCD           TO  SSANTEI-SRYCD
           MOVE    SPA-PTID            TO  SSANTEI-PTID
           MOVE    SPA-SRYYMD          TO  SSANTEI-SRYYMD
           MOVE    WRK-KAISU           TO  SSANTEI-KAISU
      *    数量
      *    １日上限で単位が分以外の時、数量編集
           IF      (WRK-SRYSURYO       >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    WRK-SRYSURYO        TO  SSANTEI-SRYSURYO
           END-IF
      *    診療科
           MOVE    SRY-SRYKA           TO  SSANTEI-SRYKA
      *    保険区分は同じ
      *****MOVE    SPA-HKNKBN          TO  SSANTEI-HKNKBN
           MOVE    ACCT-HKNCOMBI       TO  SSANTEI-HKNCOMBINUM
           MOVE    JYURRK-HKNKBN       TO  SSANTEI-HKNKBN
      *    労災・自賠責区分
           MOVE    SPACE               TO  SSANTEI-RSI-JUNKYO
           EVALUATE    JYURRK-HKNKBN
               WHEN    "1"
                   MOVE    "1"             TO  SSANTEI-RSI-HKNKBN
               WHEN    "2"
                   MOVE    "1"             TO  SSANTEI-HKNKBN
                   MOVE    "4"             TO  SSANTEI-RSI-HKNKBN
               WHEN    "4"
      *            自賠責 健保準拠
                   MOVE    "1"             TO  SSANTEI-HKNKBN
      ************ MOVE    "5"             TO  SSANTEI-RSI-HKNKBN
                   MOVE    "4"             TO  SSANTEI-RSI-HKNKBN
                   MOVE    "1"             TO  SSANTEI-RSI-JUNKYO
               WHEN    "5"
      *            労災（公務災害）自賠責 健保準拠
                   MOVE    "1"             TO  SSANTEI-HKNKBN
                   MOVE    "5"             TO  SSANTEI-RSI-HKNKBN
                   MOVE    "1"             TO  SSANTEI-RSI-JUNKYO
               WHEN    "6"
      *            公害
                   MOVE    "2"             TO  SSANTEI-HKNKBN
      *
               WHEN    "7"
      *            自賠責（第三者行為）
                   MOVE    "1"             TO  SSANTEI-HKNKBN
                   MOVE    "6"             TO  SSANTEI-RSI-HKNKBN
                   MOVE    "1"             TO  SSANTEI-RSI-JUNKYO
           END-EVALUATE
           CALL    "ORCSSANTEI"        USING
                                       ORCSSANTEIAREA
                                       SPA-AREA
      *    内分泌負荷試験削除フラグ
           IF      SSANTEI-MSANTEIDEL  =   1
               MOVE    1               TO  FLG-NAIBUN
           END-IF
      *
      *!!!
      *    リハビリ開始日コード
           IF      SSANTEI-SANTEIPLUS  =   1
               ADD     1               TO  IDX-RIH
               MOVE    WRK-SRYCD       TO  TBL-RIHA-SRYCD (IDX-RIH)
               MOVE    SSANTEI-SRYKA   TO  TBL-RIHA-SRYKA (IDX-RIH)
               MOVE    SSANTEI-HKNCOMBINUM
                                       TO  TBL-RIHA-HKNCOMBI (IDX-RIH)
               MOVE    SSANTEI-HKNKBN  TO  TBL-RIHA-HKNKBN (IDX-RIH)
           END-IF
      *!!!!
      *
           .
       45001131-SANTEI-DEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴削除処理
      *****************************************************************
       4500114-TEISEI-JYURRKDEL-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   WRK-KAIKEI-RENNUM
      *
               MOVE    ZERO                TO  FLG-JYURRK
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL       FLG-JYURRK  NOT =   ZERO
                   MOVE    SPACE               TO  JYURRK-REC
                   INITIALIZE                      JYURRK-REC
                   MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
                   MOVE    SPA-PTID            TO  JYURRK-PTID
                   MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      ***********  MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
                   IF      SPA-OLD-SRYKA       =   SPACE
                       MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
                   ELSE
                       MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
                   END-IF
                   MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
                   MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
                   MOVE    WRK-JYURRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
                   MOVE    IDX                 TO  JYURRK-KAIKEI-RENNUM
                   MOVE    IDY                 TO  JYURRK-EDANUM
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 910-DBSELECT-SEC
                   IF      MCP-RC              =   ZERO
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
                       PERFORM 970-JYURRK-NEXT-SEC
                   ELSE
                       MOVE    1                   TO  FLG-JYURRK
                   END-IF
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 990-DBCLOSE-SEC
      *
                   IF      FLG-JYURRK          =   ZERO
      *                受診履歴削除
                       MOVE    JYURRK-REC          TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           MOVE    ZERO                TO  FLG-JYURRK
      *
           .
       4500114-TEISEI-JYURRKDEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険変更時の受診履歴連番号変更処理
      *****************************************************************
       3009-TEISEI-JYURRK-SEC          SECTION.
      *
      *    変更後の保険組合せで受診履歴が登録済みの時、連番号変更
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-HKNCOMBINUM     TO  JYURRK-HKNCOMBINUM
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key38"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key38"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-JYURRK          =   ZERO
               PERFORM 4500115-TEISEI-RENNUM-SEC
           END-IF
           .
       3009-TEISEI-JYURRK-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除・科変更時の受診履歴の連番変更処理
      *****************************************************************
       4500115-TEISEI-RENNUM-SEC          SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           IF      SPA-OLD-SRYKA       =   SPACE
               MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           ELSE
                MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           END-IF
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    ZERO                TO  WRK-RENNUM
           MOVE    ZERO                TO  WRK-RENNUM-KEY
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
           MOVE    ZERO                TO  WRK-DOUJI-RENNUM-KEY
           PERFORM
                   UNTIL       FLG-JYURRK  =   1
      *        IF      JYURRK-RENNUM       =   WRK-RENNUM-KEY
      *            CONTINUE
      *        ELSE
      *            ADD     1                   TO  WRK-JYURRK-RENNUM
      *            MOVE    JYURRK-RENNUM       TO  WRK-RENNUM-KEY
      *        END-IF
      *!!      同日再入院の為、同じ連番の同日連番を変更する
             IF     (SPA-RENNUM          =   JYURRK-RENNUM )
               MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
               IF      JYURRK-DOUJI-RENNUM =   WRK-DOUJI-RENNUM-KEY
                   CONTINUE
                   IF      JYURRK-EDANUM       =   1
                       ADD     1               TO  WRK-RENNUM
                   END-IF
               ELSE
                   IF      JYURRK-EDANUM       =   1
                       ADD     1               TO  WRK-RENNUM
                       IF      WRK-RENNUM      >   1
                          ADD     1            TO
                                               WRK-JYURRK-DOUJI-RENNUM
                       END-IF
                   END-IF
                   MOVE    JYURRK-DOUJI-RENNUM TO  WRK-DOUJI-RENNUM-KEY
               END-IF
      *
               IF      JYURRK-RENNUM       =   WRK-JYURRK-RENNUM
                  AND  JYURRK-DOUJI-RENNUM =   WRK-JYURRK-DOUJI-RENNUM
                   CONTINUE
               ELSE
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
      *
                   IF      JYURRK-RENNUM   NOT =   WRK-JYURRK-RENNUM
                       MOVE    1               TO  FLG-RENNUM
                   ELSE
                       MOVE    ZERO            TO  FLG-RENNUM
                   END-IF
      *
                   MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
                   MOVE    WRK-JYURRK-DOUJI-RENNUM
                                               TO  JYURRK-DOUJI-RENNUM
      *
                   MOVE    SMCNDATE-YMD        TO  JYURRK-CREYMD
                   MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
                   MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
                   MOVE    SPA-OPID            TO  JYURRK-OPID
      *
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K08 JYURRK INS ERR:" JYURRK-KEY
                           ",MCP-RC:" MCP-RC
                       MOVE    "0005"          TO  SPA-ERRCD
                       STRING  "JYURRK INS ERR KEY : "
                               JYURRK-KEY      DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                       END-STRING
                   END-IF
      *            診療会計修正
      ***          IF      FLG-RENNUM          =   1
      *                PERFORM 4500115-TEISEI-SRYACTUPD-SEC
      ****         END-IF
                END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key9"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key9"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       4500115-TEISEI-RENNUM-EXT.
           EXIT.
      *****************************************************************
      *    診療会計修正処理
      *****************************************************************
       4500115-TEISEI-SRYACTUPD-SEC          SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )  OR
                              (JYURRK-ZAINUM (IDX) =   ZERO)
                           OR (SPA-ERRCD       NOT =   SPACE )
      *
               INITIALIZE                      SRYACCT-REC
               MOVE    JYURRK-HOSPNUM      TO  ACCT-HOSPNUM
               MOVE    SPA-NYUGAIKBN       TO  ACCT-NYUGAIKBN
               MOVE    JYURRK-PTID         TO  ACCT-PTID
               MOVE    JYURRK-SRYKA        TO  ACCT-SRYKA
               MOVE    JYURRK-SRYYMD(1:6)  TO  ACCT-SRYYM
               MOVE    JYURRK-ZAINUM (IDX) TO  ACCT-ZAINUM
      *
               MOVE    SRYACCT-REC         TO  MCPDATA-REC
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-SRYACCT
               END-IF
               PERFORM
                   UNTIL   (FLG-SRYACCT        =   1    )
                       OR  (SPA-ERRCD      NOT =   SPACE)
      *
                   MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
      ********     EVALUATE    JYURRK-RENNUM
      *                WHEN    1
      *                    MOVE    2               TO  IDX-1
      *                WHEN    2
      *                    MOVE    3               TO  IDX-1
      *                WHEN    OTHER
      *                    MOVE    4               TO  IDX-1
      *            END-EVALUATE
      *                EVALUATE    WRK-RENNUM-KEY
      *                    WHEN    1
      *                        MOVE    2               TO  IDX-3
      *                    WHEN    2
      *                        MOVE    3               TO  IDX-3
      *                    WHEN    OTHER
      *                        MOVE    4               TO  IDX-3
      ********         END-EVALUATE
      *ver.4.8
                   COMPUTE IDX-1           =   JYURRK-RENNUM   +   1
                   COMPUTE IDX-3           =   WRK-RENNUM-KEY  +   1
      *
      *                剤回数　変更
                       ADD     ACCT-DAY (IDX-3 IDX-2)
                                              TO  ACCT-DAY (IDX-1 IDX-2)
                       MOVE    ZERO           TO  ACCT-DAY (IDX-3 IDX-2)
      *
      *
                       MOVE    SMCNDATE-YMD        TO  ACCT-UPYMD
                       MOVE    SMCNDATE-HMS        TO  ACCT-UPHMS
      *
                       MOVE    SPA-OPID            TO  ACCT-OPID
      *
                       MOVE    SRYACCT-REC         TO  MCPDATA-REC
                       MOVE    "DBUPDATE"          TO  MCP-FUNC
                       MOVE    "tbl_sryacct"       TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
      *
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "K08 SRYACCT UPD ERR:" MCP-RC
                             ",KEY:"  ACCT-KEY
                           MOVE    "0005"          TO  SPA-ERRCD
                           STRING  "SRYACCT INS ERR KEY : "
                                   ACCT-KEY      DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                           END-STRING
                       END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-NEXT-SEC
               END-PERFORM
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
           .
       4500115-TEISEI-SRYACTUPD-EXT.
           EXIT.
      *****************************************************************
      *    算定用の診療コードへ変更処理
      *****************************************************************
       45020-SANTEI-SRYCD-SEC              SECTION.
      *
      *    包括逓減区分判定
           EVALUATE    TNS-HOUKATUTEIGENKBN
      *        頭部
               WHEN    201
                   MOVE    "099700101"         TO  WRK-SRYCD
      *        躯幹
               WHEN    202
                   MOVE    "099700102"         TO  WRK-SRYCD
      *        四肢
               WHEN    203
                   MOVE    "099700103"         TO  WRK-SRYCD
           END-EVALUATE
           .
       45020-SANTEI-SRYCD-EXT.
           EXIT.
      *!!!!
      *****************************************************************
      *    リハビリ開始日の履歴付加削除
      *****************************************************************
       45005-SANTEIPLUS-DEL-SEC              SECTION.
      *
           PERFORM VARYING IDX-R2      FROM    1   BY  1
                   UNTIL  (IDX-R2      >   10      )
               IF      TBL-RIHA-SRYCD(IDX-R2)  NOT =   SPACE
                   INITIALIZE                  ORCSSANTEIAREA
                   MOVE    "3"                 TO  SSANTEI-SYORI
                   MOVE    "2"                 TO  SSANTEI-SYORI2
                   MOVE    "K03"               TO  SSANTEI-PG
                   MOVE    TBL-RIHA-SRYCD(IDX-R2)
                                               TO  SSANTEI-SRYCD
                   MOVE    SPA-PTID            TO  SSANTEI-PTID
                   MOVE    SPA-SRYYMD          TO  SSANTEI-SRYYMD
                   MOVE    ZERO                TO  SSANTEI-KAISU
                   MOVE    TBL-RIHA-SRYKA (IDX-R2)
                                               TO  SSANTEI-SRYKA
                   MOVE    TBL-RIHA-HKNCOMBI (IDX-R2)
                                               TO  SSANTEI-HKNCOMBINUM
                   MOVE    TBL-RIHA-HKNKBN (IDX-R2)
                                               TO  SSANTEI-HKNKBN
                   CALL    "ORCSSANTEI"        USING
                                               ORCSSANTEIAREA
                                               SPA-AREA
               END-IF
           END-PERFORM
           .
       45005-SANTEIPLUS-DEL-EXT.
           EXIT.
      *!!!!
      *
      *****************************************************************
      *    診療科履歴更新処理
      *****************************************************************
       500-SRYKARRK-UPD-SEC              SECTION.
      *
      *    退院日がある時、最終受診日を編集する
           INITIALIZE                  ORCSKARRKAREA
           MOVE    ORCSCNYUIN-YMD(1:6) TO  SKARRK-SRYYM
           IF      ORCSCNYUIN-YMD      =   SPACE
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
           END-IF
           MOVE    ORCSCNYUIN-TAIINYMD TO  SKARRK-LASTYMD
           MOVE    TBL-NYU-SRYKA (01)  TO  SKARRK-SRYKA
           CALL    "ORCSKARRK"         USING
                                       ORCSKARRKAREA
                                       SPA-AREA
           IF      SKARRK-RC       NOT =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               STRING  "ORCSKARRK ERR "
                       SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                       INTO  ORCSCNYUIN-ERRMSG
               END-STRING
           END-IF
      *    退院科以外の診療科
           PERFORM VARYING     IDX-K   FROM    2   BY  1
                   UNTIL      (IDX-K   >   10   )  OR
                              (SPA-ERRCD   NOT =   SPACE)  OR
                              (TBL-NYU-SRYKA (IDX-K)  =   SPACE)
               INITIALIZE                  ORCSKARRKAREA
               MOVE    ORCSCNYUIN-YMD(1:6) TO  SKARRK-SRYYM
               IF      ORCSCNYUIN-YMD      =   SPACE
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
               END-IF
               MOVE    TBL-NYU-SRYKA(IDX-K)    TO  SKARRK-SRYKA
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "ORCSKARRK ERR "
                           SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                           INTO  ORCSCNYUIN-ERRMSG
                   END-STRING
               END-IF
           END-PERFORM
           .
       500-SRYKARRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為マスタ−削除処理
      *****************************************************************
       400-WKSRYACT-DEL-SEC              SECTION.
      *
      *    展開中の削除
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
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
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
               END-IF
      *
               MOVE    "tbl_wksryact"      TO  MCP-TABLE
               MOVE    "key7"              TO  MCP-PATHNAME
               PERFORM 940-WKSRYACT-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_wksryact"      TO  MCP-TABLE
           MOVE    "key7"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       400-WKSRYACT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受付更新処理
      *****************************************************************
       410-UKETUKE-SYORI-SEC              SECTION.
      *
           ACCEPT  WRK-TIME11          FROM  TIME
           MOVE    WRK-TIME1(1:4)      TO  WRK-TIME2(1:4)
      *    受診内容ファイルを特定する
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    SPA-SRYKA           TO  UKE-SRYKA
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 976-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           PERFORM
                   UNTIL       FLG-UKETUKE     =    1
               IF     (UKE-KAIKEITIME      =   ZERO )
                 AND  (UKE-UKEID       NOT =   ZERO )
                   MOVE    WRK-TIME22          TO  UKE-KAIKEITIME
      *
                   MOVE    SMCNDATE-YMD        TO  UKE-UPYMD
                   MOVE    SMCNDATE-HMS        TO  UKE-UPHMS
      *
                   MOVE    SPA-OPID            TO  UKE-OPID
      *
                   MOVE    UKETUKE-REC         TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_uketuke"       TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0005"          TO  SPA-ERRCD
                       STRING  "UKETUKE UPD ERR KEY : "
                               UKE-KEY      DELIMITED  BY  SIZE
                                            INTO  ORCSCNYUIN-ERRMSG
                       END-STRING
                   END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 976-UKETUKE-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
        410-UKETUKE-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    退院処理（診療会計削除）処理
      *****************************************************************
       200-TAIIN-SYORI-SEC             SECTION.
      *
      *    同日再入院分
           IF      ORCSCNYUIN-DOUJITSUKBN  =   "1"
               MOVE    1                   TO  FLG-DOUJITU
           ELSE
               MOVE    ZERO                TO  FLG-DOUJITU
           END-IF
      *
           INITIALIZE                      TBL-NYU-SRYKA-AREA
           MOVE    ZERO                TO  IDX-K
      *    入院履歴判定
           MOVE    SPACE               TO  PTNYUINRRK-REC
           INITIALIZE                  PTNYUINRRK-REC
           MOVE    SPA-HOSPNUM         TO  PTNYUINRRK-HOSPNUM
           MOVE    SPA-PTID            TO  PTNYUINRRK-PTID
      *
           MOVE    PTNYUINRRK-REC      TO  MCPDATA-REC
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           ELSE
               MOVE    1               TO  FLG-PTNYUINRRK
               INITIALIZE                  PTNYUINRRK-REC
           END-IF
           MOVE    SPACE               TO  WRK-SRYKA
           PERFORM
                   UNTIL      (FLG-PTNYUINRRK      =   1 )  OR
                              (WRK-SRYKA       NOT =   SPACE)
      *        退院対象処理
               IF     (ORCSCNYUIN-TAIINYMD NOT =   SPACE 
                                           AND     "99999999" ) AND
                      (PTNYUINRRK-TAIINYMD     =   ORCSCNYUIN-TAIINYMD)
                   MOVE    PTNYUINRRK-NYUINKA  TO  WRK-SRYKA
                   ADD     1                   TO  IDX-K
                   MOVE    PTNYUINRRK-NYUINKA  TO  TBL-NYU-SRYKA
                                                           (IDX-K)
               END-IF
      *
               MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
               MOVE    "key33"             TO  MCP-PATHNAME
               PERFORM 910-PTNYUINRRK-READ-SEC
           END-PERFORM
           MOVE    "tbl_ptnyuinrrk"    TO  MCP-TABLE
           MOVE    "key33"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    ORCSCNYUIN-YMD      TO  JYURRK-SRYYMD
      *    退院日以降すべて削除
           MOVE    ALL "9"             TO  JYURRK-UPSRYYMD
      **** MOVE    "JYURRK-KEY14"      TO  WRK-PATH
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           MOVE    SPACE                TO  WRK-LST-SRYYMD
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *
               MOVE    ZERO                TO  FLG-OK2
               IF     (ORCSCNYUIN-YMD          =   JYURRK-SRYYMD)
                 AND  (FLG-DOUJITU             =   1            )
                   IF      JYURRK-RENNUM           =   2
                       MOVE    1                   TO  FLG-OK2
                   END-IF
               ELSE
                   MOVE    1                   TO  FLG-OK2
               END-IF
      *        受診履歴削除
               IF      FLG-OK2             =   1
      *
               MOVE    JYURRK-SRYYMD           TO  SPA-SRYYMD
      *        診療会計マスタの診療回数をクリアする
               PERFORM 4500112-TEISEI-SRYACCT-SEC
      *        診療科編集
               PERFORM VARYING     IDX-K   FROM    1   BY  1
                       UNTIL       IDX-K   >   10
                   IF      TBL-NYU-SRYKA (IDX-K)  =   JYURRK-SRYKA
                       MOVE    10              TO  IDX-K
                   ELSE
                       IF      TBL-NYU-SRYKA (IDX-K)   =   SPACE
                           MOVE    JYURRK-SRYKA    TO  TBL-NYU-SRYKA
                                                           (IDX-K)
                           MOVE    10              TO  IDX-K
                       END-IF
                   END-IF
               END-PERFORM
      *
      *        受診履歴削除
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      WRK-LST-SRYYMD      <   JYURRK-SRYYMD
                   MOVE    JYURRK-SRYYMD       TO  WRK-LST-SRYYMD
               END-IF
      *ver.4.7
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key14"             TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key14"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      ORCSCNYUIN-TAIINYMD =   SPACE OR  ALL "9"
      *        入院取消の時、診療科履歴の修正
               PERFORM VARYING     IDX-K   FROM    1   BY  1
                       UNTIL      (IDX-K   >   10   )  OR
                                  (TBL-NYU-SRYKA (IDX-K)  =   SPACE)
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    ORCSCNYUIN-YMD(1:6) TO  SKARRK-SRYYM
                   MOVE    TBL-NYU-SRYKA (IDX-K)   TO  SKARRK-SRYKA
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
                   IF      SKARRK-RC       NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                       STRING  "ORCSKARRK ERR "
                               SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                               INTO  ORCSCNYUIN-ERRMSG
                       END-STRING
                   END-IF
               END-PERFORM
           ELSE
      *        退院の時、最終受診日を編集する
               PERFORM 500-SRYKARRK-UPD-SEC
           END-IF
      *
      *    入院調剤料の自動削除
           MOVE    ORCSCNYUIN-YMD(1:6) TO  WRK-STR-YYMM
           IF      WRK-LST-SRYYMD      =   SPACE
               MOVE    WRK-STR-YYMM        TO  WRK-LST-YYMM
           END-IF
      *    最終受診年月＋１月を削除対象とする
           COMPUTE WRK-LST-MM          =   WRK-LST-MM  +   1
           IF      WRK-LST-MM          >   12
               COMPUTE WRK-LST-YY          =   WRK-LST-YY  +   1
               MOVE    1                   TO  WRK-LST-MM
           END-IF
           PERFORM
               UNTIL    WRK-STR-YYMM   >   WRK-LST-YYMM
      *        入院調剤料削除
               INITIALIZE                  ORCSCHOZAIAREA
               MOVE    "2"                 TO  ORCSCHO-KBN
               MOVE    SPA-PTID            TO  ORCSCHO-PTID
               MOVE    WRK-STR-YYMM        TO  ORCSCHO-SRYYM
               CALL    "ORCSCHOZAI"        USING
                                           SPA-AREA
                                           ORCSCHOZAIAREA
               COMPUTE WRK-STR-MM          =   WRK-STR-MM  +   1
               IF      WRK-STR-MM          >   12
                   COMPUTE WRK-STR-YY          =   WRK-STR-YY  +   1
                   MOVE    1                   TO  WRK-STR-MM
               END-IF
           END-PERFORM
      *
           MOVE    ZERO                TO  SPA-SRYKA
      *
           .
       200-TAIIN-SYORI-EXT.
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
      *    附加情報の上限回数設定
           IF     (FLG-TENSU           =   ZERO )  AND
                  (TNS-SRYCD(1:1)      =   "1"  )
               PERFORM 9101-TENSUPLUS-SYORI-SEC
           END-IF
      *
           .
       910-TENSU-READ-EXT.
           EXIT.
      *****************************************************************
      *    点数マスタ付加情報より編集処理
      *****************************************************************
       9101-TENSUPLUS-SYORI-SEC          SECTION.
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
                   MOVE    1                   TO  FLG-TENSUPLUS
                   INITIALIZE                      TENSUPLUS-REC
               END-IF
           ELSE
               MOVE    1                   TO  FLG-TENSUPLUS
               INITIALIZE                      TENSUPLUS-REC
           END-IF
           MOVE    "tbl_tensuplus"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    上限回数
           IF      TNSP-SANTEIRRKKBN   =   1
               MOVE    TNSP-SANTEIRRKKBN   TO  TNS-SANTEIRRKKBN
               MOVE    TNSP-JGNCNT         TO  TNS-JGNCNT
               MOVE    TNSP-JGNCNT1D       TO  TNS-JGNCNT1D
               MOVE    TNSP-JGNCNTERR      TO  TNS-JGNCNTERR
      *
               MOVE    TNSP-JGNCNTETCM     TO  TNS-JGNCNTETCM
               MOVE    TNSP-JGNCNTETC      TO  TNS-JGNCNTETC
           END-IF
      *
           .
       9101-TENSUPLUS-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター読込
      *****************************************************************
       915-SRYACT-NEXT-SEC         SECTION.
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
       915-SRYACT-NEXT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスター読込
      *****************************************************************
       930-SRYACCT-NEXT-SEC         SECTION.
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
       930-SRYACCT-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者マスター読込
      *****************************************************************
       940-PTINF-READ-SEC         SECTION.
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
                   MOVE    MCPDATA-REC        TO  PTINF-REC
                   MOVE    ZERO               TO  FLG-PTINF
               ELSE
                   MOVE    1                  TO  FLG-PTINF
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTINF
           END-IF
      *
           MOVE    "tbl_ptinf"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       940-PTINF-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    患者コメントマスター読込
      *****************************************************************
       950-PTCOM-READ-SEC         SECTION.
      *
           MOVE    PTCOM-REC           TO  MCPDATA-REC
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_ptcom"         TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 920-DBFETCH-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC        TO  PTCOM-REC
                   MOVE    ZERO               TO  FLG-PTCOM
               ELSE
                   MOVE    1                  TO  FLG-PTCOM
               END-IF
           ELSE
               MOVE    1              TO  FLG-PTCOM
           END-IF
      *
           MOVE    "tbl_ptcom"         TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           .
       950-PTCOM-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    システム管理マスタ読込
      *****************************************************************
       960-KANRIMST-READ-SEC         SECTION.
      *
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
      *****************************************************************
      *    受診履歴順読み
      *****************************************************************
       970-JYURRK-NEXT-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  JYURRK-REC 
               MOVE    ZERO                TO  FLG-JYURRK
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
      *
           .
       970-JYURRK-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科歴データ主キー検索
      *****************************************************************
       975-SRYKARRK-READ-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYKARRK-REC
               MOVE    ZERO                TO  FLG-SRYKARRK
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
               INITIALIZE                  SRYKARRK-REC
           END-IF
      *
           .
       975-SRYKARRK-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    保険組合せマスター読込
      *****************************************************************
       910-HKNCOMBI-READ-SEC         SECTION.
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
      *
      *****************************************************************
      *    受診内容データ読み込み
      *****************************************************************
       976-UKETUKE-NEXT-SEC           SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  UKETUKE-REC 
               MOVE    ZERO                TO  FLG-UKETUKE
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
      *
           .
       976-UKETUKE-NEXT-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為データ読込
      *****************************************************************
       940-WKSRYACT-READ-SEC          SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  WKSRYACT-REC
               MOVE    ZERO            TO  FLG-WKSRYACT
           ELSE
               INITIALIZE                  WKSRYACT-REC
               MOVE    1               TO  FLG-WKSRYACT
           END-IF
      *
           .
       940-WKSRYACT-READ-EXT.
           EXIT.
      *
      *****************************************************************
      *    入院履歴マスター読込
      *****************************************************************
       910-PTNYUINRRK-READ-SEC         SECTION.
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
      *
