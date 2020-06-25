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
       PROGRAM-ID.                 ORCSCGAIRAI.
      *****************************************************************
      *  システム名        : ＯＲＣＡ
      *  サブシステム名    : 診療行為入力
      *  コンポーネント名  : 外来用マスタ更新処理(K03,K08で使用）
      *  管理者            : 
      *  作成日付    作業者        記述
      *  03/12/22    NACL−多々納  新規作成
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  01.00.01    NACL-多々納  05/04/22  レセ電対応
      *  01.00.02    NACL-小豆沢  05/10/17  入金額ゼロでも入金方法を
      *                                     セットする
      *  01.00.03    NACL-多々納  05/11/28  MONFUNC 対応
      *  01.00.04    NACL-多々納  06/03/06  主科設定追加
      *  03.04.00    NACL-多々納  07/01/22  収納差額処理追加
      *  03.04.01    NACL-多々納  07/02/20  収納履歴マスタ削除追加
      *  03.04.02    NACL-多々納  07/02/28  複数保険に包括保険追加
      *  03.05.00    NACL-多々納  07/04/XX  グループ診療対応
      *  04.01.00    NACL-多々納  07/11/XX  公務災害・公害対応
      *                                     器材商品名コード対応
      *  04.02.00    NACL-多々納  08/03/XX  Ｈ２０年４月改正対応
      *  04.05.00    NACL-多々納  09/06/XX  数量小数５桁対応
      *  04.05.00    NACL-多々納  09/09/15  同日会計の上限エラーメッセー変更対応
      *  04.05.00    NACL-多々納  09/10/XX  換算値入力・多剤投与他対応
      *  04.06.00    NACL-多々納  10/09/10  分娩管理テーブル削除対応
      *  04.06.00    NACL-多々納  10/11/05  発行日変更対応
      *  04.07.00    NACL-多々納  11/08/XX  算定履歴付加テーブル削除対応
      *  04.08.00    NACL-多々納  13/01/11  診療回数テーブル１０対応
      *  04.08.00    NACL-多々納  13/07/XX  第三者行為対応
      *  04.08.00    NACL-多々納  14/05/27  一時ファイル変更
      *  04.08.00    NACL-多々納  14/08/22  外用薬剤回数チェック対応
      *  04.08.00    NACL-多々納  14/09/XX  Ｈ２６年１０月改正対応
      *  04.08.00    NACL-多々納  15/03/XX  Ｈ２７年０４月改正対応
      *  04.08.00    NACL-多々納  18/03/XX  Ｈ３０年０４月改正対応
      *  05.00.00    NACL-多々納  19/10/XX  削除ログ追加対応
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
           03  FLG-SYSKANRI        PIC 9(01).
           03  FLG-SYUNOU          PIC 9(01).
           03  FLG-WKSRYACT        PIC 9(01).
           03  FLG-SRYACT          PIC 9(01).
           03  FLG-SRYACCT         PIC 9(01).
           03  FLG-SYUMEI          PIC 9(01).
           03  FLG-PTINF           PIC 9(01).
           03  FLG-PTCOM           PIC 9(01).
           03  FLG-JYURRK          PIC 9(01).
           03  FLG-SRYKARRK        PIC 9(01).
           03  FLG-UKETUKE         PIC 9(01).
           03  FLG-TENSU           PIC 9(01).
           03  FLG-TENSUPLUS       PIC 9(01).
           03  FLG-SANTEI          PIC 9(01).
           03  FLG-PARA            PIC 9(01).
           03  FLG-SYUTTL          PIC 9(01).
           03  FLG-SRYACCTPLUS     PIC 9(01).
      *
           03  FLG-OK              PIC 9(01).
           03  FLG-CHK             PIC 9(01).
           03  FLG-ERR             PIC 9(01).
           03  FLG-ERR2            PIC 9(01).
           03  FLG-ARI             PIC 9(01).
           03  FLG-SINKI           PIC 9(01).
      *
           03  FLG-SYOSIN-DUMMY    PIC 9(01).
      *
           03  FLG-TOKTEI          PIC 9(01).
      *
           03  FLG-CHK-OK          PIC 9(01).
      *    複数科まとめ有無
           03  FLG-FUKU-FLG              PIC 9(01).
           03  FLG-SRYKA                 PIC 9(01).
      *    内分泌負荷試験
           03  FLG-NAIBUN                PIC 9(01).
      *    内服逓減あり
           03  FLG-GEN-ARI               PIC 9(01).
      *
           03  FLG-SRYKA-OK              PIC 9(01).
      *    同日初診
           03  FLG-DOUJITU               PIC 9(01).
      *    算定履歴付加チェック
           03  FLG-SANTEI-ARI            PIC 9(01).
      *    処理有
           03  FLG-SYORI-ARI            PIC 9(01).
      *
      *    添字領域
       01  IDX-AREA.
           03  IDX                 PIC 9(04).
           03  IDY                 PIC 9(04).
           03  IDX1                PIC 9(04).
           03  IDX2                PIC 9(02).
           03  IDX-P               PIC 9(04).
           03  IDX-KOUI            PIC 9(04).
           03  IDX-SIN             PIC 9(04).
           03  IDX-ATO             PIC 9(04).
           03  IDX-YAKUZAI         PIC 9(04).
           03  IDX-D               PIC 9(02).
      *
           03  IDX-1               PIC 9(04).
           03  IDX-2               PIC 9(02).
           03  IDX-3               PIC 9(02).
           03  IDX-A               PIC 9(04).
      *
           03  IDH                 PIC 9(04).
           03  IDH2                PIC 9(04).
           03  IDH3                PIC 9(04).
           03  IDXF                PIC 9(04).
           03  IDH-GAI             PIC 9(04).
      *
           03  IDW                 PIC 9(04).
           03  IDW2                PIC 9(04).
      *
           03  IDX-GEN4            PIC 9(04).
           03  IDX-GEN5            PIC 9(04).
           03  IDX-GEN6            PIC 9(04).
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
      *    初診算定日保存
           03  WRK-KARRK-SYOSINYMD2    PIC X(08).
      *    エラー
           03  ORCSCGAIRAI-ERRMSG      PIC X(80).
      *
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
      *
           03  WRK-SRYSURYO           PIC 9(07)V9(05).
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
      *
           03  WRK-PATH                    PIC X(64).
           03  WRK-MEIBAN                  PIC 9(03).
      *
           03  WRK-DENPNUM                 PIC 9(07).
           03  WRK-KA-DENPNUM              PIC 9(07).
           03  WRK-GRP-DENPNUM             PIC 9(07).
           03  WRK-GRP-RENNUM              PIC 9(02).
      *
      *    診療種別
           03  WRK-SRYSYUKBN              PIC X(03).
           03  WRK-MAE-SRYSYUKBN          PIC X(03).
      *    末日
           03  WRK-MATU                    PIC 9(02).
           03  WRK-NEXT-SRYYMD             PIC X(08).
      *
           03  WRK-DEL-RENNUM              PIC 9(01).
           03  WRK-JYURRK-SRYKA            PIC X(02).
           03  WRK-JYURRK-RENNUM           PIC 9(01).
           03  WRK-JYURRK-EDANUM           PIC 9(01).
           03  WRK-JYURRK-DOUJI-RENNUM     PIC 9(01).
           03  WRK-KAIKEI-RENNUM           PIC  9(03).
           03  WRK-MAE-RENNUM              PIC 9(01).
      *請求金額
           03  WRK-KON-SKYMONEY        PIC  9(07).
           03  WRK-KON-NYUKIN          PIC  9(07).
      *請求金額
           03  WRK-SKYMONEY            PIC  9(07).
      *入金合計額
           03  WRK-NYUKIN-TOTAL        PIC  9(07).
      *入金回数
           03  WRK-NYUKIN-KAISU        PIC  9(07).
      *伝票番号枝番
           03  WRK-DENPEDANUM          PIC  9(02).
      *入金連番
           03  WRK-NYUKINRENNUM        PIC  9(02).
      *伝票状態区分
           03  WRK-DENPJTIKBN          PIC  X(01).
      *
           03  WRK-ERRCD               PIC X(04).
      *
           03  WRK-RENNUM-KEY          PIC 9(01).
           03  WRK-RENNUM              PIC 9(01).
      *    訂正前の連番
           03  WRK-TEISEI-AREA.
               05  WRK-TEISEI-TBL      OCCURS   15.
                   07  WRK-TEI-SRYKA           PIC X(02).
                   07  WRK-TEI-RENNUM          PIC 9(01).
                   07  WRK-TEI-KAIKEI-RENNUM   PIC 9(01).
                   07  WRK-TEI-DOUJI-RENNUM    PIC 9(01).
      *
           03  TBL-SRYKA-AREA.
               05  TBL-SRYKA-OCC           OCCURS   15.
                   07  TBL-SRYKA               PIC X(02).
                   07  TBL-SRYKA-MAX           PIC 9(02).
                   07  TBL-SRYKA-REN           PIC 9(02).
      *
           03  WRK-SYUNOU-MAX          PIC 9(03).
      *
           03  WRK-SRYKA-MAX           PIC 9(02).
           03  WRK-SRYKA-REN           PIC 9(02).
           03  WRK-MAE-SRYKA           PIC X(02).
           03  WRK-SRYKA               PIC X(02).
           03  WRK-HKNCOMBI            PIC X(04).
           03  WRK-DOUJI-RENNUM        PIC 9(01).
      *
      *    収納の入金額
           03  WRK-SYUTTL-G.
               05  WRK-TTL-NYUKIN-TOTAL    PIC 9(07)
                                           OCCURS   15.
      *    入金合計額
           03  WRK-SUM-NYUKIN          PIC  9(07).
      *    受診時間
           03  WRK-TIME1.
               05  WRK-TIME11      PIC 9(08).
           03  WRK-TIME2.
               05  WRK-TIME22      PIC 9(04).
      *    保険組合せ保存
           03  WRK-HZN-HKNCOMBINUM     PIC X(04).
           03  WRK-HZN-HKNKBN          PIC X(01).
           03  WRK-HZN-SRYKA           PIC X(02).
      *    診療行為附加情報設定（科毎）
           03  WRK-ACTP-AREA.
               05  WRK-ACTP-ZAINUM-G       OCCURS   30.
                   07  WRK-ACTP-SRYKA      PIC X(02).
                   07  WRK-ACTP-HKNCOMBI   PIC X(04).
      *H26.10      診療区分毎
                   07  WRK-ACTP-SRYKBN     PIC X(02).
                   07  WRK-ACTP-MAX        PIC 9(04).
                   07  WRK-ACTP-ZAINUM     PIC 9(08)   OCCURS   50.
               05  IDX-GEN                 PIC 9(04).
               05  IDX-GEN2                PIC 9(04).
      *H27.4
      *    診療行為附加情報設定（科毎）３０日逓減
           03  WRK-ACTP30-AREA.
               05  WRK-ACTP30-ZAINUM-G       OCCURS   30.
                   07  WRK-ACTP30-SRYKA      PIC X(02).
                   07  WRK-ACTP30-HKNCOMBI   PIC X(04).
                   07  WRK-ACTP30-SRYKBN     PIC X(02).
                   07  WRK-ACTP30-MAX        PIC 9(04).
                   07  WRK-ACTP30-ZAINUM     PIC 9(08)   OCCURS   50.
               05  IDX-GEN3                 PIC 9(04).
               05  IDX-GEN32                PIC 9(04).
      *
      *    剤テーブル領域
       01  WRK-TBL-ZAINUM-AREA.
           03  WRK-TBL-ZAINUM-G            OCCURS   150.
               05  WRK-TBL-ZAINUM          PIC 9(08).
           03  WRK-TBL-IDX                 PIC 9(04).
           03  FLG-ZAINUM                  PIC 9(01).
      *
      *R01.10
      *    削除ログ内容
       01  WRK-PUSHINFO-AREA.
           03  WRK-PUSHINFO.
               05  WRK-PUSH-HKNCOMBI       PIC X(04).
               05  WRK-FILLER1             PIC X(01).
               05  WRK-PUSH-SRYKA          PIC X(02).
               05  WRK-FILLER2             PIC X(01).
               05  WRK-PUSH-DRCD           PIC X(05).
               05  WRK-FILLER3             PIC X(01).
               05  WRK-PUSH-DENPNUM        PIC X(07).
      *    日付編集
           03  WRK-HEN-DATE.
               05  WRK-HEN-YY          PIC X(04).
               05  WRK-HEN-H1          PIC X(01).
               05  WRK-HEN-MM          PIC X(02).
               05  WRK-HEN-H2          PIC X(01).
               05  WRK-HEN-DD          PIC X(02).
      *    時間編集
           03  WRK-THMS.
               05  WRK-THH             PIC X(02).
               05  WRK-TMM             PIC X(02).
               05  WRK-TSS             PIC X(02).
           03  WRK-HEN-TIME.
               05  WRK-HEN-THH         PIC X(02).
               05  WRK-HEN-T1          PIC X(01).
               05  WRK-HEN-TMM         PIC X(02).
               05  WRK-HEN-T2          PIC X(01).
               05  WRK-HEN-TSS         PIC X(02).
      *
      *
      *    診療会計附加情報マスタ−（内服逓減）
       01  TBL-SRYACCTPLUS-AREA.
           02  TBL-SRYACCTPLUS-REC   OCCURS   30.
           COPY    "CPSRYACCTPLUS.INC"  REPLACING
                                     //ACCTP-// BY //TBL-ACCTP-//.
           03  TBL-ACCTP-HKNCOMBI      PIC X(04).
           03  TBL-ACCTP-SRYKBN        PIC X(02).
      *H27.4
      *    診療会計附加情報マスタ−（３０日逓減）
       01  TBL-SRYACCTPLUS30-AREA.
           02  TBL-SRYACCTPLUS30-REC   OCCURS   30.
           COPY    "CPSRYACCTPLUS.INC"  REPLACING
                                     //ACCTP-// BY //TBL-ACCTP30-//.
           03  TBL-ACCTP30-HKNCOMBI      PIC X(04).
           03  TBL-ACCTP30-SRYKBN        PIC X(02).
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
      *H26.10
      * 向精神薬多剤投与減算コード
       01  CONS-TAZAI-SRYCD        PIC X(09)   VALUE    "630010005".
      * （精減）
       01  CONS-SGEN-SRYCD         PIC X(09)   VALUE    "820000166".
      *H28.4
      * （湿布減）
       01  CONS-SPCM-SRYCD         PIC X(09)   VALUE    "820000047".
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
           COPY    "CPSK1041.INC".
      *
      *
      *    患者マスタ−
       01  PTINF-REC.
           COPY    "CPPTINF.INC".
      *
      *    収納マスタワーク
       01  SYUNOU-REC.
           COPY    "CPSYUNOU.INC".
      *
      *    診療行為マスター
       01  SRYACT-REC.
           COPY    "CPSRYACT.INC".
      *
      *    収納明細マスタ−
       01  SYUMEI-REC.
           COPY    "CPSYUMEI.INC".
      *
      *    収納合計マスタ−
       01  SYUTTL-REC.
           COPY    "CPSYUTOTAL.INC".
      *
      *    収納履歴マスタ
       01  SYURRK-REC.
           COPY    "CPSYURRK.INC".
      *
      *    診療会計マスタ−
       01  SRYACCT-REC.
           COPY    "CPSRYACCT.INC".
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
      *    診療科履歴
       01  SRYKARRK-REC.
           COPY    "CPSRYKARRK.INC".
      *
      *    ワーク診療行為マスタ−
       01  WKSRYACT-REC.
           COPY    "CPWKSRYACT.INC".
      *DDD 附加情報を追加
      *DDD 03  WKSRYACTPLUS-REC.
      *    COPY    "CPWKSRYACTPLUS.INC".
      *
      *    受診内容
       01  UKETUKE-REC.
           COPY    "CPUKETUKE.INC".
      *
      *    算定履歴
       01  SANTEI-REC.
           COPY    "CPSANTEI.INC".
      *
      *    点数マスタ−
      *01  TENSU-REC.
           COPY    "CPTENSU.INC".
      *    点数マスタ付加コード
        01 TENSUPLUS-REC.
           COPY    "CPTENSUPLUS.INC".
      *
      *    分娩管理テーブル
       01  BUNBEN-REC.
           COPY    "CPBUNBEN.INC".
      *
      *    診療行為マスタワーク
       01  LNK-WKSRYACT-AREA.
           02  LNK-WKSRYACT-REC      OCCURS   50.
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
      *    保存用収納ワーク
       01  WRK-SYUNOU-REC.
           COPY    "CPSYUNOU.INC"  REPLACING
                                     //SYU-// BY //WRK-SYU-//.
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
      *R01.10
      *    削除ログ
       01  PUSHINFO-REC.
           COPY    "CPPUSH-INFO.INC".
      *
      *R01.10
      *    ＵＩＤ取得用エリア
       01  UIDIO-AREA.
           03  C-RET               PIC X(2).
           03  C-UID               PIC X(36).
       01  UIDIO-ST                PIC 9(2).
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
      **** COPY    "CPORCSC97.INC".
      *
      *   算定履歴更新サブ
           COPY    "CPORCSSANTEI.INC".
      *
      *   診療科算定履歴更新サブ
           COPY    "CPORCSKARRK.INC".
      *
      *    診療会計剤チェックパラメタ
           COPY    "CPORCSCZAICHK.INC".
      *
      *    マシン日付取得サブ
           COPY    "CPORCSMCNDATE.INC".
      *
      *    日報キー取得サブ
           COPY    "CPORCSDAILYKEY.INC".
      *
      *    ＳＰＡパラメタ（ORCSSANFUKA.CBL 用のDAMMY)
       01  SPA-SCR-REC.
           COPY    "CPK02SPASCR.INC".
      *
      *    ＤＢ検索
           COPY    "MCPDATA.INC".
      *****COPY    "CPORCMCP.INC".
      *
      *    診療行為用共通パラメタ
           COPY    "K01COMMON-SPA".
      *    内分泌負荷試験更新パラメタ
           COPY    "CPORCSSANFUKA.INC".
      *
      *    レセプト作成管理マスタ更新サブ
           COPY    "CPORCSRECEUPD.INC".
      *
      *    主情報アクセス用パラメタ
           01  ORCSSYUACCAREA.
               COPY    "CPORCSSYUACC.INC".
      *    収納更新差額取得パラメタ
           COPY    "CPORCSS002.INC".
      *
           COPY    "MCPAREA".
           COPY    "ORCA-SPA".
      *****COPY    "ORCA-DBPATH".
      *
      *
      *****************************************************************
      *    連絡　領域
      *****************************************************************
        LINKAGE                     SECTION.
      *
      *    スパ領域
           COPY    "COMMON-SPA".
      *    診療行為入院更新パラメタ領域
           COPY    "CPORCSCGAIRAI.INC".
      *
       PROCEDURE                   DIVISION    USING
           SPA-AREA
           ORCSCGAIRAIAREA
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
           MOVE    SPACE               TO  ORCSCGAIRAI-ERRMSG
      *
      *    更新日取得
           INITIALIZE                  ORCSMCNDATEAREA
           CALL    "ORCSMCNDATE"       USING
                                       ORCSMCNDATEAREA
      *
           EVALUATE    ORCSCGAIRAI-KBN
      *        診療行為作成
               WHEN    "1"
                   PERFORM 100-SAKUSEI-SYORI-SEC
      *        削除
               WHEN    "2"
                   PERFORM 300-DELETE-SYORI-SEC
      *        初診算定日のみ設定
               WHEN    "3"
                   PERFORM 400-SYOSIN-SYORI-SEC
           END-EVALUATE
      *
           IF     (ORCSCGAIRAI-ERRMSG  NOT =   SPACE ) AND
                  (SPA-ERRCD           NOT =   SPACE )
               MOVE    ORCSCGAIRAI-ERRMSG  TO  SPA-ERRMSG2
           END-IF
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
      *    発行日
           IF      ORCSCGAIRAI-HAKYMD  =   SPACE
               MOVE    SPA-SYSYMD          TO  ORCSCGAIRAI-HAKYMD
           END-IF
      *
      *    初診料算定のみの時初診料を算定する
           IF      SPA-SYOSIN-YMD  NOT =   SPACE
               PERFORM 400-SYOSIN-SYORI-SEC
           END-IF
      *
      *    訂正時、受診履歴等の削除
           IF      SPA-SYORIFLG         =   1
               PERFORM 1001-TEISEI-SYORI-SEC
           END-IF
      *    収納から包括保険の件数を削除
           MOVE    ZERO                TO  WRK-SYUNOU-MAX
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSCGAIRAI-SYUNOU-MAX)
               MOVE    ORCSCGAIRAI-SYUNOU-REC(IDX) TO  SYUNOU-REC
               IF     (SYU-HKNCOMBINUM     NOT =   "9999" )
                   ADD     1               TO  WRK-SYUNOU-MAX
                END-IF
           END-PERFORM
      *
           INITIALIZE                  WRK-SYUTTL-G
      *    科毎の保険数を設定する
           INITIALIZE                  TBL-SRYKA-AREA
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   15  )  OR
                              (SPA-TBL-HKNCOMBI(IDH)   =   SPACE)
               PERFORM VARYING     IDH2    FROM    1   BY  1
                       UNTIL      (IDH2    >   10  )  OR
                                  (SPA-TBL-SRYKA(IDH IDH2) =   SPACE)
                   MOVE    SPA-TBL-SRYKA(IDH IDH2) TO  WRK-SRYKA
                   PERFORM VARYING     IDH3    FROM    1   BY  1
                           UNTIL       IDH3    >   15
                       IF     TBL-SRYKA (IDH3)     =   SPACE
                           MOVE    WRK-SRYKA   TO  TBL-SRYKA (IDH3)
                       END-IF
                       IF      TBL-SRYKA (IDH3)    =   WRK-SRYKA
                           ADD     1           TO  TBL-SRYKA-MAX(IDH3)
                           MOVE    15          TO  IDH3
                       END-IF
                   END-PERFORM
               END-PERFORM
           END-PERFORM
      *    保険毎に処理する
           MOVE    ZERO                TO  WRK-GRP-DENPNUM
           MOVE    ZERO                TO  WRK-GRP-RENNUM
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-DENPNUM         TO  WRK-GRP-DENPNUM
           END-IF
           INITIALIZE                  WRK-ACTP-AREA
           INITIALIZE                  TBL-SRYACCTPLUS-AREA
           INITIALIZE                  WRK-ACTP30-AREA
           INITIALIZE                  TBL-SRYACCTPLUS30-AREA
      *
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   15  )  OR
                              (SPA-TBL-HKNCOMBI(IDH)   =   SPACE)
                          OR  (SPA-ERRCD   NOT =   SPACE )
               MOVE    ZERO                TO  WRK-KA-DENPNUM
               MOVE    SPA-TBL-HKNCOMBI (IDH)  TO  WRK-HKNCOMBI
               IF     (SPA-TBL-SRYKA(IDH 2)    =   SPACE ) OR
                      (SPA-KAGRPFLG        NOT =   "0"    )  OR
                      (WRK-HKNCOMBI            =   "9999" )
                   MOVE    ZERO                TO  FLG-FUKU-FLG
               ELSE
      *            複数科まとめあり
                   MOVE    1                   TO  FLG-FUKU-FLG
               END-IF
               PERFORM VARYING     IDH2    FROM    1   BY  1
                       UNTIL      (IDH2    >   10  )  OR
                                  (SPA-TBL-SRYKA(IDH IDH2) =   SPACE)
                              OR  (SPA-ERRCD   NOT =   SPACE )
                   MOVE    SPA-TBL-SRYKA(IDH IDH2) TO  WRK-SRYKA
      *
                   MOVE    SPACE               TO  WRK-KARRK-SYOSINYMD2
                   PERFORM 1001-DBKOUSIN-SYORI-SEC
               END-PERFORM
               IF      FLG-FUKU-FLG            =   1
                   MOVE    ZERO                TO  WRK-SRYKA
      *            複数科まとめ収納作成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 1004-DBKOUSIN-SYUTTL-SEC
                   END-IF
               END-IF
           END-PERFORM
      *    内服７種類逓減の診療会計附加情報追加
           IF      SPA-ERRCD           =   SPACE
               PERFORM 4504-SRYACCTPLUS-GENINS-SEC
           END-IF
      *!!!!
           IF     (SPA-SYORIFLG        =   1    )
              AND (SPA-ERRCD           =   SPACE)
      *        リハビリ開始日の履歴付加削除
               PERFORM 45005-SANTEIPLUS-DEL-SEC
           END-IF
      *!!!!
      *
           IF     (SPA-SYORIFLG        =   1    )
              AND (SPA-ERRCD           =   SPACE)
      *        訂正の時、前回の削除分を削除する
               PERFORM 45004-ALL-JYURRK-UPD-SEC
           END-IF
      *!!!
      *    明細の登録なしはＤＢ異常終了とする（収納が残る為）
           IF     (SPA-ERRCD       NOT =   SPACE)
             OR   (FLG-SYORI-ARI       =   ZERO )
               MOVE    SPACE               TO  MCPDATA-REC
               MOVE    "DBSELECT"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "abort"             TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
               IF      SPA-ERRCD           =   SPACE
                   MOVE    "0051"              TO  SPA-ERRCD
               END-IF
           END-IF
      *!!!
      *
           IF      SPA-ERRCD           =   SPACE
      *        ワーク診療行為マスタ−が存在した時、削除する
               PERFORM 45002-WKSRYACT-DEL-SEC
      *        受診内容データに時間を設定し、更新する
               IF     (SPA-SYORIFLG        =   ZERO )
                  AND (SPA-SYSYMD          =   SPA-SRYYMD)
                   PERFORM 45003-UKETUKE-SEC
               END-IF
           END-IF
      *
      *H25.5
      * 訂正時のみ
      *    レセプト作成管理マスタ更新処理
           IF     (SPA-SYORIFLG        =   1    )
             AND  (SPA-HKNCOMBINUM NOT =   "9999")
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 3009-RECEUPD-UPDATE-SEC
           END-IF
      *
           IF      SPA-ERRCD           =   SPACE
      *        主科設定
               PERFORM 1009-ORCSSYUKAMAIN-SEC
           END-IF
           IF      FLG-NAIBUN          =   1
             AND   SPA-ERRCD           =   SPACE
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
           .
       100-SAKUSEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    主科設定処理
      *****************************************************************
       1009-ORCSSYUKAMAIN-SEC             SECTION.
      *
      *    包括まとめ入力以外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
               INITIALIZE                      ORCSSYUACCAREA
               MOVE    "03"                TO  LNK-SYUACC-I-KBN
      *        診療年月
               MOVE    SPA-SRYYMD(1:6)     TO  LNK-SYUACC-I-SRYYM
               CALL    "ORCSSYUKAMAIN"     USING
                                           SPA-AREA
                                           ORCSSYUACCAREA
           END-IF
           .
      *
       1009-ORCSSYUKAMAIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴作成処理
      *****************************************************************
       1003-SRYKARRK-SAKUSEI-SEC             SECTION.
      *
      *    包括まとめ入力以外
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
      *        診療科履歴判定
               PERFORM 45009-SRYKARRK-SYOKI-SEC
               IF     (SPA-SYORIFLG        =   ZERO )  OR
                      (FLG-SRYKARRK        =   1    )
      *            新規か訂正で診療科履歴なしの時
      *            診療科履歴作成処理
                   PERFORM 45010-SRYKARRK-SYORI-SEC
               ELSE
      *            診療科履歴変更
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
                   MOVE    WRK-SRYKA           TO  SKARRK-SRYKA
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
                   IF      SKARRK-RC       NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                       STRING  "ORCSKARRK ERR "
                               SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                               INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-IF
           .
       1003-SRYKARRK-SAKUSEI-EXT.
           EXIT.
      *
      *****************************************************************
      *    各マスタ更新処理
      *****************************************************************
       1001-DBKOUSIN-SYORI-SEC             SECTION.
      *
      *    科毎の連番
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL      (IDH3    >   15  )   OR
                              (TBL-SRYKA (IDH3)    =   SPACE)
               IF     TBL-SRYKA (IDH3)     =   WRK-SRYKA
                   ADD     1               TO  TBL-SRYKA-REN(IDH3)
                   MOVE    TBL-SRYKA-MAX (IDH3)
                                           TO  WRK-SRYKA-MAX
                   MOVE    TBL-SRYKA-REN (IDH3)
                                           TO  WRK-SRYKA-REN
               END-IF
           END-PERFORM
      *
           MOVE    ZERO            TO  WRK-RENNUM
           MOVE    ZERO            TO  WRK-KAIKEI-RENNUM
           MOVE    ZERO            TO  WRK-DOUJI-RENNUM
      *    訂正の時、科毎の連番を決定する
           IF      SPA-SYORIFLG        =   1
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   15  )  OR
                                  (WRK-TEI-SRYKA (IDX) =   SPACE)
                   IF      WRK-TEI-SRYKA(IDX)  =   WRK-SRYKA
                       MOVE    WRK-TEI-RENNUM (IDX)
                                           TO  WRK-RENNUM
                       MOVE    WRK-TEI-KAIKEI-RENNUM (IDX)
                                           TO  WRK-KAIKEI-RENNUM
                       MOVE    WRK-TEI-DOUJI-RENNUM (IDX)
                                           TO  WRK-DOUJI-RENNUM
                       IF      WRK-SRYKA-MAX   >   ZERO
                           MOVE    ZERO        TO  WRK-DOUJI-RENNUM
                       END-IF
                   END-IF
               END-PERFORM
           END-IF
      *
      *    MOVE    SPACE               TO  SYUNOU-REC
      *    INITIALIZE                  SYUNOU-REC
      *    MOVE    ZERO                TO  IDX-KOUI
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    OPEN    INPUT               PARA-FILE
      *    PERFORM 980-PARA-READ-SEC
      *
      *    PERFORM
      *        UNTIL   FLG-PARA        NOT =   ZERO 
      *        IF      PARA-FILEMEI        =   "SYUNOU"
      *            パラメタの収納データ読込
      *            MOVE    PARA-DATA-REC       TO  SYUNOU-REC
      *            MOVE    1                   TO  FLG-PARA
      *        ELSE
      *            PERFORM 980-PARA-READ-SEC
      *        END-IF
      *    END-PERFORM
      *
      **** CLOSE                        PARA-FILE
      *
           MOVE    ZERO                TO  IDH-GAI
           MOVE    ZERO                TO  FLG-OK
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSCGAIRAI-SYUNOU-MAX) OR
                              (FLG-OK  =   1    )
               MOVE    ORCSCGAIRAI-SYUNOU-REC(IDX) TO  SYUNOU-REC
               IF     (SYU-SRYKA       =   WRK-SRYKA )  AND
                      (SYU-HKNCOMBINUM =   WRK-HKNCOMBI)
                   MOVE    1               TO  FLG-OK
                   MOVE    IDX             TO  IDH-GAI
                END-IF
           END-PERFORM
           IF      FLG-OK              =   1
      *        収納マスタを更新する
               PERFORM 1002-SYUNOU-OUT-SEC
               MOVE    SYUNOU-REC          TO  ORCSCGAIRAI-SYUNOU-REC
                                                             (IDH-GAI)
           ELSE
      *        収納がない場合、エラー
               MOVE    "0051"              TO  SPA-ERRCD
           END-IF
      *
      *    剤番号テーブル
           INITIALIZE                  WRK-TBL-ZAINUM-AREA
           IF      SPA-ERRCD           =   SPACE
      *        受診履歴初期処理
               PERFORM 1003-JYURRK-SYORI1-SEC
           END-IF
           MOVE    SPACE               TO  WRK-SRYYMD
      *
      *    診療行為マスター更新
           MOVE    ZERO                TO  FLG-PARA
           MOVE    SPACE               TO  LNK-WKSRYACT-AREA
           MOVE    ZERO                TO  IDX-KOUI
      *
      *    MOVE    SPA-TERMID          TO  FILE-TERMID2
      *    MOVE    SPA-HOSPNUM         TO  FILE-HOSPNUM2
      *    OPEN    INPUT               PARA-FILE
      *    IF      STS-PARA        NOT =   ZERO
      *        DISPLAY "ORCSGAIARI PARA STS-PARA:" STS-PARA
      *    END-IF
      *****PERFORM 980-PARA-READ-SEC
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
           PERFORM
                   UNTIL      (FLG-PARA   =    1  )  OR
                              (SPA-ERRCD  NOT =   SPACE)
      *!!!
      *        対象有
               MOVE    1                   TO  FLG-SYORI-ARI
      *!!!
               MOVE    PARA-DATA-REC       TO  WRK-WKSRYACT-REC
               IF     (WRK-WKSRY-SRYKA     =   WRK-SRYKA)  AND
                      (WRK-WKSRY-HKNCOMBI  =   WRK-HKNCOMBI) AND
                      (WRK-WKSRY-SRYCD (1)(1:1)
                                           NOT =  "$"  AND "#" )
      *
                   IF     (IDX-KOUI        >   ZERO  )  AND
                          (WRK-WKSRY-ZAINUM     NOT =
                                           LNK-WKSRY-ZAINUM (IDX-KOUI))
      *                剤毎に処理を行う
                       PERFORM 4501-SRYACT-KOU-SEC
                       IF      SPA-ERRCD       =   SPACE
                           PERFORM 45001-JYURRK-SYORI2-SEC
                       END-IF
                       MOVE    SPACE           TO  LNK-WKSRYACT-AREA
                       MOVE    ZERO            TO  IDX-KOUI
                   END-IF
                   ADD     1               TO  IDX-KOUI
                   MOVE    PARA-DATA-REC   TO  LNK-WKSRYACT-REC
                                                            (IDX-KOUI)
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
           IF     (SPA-ERRCD       =   SPACE ) AND
                  (IDX-KOUI        >   ZERO  )
               PERFORM 4501-SRYACT-KOU-SEC
               IF      SPA-ERRCD       =   SPACE
                   PERFORM 45001-JYURRK-SYORI2-SEC
               END-IF
               MOVE    SPACE           TO  LNK-WKSRYACT-AREA
               MOVE    ZERO            TO  IDX-KOUI
               MOVE    ZERO            TO  WRK-ZAINUM
           END-IF
      *
           IF      SPA-ERRCD       =   SPACE
      *        受診履歴作成終了処理
               PERFORM 45001-JYURRK-SYORI3-SEC
           END-IF
      *    診療科履歴作成
           IF    ( SPA-ERRCD           =   SPACE ) AND
                 ( WRK-SRYKA-REN       =   1 )
               PERFORM 1003-SRYKARRK-SAKUSEI-SEC
           END-IF
      *
           .
       1001-DBKOUSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ更新処理
      *****************************************************************
       1002-SYUNOU-OUT-SEC             SECTION.
      *
           IF     (SPA-SYORIFLG        =   ZERO ) OR
                  (SYU-DENPNUM         =   ZERO )
      *        システム管理より、伝票番号を取得する
               PERFORM 45001-DENPNUM-SET-SEC
           ELSE
               MOVE    SYU-DENPNUM     TO  WRK-DENPNUM
           END-IF
      *    主の伝票番号
           IF      WRK-GRP-DENPNUM     =   ZERO
               MOVE    WRK-DENPNUM         TO  WRK-GRP-DENPNUM
           END-IF
      *    複数科の伝票番号
           IF      WRK-KA-DENPNUM      =   ZERO
               MOVE    WRK-DENPNUM         TO  WRK-KA-DENPNUM
           END-IF
      *    伝票番号を編集する
      **** MOVE    WRK-DENPNUM         TO  ORCSCGAIRAI-DENPNUM
      *
           IF      SPA-ERRCD           =   SPACE
      *        包括保険の場合、伝票番号のみ取得
               IF     (SPA-HKNCOMBINUM     =   "9999" )
                  OR  (WRK-HKNCOMBI        =   "9999" )
                   MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *            まとめ入力伝票番号
                   IF      ORCSCGAIRAI-SYUNOU-MAX  >   1
                       ADD     1               TO  WRK-GRP-RENNUM
                   END-IF
               ELSE
                   PERFORM 10021-SYUNOU-SYORI-SEC
               END-IF
           END-IF
      *
           .
       1002-SYUNOU-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納マスタ更新処理
      *****************************************************************
       10021-SYUNOU-SYORI-SEC             SECTION.
      *
      *
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *    伝票番号発行日
      *!!! MOVE    SPA-SYSYMD          TO  SYU-DENPPRTYMD
           MOVE    ORCSCGAIRAI-HAKYMD  TO  SYU-DENPPRTYMD
      *    ドクター
      *!   MOVE    SPA-DRCD(2:4)       TO  SYU-DRCD
      *    調整金
      *!   MOVE    ORCSCGAIRAI-CHOSEI  TO  SYU-CHOSEI
      *    請求書作成区分
           MOVE    ORCSCGAIRAI-HAKFLG  TO  SYU-SKYKUKBN
      *    請求額
      *!   COMPUTE SYU-SKYMONEY        =   ORCSCGAIRAI-KON-SKYMONEY
           COMPUTE SYU-SKYMONEY        =   SYU-SKYMONEY
                                       +   SYU-CHOSEI
           MOVE    SYU-SKYMONEY        TO  WRK-KON-SKYMONEY
      *    入金額
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-KON-NYUKIN
      **
      *    返金充当分の加算
           IF      ORCSCGAIRAI-SYU-HENKIN (IDH-GAI) >  ZERO
               COMPUTE SYU-NYUKIN-TOTAL    =   SYU-NYUKIN-TOTAL
                                           +   ORCSCGAIRAI-SYU-HENKIN
                                                       (IDH-GAI)
           END-IF
      *!***MOVE    ORCSCGAIRAI-NYUKIN  TO  SYU-NYUKIN-TOTAL
      *    入金回数
           MOVE    1                   TO  SYU-NYUKIN-KAISU
      *    伝票状態区分
           MOVE    SYU-DENPJTIKBN      TO  WRK-DENPJTIKBN
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SYU-DENPJTIKBN
           ELSE
               IF      SYU-SKYMONEY        <=  SYU-NYUKIN-TOTAL
                   MOVE    "1"                 TO  SYU-DENPJTIKBN
               ELSE
                   MOVE    "2"                 TO  SYU-DENPJTIKBN
               END-IF
           END-IF
      *    継続区分判定
           IF      SPA-SYORIFLG        =   ZERO
               IF      SPA-CONTKBN         =   "1"
                   MOVE    "7"                 TO  SYU-DENPJTIKBN
                   IF      SPA-DOUJI-DENPNUM   =   ZERO
                       MOVE    SYU-DENPNUM     TO  SPA-DOUJI-DENPNUM
                   END-IF
               END-IF
      *        継続区分
               MOVE    SPA-CONTKBN         TO  SYU-CONTKBN
           ELSE
      *        継続区分
               IF      SPA-CONTKBN         =   SYU-CONTKBN
                   MOVE    SYU-DOUJI-DENPNUM   TO  SPA-DOUJI-DENPNUM
                   IF      SPA-CONTKBN         =   "1"
                       IF      WRK-DENPJTIKBN      =   "7"
                           MOVE    "7"             TO  SYU-DENPJTIKBN
                       END-IF
                   END-IF
               ELSE
                   EVALUATE    SPA-CONTKBN
      *                継続区分がなくなった時
                       WHEN    "0"
                           MOVE    ZERO        TO  SPA-DOUJI-DENPNUM
      *                継続区分に変更
                       WHEN    "1"
                           MOVE    "7"         TO  SYU-DENPJTIKBN
                           MOVE    SYU-DENPNUM TO  SPA-DOUJI-DENPNUM
                   END-EVALUATE
               END-IF
      *        継続区分
               MOVE    SPA-CONTKBN         TO  SYU-CONTKBN
           END-IF
      *
      *    院外処方区分
           MOVE    SPA-INGAIKBN        TO  SYU-INGAISHOHOKBN
      *    同時診療伝票番号
           MOVE    SPA-DOUJI-DENPNUM   TO  SYU-DOUJI-DENPNUM
      *
      *    複数科まとめ編集
      *    IF      ORCSCGAIRAI-SYORI   =   ZERO
      *        訂正の時はそのまま
      *        IF      SPA-SYORIFLG        =   ZERO
      *            MOVE    ZERO                TO  SYU-FUKU-DENPNUM
      *            MOVE    SPACE               TO  SYU-FUKU-KBN
      *        END-IF
      *    ELSE
      *        PERFORM 45011-SYUNOU-FUKU-SEC
      *****END-IF
           IF     (SPA-SYORIFLG        =   1  )  AND
                  (SYU-FUKU-DENPNUM    NOT =   ZERO ) AND
                  (SYU-GRP-DENPNUM         =   ZERO )
      *        複数科一括入力以前の時はそのまま
               CONTINUE
           ELSE
               IF      FLG-FUKU-FLG            =   1
      *            複数科まとめ識別伝票番号 
                   MOVE    WRK-KA-DENPNUM      TO  SYU-FUKU-DENPNUM
                   MOVE    "1"                 TO  SYU-FUKU-KBN
                   IF     (IDH2                =   10  )  OR
                          (SPA-TBL-SRYKA(IDH IDH2 + 1)
                                               =   SPACE )
                       MOVE    "2"                 TO  SYU-FUKU-KBN
                   END-IF
               ELSE
                   MOVE    ZERO                TO  SYU-FUKU-DENPNUM
                   MOVE    SPACE               TO  SYU-FUKU-KBN
               END-IF
           END-IF
      *    まとめ入力伝票番号
           IF      ORCSCGAIRAI-SYUNOU-MAX  >   1
               MOVE    WRK-GRP-DENPNUM     TO  SYU-GRP-DENPNUM
               ADD     1                   TO  WRK-GRP-RENNUM
               MOVE    WRK-GRP-RENNUM      TO  SYU-GRP-RENNUM
      *        発行方法
               MOVE    ORCSCGAIRAI-HAKHOUFLG   TO  SYU-GRP-HAKHOUFLG
      *        包括保険以外は１件の時
               IF      WRK-SYUNOU-MAX      =   1
                   MOVE    ZERO                TO  SYU-GRP-DENPNUM
                   MOVE    ZERO                TO  SYU-GRP-RENNUM
                   MOVE    ZERO                TO  SYU-GRP-HAKHOUFLG
               END-IF
           ELSE
               MOVE    ZERO                TO  SYU-GRP-DENPNUM
               MOVE    ZERO                TO  SYU-GRP-RENNUM
      *        発行方法
               MOVE    ZERO                TO  SYU-GRP-HAKHOUFLG
           END-IF
      *
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           IF      SPA-SYORIFLG        =   ZERO
      *        収納更新差額取得
               PERFORM 100218-ORCSS002-CALL-SEC
      *        新規作成
               MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "SYUNOU INS ERR KEY : "
                           SYU-KEY         DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
                   DISPLAY "K03 SYUNOU INS ERR MCP-RC:" MCP-RC
                           ",KEY:" SYU-KEY
               END-IF
      *
      *        収納明細マスター作成
               IF      SPA-ERRCD           =   SPACE
                   PERFORM 45001-SINMEI-SYORI-SEC
               END-IF
      *
      *****    MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
      *        継続が確定した時、伝票状態区分を更新する
      *        IF      SPA-CONTKBN         =   "2"
      *            PERFORM 45011-SYUNOU-UPD-SEC
      *        END-IF
      *
      ******** MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
           END-IF
      *
           MOVE    SYUNOU-REC          TO  WRK-SYUNOU-REC
      *
      *    訂正の時、収納を読み、あれば更新、なければ追加とする
           IF      SPA-SYORIFLG    NOT =   ZERO
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 980-SYUNOU-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUNOU
               END-IF
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *        請求金額
               MOVE    SYU-SKYMONEY        TO  WRK-SKYMONEY
      *        入金合計額
               MOVE    SYU-NYUKIN-TOTAL    TO  WRK-NYUKIN-TOTAL
      *        入金回数
               MOVE    SYU-NYUKIN-KAISU    TO  WRK-NYUKIN-KAISU
      *
               MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
      *        入金合計額
               IF      SYU-NYUKIN-TOTAL    >   ZERO
                   COMPUTE SYU-NYUKIN-TOTAL    =   WRK-NYUKIN-TOTAL
                                               +   SYU-NYUKIN-TOTAL
                   MOVE    WRK-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
                   ADD     1                   TO  SYU-NYUKIN-KAISU
               ELSE
                   MOVE    WRK-NYUKIN-TOTAL    TO  SYU-NYUKIN-TOTAL
                   MOVE    WRK-NYUKIN-KAISU    TO  SYU-NYUKIN-KAISU
               END-IF
      *        返金反映
               IF      ORCSCGAIRAI-SYU-HENKIN (IDH-GAI) <  ZERO
                   IF     (ORCSCGAIRAI-SYU-HENKIN (IDH-GAI) * -1)
                                             <=  SYU-NYUKIN-TOTAL
                       COMPUTE SYU-NYUKIN-TOTAL  =   SYU-NYUKIN-TOTAL
                                             +   ORCSCGAIRAI-SYU-HENKIN
                                                       (IDH-GAI)
                   ELSE
                       MOVE    ZERO          TO  SYU-NYUKIN-TOTAL
                   END-IF
               END-IF
      *        伝票状態区分
               IF      SYU-DENPJTIKBN  NOT =   "7"
                   IF      SYU-SKYMONEY        >   SYU-NYUKIN-TOTAL
                       MOVE    "2"             TO  SYU-DENPJTIKBN
                   ELSE
                       MOVE    "1"             TO  SYU-DENPJTIKBN
                   END-IF
      *            請求額がゼロの時空白
                   IF      SYU-SKYMONEY        =   ZERO
                       MOVE    SPACE               TO  SYU-DENPJTIKBN
                   END-IF
               END-IF
      *        収納更新差額取得
               PERFORM 100218-ORCSS002-CALL-SEC
               IF      FLG-SYUNOU          =   ZERO
      *            会計変更区分
                   MOVE    SPACE               TO  SYU-ACCT-UPDKBN
                   MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                   MOVE    SPA-OPID            TO  SYU-OPID
      *
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0005"          TO  SPA-ERRCD
                       STRING  "SYUNOU UPD ERR KEY : "
                               SYU-KEY         DELIMITED  BY  SIZE
                                               INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                       DISPLAY "K03 SYUNOU UPD MCP-RC:" MCP-RC
                               ",KEY:" SYU-KEY
                   END-IF
      *            収納明細マスター作成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 45001-SINMEI-SYORI2-SEC
                   END-IF
               ELSE
                   MOVE    SMCNDATE-YMD        TO  SYU-CREYMD
                   MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
                   MOVE    SPA-OPID            TO  SYU-OPID
      *
                   MOVE    SYUNOU-REC          TO  MCPDATA-REC
                   MOVE    "DBINSERT"          TO  MCP-FUNC
                   MOVE    "tbl_syunou"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       MOVE    "0005"          TO  SPA-ERRCD
                       STRING  "SYUNOU INS ERR KEY : "
                               SYU-KEY         DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                       DISPLAY "K03 SYUNOU INS MCP-RC:" MCP-RC
                               ",KEY:" SYU-KEY
                   END-IF
      *            収納明細マスター作成
                   IF      SPA-ERRCD           =   SPACE
                       PERFORM 45001-SINMEI-SYORI-SEC
                   END-IF
               END-IF
           END-IF
      *    収納の入金額合計
           MOVE    SYU-NYUKIN-TOTAL    TO  WRK-TTL-NYUKIN-TOTAL
                                                        (IDH-GAI)
      *
           MOVE    WRK-SYUNOU-REC      TO  SYUNOU-REC
      *
           .
       10021-SYUNOU-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   収納更新差額取得処理
      *****************************************************************
       100218-ORCSS002-CALL-SEC            SECTION.
      *
           INITIALIZE                  SS002-AREA
           CALL    "ORCSS002"          USING    SS002-AREA
                                                SYUNOU-REC
                                                SPA-AREA
      *
           .
       100218-ORCSS002-CALL-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票状態区分更新処理
      *****************************************************************
       45011-SYUNOU-UPD-SEC            SECTION.
      *
      *    同時診療伝票番号を検索する
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
           PERFORM UNTIL   (FLG-SYUNOU     =   1 )
                        OR (SPA-ERRCD  NOT =   SPACE)
               IF      SYU-DENPJTIKBN      =   "7"
      *            伝票状態区分
                   MOVE    "1"                 TO  SYU-DENPJTIKBN
               END-IF
      *        収納明細更新
               PERFORM 45012-SYUMEI-UPD-SEC
      *
               MOVE    SMCNDATE-YMD        TO  SYU-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYU-UPHMS
      *
               MOVE    SPA-OPID            TO  SYU-OPID
      *
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUNOU UPD ERR:"  MCP-RC
                             ",KEY:" SYU-KEY
                   STRING  "SYUNOU UPD ERR KEY : "
                           SYU-KEY         DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
      *
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key3"              TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key3"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45011-SYUNOU-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *   伝票状態区分更新処理
      *****************************************************************
       45012-SYUMEI-UPD-SEC                SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
           PERFORM UNTIL   (FLG-SYUMEI     =   1    )
                        OR (SPA-ERRCD  NOT =   SPACE)
               IF      SMEI-JOUTAIKBN      =   "7"
      *            伝票状態区分
                   MOVE    "1"                 TO  SMEI-JOUTAIKBN
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
               MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
               MOVE    SPA-OPID            TO  SMEI-OPID
      *
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUMEI UPD ERR:"  MCP-RC
                           ",KEY:" SMEI-KEY
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "SYUMEI UPD ERR KEY : "
                           SMEI-KEY        DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
      *
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUMEI-READ-SEC
           END-PERFORM
      *
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45012-SYUMEI-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    複数科まとめ編集処理
      *****************************************************************
      *45011-SYUNOU-FUKU-SEC            SECTION.
      *
      *    IF      ORCSCGAIRAI-SYORI   =   1
      *        継続
      *        IF      SPA-FUKU-DENPNUM    =   ZERO
      *            MOVE    SYU-DENPNUM     TO  SPA-FUKU-DENPNUM
      *        END-IF
      *        MOVE    SPA-FUKU-DENPNUM    TO  SYU-FUKU-DENPNUM
      *        MOVE    "1"                 TO  SYU-FUKU-KBN
      *    ELSE
      *        確定
      *        MOVE    SPA-FUKU-DENPNUM    TO  SYU-FUKU-DENPNUM
      *        MOVE    "2"                 TO  SYU-FUKU-KBN
      *    END-IF
      *
      *    .
      *45011-SYUNOU-FUKU-EXT.
      *    EXIT.
      *****************************************************************
      *    システム管理より、伝票番号取得処理
      *****************************************************************
       45001-DENPNUM-SET-SEC            SECTION.
      *
           INITIALIZE                  SYS-0041-REC.
           MOVE    "0041"              TO  SYS-0041-KANRICD
           MOVE    "*"                 TO  SYS-0041-KBNCD
           MOVE    SPA-SRYYMD          TO  SYS-0041-STYUKYMD
           MOVE    SPA-SRYYMD          TO  SYS-0041-EDYUKYMD
      *
           MOVE    SPA-HOSPNUM         TO  SYS-0041-HOSPNUM
           MOVE    SYS-0041-REC        TO  MCPDATA-REC
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key10"             TO  MCP-PATHNAME
               PERFORM 960-KANRIMST-READ-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    MCPDATA-REC         TO  SYS-0041-REC
               ELSE
                   INITIALIZE                  SYS-0041-REC
               END-IF
           ELSE
               INITIALIZE                  SYS-0041-REC
               MOVE    1                   TO  FLG-SYSKANRI
           END-IF
           MOVE    "tbl_syskanri"      TO  MCP-TABLE
           MOVE    "key10"             TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYSKANRI        =   ZERO
               ADD     1                   TO  SYS-0041-DENPNUMMAX
               IF      SYS-0041-DENPNUMMAX =    0
                   ADD     1                   TO  SYS-0041-DENPNUMMAX
               END-IF
               MOVE    SYS-0041-DENPNUMMAX
                                           TO  WRK-DENPNUM
      *
               MOVE    SMCNDATE-YMD        TO  SYS-0041-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYS-0041-UPHMS
      *
               MOVE    SPA-OPID            TO  SYS-0041-OPID
      *
               MOVE    SYS-0041-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syskanri"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC         NOT  =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "SYSKANRI UPD ERR KEY : "
                           SYS-0041-KEY     DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
                   DISPLAY "K03 SYSKANRI UPDATE:" MCP-RC
                           ",KEY:" SYS-0041-KEY
               END-IF
           ELSE
               MOVE    "0005"              TO  SPA-ERRCD
               STRING  " DENPNUM-SET ERR KEY : "
                        SYS-0041-KEY     DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
               DISPLAY "K03 DENPNUM-SET ERR:" MCP-RC
                           ",KEY:" SYS-0041-KEY
           END-IF
      *
           .
       45001-DENPNUM-SET-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理
      *****************************************************************
       45001-SINMEI-SYORI-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-DENPEDANUM
           MOVE    ZERO                TO  WRK-NYUKINRENNUM
      *    共通編集
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
      *    請求金額
           MOVE    WRK-KON-SKYMONEY    TO  SMEI-SKYMONEY
      *
      *    入金額がゼロの時
           IF     (WRK-KON-NYUKIN      =   ZERO ) AND
                  (ORCSCGAIRAI-SYU-HENKIN (IDH-GAI)  =   ZERO)
               MOVE    ORCSCGAIRAI-NYUKIN-HOHO  TO  SMEI-NYUKIN-HOHO
           ELSE
               IF      ORCSCGAIRAI-SYU-HENKIN (IDH-GAI)  =   ZERO
      *            入金編集
                   PERFORM 450012-SMEI-NYUKIN-SEC
               ELSE
      *            返金編集
                   PERFORM 450013-SMEI-HENKIN-SEC
               END-IF
           END-IF
      *状態区分
           IF      SYU-SKYMONEY        =   ZERO
               MOVE    SPACE               TO  SMEI-JOUTAIKBN
           ELSE
               MOVE    "1"                 TO  SMEI-JOUTAIKBN
           END-IF
      *    継続区分判定
           IF      SPA-CONTKBN         =   "1"
               MOVE    "7"                 TO  SMEI-JOUTAIKBN
           END-IF
      *
      *    出力編集
           PERFORM 450013-SMEI-INSERT-SEC
      *
      *    入金を作成
           IF     (ORCSCGAIRAI-SYU-HENKIN  (IDH-GAI)
                                   NOT =   ZERO ) AND
                  (WRK-KON-NYUKIN  NOT =   ZERO )
               PERFORM 45002-SMEI-NYUKINSYORI-SEC
           END-IF
           .
       45001-SINMEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細入金編集追加処理
      *****************************************************************
       450012-SMEI-NYUKIN-SEC          SECTION.
      *
      *入返金区分
           MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    WRK-KON-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *入金方法
           MOVE    ORCSCGAIRAI-NYUKIN-HOHO
                                       TO  SMEI-NYUKIN-HOHO
           .
       450012-SMEI-NYUKIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細返金編集追加処理
      *****************************************************************
       450013-SMEI-HENKIN-SEC          SECTION.
      *
      *入返金区分
           MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
           MOVE    ORCSCGAIRAI-SYU-HENKIN  (IDH-GAI)
                                       TO  SMEI-NYUHEN-MONEY
      *入金方法
           MOVE    "01"                TO  SMEI-NYUKIN-HOHO
      *明細状態区分
           MOVE    "1"                 TO  SMEI-MEISAIJOUTAIKBN
           .
       450013-SMEI-HENKIN-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細共通編集追加処理
      *****************************************************************
       450013-SMEI-INSERT-SEC          SECTION.
      *
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *請求発行日（発行日の西暦）
      *?   MOVE    SPA-SYSYMD          TO  SMEI-SKYPRINTYMD
           MOVE    ORCSCGAIRAI-HAKYMD  TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD
      *伝票番号枝番
           ADD     1                   TO  WRK-DENPEDANUM
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           ADD     1                   TO  WRK-NYUKINRENNUM
           MOVE    WRK-NYUKINRENNUM    TO  SMEI-NYUKINRENNUM
      *    収納履歴編集処理
           PERFORM 4500131-SRYURRK-HEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               STRING  "SYUMEI INS ERR KEY : "
                       SMEI-KEY        DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
               DISPLAY "K03 SINMEI-SYORI MCP-RC:" MCP-RC
                        ",KEY:" SMEI-KEY
           END-IF
           .
       450013-SMEI-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理（入金）
      *****************************************************************
       45002-SMEI-NYUKINSYORI-SEC          SECTION.
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
      *    請求金額
           MOVE    ZERO                TO  SMEI-SKYMONEY
      *    入金編集
           PERFORM 450012-SMEI-NYUKIN-SEC
      *    状態区分
           MOVE    "2"                 TO  SMEI-JOUTAIKBN
      *    出力編集
           PERFORM 450013-SMEI-INSERT-SEC
      *
           .
       45002-SMEI-NYUKINSYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納明細マスター作成処理（訂正時）
      *****************************************************************
       45001-SINMEI-SYORI2-SEC          SECTION.
      *
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUMEI-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
           MOVE    ZERO                TO  WRK-DENPEDANUM
           MOVE    ZERO                TO  WRK-NYUKINRENNUM
           PERFORM UNTIL    FLG-SYUMEI     =   1
               IF      SMEI-DENPEDANUM     >   WRK-DENPEDANUM
                   MOVE    SMEI-DENPEDANUM     TO  WRK-DENPEDANUM
               END-IF
               IF      SMEI-NYUKINRENNUM   >   WRK-NYUKINRENNUM
                   MOVE    SMEI-NYUKINRENNUM   TO  WRK-NYUKINRENNUM
               END-IF
      *
      *        診療科変更
               IF      SYU-SRYKA       NOT =   SMEI-SRYKA
                   MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *
                   MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
                   MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
      *
                   MOVE    SPA-OPID            TO  SMEI-OPID
      *
                   MOVE    SYUMEI-REC          TO  MCPDATA-REC
                   MOVE    "DBUPDATE"          TO  MCP-FUNC
                   MOVE    "tbl_syumei"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K03 SYUMEI UPD ERR:"  MCP-RC
                               ",KEY:" SMEI-KEY
                       STRING  "SYUMEI UPD ERR KEY : "
                                SMEI-KEY        DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                        END-STRING
                       MOVE    "0005"          TO  SPA-ERRCD
                   END-IF
               END-IF
      *
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 900-SYUMEI-READ-SEC
           END-PERFORM
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *H31.2
      *    伝票番号枝番
           IF      WRK-DENPEDANUM      =   99
               MOVE    "0053"              TO  SPA-ERRCD
           END-IF
           IF      SPA-ERRCD       NOT =   SPACE
               GO  TO  45001-SINMEI-SYORI2-EXT
           END-IF
      *
           MOVE    SPACE               TO  SYUMEI-REC
           INITIALIZE                      SYUMEI-REC
           MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
           MOVE    SYU-PTID            TO  SMEI-PTID
      *伝票番号
           MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
      *伝票番号枝番
           ADD     1                   TO  WRK-DENPEDANUM
           MOVE    WRK-DENPEDANUM      TO  SMEI-DENPEDANUM
      *入金連番
           ADD     1                   TO  WRK-NYUKINRENNUM
           MOVE    WRK-NYUKINRENNUM    TO  SMEI-NYUKINRENNUM
      *伝票状態区分
           MOVE    SPACE               TO  SMEI-MEISAIJOUTAIKBN
      *請求発行日（発行日の西暦）
      *??  MOVE    SPA-SYSYMD          TO  SMEI-SKYPRINTYMD
           MOVE    ORCSCGAIRAI-HAKYMD  TO  SMEI-SKYPRINTYMD
      *請求再発行日
           MOVE    SPACE               TO  SMEI-SKYREPRINTYMD
      *領収書発行日
           MOVE    SPACE               TO  SMEI-RYOSYUPRINTYMD
      *領収書再発行日
           MOVE    SPACE               TO  SMEI-RYOSYUREPRINTYMD
      *診療科
           MOVE    SYU-SRYKA           TO  SMEI-SRYKA
      *請求金額
           IF      SYU-SKYMONEY    NOT =   WRK-SKYMONEY
               COMPUTE SMEI-SKYMONEY       =   SYU-SKYMONEY
                                           -   WRK-SKYMONEY
           ELSE
               MOVE    ZERO                TO  SMEI-SKYMONEY
           END-IF
      *
      *入返金日
           MOVE    SMEI-SKYPRINTYMD    TO  SMEI-NYUHEN-YMD 
      *
      *    入金額がゼロの時、入金力なし
           IF      WRK-KON-NYUKIN      =   ZERO
      *入金方法
               MOVE    ZERO                TO  SMEI-NYUKIN-HOHO
           ELSE
      *入返金区分
               MOVE    "1"                 TO  SMEI-NYUHEN-KBN
      *入返金額
               MOVE    WRK-KON-NYUKIN      TO  SMEI-NYUHEN-MONEY
      *入金方法
               MOVE    ORCSCGAIRAI-NYUKIN-HOHO
                                           TO  SMEI-NYUKIN-HOHO
           END-IF
      *    請求額がマイナスで返金がある時、返金を作成
           IF      SMEI-SKYMONEY       <   ZERO
               IF      ORCSCGAIRAI-SYU-HENKIN (IDH-GAI)  <   ZERO
      *            入返金区分
                   MOVE    "2"             TO  SMEI-NYUHEN-KBN
      *            返金額
                   MOVE    ORCSCGAIRAI-SYU-HENKIN (IDH-GAI)
                                           TO  SMEI-NYUHEN-MONEY
      *            入金方法は現金とする
                   MOVE    "00"            TO  SMEI-NYUKIN-HOHO
               END-IF
           END-IF
      *
      *状態区分
           MOVE    "8"                 TO  SMEI-JOUTAIKBN
      *    収納履歴編集処理
           PERFORM 4500131-SRYURRK-HEN-SEC
      *
           MOVE    SMCNDATE-YMD        TO  SMEI-CREYMD
           MOVE    SMCNDATE-YMD        TO  SMEI-UPYMD
           MOVE    SMCNDATE-HMS        TO  SMEI-UPHMS
           MOVE    SMCNDATE-HMS        TO  SMEI-CREHMS
      *
           MOVE    SPA-OPID            TO  SMEI-OPID
      *
           MOVE    SMEI-CREYMD         TO  SDAILYKEY-CREYMD
           MOVE    SMEI-CREHMS         TO  SDAILYKEY-CREHMS
           MOVE    SMEI-NYUHEN-YMD     TO  SDAILYKEY-BASEYMD
           PERFORM 800-DAILYKEY-SEC
           MOVE    SDAILYKEY-DAILYKEY  TO  SMEI-DAILYKEY
      *
           MOVE    SYUMEI-REC          TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_syumei"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
grpsys     CALL    "ORCDBMAIN"         USING   MCPAREA
                                               MCPDATA-REC
                                               SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               MOVE    "0005"          TO  SPA-ERRCD
               STRING  "SYUMEI INS ERR KEY : "
                        SMEI-KEY        DELIMITED  BY  SIZE
                                        INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
               DISPLAY "K03 SINMEI-SYORI MCP-RC:" MCP-RC
                       ",KEY:" SMEI-KEY
           END-IF
      *
           .
       45001-SINMEI-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *   収納履歴編集処理処理
      *****************************************************************
       4500131-SRYURRK-HEN-SEC            SECTION.
      *
           IF      SMEI-JOUTAIKBN       =   "1"
                                        OR  "3"
                                        OR  "7"
                                        OR  "8"
                                        OR  SPACE
      *        収納履歴番号
               MOVE    SS002-SYURRKNUM     TO  SMEI-SYURRKNUM
      *        収納履歴枝番号
               MOVE    SS002-SYUEDANUM     TO  SMEI-SYUEDANUM
      *        収納履歴更新区分
               MOVE    SS002-SYURRKUPDKBN  TO  SMEI-SYURRKUPDKBN
           END-IF
           .
       4500131-SRYURRK-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（初期処理）
      *****************************************************************
       1003-JYURRK-SYORI1-SEC          SECTION.
      *
      **** 訂正時、受診履歴等の削除
      *    IF     (SPA-SYORIFLG         =   1  )  AND
      *           (SPA-OLD-SRYKA        =   SPA-SRYKA)
      *        GO      TO      1003-JYURRK-SYORI1-EXT
      **** END-IF
      *
      *    受診履歴の同日作成分カウント
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
      *!   IF      SPA-DOUJI-RENNUM    NOT =   ZERO
      *!       MOVE    SPA-DOUJI-RENNUM    TO  WRK-JYURRK-RENNUM
      *    IF      WRK-DOUJI-RENNUM    NOT =   ZERO
      *        MOVE    WRK-DOUJI-RENNUM    TO  WRK-JYURRK-RENNUM
      *    END-IF
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      **** MOVE    SPA-SRYKA           TO  JYURRK-SRYKA
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           IF      WRK-RENNUM          =   ZERO
               MOVE    "key4"              TO  WRK-PATH
           ELSE
               MOVE    WRK-RENNUM          TO  JYURRK-RENNUM
               MOVE    "key5"              TO  WRK-PATH
           END-IF
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *!       IF      SPA-DOUJI-RENNUM    =   ZERO
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                           WRK-JYURRK-DOUJI-RENNUM
      *新規の継続はないこととする
      *!       ELSE
      *            MOVE    JYURRK-DOUJI-RENNUM TO
      *                                    WRK-JYURRK-DOUJI-RENNUM
      *!       END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    WRK-PATH            TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    WRK-PATH            TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    連番の設定
           IF      WRK-RENNUM          =   ZERO
      *        同時複数保険の場合
               IF     (WRK-SRYKA-MAX       >   1  )  AND
                      (WRK-SRYKA-REN       >   1  )
                   ADD     1               TO  WRK-JYURRK-DOUJI-RENNUM
               ELSE
                   IF      WRK-JYURRK-RENNUM   =   9
                       MOVE    "XXXX"              TO  WRK-ERRCD
                   END-IF
                   ADD     1               TO  WRK-JYURRK-RENNUM
                   IF      WRK-SRYKA-MAX   >   1
                       MOVE    1           TO  WRK-JYURRK-DOUJI-RENNUM
                   ELSE
                       MOVE    ZERO        TO  WRK-JYURRK-DOUJI-RENNUM
                   END-IF
               END-IF
               MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
           ELSE
      *        同時複数保険の場合
               IF     (WRK-SRYKA-MAX       >   1  )  AND
                      (WRK-SRYKA-REN       >   1  )
                   ADD     1               TO  WRK-JYURRK-DOUJI-RENNUM
               ELSE
                   MOVE    WRK-RENNUM          TO  WRK-JYURRK-RENNUM
                   IF      WRK-SRYKA-MAX   >   1
                       MOVE    1           TO  WRK-JYURRK-DOUJI-RENNUM
                   ELSE
                       MOVE    WRK-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
                   END-IF
               END-IF
           END-IF
      *!   IF      SPA-DOUJI-RENNUM    =   ZERO
      *        ADD     1                   TO  WRK-JYURRK-RENNUM
      *!   END-IF
      *!   IF      SPA-CONTKBN         =   "1"  OR  "2"
      *!       ADD     1                   TO  WRK-JYURRK-DOUJI-RENNUM
      *!   ELSE
      *    IF      WRK-JYURRK-DOUJI-RENNUM >   ZERO
      *        ADD     1                   TO  WRK-JYURRK-DOUJI-RENNUM
      *    END-IF
      *!       MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
      *    初期編集
           PERFORM 45001-JYURRK-SYOKI-SEC
      *
           .
       1003-JYURRK-SYORI1-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（初期）
      *****************************************************************
       45001-JYURRK-SYOKI-SEC          SECTION.
      *    枝番
           ADD     1                   TO  WRK-JYURRK-EDANUM
      *
      *受診履歴レコードの作成
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
           MOVE    WRK-JYURRK-DOUJI-RENNUM
                                       TO  JYURRK-DOUJI-RENNUM
           MOVE    WRK-JYURRK-EDANUM   TO  JYURRK-EDANUM
      *    会計連番
           COMPUTE JYURRK-KAIKEI-RENNUM
                                       =   WRK-KAIKEI-RENNUM
                                       +   1
      *
      **   MOVE    SPA-DRCD            TO  JYURRK-DRCD
           MOVE    SYU-DRCD-G          TO  JYURRK-DRCD
           MOVE    WRK-HKNCOMBI        TO  JYURRK-HKNCOMBINUM
      *
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *    まとめ入力伝票番号
           IF      ORCSCGAIRAI-SYUNOU-MAX  >   1
               MOVE    WRK-GRP-DENPNUM     TO  JYURRK-GRP-DENPNUM
               MOVE    WRK-GRP-RENNUM      TO  JYURRK-GRP-RENNUM
           ELSE
               MOVE    ZERO                TO  JYURRK-GRP-DENPNUM
               MOVE    ZERO                TO  JYURRK-GRP-RENNUM
           END-IF
      *
      *    労災用に保険区分をセットする（すべて同じとする）
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
      *
           MOVE    ZERO                TO  IDX-YAKUZAI
           .
       45001-JYURRK-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正時、受診履歴等の削除処理
      *****************************************************************
       1001-TEISEI-SYORI-SEC          SECTION.
      *
      *    伝票番号がゼロの時エラーとする
           IF      SPA-DENPNUM         =   ZERO
               MOVE    "1036"              TO  SPA-ERRCD
           ELSE
      *
           INITIALIZE                  TBL-RIHADEL-AREA
           INITIALIZE                  WRK-TEISEI-AREA
           IF      SPA-TEI-DENPNUM (1) =   ZERO
               MOVE    SPA-DENPNUM         TO  WRK-DENPNUM
               MOVE    SPA-DOUJI-RENNUM    TO  WRK-DOUJI-RENNUM
               MOVE    SPA-OLD-HKNCOMBI    TO  WRK-HKNCOMBI
               PERFORM 10011-TEISEI-SYORI1-SEC
           ELSE
               PERFORM VARYING     IDXF    FROM    1   BY  1
                       UNTIL      (IDXF    >   15  )  OR
                                  (SPA-TEI-DENPNUM (IDXF)  =   ZERO )
                   MOVE    SPA-TEI-DENPNUM (IDXF)  TO  WRK-DENPNUM
                   MOVE    SPA-TEI-DOUJI-RENNUM (IDXF)
                                                   TO  WRK-DOUJI-RENNUM
                   MOVE    SPA-TEI-HKNCOMBI (IDXF)
                                               TO  WRK-HKNCOMBI
                   PERFORM 10011-TEISEI-SYORI1-SEC
                   IF      WRK-HKNCOMBI    NOT =   "9999"
                       PERFORM 10012-TEISEI-SYUTTL-DEL-SEC
                   END-IF
               END-PERFORM
           END-IF
      *
           END-IF
      *    診療科分の診療科履歴更新
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   15  )   OR
                              (WRK-TEI-SRYKA (IDX) =   SPACE )
               IF      (WRK-TEI-SRYKA (IDX)    =   SPA-SRYKA)
                   MOVE    1               TO  FLG-SRYKA-OK
               ELSE
                   MOVE    ZERO            TO  FLG-SRYKA-OK
               END-IF
               PERFORM VARYING     IDH     FROM    1   BY  1
                       UNTIL      (IDH     >   15  )
                               OR (SPA-TBL-HKNCOMBI(IDH)   =   SPACE)
                               OR (FLG-SRYKA-OK   =   1  )
                   PERFORM VARYING     IDH2    FROM    1   BY  1
                           UNTIL      (IDH2    >   10  )  OR
                                   (SPA-TBL-SRYKA(IDH IDH2) =   SPACE)
                                   OR (FLG-SRYKA-OK   =   1  )
                       IF      SPA-TBL-SRYKA(IDH IDH2) =
                                             WRK-TEI-SRYKA (IDX)
                           MOVE    1             TO  FLG-SRYKA-OK
                       END-IF
                   END-PERFORM
               END-PERFORM
      *
               IF      FLG-SRYKA-OK           =   ZERO
      *        診療科履歴変更
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
                   MOVE    WRK-TEI-SRYKA (IDX) TO  SKARRK-SRYKA
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
                   IF      SKARRK-RC       NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                       STRING  "ORCSKARRK ERR "
                               SKARRK-ERRMSG      DELIMITED  BY  SIZE
                                            INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
      *H25.5
      *    レセプト作成管理マスタ更新処理
      *    IF     (SPA-HKNCOMBINUM NOT =   "9999")
      *      AND  (SPA-ERRCD           =   SPACE )
      *        PERFORM 3009-RECEUPD-UPDATE-SEC
      *    END-IF
      *
           .
       1001-TEISEI-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正時、受診履歴等の削除処理
      *****************************************************************
       10011-TEISEI-SYORI1-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-DEL-RENNUM
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
           MOVE    WRK-DENPNUM         TO  JYURRK-DENPNUM
      *
           MOVE    JYURRK-REC          TO  MCPDATA-REC
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-JYURRK
           END-IF
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
      *?       IF      WRK-DOUJI-RENNUM    =   JYURRK-DOUJI-RENNUM
      *
      *H24.5   伝票番号重複対応
               IF     (JYURRK-SRYYMD       =   SPA-SRYYMD )
                AND   (JYURRK-HKNCOMBINUM  =   WRK-HKNCOMBI )
      *
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-RENNUM       TO  WRK-DEL-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                               WRK-JYURRK-DOUJI-RENNUM
                   MOVE    JYURRK-SRYKA        TO  WRK-MAE-SRYKA
      *            診療会計マスタの診療回数をクリアする
                   PERFORM 4500112-TEISEI-SRYACCT-SEC
      *            会計連番
                   MOVE    JYURRK-KAIKEI-RENNUM
                                               TO  WRK-KAIKEI-RENNUM
      *
      *            訂正の時受診履歴を削除する
                   MOVE    JYURRK-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
                   IF      MCP-RC          NOT =   ZERO
                        DISPLAY "K03 JYURRK DEL ERR:" MCP-RC
                                ",KEY:" JYURRK-KEY
                       MOVE    "0005"              TO  SPA-ERRCD
                  END-IF
               END-IF
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key6"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    科毎の連番
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   15
               IF      WRK-TEI-SRYKA (IDX) =   SPACE
                   MOVE    WRK-MAE-SRYKA   TO  WRK-TEI-SRYKA(IDX)
               END-IF
               IF      WRK-TEI-SRYKA(IDX)  =   WRK-MAE-SRYKA
                   MOVE    WRK-JYURRK-RENNUM   TO  WRK-TEI-RENNUM(IDX)
                   MOVE    WRK-KAIKEI-RENNUM   TO  WRK-TEI-KAIKEI-RENNUM
                                                                 (IDX)
                   MOVE    WRK-JYURRK-DOUJI-RENNUM
                                               TO  WRK-TEI-DOUJI-RENNUM
                                                                 (IDX)
                   MOVE    15                  TO  IDX
               END-IF
           END-PERFORM
      *
      *    訂正で診療科に変更がある時対象の科から連番の変更
      *!更新後に行う
      *    IF      SPA-OLD-SRYKA   NOT =   SPA-SRYKA
      *        PERFORM 4500111-TEISEI-JYURRK-HEN-SEC
      *    END-IF
      *
      *    初期編集
      *;   IF      SPA-CONTKBN         =   "1" OR  "2"
      *        IF      WRK-JYURRK-DOUJI-RENNUM =   ZERO
      *            ADD     1               TO  WRK-JYURRK-DOUJI-RENNUM
      *        END-IF
      *    ELSE
      *        MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
      *    END-IF
      *    MOVE    SPA-DENPNUM         TO  SYU-DENPNUM
      *    MOVE    SPA-DENPNUM         TO  WRK-DENPNUM
      *****PERFORM 45001-JYURRK-SYOKI-SEC
      *
           .
       1001-TEISEI-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       4500111-TEISEI-JYURRK-HEN-SEC          SECTION.
      *
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           MOVE    ZERO                TO  WRK-JYURRK-EDANUM
           MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
      **** MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           MOVE    WRK-SRYKA           TO  JYURRK-SRYKA
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
                   UNTIL      (FLG-JYURRK      =   1 )
                           OR (SPA-ERRCD   NOT =   SPACE )
      *
               MOVE    JYURRK-KEY          TO  JYURRK-UPKEY
               MOVE    JYURRK-RENNUM       TO  WRK-MAE-RENNUM
               IF     (JYURRK-EDANUM       =   1 )  AND
                     ((JYURRK-DOUJI-RENNUM     =   ZERO )  OR
                      (JYURRK-DOUJI-RENNUM NOT =
                                           WRK-JYURRK-DOUJI-RENNUM))
                   ADD     1               TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM
                                           TO  WRK-JYURRK-DOUJI-RENNUM
               END-IF
               MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
      *        会計連番の修正はしない
      *****    IF     (WRK-KAIKEI-RENNUM   =   JYURRK-KAIKEI-RENNUM) AND
      *               (WRK-DEL-RENNUM      =   JYURRK-EDANUM  )
      *            ADD     1               TO  JYURRK-KAIKEI-RENNUM
      ******   END-IF
      *        会計照会の更新
               IF      WRK-MAE-RENNUM  NOT =   JYURRK-RENNUM
                   PERFORM 45001112-TEISEI-ACCT-SEC
               END-IF
      *
               MOVE    SMCNDATE-YMD        TO  JYURRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  JYURRK-UPHMS
      *
               MOVE    SPA-OPID            TO  JYURRK-OPID
      *
      *        キー変換
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "upd1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                    DISPLAY "K03 JYURRK UPD1 ERR:" MCP-RC
                           ",KEY:" JYURRK-KEY
                    STRING  "JYURRK UPD ERR KEY : "
                            JYURRK-KEY        DELIMITED  BY  SIZE
                                        INTO  ORCSCGAIRAI-ERRMSG
                    END-STRING
                    MOVE    "0005"              TO  SPA-ERRCD
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
           .
       4500111-TEISEI-JYURRK-HEN-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数変更処理
      *****************************************************************
       45001112-TEISEI-ACCT-SEC          SECTION.
      *
           MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
      *
      *ver.4.8
      *    EVALUATE    JYURRK-RENNUM
      *        WHEN    1
      *            MOVE    2               TO  IDX-1
      *        WHEN    2
      *            MOVE    3               TO  IDX-1
      *        WHEN    OTHER
      *            MOVE    4               TO  IDX-1
      *    END-EVALUATE
      *    EVALUATE    WRK-MAE-RENNUM
      *        WHEN    1
      *            MOVE    2               TO  IDX-3
      *        WHEN    2
      *            MOVE    3               TO  IDX-3
      *        WHEN    OTHER
      *            MOVE    4               TO  IDX-3
      ***  END-EVALUATE
      *ver.4.8
      *    連番＋１　を会計テーブル位置とする
           COMPUTE IDX-1               =   JYURRK-RENNUM   +   1
           COMPUTE IDX-3               =   WRK-MAE-RENNUM  +   1
      *
      *
           IF      IDX-1           NOT =   IDX-3
      *        回数テーブルが異なる時のみ変更
               PERFORM 45001112-ACCTDAY-SEC
           END-IF
      *
           .
       45001112-TEISEI-ACCT-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       45001112-ACCTDAY-SEC            SECTION.
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )
                           OR (SPA-ERRCD   NOT =   SPACE )
               IF    (JYURRK-ZAINUM (IDX)    NOT =   ZERO )
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
                       UNTIL   FLG-SRYACCT     =   1
                           OR (SPA-ERRCD       NOT =   SPACE )
      *                剤回数　変更
                       ADD     ACCT-DAY (IDX-3 IDX-2)
                                               TO  ACCT-DAY
                                                       (IDX-1 IDX-2)
                       MOVE    ZERO            TO  ACCT-DAY
                                                       (IDX-3 IDX-2)
      *                剤回数編集
                       PERFORM 450140-ZAIKAISU-HEN-SEC
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
                           DISPLAY "K03 SRYACCT UPD ERR:" MCP-RC
                                   ",KEY:" ACCT-KEY
                           MOVE    "0005"              TO  SPA-ERRCD
                           STRING  "SRYACCT UPD ERR KEY : "
                                   ACCT-KEY        DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
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
      *
               END-IF
           END-PERFORM
      *
           .
       45001112-ACCTDAY-EXT.
           EXIT.
      *****************************************************************
      *    診療会計マスタの診療回数クリア処理
      *****************************************************************
       4500112-TEISEI-SRYACCT-SEC          SECTION.
      *
      *!   保険組合せを保存する
      *!   MOVE    SPA-HKNCOMBINUM     TO  WRK-HZN-HKNCOMBINUM
      *!   MOVE    SPA-HKNKBN          TO  WRK-HZN-HKNKBN
      *!   MOVE    JYURRK-HKNCOMBINUM  TO  SPA-HKNCOMBINUM
      *    MOVE    JYURRK-HKNKBN       TO  SPA-HKNKBN
      *
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX         >   15  )
                           OR (SPA-ERRCD       NOT =   SPACE )
             IF   (JYURRK-ZAINUM (IDX) NOT =   ZERO )
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
                        OR (SPA-ERRCD      NOT =   SPACE )
      *
                   MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
      *ver.4.8
      ****         EVALUATE    JYURRK-RENNUM
      *                WHEN    1
      *                    MOVE    2               TO  IDX-1
      *                WHEN    2
      *                    MOVE    3               TO  IDX-1
      *                WHEN    OTHER
      *                    MOVE    4               TO  IDX-1
      ************ END-EVALUATE
                   COMPUTE IDX-1       =   JYURRK-RENNUM   +   1
      *
      *            算定履歴の削除処理
                   MOVE    ACCT-DAY (IDX-1 IDX-2)  TO  WRK-KAISU
                   PERFORM 4500113-TEISEI-SANTEI-SEC
      *
      *            剤回数　削除
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
                            DISPLAY "K03 SRYACCT UPD ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                            MOVE    "0005"              TO  SPA-ERRCD
                            STRING  "SRYACCT UPD ERR KEY : "
                                     ACCT-KEY        DELIMITED  BY  SIZE
                                           INTO  ORCSCGAIRAI-ERRMSG
                           END-STRING
                       END-IF
                   END-IF
      *
                   MOVE    "tbl_sryacct"       TO  MCP-TABLE
                   MOVE    "key5"              TO  MCP-PATHNAME
                   PERFORM 930-SRYACCT-NEXT-SEC
               END-PERFORM
               MOVE    "tbl_sryacct"       TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
      *
             END-IF
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
                DISPLAY "K03 SRYACT DEL ERR:" MCP-RC
                        ",KEY:" SRY-KEY
               MOVE    "0005"              TO  SPA-ERRCD
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
               DISPLAY "K03  PTCOM  DEL ERR:" MCP-RC
                     ",KEY:" PTCOM-KEY
               MOVE    "0005"              TO  SPA-ERRCD
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
               DISPLAY "K03 SRYACCT DEL ERR:" MCP-RC
                        ",KEY:" ACCT-KEY
               MOVE    "0005"              TO  SPA-ERRCD
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
               DISPLAY "K03  SRYACCTPLUS  DEL ERR:" MCP-RC
                     ",KEY:" ACCTP-KEY
               MOVE    "0005"              TO  SPA-ERRCD
           END-IF
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
               PERFORM
                   UNTIL  (FLG-SRYACT          =   1     )
                       OR (SPA-ERRCD       NOT =   SPACE )
      *
                   PERFORM VARYING     IDY     FROM    1   BY  1
                           UNTIL      (IDY     >   5   )  OR
                                      (SRY-SRYCD (IDY) =   SPACE)
                                   OR (SPA-ERRCD      NOT =   SPACE )
      *
      *            手技料の時、算定歴を判定する
                   IF     (SRY-SRYCD (IDY)(1:1)    =   "1"    )
      *                システム予約コード
                       OR (SRY-SRYCD (IDY)(1:3)    =   "099" )
                       MOVE    SRY-SRYCD (IDY)     TO  WRK-SRYCD
                       MOVE    SRY-SRYSURYO (IDY)  TO  WRK-SRYSURYO
      *
                       MOVE    WRK-SRYCD           TO  TNS-SRYCD
                       PERFORM 910-TENSU-READ-SEC
      *                算定用の診療コードへ変更
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
           IF      ORCSCGAIRAI-KBN     =   "2"
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
           .
       45001131-SANTEI-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    訂正時の合計収納削削除処理
      *****************************************************************
       10012-TEISEI-SYUTTL-DEL-SEC          SECTION.
      *
      *    合計収納削除
           INITIALIZE                  SYUTTL-REC
           MOVE    SPA-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYUTTL-PTID
           MOVE    WRK-DENPNUM         TO  SYUTTL-DENPNUM
      *
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           IF      FLG-SYUTTL          =   ZERO
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *
           .
       10012-TEISEI-SYUTTL-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（明細編集）
      *****************************************************************
       45001-JYURRK-SYORI2-SEC          SECTION.
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
      *
      *    同一の剤の判定
           MOVE    ZERO                TO  FLG-ZAINUM
           PERFORM VARYING WRK-TBL-IDX     FROM    1   BY  1
                   UNTIL  (WRK-TBL-IDX     >   100 )  OR
                          (FLG-ZAINUM      =   1   )
               IF      WRK-TBL-ZAINUM (WRK-TBL-IDX)    =   WRK-ZAINUM
                   MOVE    1               TO  FLG-ZAINUM
               ELSE
                   IF      WRK-TBL-ZAINUM (WRK-TBL-IDX)    =   ZERO
                       MOVE    WRK-ZAINUM      TO  WRK-TBL-ZAINUM
                                                      (WRK-TBL-IDX)
                       MOVE    100             TO  WRK-TBL-IDX
                   END-IF
               END-IF
           END-PERFORM
      *
           IF      FLG-ZAINUM          =   ZERO
               ADD     1       TO    IDX-YAKUZAI
               IF      IDX-YAKUZAI   >    15
      *            受診履歴作成処理
                   PERFORM 45001-JYURRK-SYORI3-SEC
                   PERFORM 45001-JYURRK-SYOKI-SEC
      *
                   MOVE    1             TO  IDX-YAKUZAI
               END-IF
               MOVE    WRK-ZAINUM        TO  JYURRK-ZAINUM(IDX-YAKUZAI)
           END-IF
      *
      *    初診料履歴のみ算定時、初診日２へセット
           IF      SPA-SYOSIN-YMD  NOT =   SPACE
               MOVE    SPA-SYOSIN-YMD      TO  WRK-KARRK-SYOSINYMD2
           END-IF
      *    初診料算定日設定処理
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
               MOVE    ZERO                TO  FLG-DOUJITU
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
                   EVALUATE    LNK-WKSRY-SRYCD (IDX IDY)
      *                小児科外来診療料
                       WHEN    "113003510"
                       WHEN    "113003710"
                           MOVE    SYU-SRYYMD      TO
                                                   WRK-KARRK-SYOSINYMD2
      *H28.4           小児かかりつけ診療料
                       WHEN    "113019710"
                       WHEN    "113019910"
                           MOVE    SYU-SRYYMD      TO
                                                   WRK-KARRK-SYOSINYMD2
      *                同日初診
                       WHEN    "101110040"
                       WHEN    "111011810"
      *H25.4           紹介なし
                       WHEN    "111012610"
      *H28.4           妥結５割以下
                       WHEN    "111012810"
                           IF      LNK-WKSRY-SRYKBN (IDX)  =   "11"
                               MOVE    1               TO  FLG-DOUJITU
                           END-IF
      *                検診等より
                       WHEN    "820000003"
                       WHEN    "820000004"
                       WHEN    "820000005"
                       WHEN    "820000006"
                           IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11") AND
                                  (WRK-KARRK-SYOSINYMD2    =   SPACE)
                               MOVE    SYU-SRYYMD  TO
                                                   WRK-KARRK-SYOSINYMD2
                           END-IF
                   END-EVALUATE
               END-PERFORM
      *        診療科更新（初診料算定時、初診日２へセット
               IF     (LNK-WKSRY-SRYKBN (IDX)  =   "11" )  AND
                      (FLG-DOUJITU             =   ZERO )
                   MOVE    SYU-SRYYMD          TO  WRK-KARRK-SYOSINYMD2
               END-IF
      *
           END-PERFORM
      *
           .
       45001-JYURRK-SYORI2-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診履歴作成処理（ファイル書き出し）
      *****************************************************************
       45001-JYURRK-SYORI3-SEC          SECTION.
      *
      *    受診履歴連番を編集する
           MOVE    JYURRK-RENNUM       TO  ORCSCGAIRAI-JYURRK-RENNUM
                                                             (IDH-GAI)
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
               MOVE    "0005"          TO  SPA-ERRCD
               DISPLAY "K03 JYURRK-SYORI3 INS ERR:" MCP-RC 
                        ",KEY:" JYURRK-KEY "."
               STRING  "JYURRK-SYORI3 INS ERR KEY : "
                       JYURRK-KEY      DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
               IF       WRK-ERRCD      NOT =   SPACE
                   MOVE    "0105"          TO  SPA-ERRCD
                   MOVE    SPACE           TO  ORCSCGAIRAI-ERRMSG
               END-IF
           END-IF
      *
           .
       45001-JYURRK-SYORI3-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科初期処理（初期処理）
      *****************************************************************
       45009-SRYKARRK-SYOKI-SEC           SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    WRK-SRYKA           TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45009-SRYKARRK-SYOKI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴更新処理
      *****************************************************************
       45010-SRYKARRK-SYORI-SEC           SECTION.
      *
           IF      FLG-SRYKARRK        =   ZERO
      *        最終受診日更新
               IF      KARRK-LASTYMD       <   SPA-SRYYMD
                   MOVE    SPA-SRYYMD      TO  KARRK-LASTYMD
               END-IF
           ELSE
      *        診療科新規作成
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    WRK-SRYKA           TO  KARRK-SRYKA
               MOVE    SPA-SRYYMD          TO  KARRK-SYOSINYMD1
               MOVE    SPA-SRYYMD          TO  KARRK-LASTYMD
      *        初回算定日があればその日
               IF     (SPA-SYOSIN-YMD  NOT =   SPACE  )  AND
                      (SPA-SRYYMD          >   SPA-SYOSIN-YMD )
                   MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD1
               END-IF
           END-IF
           IF      WRK-KARRK-SYOSINYMD2    NOT =   SPACE
               MOVE    WRK-KARRK-SYOSINYMD2    TO  KARRK-SYOSINYMD2
           END-IF
           PERFORM 45009-SRYKARRK-OUT-SEC
      *
           .
       45010-SRYKARRK-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴作成終了処理
      *****************************************************************
       45009-SRYKARRK-OUT-SEC            SECTION.
      *
           IF      FLG-SRYKARRK           =   ZERO
      *
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SPA-OPID            TO  KARRK-OPID
      *
      *        更新
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYKARRK UPD ERR:"  MCP-RC
                           ",KEY:" KARRK-KEY  ";"
                   STRING  "SRYKARRK UPD ERR KEY : "
                           KARRK-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
                   MOVE    "0005"              TO  SPA-ERRCD
               END-IF
           ELSE
      *        新規
               MOVE    SMCNDATE-YMD        TO  KARRK-CREYMD
               MOVE    SMCNDATE-YMD        TO  KARRK-UPYMD
               MOVE    SMCNDATE-HMS        TO  KARRK-UPHMS
      *
               MOVE    SPA-OPID            TO  KARRK-OPID
      *
               MOVE    SRYKARRK-REC        TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYKARRK INS ERR:"  MCP-RC
                           ",KEY:" KARRK-KEY  ";"
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "SRYKARRK INS ERR KEY : "
                           KARRK-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
           END-IF
      *
           .
       45009-SRYKARRK-OUT-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療行為マスター更新処理
      *****************************************************************
       4501-SRYACT-KOU-SEC            SECTION.
      *
           MOVE    ZERO                TO  WRK-ZAINUM
      *
           MOVE    ZERO                TO  FLG-SINKI
      *    診療会計マスター処理
           PERFORM 4501-SRYACCT-SYORI-SEC
      *
      *    診療コードがない時、処理をしない
           IF      WRK-MEISAISU        =   ZERO 
               GO      TO      4501-SRYACT-KOU-EXT
           END-IF
      *
      *    診療行為マスター処理 
           IF     (FLG-SINKI           =   1     )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 4501-SRYACT-SYORI-SEC
           END-IF
      *
      *!!
      *    診療会計附加情報処理 
           IF     (FLG-SINKI           =   1 )  AND
                  (LNK-WKSRYP-ADDKBN(01)   =   "1"  )
             AND  (SPA-ERRCD           =   SPACE )
               PERFORM 4503-SRYACCTPLUS-SYORI-SEC
           END-IF
      *!!
      *
      *    包括まとめは登録しない
           IF      SPA-HKNCOMBINUM     =   "9999"
               CONTINUE
           ELSE
      *        算定履歴更新処理
               PERFORM 4502-SANTEI-SYORI-SEC
           END-IF
           .
       4501-SRYACT-KOU-EXT.
           EXIT.
      *
      *****************************************************************
      *     診療会計マスター処理
      *****************************************************************
       4501-SRYACCT-SYORI-SEC              SECTION.
      *
      *    診療コード計計算
           MOVE    ZERO                TO  WRK-ZAITEN
           MOVE    ZERO                TO  WRK-SRYKAISUKEI
           MOVE    ZERO                TO  WRK-SRYSURYOKEI
           MOVE    ZERO                TO  WRK-MEISAISU
           MOVE    ZERO                TO  WRK-SRYCDKEI
           INITIALIZE                      WRK-WKSRY-AREA
           MOVE    ZERO                TO  FLG-GEN-ARI
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL       IDX     >   IDX-KOUI
      *        剤点数計
               MOVE    LNK-WKSRY-ZAITENKEI(IDX)    TO  WRK-ZAITEN
      *        回数計
               MOVE    LNK-WKSRY-ZAIKAIKEI(IDX)    TO  WRK-SRYKAISUKEI
      *        手技料１
               MOVE    LNK-WKSRY-SYUTEN1  (IDX)    TO  WRK-SYUTEN1
      *        手技料２
               MOVE    LNK-WKSRY-SYUTEN2  (IDX)    TO  WRK-SYUTEN2
      *        薬剤
               MOVE    LNK-WKSRY-YKZTEN  (IDX)     TO  WRK-YKZTEN
      *        器材
               MOVE    LNK-WKSRY-KIZAITEN (IDX)    TO  WRK-KIZAITEN
      *        自費
               IF      LNK-WKSRY-JIHIMONEYTOTAL (IDX)  NOT =   ZERO
      *************MOVE    LNK-WKSRY-JIHIMONEY (IDX)  TO  WRK-JIHIMONEY
                   MOVE    LNK-WKSRY-JIHIMONEYTOTAL (IDX) 
                                                   TO  WRK-JIHIMONEY
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
      *            内服逓減の減の有無判定
                   IF     (LNK-WKSRY-SRYCD (IDX IDY)   =   "820000047")
                      AND (LNK-WKSRY-SRYKBN (IDX)      =   "21"  )
                       MOVE    1               TO  FLG-GEN-ARI
                   END-IF
      *H27.4
                   IF     (LNK-WKSRY-SRYCD (IDX IDY)   =   "820000047")
                      AND (LNK-WKSRY-SRYKBN (IDX)      =   "22"  )
                       MOVE    1               TO  FLG-GEN-ARI
                   END-IF
      *H28.4       湿布薬減点
                   IF     (LNK-WKSRY-SRYCD (IDX IDY)
                                                   =   CONS-SPCM-SRYCD)
                      AND (LNK-WKSRY-SRYKBN (IDX)      =   "23"  )
                       MOVE    1               TO  FLG-GEN-ARI
                   END-IF
      *H26.10
      *            向精神薬多剤逓減の減の有無判定
                   IF     (LNK-WKSRY-SRYCD (IDX IDY)
                                                   =   CONS-SGEN-SRYCD)
                      AND (LNK-WKSRY-SRYKBN (IDX)  =   "21"
                                                   OR  "22"
                                                   OR  "23"     )
                       MOVE    1               TO  FLG-GEN-ARI
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
               GO      TO      4501-SRYACCT-SYORI-EXT
           END-IF 
      *!!!
      *    対象有
           MOVE    1                   TO  FLG-SYORI-ARI
      *!!!
      *
      *    診療会計剤チェックサブ
           INITIALIZE                      ORCSCZAICHKAREA
           MOVE    "1"                     TO  ORCSCZAICHK-KBN
           MOVE    LNK-WKSRY-SRYKA(01)     TO  ORCSCZAICHK-SRYKA
           MOVE    LNK-WKSRY-SRYYMD(01)(1:6)
                                           TO  ORCSCZAICHK-SRYYM
           MOVE    LNK-WKSRY-SRYKBN(01)    TO  ORCSCZAICHK-SRYKBN
           MOVE    LNK-WKSRY-SRYSYUKBN(01) TO  ORCSCZAICHK-SRYSYUKBN
           MOVE    WRK-HKNCOMBI            TO  ORCSCZAICHK-HKNCOMBI
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
      *    内服逓減の剤チェック
           IF     (WRK-ZAINUM      NOT =   ZERO )  AND
                  (LNK-WKSRY-SRYKBN    (1) =   "21"
                                           OR  "22"
                                           OR  "23" )  AND
                  (LNK-WKSRYP-TEI-NKBN (1) =   2    )
               PERFORM 45015-TEIGEN-CHK-SEC
           END-IF
      *H27.4
      *    ３０日以上逓減の剤チェック
           IF     (WRK-ZAINUM      NOT =   ZERO )  AND
                  (LNK-WKSRY-SRYKBN    (1) =   "21"
                                           OR  "22" )  AND
                  (LNK-WKSRYP-TEI-NKBN30(1) =   2    )
               PERFORM 45015-TEIGEN30-CHK-SEC
           END-IF
      *H28.4
      *    湿布７０枚以上逓減の剤チェック
           IF     (WRK-ZAINUM      NOT =   ZERO )  AND
                  (LNK-WKSRY-SRYKBN    (1) =   "23" )  AND
                  (LNK-WKSRYP-TEI-NKBN30(1) =   2    )
               PERFORM 45015-TEIGEN30-CHK-SEC
           END-IF
      *
           IF      WRK-ZAINUM          =   ZERO
      *        剤番号取得処理
               PERFORM 45013-ZAINUM-SET-SEC
               IF      SPA-ERRCD       NOT =   SPACE
                   GO      TO      4501-SRYACCT-SYORI-EXT
               END-IF
           END-IF
      *!!
      *    内服逓減処理
      *****IF      FLG-GEN-ARI         =   1
      *H27.4
      *    内服逓減対象剤
           IF      LNK-WKSRYP-TEI-NKBN (1) =   1
               PERFORM VARYING IDX-GEN2    FROM    1   BY  1
                       UNTIL   IDX-GEN2    >   30
                   IF      WRK-ACTP-SRYKA(IDX-GEN2)    =   SPACE
                       MOVE    LNK-WKSRY-SRYKA(01)
                                           TO  WRK-ACTP-SRYKA(IDX-GEN2)
                       MOVE    LNK-WKSRY-HKNCOMBI(01)
                                           TO  WRK-ACTP-HKNCOMBI
                                                            (IDX-GEN2)
      *H26.10
                       MOVE    LNK-WKSRY-SRYKBN(01)
                                           TO  WRK-ACTP-SRYKBN
                                                            (IDX-GEN2)
                   END-IF
                   IF     (WRK-ACTP-SRYKA(IDX-GEN2)
                                           =   LNK-WKSRY-SRYKA(01) )
                     AND  (WRK-ACTP-HKNCOMBI(IDX-GEN2)
                                           =   LNK-WKSRY-HKNCOMBI(01))
      *H26.10
                     AND  (WRK-ACTP-SRYKBN  (IDX-GEN2)
                                           =   LNK-WKSRY-SRYKBN  (01))
                       ADD     1           TO  WRK-ACTP-MAX (IDX-GEN2)
                       MOVE    WRK-ACTP-MAX (IDX-GEN2) 
                                           TO  IDX-GEN
                       IF      IDX-GEN     >   50
                           MOVE    50                  TO  IDX-GEN
                       END-IF
                       MOVE    WRK-ZAINUM      TO  WRK-ACTP-ZAINUM
                                                    (IDX-GEN2 IDX-GEN)
                       MOVE    30              TO  IDX-GEN2
                   END-IF
               END-PERFORM
           END-IF
      *H27.4
      *    ３０日以上逓減対象剤
           IF      LNK-WKSRYP-TEI-NKBN30 (1) =   1
               PERFORM VARYING IDX-GEN2    FROM    1   BY  1
                       UNTIL   IDX-GEN2    >   30
                   IF      WRK-ACTP30-SRYKA(IDX-GEN2)    =   SPACE
                       MOVE    LNK-WKSRY-SRYKA(01)
                                           TO  WRK-ACTP30-SRYKA
                                                            (IDX-GEN2)
                       MOVE    LNK-WKSRY-HKNCOMBI(01)
                                           TO  WRK-ACTP30-HKNCOMBI
                                                            (IDX-GEN2)
                       MOVE    LNK-WKSRY-SRYKBN(01)
                                           TO  WRK-ACTP30-SRYKBN
                                                            (IDX-GEN2)
                   END-IF
                   IF     (WRK-ACTP30-SRYKA(IDX-GEN2)
                                           =   LNK-WKSRY-SRYKA(01) )
                     AND  (WRK-ACTP30-HKNCOMBI(IDX-GEN2)
                                           =   LNK-WKSRY-HKNCOMBI(01))
                     AND  (WRK-ACTP30-SRYKBN  (IDX-GEN2)
                                           =   LNK-WKSRY-SRYKBN  (01))
                       ADD     1           TO  WRK-ACTP30-MAX (IDX-GEN2)
                       MOVE    WRK-ACTP30-MAX (IDX-GEN2) 
                                           TO  IDX-GEN
                       IF      IDX-GEN     >   50
                           MOVE    50                  TO  IDX-GEN
                       END-IF
                       MOVE    WRK-ZAINUM      TO  WRK-ACTP30-ZAINUM
                                                    (IDX-GEN2 IDX-GEN)
                       MOVE    30              TO  IDX-GEN2
                   END-IF
               END-PERFORM
           END-IF
      *!!
      *    診療会計マスター更新用読み込み
           INITIALIZE                          SRYACCT-REC
           MOVE    LNK-WKSRY-HOSPNUM (01)  TO  ACCT-HOSPNUM
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCT-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCT-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCT-SRYKA
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCT-SRYYM
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
      *
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
                   DISPLAY "K03 SRYACCT INS ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "SRYACCT INS ERR KEY : "
                           ACCT-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
      *        新規のときのみ、診療行為マスターを追加する
               MOVE    1                   TO  FLG-SINKI
           ELSE
      *        診療会計マスター編集処理
               PERFORM 45014-SRYACCT-HEN-SEC
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
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "SYRACCT UPDTE ERR:" MCP-RC
                                ",KEY:" ACCT-KEY
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "SRYACCT UPD ERR KEY : "
                           ACCT-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF   
               MOVE    ZERO                TO  FLG-SINKI
           END-IF
           .
       4501-SRYACCT-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *   内服逓減剤内容判定処理
      *****************************************************************
       45015-TEIGEN-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    LNK-WKSRY-HOSPNUM (01)  TO  ACCTP-HOSPNUM
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCTP-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCTP-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCTP-SRYKA
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCTP-SRYYM
           MOVE    WRK-ZAINUM              TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL  (FLG-SRYACCTPLUS =   1 )  OR
                          (FLG-ERR         =   1 )
               PERFORM VARYING IDX-GEN4    FROM    1   BY  1
                       UNTIL  (IDX-GEN4    >   5   )  OR
                              (ACCTP-GRP-ZAINUM(IDX-GEN4)  =   ZERO) OR
                              (FLG-ERR         =   1 )
                   MOVE    ZERO                TO  FLG-ERR2
                   PERFORM VARYING IDX-GEN5    FROM    1   BY  1
                           UNTIL  (IDX-GEN5    >   30   )  OR
                                  (WRK-ACTP-SRYKA(IDX-GEN5)
                                               =   SPACE  )  OR
                                  (FLG-ERR2    =   1      )
                       IF     (LNK-WKSRY-SRYKA(01)
                                           =   WRK-ACTP-SRYKA(IDX-GEN5))
                         AND  (LNK-WKSRY-HKNCOMBI (01)
                                           =   WRK-ACTP-HKNCOMBI
                                                            (IDX-GEN5))
                         AND  (LNK-WKSRY-SRYKBN   (01)
                                           =   WRK-ACTP-SRYKBN
                                                            (IDX-GEN5))
                           PERFORM VARYING IDX-GEN6    FROM    1   BY  1
                                   UNTIL  (IDX-GEN6    >   50  )  OR
                                          (WRK-ACTP-ZAINUM
                                                    (IDX-GEN5 IDX-GEN6)
                                                       =   ZERO   ) OR
                                          (FLG-ERR2    =   1      )
                               IF      WRK-ACTP-ZAINUM
                                                     (IDX-GEN5 IDX-GEN6)
                                           =   ACCTP-GRP-ZAINUM
                                                         (IDX-GEN4)
                                   MOVE    1           TO  FLG-ERR2
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
                   IF      FLG-ERR2            =   ZERO
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-PERFORM
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診療会計附加情報の逓減剤内容が違う時、新規
           IF      FLG-ERR             =   1
               MOVE    ZERO            TO  WRK-ZAINUM
           END-IF
      *
           .
       45015-TEIGEN-CHK-EXT.
           EXIT.
      *
      *H27.4
      *****************************************************************
      *   ３０日以上逓減剤内容判定処理
      *****************************************************************
       45015-TEIGEN30-CHK-SEC          SECTION.
      *
           MOVE    SPACE               TO  SRYACCTPLUS-REC
           INITIALIZE                  SRYACCTPLUS-REC
           MOVE    LNK-WKSRY-HOSPNUM (01)  TO  ACCTP-HOSPNUM
           MOVE    LNK-WKSRY-NYUGAIKBN(01) TO  ACCTP-NYUGAIKBN
           MOVE    LNK-WKSRY-PTID   (01)   TO  ACCTP-PTID
           MOVE    LNK-WKSRY-SRYKA  (01)   TO  ACCTP-SRYKA
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCTP-SRYYM
           MOVE    WRK-ZAINUM              TO  ACCTP-ZAINUM
      *
           MOVE    SRYACCTPLUS-REC     TO  MCPDATA-REC
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
           MOVE    ZERO                TO  FLG-ERR
           PERFORM UNTIL  (FLG-SRYACCTPLUS =   1 )  OR
                          (FLG-ERR         =   1 )
               PERFORM VARYING IDX-GEN4    FROM    1   BY  1
                       UNTIL  (IDX-GEN4    >   5   )  OR
                              (ACCTP-GRP-ZAINUM(IDX-GEN4)  =   ZERO) OR
                              (FLG-ERR         =   1 )
                   MOVE    ZERO                TO  FLG-ERR2
                   PERFORM VARYING IDX-GEN5    FROM    1   BY  1
                           UNTIL  (IDX-GEN5    >   30   )  OR
                                  (WRK-ACTP30-SRYKA(IDX-GEN5)
                                               =   SPACE  )  OR
                                  (FLG-ERR2    =   1      )
                       IF     (LNK-WKSRY-SRYKA(01)
                                           =   WRK-ACTP30-SRYKA
                                                            (IDX-GEN5))
                         AND  (LNK-WKSRY-HKNCOMBI (01)
                                           =   WRK-ACTP30-HKNCOMBI
                                                            (IDX-GEN5))
                         AND  (LNK-WKSRY-SRYKBN   (01)
                                           =   WRK-ACTP30-SRYKBN
                                                            (IDX-GEN5))
                           PERFORM VARYING IDX-GEN6    FROM    1   BY  1
                                   UNTIL  (IDX-GEN6    >   50  )  OR
                                          (WRK-ACTP30-ZAINUM
                                                    (IDX-GEN5 IDX-GEN6)
                                                       =   ZERO   ) OR
                                          (FLG-ERR2    =   1      )
                               IF      WRK-ACTP30-ZAINUM
                                                     (IDX-GEN5 IDX-GEN6)
                                           =   ACCTP-GRP-ZAINUM
                                                         (IDX-GEN4)
                                   MOVE    1           TO  FLG-ERR2
                               END-IF
                           END-PERFORM
                       END-IF
                   END-PERFORM
                   IF      FLG-ERR2            =   ZERO
                       MOVE    1               TO  FLG-ERR
                   END-IF
               END-PERFORM
               MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 910-SRYACCTPLUS-READ-SEC
           END-PERFORM
           MOVE    "tbl_sryacctplus"   TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *    診療会計附加情報の逓減剤内容が違う時、新規
           IF      FLG-ERR             =   1
               MOVE    ZERO            TO  WRK-ZAINUM
           END-IF
      *
           .
       45015-TEIGEN30-CHK-EXT.
           EXIT.
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
               DISPLAY "K03 ZAINUM-SET ERR:" SPA-PTID
               STRING  "PTINF READ ERR PTID : "
                       SPA-PTID        DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
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
               STRING  "PTINF UPDTE ERR KEY : "
                       PTINF-KEY      DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
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
           MOVE    LNK-WKSRY-SRYYMD (01)(1:6)
                                           TO  ACCT-SRYYM
           MOVE    LNK-WKSRY-SRYKBN (01)   TO  ACCT-SRYKBN
           MOVE    WRK-ZAINUM          TO  ACCT-ZAINUM
           MOVE    WRK-HKNCOMBI        TO  ACCT-HKNCOMBI
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
      *    内服逓減対象剤
           IF      LNK-WKSRYP-TEI-NKBN (1) =   1
               MOVE    "2"                 TO  ACCT-ZAIREQFLG
           END-IF
      *H27.4
      *    ３０日逓減対象剤
           IF      LNK-WKSRYP-TEI-NKBN30 (1) =   1
               MOVE    "2"                 TO  ACCT-ZAIREQFLG
           END-IF
      *!!
      *@
      *    包括剤の設定
           IF      LNK-WKSRY-CONTKBN (1)   =   "H"
               MOVE    "1"                 TO  ACCT-JIHOKBN
           ELSE
               MOVE    SPACE               TO  ACCT-JIHOKBN
           END-IF
      *
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
           MOVE    SPA-SRYYMD(7:2)     TO  IDX-2
           ADD     WRK-SRYKAISUKEI     TO  ACCT-DAY  (1 IDX-2)
      *
           COMPUTE IDX1                =   WRK-JYURRK-RENNUM
                                       +   1
      *ver.4.8
      ***  IF      IDX1                >   4
      *        MOVE    4                   TO  IDX1
      **** END-IF
           ADD     WRK-SRYKAISUKEI     TO  ACCT-DAY  (IDX1 IDX-2)
      *
      *H26.8
      *    外用で回数１以上はエラー
           IF     (ACCT-SRYKBN         =   "23" )
             AND  (WRK-SYUTEN1         =   ZERO )
             AND  (WRK-SRYKAISUKEI     =   1    )
             AND  (ACCT-DAY  (IDX1 IDX-2)  >   1)
      *v4.7 は集計以外
      **     AND  (WRK-JYURRK-RENNUM       <   4 )
               MOVE    "0052"          TO  SPA-ERRCD
           END-IF
      *
      *    剤回数編集
           PERFORM 450140-ZAIKAISU-HEN-SEC
           .
       45014-SRYACCT-HEN-EXT.
           EXIT.
      *
      *****************************************************************
      *   剤回数編集処理
      *****************************************************************
       450140-ZAIKAISU-HEN-SEC          SECTION.
      *
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
           EXIT.
      *****************************************************************
      *    診療行為マスター処理
      *****************************************************************
       4501-SRYACT-SYORI-SEC          SECTION.
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
               MOVE    LNK-WKSRY-SRYYMD (IDX)(1:6)
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
      *
               PERFORM VARYING IDX-SIN     FROM    1   BY  1
                       UNTIL   IDX-SIN     >   5
                   MOVE    LNK-WKSRY-SRYCD (IDX IDX-SIN)
                                               TO  SRY-SRYCD (IDX-SIN)
                   MOVE    LNK-WKSRY-SRYSURYO(IDX IDX-SIN)
                                               TO  SRY-SRYSURYO
                                                            (IDX-SIN)
                   MOVE    LNK-WKSRY-SRYKAISU(IDX IDX-SIN)
                                               TO  SRY-SRYKAISU
                                                            (IDX-SIN)
      *            明細請求区分
                   IF      LNK-WKSRY-AUTOKBN(IDX IDX-SIN)  =   "T"
                       MOVE    "1"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN)
                       MOVE    SPACE           TO  LNK-WKSRY-AUTOKBN
                                                         (IDX IDX-SIN)
                   END-IF
                   MOVE    LNK-WKSRY-AUTOKBN(IDX IDX-SIN)
                                               TO  SRY-AUTOKBN
                                                            (IDX-SIN)
                   MOVE    LNK-WKSRY-JIHIMONEY (IDX IDX-SIN)
                                               TO  SRY-JIHIMONEY
                                                            (IDX-SIN)
      *            換算値数量
                   MOVE    LNK-WKSRY-KANSURYO(IDX IDX-SIN)
                                               TO  SRY-KANSURYO
                                                            (IDX-SIN)
                   EVALUATE    LNK-WKSRY-INPUTKBN(IDX IDX-SIN)
      *            関連コメント指示
                       WHEN    "1"
                           MOVE    "2"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN)
      *            多剤投与指示
                       WHEN    "2"
                           MOVE    "3"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN)
      *H25.3
      *            労災読み替え手技
                       WHEN    "R"
                           IF      SPA-SRYYMD      >=  "20130701"
                               MOVE    "R"             TO  SRY-MEISKYFLG
                                                            (IDX-SIN)
                            END-IF
                   END-EVALUATE
      *
      *            患者コメント作成
                   IF     (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:1)  =  "8")
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:3)  =  "008")
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:3)  =  "001")
      *H30.4
                      OR  (LNK-WKSRY-SRYCD (IDX IDX-SIN)(1:7)
                                                         =   "0992081")
      *               入力があれば作成
                       IF      (LNK-WKSRY-INPUTCHI-G (IDX IDX-SIN)
                                                       =  SPACE ) AND
                               (LNK-WKSRY-INPUTCOMENT(IDX IDX-SIN)
                                                       =  SPACE )
                           CONTINUE
                       ELSE
                           PERFORM 45011-PTCOM-SYROI-SEC
                       END-IF
                   END-IF
               END-PERFORM
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
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SRYACT INS ERR:" SRY-KEY  "."
                   DISPLAY "MCP-RC:" MCP-RC ":" 
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "SRYACT INS  ERR KEY : "
                           SRY-KEY      DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
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
               DISPLAY "K03 PTCOM INS ERR:" PTCOM-KEY
               MOVE    "0005"          TO  SPA-ERRCD
               STRING  "PTCOM INS  ERR KEY : "
                       PTCOM-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
           END-IF
      *
      *    明細番号
           MOVE    WRK-MEIBAN          TO  SRY-INPUTNUM  (IDX-SIN)
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
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   IDX-KOUI  )
                           OR (SPA-ERRCD   NOT =   SPACE )
      *      包括剤の設定
      *****  IF     (LNK-WKSRY-CONTKBN (IDX)   =   "H" )  OR
             IF     (LNK-WKSRY-HKNCOMBI(IDX)   =   "9999")
               CONTINUE
             ELSE
               PERFORM VARYING     IDY     FROM    1   BY  1
                       UNTIL      (IDY     >   5   )  OR
                                  (LNK-WKSRY-SRYCD (IDX IDY) =   SPACE)
      *            手技料の時、算定歴を判定する
                   IF    ((LNK-WKSRY-SRYCD (IDX IDY)(1:1)  =   "1" )
      *                システム予約コード
                       OR (LNK-WKSRY-SRYCD (IDX IDY)(1:3)  =   "099"))
      *            自費の時、履歴なし
                   AND    (LNK-WKSRY-SRYKBN(IDX)   NOT =   "95"  AND
                                                           "96"  )
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
                           PERFORM 45021-SANTEI-UPD-SEC
                       END-IF
                   END-IF
               END-PERFORM
      *
             END-IF
           END-PERFORM
      *
           .
       4502-SANTEI-SYORI-EXT.
           EXIT.
      *
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
      *
      *****************************************************************
      *    算定履歴更新処理
      *****************************************************************
       45021-SANTEI-UPD-SEC              SECTION.
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
      *    １日上限で単位が分以外の時、数量編集
           IF      (WRK-SRYSURYO       >   1    )
               AND (TNS-JGNCNT1D       >   ZERO )
               AND (TNS-TANICD     NOT =   "001")
               MOVE    WRK-SRYSURYO        TO  SSANTEI-SRYSURYO
           END-IF
      *    初診料算定日
      *    訂正の時
           IF      SPA-SYORIFLG        =   1
               MOVE    SPA-SYOYMD-OLD      TO  SSANTEI-SYOYMD
           ELSE
               IF      WRK-KARRK-SYOSINYMD2    =   SPACE
                   MOVE    SPA-SYOYMD              TO  SSANTEI-SYOYMD
               ELSE
                   MOVE    WRK-KARRK-SYOSINYMD2    TO  SSANTEI-SYOYMD
               END-IF
           END-IF
      *    診療科
           MOVE    WRK-SRYKA           TO  SSANTEI-SRYKA
           MOVE    WRK-HKNCOMBI        TO  SSANTEI-HKNCOMBINUM
      *    保険区分は同じ
           MOVE    SPA-HKNKBN          TO  SSANTEI-HKNKBN
           MOVE    SPA-RSI-HKNKBN      TO  SSANTEI-RSI-HKNKBN
           MOVE    SPA-RSI-JUNKYO      TO  SSANTEI-RSI-JUNKYO
           IF      FLG-SYOSIN-DUMMY    =   1
               CONTINUE
           ELSE
      *        内分泌負荷試験用
               IF      LNK-WKSRYP-MSANTEITFLG(IDX)   =   1
                   MOVE    LNK-WKSRYP-MSANTEITEN (IDX)
                                        TO  SSANTEI-MSANTEITEN
                   MOVE    LNK-WKSRYP-MSANTEITENKBN (IDX)
                                        TO  SSANTEI-MSANTEITENKBN
                   MOVE    1                TO  FLG-NAIBUN
               END-IF
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
      *
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
      *    連番号（通常は１件のみ、内服薬の逓減の場合複数）
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
      *    内服７種類逓減対象剤番号
           IF      LNK-WKSRYP-TEI-NKBN (1) =   2
               PERFORM VARYING     IDW     FROM    1   BY  1
                       UNTIL       IDW     >   30
                   IF      TBL-ACCTP-HOSPNUM (IDW)  =   ZERO
                       MOVE    SRYACCTPLUS-REC
                                           TO  TBL-SRYACCTPLUS-REC(IDW)
                       MOVE    ACCT-HKNCOMBI
                                           TO  TBL-ACCTP-HKNCOMBI (IDW)
                       MOVE    ACCT-SRYKBN
                                           TO  TBL-ACCTP-SRYKBN   (IDW)
                       MOVE    30          TO  IDW
                   END-IF
               END-PERFORM
           END-IF
      *H27.4
      *    ３０日逓減対象剤
           IF      LNK-WKSRYP-TEI-NKBN30 (1)   =   2
               PERFORM VARYING     IDW     FROM    1   BY  1
                       UNTIL       IDW     >   30
                   IF      TBL-ACCTP30-HOSPNUM (IDW)  =   ZERO
                       MOVE    SRYACCTPLUS-REC
                                           TO  TBL-SRYACCTPLUS30-REC
                                                                (IDW)
                       MOVE    ACCT-HKNCOMBI
                                           TO  TBL-ACCTP30-HKNCOMBI
                                                                 (IDW)
                       MOVE    ACCT-SRYKBN
                                           TO  TBL-ACCTP30-SRYKBN(IDW)
                       MOVE    30          TO  IDW
                   END-IF
               END-PERFORM
           END-IF
           IF      (LNK-WKSRYP-TEI-NKBN (1)    =   2 )
               OR  (LNK-WKSRYP-TEI-NKBN30 (1)  =   2 )
               CONTINUE
           ELSE
               PERFORM 4503-SRYACCTPLUS-INS-SEC
           END-IF
      *
           .
       4503-SRYACCTPLUS-SYORI-EXT.
           EXIT.
      *****************************************************************
      *    内服７種類逓減診療会計附加情報追加処理
      *****************************************************************
       4504-SRYACCTPLUS-GENINS-SEC              SECTION.
      *
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL      (IDW     >   30   )   OR
                              (TBL-ACCTP-HOSPNUM (IDW)  =   ZERO )
               MOVE    TBL-SRYACCTPLUS-REC(IDW)    TO  SRYACCTPLUS-REC
               INITIALIZE                  ACCTP-GRP-ZAINUM-G
               MOVE    ZERO                TO  IDW2
               PERFORM VARYING     IDX-GEN2    FROM    1   BY  1
                       UNTIL      (IDX-GEN2    >   30   )   OR
                                  (WRK-ACTP-SRYKA(IDX-GEN2)
                                              =   SPACE)
                   IF     (ACCTP-SRYKA        =    WRK-ACTP-SRYKA
                                                          (IDX-GEN2))
                     AND  (TBL-ACCTP-HKNCOMBI(IDW)
                                              =    WRK-ACTP-HKNCOMBI
                                                          (IDX-GEN2))
                     AND  (TBL-ACCTP-SRYKBN  (IDW)
                                              =    WRK-ACTP-SRYKBN
                                                          (IDX-GEN2))
                       PERFORM VARYING IDX-GEN FROM    1   BY  1
                               UNTIL   (IDX-GEN    >   50  )   OR
                                       (WRK-ACTP-ZAINUM
                                                  (IDX-GEN2 IDX-GEN)
                                                       =  ZERO)
                           ADD     1                   TO  IDW2
                           IF      IDW2                >   15
                               PERFORM 4503-SRYACCTPLUS-INS-SEC
                               MOVE    1           TO  IDW2
                               INITIALIZE          ACCTP-GRP-ZAINUM-G
                               ADD     1           TO  ACCTP-RENNUM
                           END-IF
                           MOVE    WRK-ACTP-ZAINUM (IDX-GEN2 IDX-GEN)
                                          TO  ACCTP-GRP-ZAINUM (IDW2)
                       END-PERFORM
                       IF      IDW2                >   ZERO
                           PERFORM 4503-SRYACCTPLUS-INS-SEC
                       END-IF
                       MOVE    30             TO  IDX-GEN2
                   END-IF
               END-PERFORM
           END-PERFORM
      *
      *H27.4
      *    30日逓減
           PERFORM VARYING     IDW     FROM    1   BY  1
                   UNTIL      (IDW     >   30   )   OR
                              (TBL-ACCTP30-HOSPNUM (IDW)  =   ZERO )
               MOVE    TBL-SRYACCTPLUS30-REC(IDW)    TO  SRYACCTPLUS-REC
               INITIALIZE                  ACCTP-GRP-ZAINUM-G
               MOVE    ZERO                TO  IDW2
               PERFORM VARYING     IDX-GEN2    FROM    1   BY  1
                       UNTIL      (IDX-GEN2    >   30   )   OR
                                  (WRK-ACTP30-SRYKA (IDX-GEN2)
                                              =   SPACE)
                   IF     (ACCTP-SRYKA        =    WRK-ACTP30-SRYKA
                                                          (IDX-GEN2))
                     AND  (TBL-ACCTP30-HKNCOMBI(IDW)
                                              =    WRK-ACTP30-HKNCOMBI
                                                          (IDX-GEN2))
                     AND  (TBL-ACCTP30-SRYKBN  (IDW)
                                              =    WRK-ACTP30-SRYKBN
                                                          (IDX-GEN2))
                       PERFORM VARYING IDX-GEN FROM    1   BY  1
                               UNTIL   (IDX-GEN    >   50  )   OR
                                       (WRK-ACTP30-ZAINUM
                                                  (IDX-GEN2 IDX-GEN)
                                                       =  ZERO)
                           ADD     1                   TO  IDW2
                           IF      IDW2                >   15
                               PERFORM 4503-SRYACCTPLUS-INS-SEC
                               MOVE    1           TO  IDW2
                               INITIALIZE          ACCTP-GRP-ZAINUM-G
                               ADD     1           TO  ACCTP-RENNUM
                           END-IF
                           MOVE    WRK-ACTP30-ZAINUM (IDX-GEN2 IDX-GEN)
                                          TO  ACCTP-GRP-ZAINUM (IDW2)
                       END-PERFORM
                       IF      IDW2                >   ZERO
                           PERFORM 4503-SRYACCTPLUS-INS-SEC
                       END-IF
                       MOVE    30             TO  IDX-GEN2
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           .
       4504-SRYACCTPLUS-GENINS-EXT.
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
              DISPLAY "K03 SRYACCTPLUS INS ERR:" ACCTP-KEY  "."
              DISPLAY "MCP-RC:" MCP-RC ":" 
              MOVE    "0005"          TO  SPA-ERRCD
              STRING  "SRYACCTPLUS INS  ERR KEY : "
                       ACCTP-KEY       DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
              END-STRING
           END-IF
      *
           .
       4503-SRYACCTPLUS-INS-EXT.
           EXIT.
      *
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
      *****************************************************************
      *    訂正で科変更のあった受診履歴の変更
      *****************************************************************
       45004-ALL-JYURRK-UPD-SEC              SECTION.
      *
           IF      SPA-TEI-DENPNUM (1)  =   ZERO
      *        複数科でない時の診療科変更
               PERFORM VARYING     IDXF    FROM    1   BY  1
                       UNTIL      (IDXF    >   15  )  OR
                                  (WRK-TEI-SRYKA (IDXF)    =   SPACE)
                   MOVE    WRK-TEI-SRYKA (IDXF)    TO  WRK-SRYKA
                   MOVE    ZERO                TO  FLG-OK
                   PERFORM VARYING     IDH3    FROM    1   BY  1
                           UNTIL      (IDH3    >   15  )  OR
                                      (TBL-SRYKA(IDH3) =   SPACE) OR
                                      (FLG-OK          =   1  )
                       IF     TBL-SRYKA (IDH3)     =   WRK-SRYKA
                           MOVE    1               TO  FLG-OK
                       END-IF
                   END-PERFORM
                   IF      FLG-OK              =   ZERO
                        PERFORM 4500111-TEISEI-JYURRK-HEN-SEC
                   END-IF
               END-PERFORM
          END-IF
      *
          PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF    >   15  )  OR
                              (SPA-TEI-DENPNUM (IDXF)  =   ZERO )
               MOVE    SPA-TEI-SRYKA (IDXF)    TO  WRK-SRYKA
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDH3    FROM    1   BY  1
                       UNTIL      (IDH3    >   15  )  OR
                                  (TBL-SRYKA(IDH3) =   SPACE) OR
                                  (FLG-OK          =   1  )
                   IF     TBL-SRYKA (IDH3)     =   WRK-SRYKA
                       MOVE    1               TO  FLG-OK
                   END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
                   PERFORM 4500111-TEISEI-JYURRK-HEN-SEC
               END-IF
      *        今回削除分の収納の削除
               MOVE    ZERO                TO  FLG-OK
               PERFORM VARYING     IDX     FROM    1   BY  1
                       UNTIL      (IDX     >   ORCSCGAIRAI-SYUNOU-MAX)
                               OR (FLG-OK  =   1    )
                   MOVE    ORCSCGAIRAI-SYUNOU-REC(IDX) TO  SYUNOU-REC
                   IF      SYU-DENPNUM     =   SPA-TEI-DENPNUM (IDXF) 
                    AND   (SYU-HKNCOMBINUM NOT =   "9999" )
                       MOVE    1               TO  FLG-OK
                       MOVE    IDX             TO  IDH-GAI
                    END-IF
               END-PERFORM
               IF      FLG-OK              =   ZERO
      *H24.5
                  IF      SPA-TEI-HKNCOMBI (IDXF)    NOT =   "9999"
      *
                    MOVE    SPA-TEI-DENPNUM (IDXF) TO  WRK-DENPNUM
                    PERFORM 450041-TEI-SYUNOU-DEL-SEC
                  END-IF
               END-IF
           END-PERFORM
           .
       45004-ALL-JYURRK-UPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    収納削除処理
      *****************************************************************
       450041-TEI-SYUNOU-DEL-SEC              SECTION.
      *    収納テーブル読み込み
           MOVE    SPACE               TO  SYUNOU-REC
           INITIALIZE                  SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    WRK-DENPNUM         TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    SPACE               TO  SYUNOU-REC
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYUNOU          =   ZERO
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        収納明細削除
               MOVE    SPACE               TO  SYUMEI-REC
               INITIALIZE                      SYUMEI-REC
               MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
               MOVE    SYU-PTID            TO  SMEI-PTID
               MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        収納履歴マスタ
               INITIALIZE                  SYURRK-REC
               MOVE    SYU-HOSPNUM         TO  SYURRK-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  SYURRK-NYUGAIKBN
               MOVE    SYU-PTID            TO  SYURRK-PTID
               MOVE    SYU-DENPNUM         TO  SYURRK-DENPNUM
      *
               MOVE    SYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syurrk"        TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
      *    分娩管理テーブル削除
           IF     (FLG-SYUNOU          =   ZERO )   AND
                  (SYU-BUNBENINFFLG    =   "1"  )
               INITIALIZE                      BUNBEN-REC
               MOVE    SYU-HOSPNUM         TO  BUNBEN-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  BUNBEN-NYUGAIKBN
               MOVE    SYU-PTID            TO  BUNBEN-PTID
               MOVE    SYU-SRYYMD(1:6)     TO  BUNBEN-SKYYM
               MOVE    SYU-DENPNUM         TO  BUNBEN-RRKNUM
               MOVE    BUNBEN-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_bunben"        TO  MCP-TABLE
               MOVE    "del3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           .
       450041-TEI-SYUNOU-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    ワーク診療行為マスタ−削除処理
      *****************************************************************
       45002-WKSRYACT-DEL-SEC              SECTION.
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
       45002-WKSRYACT-DEL-EXT.
           EXIT.
      *
      *****************************************************************
      *    受診内容ファイル更新処理
      *****************************************************************
       45003-UKETUKE-SEC            SECTION.
      *
      *
           ACCEPT  WRK-TIME11           FROM  TIME
           MOVE    WRK-TIME1(1:4)       TO  WRK-TIME2(1:4)
      *    受診内容ファイルを特定する
           INITIALIZE                      UKETUKE-REC
           MOVE    SPA-HOSPNUM         TO  UKE-HOSPNUM
           MOVE    SPA-SYSYMD          TO  UKE-UKEYMD
           MOVE    SPA-PTID            TO  UKE-PTID
           MOVE    SPA-SRYKA           TO  UKE-SRYKA
      *
           MOVE    UKETUKE-REC         TO  MCPDATA-REC
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 976-UKETUKE-NEXT-SEC
           ELSE
               MOVE    1                   TO  FLG-UKETUKE
           END-IF
           PERFORM
                   UNTIL       FLG-UKETUKE     =    1
               MOVE    ZERO                TO  FLG-SRYKA
               IF      UKE-UKEID       NOT =   ZERO
                   PERFORM 450031-UKETUKE-SRYKA-SEC
               END-IF
               IF      (FLG-SRYKA          =   1        ) AND
                       (UKE-UKEID      NOT =   ZERO     )
                   IF      UKE-KAIKEITIME      =   ZERO
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
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "K03 UKETUKE UPD ERR:" UKE-KEY
                                   ",MCP-RC:"  MCP-RC
                           MOVE    "0005"          TO  SPA-ERRCD
                           STRING  "UKETUKE UPD  ERR KEY : "
                                   UKE-KEY       DELIMITED  BY  SIZE
                                   INTO  ORCSCGAIRAI-ERRMSG
                           END-STRING
                       END-IF
                  END-IF
               END-IF
      *
               MOVE    "tbl_uketuke"       TO  MCP-TABLE
               MOVE    "key2"              TO  MCP-PATHNAME
               PERFORM 976-UKETUKE-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_uketuke"       TO  MCP-TABLE
           MOVE    "key2"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           .
       45003-UKETUKE-EXT.
           EXIT.
      *****************************************************************
      *    複数科判定処理
      *****************************************************************
       450031-UKETUKE-SRYKA-SEC        SECTION.
      *
           IF     SPA-SRYKA            =   UKE-SRYKA
               MOVE    1               TO  FLG-SRYKA
           END-IF
           PERFORM VARYING     IDH3    FROM    1   BY  1
                   UNTIL      (IDH3    >   15  )   OR
                              (FLG-SRYKA           =   1  )  OR
                              (TBL-SRYKA (IDH3)    =   SPACE)
               IF     TBL-SRYKA (IDH3)     =   UKE-SRYKA
                   MOVE    1               TO  FLG-SRYKA
               END-IF
           END-PERFORM
      *
           .
       450031-UKETUKE-SRYKA-EXT.
           EXIT.
      *****************************************************************
      *    複数科まとめ収納作成処理
      *****************************************************************
       1004-DBKOUSIN-SYUTTL-SEC        SECTION.
      *
           MOVE    ZERO                TO  FLG-OK
           MOVE    ZERO                TO  WRK-SUM-NYUKIN
           PERFORM VARYING     IDX     FROM    1   BY  1
                   UNTIL      (IDX     >   ORCSCGAIRAI-SYUNOU-MAX)
      **************      OR  (FLG-OK  =   1    )
               IF      FLG-OK          =   ZERO
                   MOVE    ORCSCGAIRAI-SYUNOU-REC(IDX) TO  SYUNOU-REC
                   IF     (SYU-SRYKA       =   WRK-SRYKA )  AND
                          (SYU-HKNCOMBINUM =   WRK-HKNCOMBI)
                       MOVE    1               TO  FLG-OK
                       MOVE    IDX             TO  IDH-GAI
                   END-IF
               END-IF
               MOVE    ORCSCGAIRAI-SYUNOU-REC(IDX) TO  WRK-SYUNOU-REC
               IF     (WRK-SYU-SRYKA   NOT =   WRK-SRYKA )  AND
                      (WRK-SYU-HKNCOMBINUM =   WRK-HKNCOMBI)
      *            収納入金額合計集計
                   ADD     WRK-TTL-NYUKIN-TOTAL(IDX)
                                           TO  WRK-SUM-NYUKIN
                END-IF
           END-PERFORM
           IF      FLG-OK              =   1
                PERFORM 10041-SYUTTL-INS-SEC
           END-IF
           .
       1004-DBKOUSIN-SYUTTL-EXT.
           EXIT.
      *****************************************************************
      *    複数科まとめ収納作成処理
      *****************************************************************
       10041-SYUTTL-INS-SEC        SECTION.
      *
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUTTL-PTID
           MOVE    WRK-KA-DENPNUM      TO  SYUTTL-DENPNUM
           MOVE    SYUTTL-REC          TO  MCPDATA-REC
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 981-SYUTTL-READ-SEC
           ELSE
               MOVE    1               TO  FLG-SYUTTL
               INITIALIZE              SYUTTL-REC
           END-IF
           MOVE    "tbl_syutotal"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
           MOVE    SPACE               TO  SYUTTL-REC
           INITIALIZE                  SYUTTL-REC
           MOVE    SYU-HOSPNUM         TO  SYUTTL-HOSPNUM
           MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
           MOVE    SYU-PTID            TO  SYUTTL-PTID
           MOVE    WRK-KA-DENPNUM      TO  SYUTTL-DENPNUM
      *    診療科（合計分の為００で固定）
           MOVE    ZERO                TO  SYUTTL-SRYKA
      *    同時入力診療科
           PERFORM VARYING     IDH2    FROM    1   BY  1
                   UNTIL      (IDH2    >   10  )  OR
                              (SPA-TBL-SRYKA(IDH IDH2) =   SPACE)
               MOVE    SPA-TBL-SRYKA(IDH IDH2)
                                       TO  SYUTTL-DOUJI-SRYKA(IDH2)
           END-PERFORM
      *    診療日
           MOVE    SPA-SRYYMD          TO  SYUTTL-SRYYMD
      *伝票作成区分（空白：外来　１：退院時作成　２：定期請求
      *              ３：入院レセプト用ダミー）
           MOVE    SPACE               TO  SYUTTL-CREATEKBN
      *    伝票発行日
           MOVE    SYU-DENPPRTYMD      TO  SYUTTL-DENPPRTYMD
           MOVE    SYU-SKY-KIKAN       TO  SYUTTL-SKY-KIKAN
           MOVE    WRK-HKNCOMBI        TO  SYUTTL-HKNCOMBINUM
      *主保険−保険番号
           MOVE    SYU-SYUHKNNUM       TO  SYUTTL-SYUHKNNUM
           MOVE    SYU-SYUHKNFTNMONEY  TO  SYUTTL-SYUHKNFTNMONEY
           MOVE    SYU-SYUCOMPFTN      TO  SYUTTL-SYUCOMPFTN
           MOVE    SYU-SYUCOMPFTN-ENTANI
                                       TO  SYUTTL-SYUCOMPFTN-ENTANI
           MOVE    SYU-SYUCOMPYKZFTN   TO  SYUTTL-SYUCOMPYKZFTN
           MOVE    SYU-SYUCOMPTOTAL    TO  SYUTTL-SYUCOMPTOTAL
           MOVE    SYU-SYUYKZFTNKBN    TO  SYUTTL-SYUYKZFTNKBN
      *公費−負担情報
           MOVE    SYU-KOH-AREA        TO  SYUTTL-KOH-AREA
      *患者負担率
           MOVE    SYU-PTFTNRATE       TO  SYUTTL-PTFTNRATE
      *請求書作成区分
           MOVE    SYU-SKYKUKBN        TO  SYUTTL-SKYKUKBN
      *請求内容
           MOVE    SYU-SKY-NAIYOU      TO  SYUTTL-SKY-NAIYOU
      *処方せん料再掲
           MOVE    SYU-SHOHOU-SAI      TO  SYUTTL-SHOHOU-SAI
      *保険適用外金額−消費税（再掲）
           MOVE    SYU-TGMONEY-TAX-SAI TO  SYUTTL-TGMONEY-TAX-SAI
      *自費１から５
           MOVE    SYU-JIHI-NAIYOU     TO  SYUTTL-JIHI-NAIYOU
      *自費小計（消費税なし）
           MOVE    SYU-JIHI-TOTAL      TO  SYUTTL-JIHI-TOTAL
           MOVE    SYU-JIHI-TOTAL-TAX  TO  SYUTTL-JIHI-TOTAL-TAX
           MOVE    SYU-JIHI-TAX        TO  SYUTTL-JIHI-TAX
      *老人一部負担金
           MOVE    SYU-RJN-FTN         TO  SYUTTL-RJN-FTN
           MOVE    SYU-KOH-FTN         TO  SYUTTL-KOH-FTN
           MOVE    SYU-KOH-FTN-ENTANI  TO  SYUTTL-KOH-FTN-ENTANI
      *調整金
           MOVE    SYU-CHOSEI          TO  SYUTTL-CHOSEI
           MOVE    SYU-CHOSEI1         TO  SYUTTL-CHOSEI1
           MOVE    SYU-CHOSEI2         TO  SYUTTL-CHOSEI2
      *まとめ入力差額
           MOVE    SYU-GRP-SGKMONEY    TO  SYUTTL-GRP-SGKMONEY
      *保険適用金額
           MOVE    SYU-HKNTEKMONEY     TO  SYUTTL-HKNTEKMONEY
      *病院減免事由区分
           MOVE    SYU-DISCOUNT-KBN    TO  SYUTTL-DISCOUNT-KBN
           MOVE    SYU-DISCOUNT-BODY   TO  SYUTTL-DISCOUNT-BODY
           MOVE    SYU-DISCOUNT-RATEKBN
                                       TO  SYUTTL-DISCOUNT-RATEKBN
           MOVE    SYU-DISCOUNT-TEIRITU
                                       TO  SYUTTL-DISCOUNT-TEIRITU
           MOVE    SYU-DISCOUNT-RATE   TO  SYUTTL-DISCOUNT-RATE
      *減免金額
           MOVE    SYU-DISCOUNT-MONEY  TO  SYUTTL-DISCOUNT-MONEY
      *労災保険
           MOVE    SYU-RSISHOSHIN-MONEY
                                       TO  SYUTTL-RSISHOSHIN-MONEY
           MOVE    SYU-RSISAISHIN-MONEY
                                       TO  SYUTTL-RSISAISHIN-MONEY
           MOVE    SYU-RSISDO-MONEY    TO  SYUTTL-RSISDO-MONEY
           MOVE    SYU-RSIETC-MONEY    TO  SYUTTL-RSIETC-MONEY
           MOVE    SYU-RSI-TAX-SAI     TO  SYUTTL-RSI-TAX-SAI
           MOVE    SYU-RSI-TOTAL       TO  SYUTTL-RSI-TOTAL
           MOVE    SYU-RSIJIBAI-SKYMONEY
                                       TO  SYUTTL-RSIJIBAI-SKYMONEY
      *請求金額−消費税（再掲）
           MOVE    SYU-SKYMONEY-TAX-SAI
                                       TO  SYUTTL-SKYMONEY-TAX-SAI
      *請求金額
           MOVE    SYU-SKYMONEY        TO  SYUTTL-SKYMONEY
           COMPUTE SYUTTL-SKYMONEY     =   SYUTTL-SKYMONEY
                                       +   SYUTTL-CHOSEI
      *    入金額は収納の合計
      **** MOVE    SYU-NYUKIN-TOTAL    TO  SYUTTL-NYUKIN-TOTAL
           MOVE    WRK-SUM-NYUKIN      TO  SYUTTL-NYUKIN-TOTAL
           MOVE    SYU-NYUKIN-KAISU    TO  SYUTTL-NYUKIN-KAISU
      *未収理由
           MOVE    SYU-MISYURIYU       TO  SYUTTL-MISYURIYU
      *在総診区分
           MOVE    SYU-ZAITAKU         TO  SYUTTL-ZAITAKU
      *診療区分別給付外点数
           MOVE    SYU-KYUFUGAI        TO  SYUTTL-KYUFUGAI
      *保険適用区分
           MOVE    SYU-HKNTEKKBN       TO  SYUTTL-HKNTEKKBN
      *
      *    伝票状態区分
           IF      SYUTTL-SKYMONEY     >   SYUTTL-NYUKIN-TOTAL
               MOVE    "2"             TO  SYUTTL-DENPJTIKBN
           ELSE
               MOVE    "1"             TO  SYUTTL-DENPJTIKBN
           END-IF
      *    請求額がゼロの時空白
           IF      SYUTTL-SKYMONEY     =   ZERO
               MOVE    SPACE               TO  SYUTTL-DENPJTIKBN
           END-IF
           IF      FLG-SYUTTL          =   1
      *        新規作成
               MOVE    SMCNDATE-YMD        TO  SYUTTL-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
      *
               MOVE    SPA-OPID            TO  SYUTTL-OPID
      *
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBINSERT"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUTTL INS ERR:" SYUTTL-KEY
                       ",MCP-RC:"  MCP-RC
                   MOVE    "0005"          TO  SPA-ERRCD
                   STRING  "SYUTTL INS  ERR KEY : "
                           SYUTTL-KEY      DELIMITED  BY  SIZE
                                   INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
           ELSE
      *        更新（新規とする）
               MOVE    SMCNDATE-YMD        TO  SYUTTL-CREYMD
               MOVE    SMCNDATE-YMD        TO  SYUTTL-UPYMD
               MOVE    SMCNDATE-HMS        TO  SYUTTL-UPHMS
      *
               MOVE    SPA-OPID            TO  SYUTTL-OPID
      *
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "DBUPDATE"          TO  MCP-FUNC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
               IF      MCP-RC          NOT =   ZERO
                   DISPLAY "K03 SYUTTL UPD ERR:" SYUTTL-KEY
                       ",MCP-RC:"  MCP-RC
                   STRING  "SYUTTL UPD  ERR KEY : "
                           SYUTTL-KEY      DELIMITED  BY  SIZE
                                   INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
                   MOVE    "0005"          TO  SPA-ERRCD
               END-IF
           END-IF
      *
           .
       10041-SYUTTL-INS-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       300-DELETE-SYORI-SEC             SECTION.
      *
           MOVE    ZERO                TO  FLG-NAIBUN
      *
      *    レセプト作成管理マスタ更新処理
           IF      SPA-HKNCOMBINUM NOT =   "9999"
               PERFORM 3009-RECEUPD-UPDATE-SEC
           END-IF
      *
      *    削除処理
      *    複数科・保険入力があった場合、すべて削除する
           IF      SPA-TEI-DENPNUM (1) NOT =   ZERO
               PERFORM 45001210-GRP-SRYACTDEL-SEC
           ELSE
               PERFORM 4500121-TEISEI-SRYACTDEL-SEC
           END-IF
      *R01.10
      *    削除ログ処理
           IF      SPA-ERRCD           =   SPACE
               PERFORM 30010-PUSHINFO-INSERT-SEC
           END-IF
      *
      *    診療科履歴変更
           IF     (SPA-HKNCOMBINUM     =   "9999" ) OR
                  (SPA-TEI-DENPNUM (1) NOT =   ZERO   )
               CONTINUE
           ELSE
               INITIALIZE                  ORCSKARRKAREA
               MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
      ******   MOVE    SPA-SRYKA           TO  SKARRK-SRYKA
               MOVE    SPA-OLD-SRYKA       TO  SKARRK-SRYKA
               CALL    "ORCSKARRK"         USING
                                           ORCSKARRKAREA
                                           SPA-AREA
               IF      SKARRK-RC       NOT =   ZERO
                   MOVE    "0005"              TO  SPA-ERRCD
                   STRING  "ORCSKARRK ERR "
                           SKARRK-ERRMSG      DELIMITED  BY  SIZE
                                   INTO  ORCSCGAIRAI-ERRMSG
                   END-STRING
               END-IF
           END-IF
      *
           IF      FLG-NAIBUN          =   1
              AND  SPA-ERRCD           =   SPACE
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
      **** レセプト作成管理マスタ更新処理
      *    IF      SPA-HKNCOMBINUM NOT =   "9999"
      *        PERFORM 3009-RECEUPD-UPDATE-SEC
      **** END-IF
           IF      SPA-ERRCD           =   SPACE
      *        ワーク診療行為マスタ−が存在した時、削除する
               PERFORM 45002-WKSRYACT-DEL-SEC
           END-IF
      *    主科設定
           IF      SPA-ERRCD           =   SPACE
               PERFORM 1009-ORCSSYUKAMAIN-SEC
           END-IF
      *
           .
       300-DELETE-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    削除処理
      *****************************************************************
       4500121-TEISEI-SRYACTDEL-SEC    SECTION.
      *
      *    受診履歴の連番をすべて削除する
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    SPA-OLD-SRYKA       TO  JYURRK-SRYKA
           MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
           MOVE    SPA-RENNUM          TO  JYURRK-RENNUM
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
      *
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               MOVE    ZERO                TO  WRK-JYURRK-RENNUM
               MOVE    ZERO                TO  WRK-JYURRK-EDANUM
               MOVE    ZERO                TO  WRK-JYURRK-DOUJI-RENNUM
               MOVE    ZERO                TO  WRK-KAIKEI-RENNUM
      *
      *        診療会計マスタの診療回数をクリアする
               PERFORM 4500112-TEISEI-SRYACCT-SEC
      *        会計連番
               MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
               MOVE    JYURRK-DOUJI-RENNUM TO  WRK-JYURRK-DOUJI-RENNUM
               MOVE    JYURRK-KAIKEI-RENNUM
                                           TO  WRK-KAIKEI-RENNUM
               MOVE    JYURRK-SRYKA        TO  WRK-JYURRK-SRYKA
      *
      *        受診履歴・収納削除
               PERFORM 4500114-TEISEI-JYURRKDEL-SEC
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key5"              TO  MCP-PATHNAME
               PERFORM 970-JYURRK-NEXT-SEC
           END-PERFORM
      *
           MOVE    "tbl_jyurrk"        TO  MCP-TABLE
           MOVE    "key5"              TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    受診履歴の連番変更する
           MOVE    SPA-OLD-SRYKA       TO  WRK-JYURRK-SRYKA
           PERFORM 4500115-TEISEI-RENNUM-SEC
      *
           .
       4500121-TEISEI-SRYACTDEL-EXT.
           EXIT.
      *****************************************************************
      *    受診履歴の連番変更処理
      *****************************************************************
       4500115-TEISEI-RENNUM-SEC          SECTION.
      *
           MOVE    SPACE               TO  JYURRK-REC
           INITIALIZE                      JYURRK-REC
           MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
           MOVE    SPA-PTID            TO  JYURRK-PTID
           MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
           MOVE    WRK-JYURRK-SRYKA    TO  JYURRK-SRYKA
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
           MOVE    SPACE               TO  TBL-JYURRK-AREA
           MOVE    ZERO                TO  IDX 
           MOVE    ZERO                TO  WRK-RENNUM-KEY
           MOVE    ZERO                TO  WRK-JYURRK-RENNUM
           PERFORM
                   UNTIL       FLG-JYURRK      =   1
               IF      JYURRK-RENNUM       =   WRK-RENNUM-KEY
                   CONTINUE
               ELSE
                   IF      WRK-JYURRK-RENNUM   =   9
                       MOVE    "XXXX"              TO  WRK-ERRCD
                   END-IF
                   ADD     1                   TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-RENNUM       TO  WRK-RENNUM-KEY
               END-IF
      *
               IF      JYURRK-RENNUM       =   WRK-JYURRK-RENNUM
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
                   IF      MCP-RC          NOT =   ZERO
                       DISPLAY "K03 JYURRK DEL ERR:" JYURRK-KEY
                               ",MCP-RC:" MCP-RC
                       MOVE    "0005"          TO  SPA-ERRCD
                   END-IF
                   MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
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
                       DISPLAY "K03 JYURRK INS ERR:" JYURRK-KEY
                               ",MCP-RC:" MCP-RC
                       MOVE    "0005"          TO  SPA-ERRCD
                       STRING  "JYURRK INS ERR KEY : "
                               JYURRK-KEY    DELIMITED  BY  SIZE
                                       INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                       IF      WRK-ERRCD       NOT =   SPACE
                           MOVE    "0105"      TO  SPA-ERRCD
                           MOVE    SPACE       TO  ORCSCGAIRAI-ERRMSG
                       END-IF
                   END-IF
      *            診療会計修正
                   PERFORM 4500115-TEISEI-SRYACTUPD-SEC
      *
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
      *    複数入力分削除処理
      *****************************************************************
       45001210-GRP-SRYACTDEL-SEC          SECTION.
      *    科毎の保険数を設定する
           INITIALIZE                  TBL-SRYKA-AREA
           PERFORM VARYING     IDH     FROM    1   BY  1
                   UNTIL      (IDH     >   15  )  OR
                              (SPA-TEI-SRYKA(IDH)   =   SPACE)
               MOVE    SPA-TEI-SRYKA(IDH)      TO  WRK-SRYKA
               PERFORM VARYING     IDH3    FROM    1   BY  1
                       UNTIL       IDH3    >   15
                   IF     TBL-SRYKA (IDH3)     =   SPACE
                       MOVE    WRK-SRYKA   TO  TBL-SRYKA (IDH3)
                   END-IF
                   IF      TBL-SRYKA (IDH3)    =   WRK-SRYKA
                       ADD     1           TO  TBL-SRYKA-MAX(IDH3)
                       MOVE    15          TO  IDH3
                   END-IF
               END-PERFORM
           END-PERFORM
      *
           PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF    >   15  )  OR
                              (SPA-TEI-DENPNUM(IDXF)   =   ZERO)
               MOVE    SPACE               TO  JYURRK-REC
               INITIALIZE                      JYURRK-REC
               MOVE    SPA-HOSPNUM         TO  JYURRK-HOSPNUM
               MOVE    SPA-PTID            TO  JYURRK-PTID
               MOVE    SPA-NYUGAIKBN       TO  JYURRK-NYUGAIKBN
               MOVE    SPA-TEI-SRYKA(IDXF) TO  JYURRK-SRYKA
               MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
               MOVE    SPA-TEI-DENPNUM(IDXF)
                                           TO  JYURRK-DENPNUM
      *
               MOVE    JYURRK-REC          TO  MCPDATA-REC
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               ELSE
                   MOVE    1                   TO  FLG-JYURRK
               END-IF
      *
               PERFORM
                       UNTIL       FLG-JYURRK      =   1
               IF      JYURRK-SRYYMD       =   SPA-SRYYMD
      *
      *            診療会計マスタの診療回数をクリアする
                   PERFORM 4500112-TEISEI-SRYACCT-SEC
      *            会計連番
                   MOVE    JYURRK-RENNUM       TO  WRK-JYURRK-RENNUM
                   MOVE    JYURRK-DOUJI-RENNUM TO
                                               WRK-JYURRK-DOUJI-RENNUM
                   MOVE    JYURRK-KAIKEI-RENNUM
                                               TO  WRK-KAIKEI-RENNUM
                   MOVE    JYURRK-SRYKA        TO  WRK-JYURRK-SRYKA
      *
      *            受診履歴・収納削除
                   PERFORM 4500114-TEISEI-JYURRKDEL-SEC
      *
               END-IF
      *
                   MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                   MOVE    "key6"              TO  MCP-PATHNAME
                   PERFORM 970-JYURRK-NEXT-SEC
               END-PERFORM
      *
               MOVE    "tbl_jyurrk"        TO  MCP-TABLE
               MOVE    "key6"              TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
           END-PERFORM
      *
      *    受診履歴の連番変更する
           MOVE    SPACE               TO  WRK-JYURRK-SRYKA
           PERFORM VARYING     IDXF    FROM    1   BY  1
                   UNTIL      (IDXF    >   15  )  OR
                              (TBL-SRYKA (IDXF)   =   SPACE)
               MOVE    TBL-SRYKA (IDXF)    TO  WRK-JYURRK-SRYKA
               PERFORM 4500115-TEISEI-RENNUM-SEC
      *        診療科履歴変更
               IF      SPA-HKNCOMBINUM     =   "9999"
                   CONTINUE
               ELSE
                   INITIALIZE                  ORCSKARRKAREA
                   MOVE    SPA-SRYYMD(1:6)     TO  SKARRK-SRYYM
                   MOVE    WRK-JYURRK-SRYKA    TO  SKARRK-SRYKA
                   CALL    "ORCSKARRK"         USING
                                               ORCSKARRKAREA
                                               SPA-AREA
                   IF      SKARRK-RC       NOT =   ZERO
                       MOVE    "0005"              TO  SPA-ERRCD
                       STRING  "ORCSKARRK ERR "
                               SKARRK-ERRMSG   DELIMITED  BY  SIZE
                                               INTO  ORCSCGAIRAI-ERRMSG
                       END-STRING
                   END-IF
               END-IF
           END-PERFORM
      *
           .
       45001210-GRP-SRYACTDEL-EXT.
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
                   MOVE    WRK-JYURRK-SRYKA    TO  JYURRK-SRYKA
                   MOVE    SPA-SRYYMD          TO  JYURRK-SRYYMD
                   MOVE    WRK-JYURRK-RENNUM   TO  JYURRK-RENNUM
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
                   IF      FLG-JYURRK          =   ZERO
      *                収納削除
                       IF      JYURRK-HKNCOMBINUM  NOT =   "9999"
                           PERFORM 45001141-SYUNOU-DELETE-SEC
                       END-IF
      *
      *                受診履歴削除
                       MOVE    JYURRK-REC          TO  MCPDATA-REC
                       MOVE    "DBDELETE"          TO  MCP-FUNC
                       MOVE    "tbl_jyurrk"        TO  MCP-TABLE
                       MOVE    "key"               TO  MCP-PATHNAME
grpsys                 CALL    "ORCDBMAIN"         USING   MCPAREA
                                                           MCPDATA-REC
                                                           SPA-AREA
                       IF      MCP-RC          NOT =   ZERO
                           DISPLAY "K03 JYURRK DEL ERR:" JYURRK-KEY
                                   ",MCP-RC:" MCP-RC
                           MOVE    "0005"          TO  SPA-ERRCD
                       END-IF
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
      *    収納マスター削除処理
      *****************************************************************
       45001141-SYUNOU-DELETE-SEC         SECTION.
      *
           INITIALIZE                      SYUNOU-REC
           MOVE    SPA-HOSPNUM         TO  SYU-HOSPNUM
           MOVE    SPA-NYUGAIKBN       TO  SYU-NYUGAIKBN
           MOVE    SPA-PTID            TO  SYU-PTID
           MOVE    JYURRK-DENPNUM      TO  SYU-DENPNUM
      *
           MOVE    SYUNOU-REC          TO  MCPDATA-REC
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 980-SYUNOU-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SYUNOU
           END-IF
           MOVE    "tbl_syunou"        TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
           IF      FLG-SYUNOU          =   ZERO
               MOVE    SYUNOU-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syunou"        TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        収納明細削除
               MOVE    SPACE               TO  SYUMEI-REC
               INITIALIZE                      SYUMEI-REC
               MOVE    SYU-HOSPNUM         TO  SMEI-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  SMEI-NYUGAIKBN
               MOVE    SYU-PTID            TO  SMEI-PTID
               MOVE    SYU-DENPNUM         TO  SMEI-DENPNUM
               MOVE    SYUMEI-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syumei"        TO  MCP-TABLE
               MOVE    "del2"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *        収納履歴マスタ
               INITIALIZE                  SYURRK-REC
               MOVE    SYU-HOSPNUM         TO  SYURRK-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  SYURRK-NYUGAIKBN
               MOVE    SYU-PTID            TO  SYURRK-PTID
               MOVE    SYU-DENPNUM         TO  SYURRK-DENPNUM
      *
               MOVE    SYURRK-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_syurrk"        TO  MCP-TABLE
               MOVE    "del1"              TO  MCP-PATHNAME
grpsys         CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
      *
           END-IF
      *    合計収納削除
           IF     (FLG-SYUNOU          =   ZERO )   AND
                  (SYU-FUKU-DENPNUM    NOT =   ZERO)
               INITIALIZE                  SYUTTL-REC
               MOVE    SYU-HOSPNUM         TO  SYUTTL-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  SYUTTL-NYUGAIKBN
               MOVE    SYU-PTID            TO  SYUTTL-PTID
               MOVE    SYU-FUKU-DENPNUM    TO  SYUTTL-DENPNUM
               MOVE    SYUTTL-REC          TO  MCPDATA-REC
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 910-DBSELECT-SEC
               IF      MCP-RC              =   ZERO
                   MOVE    "tbl_syutotal"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
                   PERFORM 981-SYUTTL-READ-SEC
               ELSE
                   MOVE    1               TO  FLG-SYUTTL
                   INITIALIZE              SYUTTL-REC
               END-IF
               MOVE    "tbl_syutotal"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 990-DBCLOSE-SEC
               IF      FLG-SYUTTL          =   ZERO
                   MOVE    SYUTTL-REC          TO  MCPDATA-REC
                   MOVE    "DBDELETE"          TO  MCP-FUNC
                   MOVE    "tbl_syutotal"      TO  MCP-TABLE
                   MOVE    "key"               TO  MCP-PATHNAME
grpsys             CALL    "ORCDBMAIN"         USING   MCPAREA
                                                       MCPDATA-REC
                                                       SPA-AREA
               END-IF
           END-IF
      *    分娩管理テーブル削除
           IF     (FLG-SYUNOU          =   ZERO )   AND
                  (SYU-BUNBENINFFLG    =   "1"  )
               INITIALIZE                      BUNBEN-REC
               MOVE    SYU-HOSPNUM         TO  BUNBEN-HOSPNUM
               MOVE    SYU-NYUGAIKBN       TO  BUNBEN-NYUGAIKBN
               MOVE    SYU-PTID            TO  BUNBEN-PTID
               MOVE    SYU-SRYYMD(1:6)     TO  BUNBEN-SKYYM
               MOVE    SYU-DENPNUM         TO  BUNBEN-RRKNUM
               MOVE    BUNBEN-REC          TO  MCPDATA-REC
               MOVE    "DBDELETE"          TO  MCP-FUNC
               MOVE    "tbl_bunben"        TO  MCP-TABLE
               MOVE    "del3"              TO  MCP-PATHNAME
               CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           END-IF
           .
       45001141-SYUNOU-DELETE-EXT.
           EXIT.
      *****************************************************************
      *    診療会計修正処理
      *****************************************************************
       4500115-TEISEI-SRYACTUPD-SEC          SECTION.
      *
      *
           MOVE    JYURRK-SRYYMD(7:2)      TO  IDX-2
      *ver.4.8
      *    EVALUATE    JYURRK-RENNUM
      *        WHEN    1
      *            MOVE    2               TO  IDX-1
      *        WHEN    2
      *            MOVE    3               TO  IDX-1
      *        WHEN    OTHER
      *            MOVE    4               TO  IDX-1
      *    END-EVALUATE
      *    EVALUATE    WRK-RENNUM-KEY
      *        WHEN    1
      *            MOVE    2               TO  IDX-3
      *        WHEN    2
      *            MOVE    3               TO  IDX-3
      *        WHEN    OTHER
      *            MOVE    4               TO  IDX-3
      **** END-EVALUATE
      *
           COMPUTE IDX-1               =   JYURRK-RENNUM   +   1
           COMPUTE IDX-3               =   WRK-RENNUM-KEY  +   1
      *
           IF      IDX-1           NOT =   IDX-3
      *        回数テーブルが異なる時のみ変更
               PERFORM 45001112-ACCTDAY-SEC
           END-IF
      *
           .
       4500115-TEISEI-SRYACTUPD-EXT.
           EXIT.
      *
      *****************************************************************
      *    初診算定日のみ設定処理
      *****************************************************************
       400-SYOSIN-SYORI-SEC         SECTION.
      *
      *    初診料算定のみの時初診料を算定する
           MOVE    SPA-SYOSIN-YMD      TO  WRK-SRYYMD
           MOVE    SPA-SYOSIN-SRYCD    TO  WRK-SRYCD
           MOVE    WRK-SRYCD           TO  TNS-SRYCD
           PERFORM 910-TENSU-READ-SEC
      *
           IF      TNS-SANTEIRRKKBN    NOT =   ZERO
               MOVE    ZERO            TO  FLG-TOKTEI
               MOVE    1               TO  WRK-KAISU
               MOVE    1               TO  WRK-SRYSURYO
               MOVE    SPA-SRYKA       TO  WRK-SRYKA
               MOVE    SPA-HKNCOMBINUM TO  WRK-HKNCOMBI
               MOVE    1               TO  FLG-SYOSIN-DUMMY
      *        算定履歴作成
               PERFORM 45021-SANTEI-UPD-SEC
      *        診療科履歴処理
               PERFORM 45099-SRYKARRK-ADD-SEC
           END-IF
           MOVE    ZERO                TO  FLG-SYOSIN-DUMMY
      *
           .
       400-SYOSIN-SYORI-EXT.
           EXIT.
      *
      *****************************************************************
      *    診療科履歴のみ登録処理
      *****************************************************************
       45099-SRYKARRK-ADD-SEC             SECTION.
      *
           MOVE    SPACE               TO  SRYKARRK-REC
           INITIALIZE                      SRYKARRK-REC
           MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
           MOVE    SPA-PTID            TO  KARRK-PTID
           MOVE    WRK-SRYKA           TO  KARRK-SRYKA
      *
           MOVE    SRYKARRK-REC        TO  MCPDATA-REC
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 910-DBSELECT-SEC
           IF      MCP-RC              =   ZERO
               MOVE    "tbl_srykarrk"      TO  MCP-TABLE
               MOVE    "key"               TO  MCP-PATHNAME
               PERFORM 975-SRYKARRK-READ-SEC
           ELSE
               MOVE    1                   TO  FLG-SRYKARRK
           END-IF
           MOVE    "tbl_srykarrk"      TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           PERFORM 990-DBCLOSE-SEC
      *
      *    新規であること
           IF      FLG-SRYKARRK           =   1
               MOVE    SPACE               TO  SRYKARRK-REC
               INITIALIZE                      SRYKARRK-REC
               MOVE    SPA-HOSPNUM         TO  KARRK-HOSPNUM
               MOVE    SPA-PTID            TO  KARRK-PTID
               MOVE    WRK-SRYKA           TO  KARRK-SRYKA
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD1
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-SYOSINYMD2
               MOVE    SPA-SYOSIN-YMD      TO  KARRK-LASTYMD
           END-IF
      *    診療科履歴作成終了処理
           PERFORM 45009-SRYKARRK-OUT-SEC
      *
           .
       45099-SRYKARRK-ADD-EXT.
           EXIT.
      *
      *****************************************************************
      *    レセプト作成管理マスタ更新処理
      *****************************************************************
       3009-RECEUPD-UPDATE-SEC           SECTION.
      *
           INITIALIZE                      ORCSRECEUPDAREA
           MOVE    "2"                 TO  LNK-UPD-SYORI
           MOVE    SPA-HOSPNUM         TO  LNK-UPD-HOSPNUM
           MOVE    SPA-PTID            TO  LNK-UPD-PTID
           MOVE    SPA-TERMID          TO  LNK-UPD-TERMID
           MOVE    SPA-OPID            TO  LNK-UPD-OPID
           MOVE    SPA-NYUGAIKBN       TO  LNK-UPD-NYUGAIKBN
           MOVE    SPA-SRYYMD(1:6)     TO  LNK-UPD-SRYYM
           MOVE    SPA-HKNKBN          TO  LNK-UPD-HKNKBN
           IF      SPA-HKNKBN          =   1
               EVALUATE    SPA-RSI-HKNKBN
                   WHEN    "1"
                   WHEN    "2"
                   WHEN    "3"
      *                 労災
                       MOVE    "1"                 TO  LNK-UPD-HKNKBN
                   WHEN    "4"
      *                自賠責
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "4"             TO  LNK-UPD-HKNKBN
                       ELSE
                           MOVE    "2"             TO  LNK-UPD-HKNKBN
                       END-IF
                   WHEN    "5"
      *                公務災害
                       IF      SPA-RSI-JUNKYO      =   "1"
      *                    健保準拠
                           MOVE    "5"             TO  LNK-UPD-HKNKBN
                       ELSE
                           MOVE    "1"             TO  LNK-UPD-HKNKBN
                       END-IF
      *
                   WHEN    "6"
      *                自賠責（第三者行為）
                       MOVE    "7"             TO  LNK-UPD-HKNKBN
               END-EVALUATE
           END-IF
      *    公害保険
           IF      SPA-HKNKBN          =   2
               MOVE    "6"                 TO  LNK-UPD-HKNKBN
           END-IF
           CALL    "ORCSRECEUPD"       USING    ORCSRECEUPDAREA
                                                SPA-AREA
           IF      LNK-UPD-RC      NOT =   ZERO
               MOVE    "0005"              TO  SPA-ERRCD
               STRING  "ORCSRECEUPD ERR LNK-UPD-RC : "
                        LNK-UPD-RC   DELIMITED  BY  SIZE
                                         INTO  ORCSCGAIRAI-ERRMSG
               END-STRING
               DISPLAY "LNK-UPD-RC =>" LNK-UPD-RC
           END-IF
      *
           .
        3009-RECEUPD-UPDATE-EXT.
           EXIT.
      *****************************************************************
      *    日報キー取得サブ
      *****************************************************************
       800-DAILYKEY-SEC         SECTION.
      *
           CALL    "ORCSDAILYKEY"   USING   SDAILYKEYAREA
                                            SPA-AREA
      *
           .
       800-DAILYKEY-EXT.
           EXIT.
      *
      *R01.10
      *****************************************************************
      *    削除ログ(PUSH-INFO）作成処理
      *****************************************************************
       30010-PUSHINFO-INSERT-SEC      SECTION.
      *
      *    UIDコード取得処理
           INITIALIZE                  UIDIO-AREA
           CALL    "orcuidset"         USING
                                       UIDIO-ST
                                       UIDIO-AREA
      *
           INITIALIZE                  WRK-PUSHINFO-AREA
      *
           MOVE    SPACE               TO  PUSHINFO-REC
           INITIALIZE                      PUSHINFO-REC
           MOVE    SPA-HOSPNUM         TO  PUSH-HOSPNUM
           MOVE    C-UID               TO  PUSH-UID
      *
           MOVE    "patient_account"   TO  PUSH-EVENT
           MOVE    "delete"            TO  PUSH-PATIENT-MODE
      *
           MOVE    SPA-PTNUM           TO  PUSH-PTNUM
           MOVE    SPA-PTID            TO  PUSH-PTID
      *
           MOVE    SMCNDATE-YMD        TO  WRK-SYMD
           MOVE    SMCNDATE-HMS        TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSH-INFORMATION-DATE
           MOVE    WRK-HEN-TIME        TO  PUSH-INFORMATION-TIME
      *    診療日付
           MOVE    SPA-SRYYMD          TO  WRK-SYMD
           MOVE    SPACE               TO  WRK-THMS
           PERFORM 801-DAYHEN01-SEC
           MOVE    WRK-HEN-DATE        TO  PUSH-PERFORM-DATE
           MOVE    SPA-SRYYMD          TO  PUSH-SRYYMD
      *    伝票番号など
           MOVE    SPA-HKNCOMBINUM     TO  WRK-PUSH-HKNCOMBI
           MOVE    SPA-SRYKA           TO  WRK-PUSH-SRYKA
           MOVE    SPA-DRCD            TO  WRK-PUSH-DRCD
           MOVE    SPA-DENPNUM         TO  WRK-PUSH-DENPNUM
      *
           MOVE    WRK-PUSHINFO        TO  PUSH-PUSHINFO
      *
      *
           MOVE    SMCNDATE-YMD        TO  PUSH-CREYMD
           MOVE    SMCNDATE-HMS        TO  PUSH-UPHMS
           MOVE    SPA-OPID            TO  PUSH-OPID
      *
           MOVE    PUSHINFO-REC        TO  MCPDATA-REC
           MOVE    "DBINSERT"          TO  MCP-FUNC
           MOVE    "tbl_push_info"     TO  MCP-TABLE
           MOVE    "key"               TO  MCP-PATHNAME
           CALL    "ORCDBMAIN"         USING   MCPAREA
                                                   MCPDATA-REC
                                                   SPA-AREA
           IF      MCP-RC          NOT =   ZERO
               DISPLAY "P02 PUSH_INFO INS ERR:" MCP-RC
                             ",KEY:"  PUSH-KEY
               MOVE    "9000"              TO  SPA-ERRCD
           END-IF
           .
       30010-PUSHINFO-INSERT-EXT.
           EXIT.
      *
      *****************************************************************
      *    日付編集処理
      *****************************************************************
       801-DAYHEN01-SEC                SECTION.
      *
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
      *
           INITIALIZE                  WRK-HEN-TIME
           MOVE    WRK-THH             TO  WRK-HEN-THH
           MOVE    WRK-TMM             TO  WRK-HEN-TMM
           MOVE    WRK-TSS             TO  WRK-HEN-TSS
           MOVE    ":"                 TO  WRK-HEN-T1
           MOVE    ":"                 TO  WRK-HEN-T2
           .
       801-DAYHEN01-EXT.
           EXIT.
      *R01.10
      *
      *****************************************************************
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
      *    収納合計マスター読込
      *****************************************************************
       981-SYUTTL-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUTTL-REC
               MOVE    ZERO                TO  FLG-SYUTTL
           ELSE
               INITIALIZE                      SYUTTL-REC
               MOVE    1                   TO  FLG-SYUTTL
           END-IF
      *
           .
       981-SYUTTL-READ-EXT.
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
      *****************************************************************
      *    収納マスター読込
      *****************************************************************
       980-SYUNOU-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC     TO  SYUNOU-REC
               MOVE    ZERO            TO  FLG-SYUNOU
           ELSE
               INITIALIZE                  SYUNOU-REC
               MOVE    1               TO  FLG-SYUNOU
           END-IF
      *
           .
       980-SYUNOU-READ-EXT.
           EXIT.
      *
      *
      *****************************************************************
      *    収納明細マスター読込
      *****************************************************************
       900-SYUMEI-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SYUMEI-REC
               MOVE    ZERO                TO  FLG-SYUMEI
           ELSE
               INITIALIZE                      SYUMEI-REC
               MOVE    1                   TO  FLG-SYUMEI
           END-IF
      *
           .
       900-SYUMEI-READ-EXT.
           EXIT.
      *****************************************************************
      *    診療会計附加マスター次読込
      *****************************************************************
       910-SRYACCTPLUS-READ-SEC         SECTION.
      *
           PERFORM 920-DBFETCH-SEC
           IF      MCP-RC              =   ZERO
               MOVE    MCPDATA-REC         TO  SRYACCTPLUS-REC
               MOVE    ZERO                TO  FLG-SRYACCTPLUS
           ELSE
               INITIALIZE                      SRYACCTPLUS-REC
               MOVE    1                   TO  FLG-SRYACCTPLUS
           END-IF
      *
           .
       910-SRYACCTPLUS-READ-EXT.
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
