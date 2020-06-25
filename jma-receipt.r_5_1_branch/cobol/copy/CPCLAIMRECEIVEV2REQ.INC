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
      *****************************************************************
      *  プログラム修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      ******************************************************************
      *    ＡＰＩリクエストＣＬＡＩＭ中途データ等取得 (xml2)
      *                                  COPY (CPCLAIMRECEIVEV02REQ.INC)
      ******************************************************************
       01  XML-CLAIM-RECEIVEV2REQ.
           02  CLREREQ-CONTEXT           PIC S9(9)   BINARY.
           02  CLREREQ-OBJECT            PIC X(36).
           02  CLREREQ-MODE              PIC S9(9)   BINARY.
           02  CLREREQ-RECORDNAME        PIC X(128).
           02  CLREREQ-RECEIVE-FRONT.
      * 作成者 施設ID
               03  CLREREQ-CRE001-FACILITYID   PIC X(20).
      * 患者主ID
               03  CLREREQ-PTID1               PIC X(20).
      * オーダー発行日時 (claim:orderTime)
               03  CLREREQ-ORDER-TIME          PIC  X(19).
      *実施日時 (claim:performTime)
               03  CLREREQ-PERFORM-TIME        PIC X(19).
      * 作成者 診療科ID
               03  CLREREQ-CRE001-DPERTID      PIC X(20).
      *
      * [健康保険モジュール]
               03  CLREREQ-HKNTBL.
      * 健康保険種別
                   05  CLREREQ-HKN-HKNSBT          PIC X(20).
      * 保険種別コード (mmlHi:ClassCode)
                   05  CLREREQ-HKN-HKNSBTCODE      PIC X(2).
      * 健康保険番号
                   05  CLREREQ-HKN-HKNJANUM        PIC  X(08).
      * 被保険者記号
                   05  CLREREQ-HKN-KIGO            PIC  X(80).
      * 被保険者番号
                   05  CLREREQ-HKN-NUM             PIC  X(80).
      * <公費負担医療情報>
                   05  CLREREQ-KOHINF       OCCURS  4.
      * 公費負担名称
                       07  CLREREQ-HKNKOH-PROVIDER     PIC  X(20).
      * 負担者番号
                       07  CLREREQ-HKNKOH-FTNJANUM     PIC  X(08).
      * 受給者番号
                       07  CLREREQ-HKNKOH-JKYSNUM      PIC  X(20).
      *
      * [診断履歴モジュール]
               03  CLREREQ-SNDRRKTBL           OCCURS  50.
      * 疾患コード (mmlRd:code)
                   05  CLREREQ-SND-SPICOD1         PIC X(50).
      * 疾患名
                   05  CLREREQ-SND-SPINAME1        PIC X(80).
      * <疾患要素>
                   05  CLREREQ-SNDSPITBL           OCCURS  6.
      * 疾患コード (mmlRd:code)
                       07  CLREREQ-SNDSPITBL-SPICOD2     PIC X(10).
      * 疾患名
                       07  CLREREQ-SNDSPITBL-SPINAME2    PIC X(20).
      * <分類>
                   05  CLREREQ-SNDBNRTBL           OCCURS  4.
      * 分類の種類コード (mmlRd:tableId)
                       07  CLREREQ-SNDBNRTBL-BNRTBL1         PIC X(07).
      * 分類名
                       07  CLREREQ-SNDBNRTBL-BNRNAME1        PIC X(30).
      * <その他診断履歴情報>
      * 疾患開始日
                   05  CLREREQ-SND-SKNSTARTDATE    PIC X(10).
      * 疾患終了日
                   05  CLREREQ-SND-SKNENDDATE      PIC X(10).
      * 転帰
                   05  CLREREQ-SND-TENKI           PIC X(20).
      * 入外区分 (mmlRd:InOutPatient)
                   05  CLREREQ-SND-NYUGAIKBN       PIC X(20).
      *
      *
      * [予約請求(Claim)モジュール]
      * 文書ユニークID
               03  CLREREQ-UID8                PIC X(36).
      * ドクターコード
               03  CLREREQ-DOCCD               PIC X(10).
      * 入院外来区分 (true OR false) (claim:admitFlag)
               03  CLREREQ-NYUGAIKBN           PIC X(05).
      *時間外区分 (claim:timeClass)
               03  CLREREQ-JIKANNGAIKBN        PIC X(01).
      * 使用保険ＵＩＤ (claim:insuranceUid)
               03  CLREREQ-HOKEN-UID           PIC X(36).
      * <診療括り 1>
               03  CLREREQ-SRYTBL-G.
                 04  CLREREQ-SRYTBL             OCCURS  40.
      * 診療行為区分コード (claim:classCode)
                   05  CLREREQ-SRYCD          PIC  X(03).
      * 診療行為区分コードのテーブル名 (claim:classCodeId) (Claim002)
                   05  CLREREQ-SRYCDTBL       PIC  X(08).
      * 用法コード (claim:adminCode)
                   05  CLREREQ-ADMINCODE      PIC  X(09).
      * 用法コメント
                   05  CLREREQ-ADMMEMO        PIC  X(100).
      * 回数もしくは日数(Z編集)
                   05  CLREREQ-KAISU          PIC  X(50).
      * <明細テーブル(診療項目 1)>
                   05  CLREREQ-SRYTBL1       OCCURS  40.
      * 医事請求コード(S + 診療行為コード) (claim:code)
                       07  CLREREQ-IJICD      PIC  X(10).
      * 医事請求コードのテーブル名 (claim:tableId)
                       07  CLREREQ-TBLID      PIC  X(20).
      * 医事請求名称
                       07  CLREREQ-ALSNAME    PIC  X(80).
      * 数量(Z編集)
                       07  CLREREQ-SURYO      PIC  X(40).
      * 数量コード (claim:numberCode)
                       07  CLREREQ-SURYOCD    PIC  X(02).
      * 診療項目メモ
                       07  CLREREQ-SRYMEMO    PIC  X(100).
      * 診療括りメモ
                   05  CLREREQ-MEMO           PIC  X(100).
      *
