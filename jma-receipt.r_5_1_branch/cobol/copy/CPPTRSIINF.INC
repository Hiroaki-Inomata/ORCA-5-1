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
      ************************************************************
      *    患者労災保険情報　    　 COPY  (CPPTRSIINF.INC)  *
      ************************************************************
      ************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *  04.07.00    NACL-多々納  14/10/XX  第三者現物支給対象等追加
      ************************************************************
      *01  PTRSIINF-REC.
      *キー
           03  PTRSI-KEY.
      *医療機関ＩＤ
               05  PTRSI-HOSPNUM            PIC 9(2).
      *患者ＩＤ
               05  PTRSI-PTID              PIC  9(10).
      *保険ＩＤ
               05  PTRSI-HKNID             PIC  9(10).
      *公費ＩＤ
               05  PTRSI-KOHID             PIC  9(10).
      *労災保険区分
           03  PTRSI-HKNKBN                PIC  X(01).
      *保険番号
           03  PTRSI-HKNNUM                PIC  X(03).
      *患者交付番号
           03  PTRSI-KOFUNUM               PIC  X(14).
      *被保険者名
           03  PTRSI-HIHKNJANAME           PIC  X(100).
      *事業所名称
           03  PTRSI-JIGYOUNAME            PIC  X(80).
      *所在地都道府県名
           03  PTRSI-PREFNAME              PIC  X(20).
      *所在地都道府県区分
           03  PTRSI-PREFKBN               PIC  X(01).
      *所在地郡市区名
           03  PTRSI-CITYNAME              PIC  X(80).
      *所在地郡市区区分
           03  PTRSI-CITYKBN               PIC  X(01).
      *新継再別
           03  PTRSI-SINKEI                PIC  X(01).
      *転帰事由
           03  PTRSI-TENKI                 PIC  X(01).
      *傷病年月日
           03  PTRSI-SHOBYOYMD             PIC  X(08).
      *四肢特例区分
           03  PTRSI-SISIKBN               PIC  X(01).
      *療養開始年月日
           03  PTRSI-RYOSTYMD              PIC  X(08).
      *療養終了年月日
           03  PTRSI-RYOEDYMD              PIC  X(08).
      *損傷区分
           03  PTRSI-SONSHOUKBN            PIC  X(02).
      *損傷区分−枝番
           03  PTRSI-SONSHOUKBN-EDANUM     PIC  X(01).
      *アフター療養開始年月日
           03  PTRSI-SINSATUYMD            PIC  X(08).
      *アフター療養終了年月日
           03  PTRSI-SINSATUEND            PIC  X(08).
      *確認年月日
           03  PTRSI-KAKUNINYMD            PIC  X(08).
      *削除区分
           03  PTRSI-SAKUJOKBN             PIC  X(01).
      *労災コメント
           03  PTRSI-COMMENT               PIC  X(200).
      *災害区分
           03  PTRSI-SAIGAIKBN             PIC  X(01).
      *自賠責患者請求区分
           03  PTRSI-JIBAISEIKBN           PIC  X(01).
      *自賠責担当医コード
           03  PTRSI-DRCD                  PIC  X(05).
      *自賠責保険会社名
           03  PTRSI-HOKENNAME             PIC  X(80).
      *労災レセ回数記載：基準年月
           03  PTRSI-KIJYUNYM              PIC  X(06).
      *労災レセ回数記載：回数
           03  PTRSI-KAISU                 PIC  9(03).
      *労働基準監督署コード
           03  PTRSI-SYOCD                 PIC  X(05).
      *第三者　現物給付計算区分
           03  PTRSI-DAISAN-GENBTKBN       PIC  X(01).
      *第三者　特記事項区分
           03  PTRSI-DAISAN-TOKKIKBN       PIC  X(01).
      *自賠責区分
           03  PTRSI-JIBAIKBN              PIC  X(01).
      *端末ＩＤ
           03  PTRSI-TERMID                PIC  X(16).
      *オペーレータＩＤ
           03  PTRSI-OPID                  PIC  X(16).
      *作成年月日
           03  PTRSI-CREYMD                PIC  X(08).
      *更新年月日
           03  PTRSI-UPYMD                 PIC  X(08).
      *更新時間
           03  PTRSI-UPHMS                 PIC  X(06).
