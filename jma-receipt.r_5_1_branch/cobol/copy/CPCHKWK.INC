      *****************************************************************
      *    チェックマスタ        COPY (CPCHKWK.INC)                   *
      *****************************************************************
      *****************************************************************
      *  コピー句修正履歴
      * Maj/Min/Rev  修正者       日付      内容
      *****************************************************************
      *01  CHKWK-REC.
      *処理区分
           03  CHKWK-SYORIKBN            PIC  X(01).
      *
           03  CHKWK-KEY.
      *チェック区分
               05  CHKWK-CHKKBN          PIC  X(01).
      *診療コード
               05  CHKWK-SRYCD           PIC  X(09).
      *有効開始年月日
               05  CHKWK-YUKOSTYMD       PIC  X(08).
      *有効終了年月日
               05  CHKWK-YUKOEDYMD       PIC  X(08).
      *コード区分
               05  CHKWK-CDKBN           PIC  X(01).
      *連番号
               05  CHKWK-RENNUM          PIC  9(02).
      *有効終了年月日2
           03  CHKWK-YUKOEDYMD2          PIC  X(08).
      *適用コード個数
           03  CHKWK-TEKCDNUM            PIC  9(03).
      *コード１から１０
           03  CHKWK-CD-G.
               05  CHKWK-CD1             PIC  X(09).
               05  CHKWK-CD2             PIC  X(09).
               05  CHKWK-CD3             PIC  X(09).
               05  CHKWK-CD4             PIC  X(09).
               05  CHKWK-CD5             PIC  X(09).
               05  CHKWK-CD6             PIC  X(09).
               05  CHKWK-CD7             PIC  X(09).
               05  CHKWK-CD8             PIC  X(09).
               05  CHKWK-CD9             PIC  X(09).
               05  CHKWK-CD10            PIC  X(09).
      *
           03  CHKWK-CD-R                REDEFINES   CHKWK-CD-G.
               05  CHKWK-CD              PIC  X(09)
                                         OCCURS   10.
      *端末ＩＤ
           03  CHKWK-TERMID              PIC  X(16).
      *オペーレータＩＤ
           03  CHKWK-OPID                PIC  X(16).
      *作成年月日
           03  CHKWK-CREYMD              PIC  X(08).
      *更新年月日
           03  CHKWK-UPYMD               PIC  X(08).
      *更新時間 
           03  CHKWK-UPHMS               PIC  X(06).
      *
