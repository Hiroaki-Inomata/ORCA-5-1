       01  TECHO-REC.
      *    診療日
           03  TECHO-SRYYMD          PIC X(08).
      *    処理回数
           03  TECHO-CNT             PIC 9(03).
      *   ドクター
           03  TECHO-DRCD            PIC X(05).
      *    処方内容
           03  TECHO-DATA-REC.
      *        最大件数
               05  TECHO-ZAI-MAX         PIC 9(03).
      *        剤テーブル
               05  TECHO-REC-TBL         OCCURS   100.
      *            剤回数
                   07  TECHO-ZAIKAIKEI       PIC 9(03).
      *            診療区分
                   07  TECHO-SRYKBN          PIC X(02).
      *            診療種別
                   07  TECHO-SRYSYUKBN       PIC X(03).
      *            剤番号
                   07  TECHO-ZAINUM          PIC  9(08).
      *            剤内容
                   07  TECHO-NAI-TBL         OCCURS   50.
      *                コード
                       09  TECHO-SRYCD           PIC  X(09).
      *                数量
                       09  TECHO-SRYSURYO        PIC  9(05)V9(05).
      *                回数（外用薬）
                       09  TECHO-SRYKAISU        PIC  9(03).
      *                コメント名称
                       09  TECHO-INPUTCOMENT     PIC  X(140).
      *                商品名コード
                       09  TECHO-SHO-SRYCD       PIC  X(09).
      *                換算値数量
                       09  TECHO-KANSURYO        PIC  9(05)V9(05).
      *                用量（１：朝、２：昼、３：夜、４：寝、５：起床）
                       09  TECHO-YOURYOU-TBL.
                           11  TECHO-YOURYOU     PIC 9(05)V9(05)
                                                   OCCURS   5.
      *            用法コード（空白あり）
                   07  TECHO-YSRYCD          PIC X(09).
      *
      *    伝票番号（主の伝票番号）
           03  TECHO-DENPNUM         PIC 9(07).
      *    伝票番号（処方内容の対象伝票番号）
           03  TECHO-GRP-DENPNUM-G.
               05  TECHO-GRP-DENPNUM     PIC 9(07)
                                           OCCURS   10.
      *
      *    COMMON-SPA
           03  TECHO-TERMID                      PIC X(64).
           03  TECHO-MOTOPG                      PIC x(08).
           03  TECHO-MOTOPG2                     PIC x(08).
           03  TECHO-MOTOPG3                     PIC x(08).
           03  TECHO-SAKIPG                      PIC x(08).
           03  TECHO-NAME                        PIC X(50).
           03  TECHO-KANANAME                    PIC X(50).
