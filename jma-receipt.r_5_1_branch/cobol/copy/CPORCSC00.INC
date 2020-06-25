      *******************************************************************
      * Project code name "ORCA"
      * ����ɸ��쥻�ץȥ��եȡ�JMA standard receipt software��
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
      *----------------------------------------------------------*
      *    ���Ź԰ٻ����Ѷ��̥ѥ�᥿��   COPY (CPORCSC00.INC)
      *                              (ORCSC10 �� ORCSC80 �ǻ��ѡ�
      *----------------------------------------------------------*
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      * 01.00.01     MCC-¿��Ǽ   01.06.15  �����ͤ��ɲ� 
      * 01.00.02     ¿��Ǽ       02/04/19  ���转���»�Ƴ��������ɲ�
      * 01.00.03     NACL-¿��Ǽ  02/10/25  �����б�
      * 01.00.04     NACL-¿��Ǽ  04/03/08  ���ֳ��û��������Ļ��û��ɲ�
      * 01.00.05     NACL-¿��Ǽ  05/12/26  ����ȯ�����ɲá�������
      * 03.05.01     NACL-¿��Ǽ  07/07/18  �鲼�����������Ѵ�����
      * 04.08.01     NACL-¿��Ǽ  16/03/XX  ��λ��ʬ���ɲ�
      ************************************************************
      *
       01  ORCSC00AREA.
      *
         02  LNK-ORCSZAI-AREA.
      *
      *    �����ޤ��ȤΥ����å�������
      *    �������ΤΥ����å�
      *    ������λ���μ�ư�û�����
           03  LNK-ORCSC-KBN           PIC X(01).
      *    1����λ��
           03  LNK-ORCSC-KBN2          PIC X(01).
      *    �ᥤ��У�
           03  LNK-ORCSC-PG            PIC X(04).
      *    �ޤ��Ȥβ��
           03  LNK-ORCSC-KAISU         PIC 9(03).
      *    �����ե饰
           03  LNK-ORCSC-SYORIFLG      PIC 9(01).
      *    ��ǥե饰�ʿ������ɤ�����Ƚ���
           03  LNK-ORCSC-SYOSIN        PIC 9(01).
      *    ��ǥե饰���ʺ����������ɤ�����Ƚ���
           03  LNK-ORCSC-SYOSIN2       PIC 9(01).
      *    Ʊ����ǥե饰���ʺ����������ɤ�����Ƚ���
           03  LNK-ORCSC-DOUSIN       PIC 9(01).
      *    ���������Ų�
           03  LNK-ORCSC-OLD-SRYKA     PIC X(02).
      *    �������ݸ��ȹ礻
           03  LNK-ORCSC-OLD-HKNCOMBI  PIC X(04).
      *
      *    ���̹԰���
           03  LNK-ORCSC-CHK-IDX       PIC 9(04).
      *    �ٹ�԰���
           03  LNK-ORCSC-KEI-IDX       PIC 9(04).
      *
      *    �������
           03  LNK-ORCSC-ZAITEN        PIC 9(08).
           03  LNK-ORCSC-ZAIKEI        PIC 9(08).
      *    Ϸ�Ͷ�ʬ
           03  LNK-ORCSC-ROUJIN        PIC 9(01).
      *    ǯ�� 
           03  LNK-ORCSC-NENREI.
               05  LNK-ORCSC-NENREI-YY     PIC 9(03).
               05  LNK-ORCSC-NENREI-MM     PIC 9(02).
               05  LNK-ORCSC-NENREI-DD     PIC 9(02).
      *    ���ֳ��û�ͭ��
           03  LNK-ORCSC-TIMEFLG           PIC 9(01).
      *    �������������̵ͭ
           03  LNK-ORCSC-GAIFLG            PIC 9(01).
      *ʣ�����Ųʤ���
           03  LNK-ORCSC-FUKU-SRYKA        PIC  9(01).
      *
      *�ǻ�������
           03  LNK-ORCSC-SINSATU-ARI       PIC  9(01).
      *
      *��ǻ��������ౡ��
           03  LNK-ORCSC-SYOYMD            PIC  X(08).
      *���������ν�ǻ�����
           03  LNK-ORCSC-SYOYMD-OLD        PIC  X(08).
      *�ǽ�������
           03  LNK-ORCSC-LSTYMD            PIC  X(08).
      *�ǽ����ǲ�
           03  LNK-ORCSC-LSTSRYKA          PIC  X(02).
      *�ǽ����ǲ�
           03  LNK-ORCSC-LSTSRYKAMEI       PIC  X(20).
      *������ʬ
           03  LNK-ORCSC-INGAIKBN          PIC  X(01).
      *���ɽ��
           03  LNK-ORCSC-TOROKU            PIC  X(01).
      *Ʊ���ƿ�̵ͭ
           03  LNK-ORCSC-RRK-LSTYMD        PIC  X(08).
      *�鲼�����������Ѵ�����
           03  LNK-ORCSC-CHG310FLG         PIC  X(01).
      *����ޡ���������Ÿ������
           03  LNK-ORCSC-AKUSEIHYOFLG      PIC  X(01).
      *�����Ѵ���ʬ
           03  LNK-ORCSC-HAIKIKBN-FLG      PIC  X(01).
      *
      *��������ư����Τߤ˻��ѡʣˣ����Τߡ�
      *Ĵ�޵��Ѵ�����̵ͭ
           03  LNK-ORCSC-CHOKI-FLG         PIC  9(01).
      *���޾�������̵ͭ(���̡�
           03  LNK-ORCSC-YAKJYO-1          PIC  9(01).
      *���޾�������̵ͭ(Ϸ�ͣ���
           03  LNK-ORCSC-YAKJYO-2          PIC  9(01).
      *���޾�������̵ͭ(Ϸ�ͣ���
           03  LNK-ORCSC-YAKJYO-3          PIC  9(01).
      *�������޼��Ŵ�����̵ͭ
           03  LNK-ORCSC-TOKUTEI-FLG       PIC  9(01).
      *�������������̵ͭ
           03  LNK-ORCSC-GAIRAISIN         PIC  9(01).
      *
      *
      *�����������Ƚ����
           03  LNK-ORCSC-SANTEI.
      *        Ϸ��������������������Ų�ͭ��
               05  LNK-ORCSC-ROUMANSEI-ARI       PIC  9(01).
               05  LNK-ORCSC-ROUMANSEI-ARI2      PIC  9(01).
      *        ��������Ϸ�ͺ�������ǻ�������
               05  LNK-ORCSC-NETAKIRI-ARI        PIC  9(01).
               05  LNK-ORCSC-NETAKIRI-ARI2       PIC  9(01).
               05  LNK-ORCSC-NETAKIRI-ARI3       PIC  9(01).
      *        ����ˬ�������ͭ��
               05  LNK-ORCSC-HOUMON-ARI           PIC  9(01).
      *        �����ʳ�����Ųʻ���ͭ��
               05  LNK-ORCSC-SYONIKA-ARI         PIC  9(01).
               05  LNK-ORCSC-SYONIKA-ARI2        PIC  9(01).
      *
      *    ���úƿǤ�̵ͭ
           03  LNK-ORCSC-TELSAI-ARI        PIC 9(01).
      *�������̵ͭ����̾��
           03  LNK-ORCSC-TOKUTEI           PIC  9(01).
           03  LNK-ORCSC-TOKUTEIFLG        PIC  9(01).
      *
      *���ǲ��
           03  LNK-ORCSC-SINSATU-CNT       PIC  9(03).
      *�������޼��Ŵ��������ǯ�����ʣɡ��ϡ�
           03  LNK-ORCSC-TOKTEI-YMD        PIC  X(08).
      *������̤���ƿǲʻ���ͭ��
           03  LNK-ORCSC-15MIMAN-ARI       PIC  9(01).
      *
      *�����ʤ����봵�ԡʺƿ�����
           03  LNK-ORCSC-TOKUTEI-KNJ       PIC  9(01).
           03  LNK-ORCSC-TOKUTEI-KNJ2      PIC  9(01).
      *�����ʤ����봵�ԡʳ��������
           03  LNK-ORCSC-TOKUTEIG-KNJ      PIC  9(01).
           03  LNK-ORCSC-TOKUTEIG-KNJ2     PIC  9(01).
      *
      *���转���»�Ƴ����������
           03  LNK-ORCSC-SEIKATU-ARI       PIC  9(01).
           03  LNK-ORCSC-SEIKATU-ARI2      PIC  9(01).
      *�͹���¡
           03  LNK-ORCSC-TOKUTEI-JNZ       PIC  9(01).
      *�ƿ����ѹ���ǧ�Ѥ�
           03  LNK-ORCSC-SAISIN-CHK        PIC  9(01).
      *�ñ����˽��������⻻��
           03  LNK-ORCSC-SYOEN-CNT         PIC  9(03).
      *����ȯ��������������
           03  LNK-ORCSC-KYUSEI-180          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-90           PIC  X(08).
           03  LNK-ORCSC-KYUSEI-YMD          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-END          PIC  X(08).
           03  LNK-ORCSC-KYUSEI-FLG          PIC  9(01).
      *���������û��μ��ʤ�����
           03  LNK-ORCSC-GAIRAI-NASI       PIC  9(01).
      *���ֳ������ʬ
           03  LNK-ORCSC-JIKANTOKU         PIC  9(01).
      *���ֳ��û��������Ļ��û�
           03  LNK-ORCSC-JIKANTOKU-NYU     PIC  9(01).
      *���ֳ��û����Ļ������å�
           03  LNK-ORCSC-JIKANERR-FLG      PIC  9(01).
      *
      *    ��ϥӥ�������ǧ
           03  LNK-ORCSC-FLG-RIHTEI            PIC 9(01).
      *
      *ver.4.8
      *    ϫ�ҥ쥻���б�
           03  LNK-ORCSC-ROURECEKBN        PIC 9(01).
      *
      *�����η�³ʬ�����å�
           03  LNK-ORCSC-KBN20.
      *��³��ʬ
               05  LNK-ORCSC-CONTKBN           PIC  X(01).
      *��ɼ�ֹ�
               05  LNK-ORCSC-DENPNUM           PIC  9(07).
      *Ʊ������Ϣ��
               05  LNK-ORCSC-DOUJI-RENNUM      PIC  9(01).
      *
      *    ���Ź԰ٷ��̥����å��ѡʸ����ѡ�
           03  LNK-NAIACCT-AREA.
               05  LNK-ACCT-TBL             OCCURS   100.
                   07  LNK-ACCT-SRYKBN          PIC  X(02).
                   07  LNK-ACCT-SRYCD           PIC  X(09).
                   07  LNK-ACCT-CDKBN.
                       09  LNK-ACCT-CDKBN-KBN      PIC  X(01).
                       09  LNK-ACCT-CDKBN-KBNNUM   PIC  X(03).
      *****************09  LNK-ACCT-CDKBN-SYO   PIC  X(01).
      *****************09  LNK-ACCT-CDKBN-BU    PIC  X(02).
      *
           03  LNK-NAIACCT2-AREA.
      *        ���������å��ѡ���ũ��
               05  LNK-ACCT2-SRYCD           PIC X(09).
               05  LNK-ACCT2-ZAINUM          PIC 9(08).
               05  LNK-ACCT2-CSYYOURYO       PIC 9(08)V9(5).
      *        ��̮����
               05  LNK-ACCT2-SRYCD-JYO1      PIC X(09).
               05  LNK-ACCT2-SRYCD-JYO2      PIC X(09).
      *
      *    �������ο��ť�����
           03  LNK-ORCSC-TEISEI-AREA.
               05  LNK-ORCSC-TEISEI-OCC      OCCURS   400.
                   07  LNK-ORCSC-TEI-SRYCD   PIC X(09).
                   07  LNK-ORCSC-TEI-KAISU   PIC 9(03).
      *
      *    ϫ�Ҥγ�������û��ɤ��ؤ�����
           03  LNK-ORCSC-GAIRAI-TEN        PIC  9(05)V9(03).
      *    ��������û�����Υ����å�
           03  LNK-ORCSC-GAIKANRIKBN-CHK   PIC  X(01).
      *
      *    ��������
           03  LNK-ORCSC-NYUINYMD           PIC X(08).
           03  LNK-ORCSC-TAIINYMD           PIC X(08).
      *    �����������
           03  LNK-ORCSC-KENSAFLG           PIC 9(01).
      *
      *    Ʊ����ǻ���ʡʳ���Τߡ�
      *D   03  LNK-ORCSC-DOUSYOSIN-AREA.
      *        05  LNK-ORCSC-DOUSYOSIN-OCC       OCCURS   10.
      *            07  LNK-ORCSC-DOU-SRYKA       PIC X(02).
      *D           07  LNK-ORCSC-DOU-SRYYMD      PIC X(08).
      *
      *    �û��ɲ��ΰ�ʹ������μ�ư�û���ñ�Ⱥޡˡ�
           03  LNK-ORCSC-KASAN.
               05  LNK-ORCSC-KASAN-TBL  OCCURS  10.
                   07  LNK-ORCSC-KASN-TBL2     OCCURS   30.
                       09  LNK-ORCSC-SRYKA         PIC X(02).
                       09  LNK-ORCSC-HKNCOMBI      PIC X(04).
                       09  LNK-ORCSC-CHIKENKBN     PIC X(01).
                       09  LNK-ORCSC-SRYSYUKBN     PIC X(03).
                       09  LNK-ORCSC-SRYKBN        PIC X(02).
                       09  LNK-ORCSC-SRYCD         PIC X(09).
                       09  LNK-ORCSC-TENSU         PIC 9(05)V9(05).
                       09  LNK-ORCSC-SURYO         PIC 9(05)V9(05).
                       09  LNK-ORCSC-SRYCD-KS      PIC X(09).
                       09  LNK-ORCSC-TENSU-KS      PIC 9(05)V9(05).
      *H28.4 �ɲ�
                       09  LNK-ORCSC-SRYCD-KS2     PIC X(09).
                       09  LNK-ORCSC-TENSU-KS2     PIC 9(05)V9(05).
                       09  LNK-ORCSC-ATAI-G.
                           11  LNK-ORCSC-ATAI   PIC X(08) OCCURS  5.
      *                �������β��
                       09  LNK-ORCSC-INPUTCD-G.
                           11  LNK-ORCSC-INPUTCD       PIC X(22)
                                                   OCCURS  5.
      *                ��ưȯ��ʬ�Ȥ��ʤ�
                       09  LNK-ORCSC-AUTOKBN       PIC X(01).
      *                ����
                       09  LNK-ORCSC-HKTKBN        PIC 9(01).
      *                ���������ʬ
                       09  LNK-ORCSC-SANTEIRRKKBN  PIC X(01).
      *    ��ǧ��å�����
           03  LNK-ORCSC-KIDCD              PIC  X(04).
      *
      *    ϫ�Ҥλ����ɤ��ؤ�����
           03  LNK-ORCSC-RSIOK              PIC  X(01).
      *    ���顼��å�����
           03  LNK-ORCSC-ERRMSG             PIC  X(80).
      *
      *    ���η��
         02  LNK-ORCSALL-AREA.
      *
           03  LNK-ORCSC-CHUSYA-AREA.
      *        ��ũ����̮����
               05  LNK-ORCSC-TEN             PIC X(01).
               05  LNK-ORCSC-JYO1            PIC X(01).
               05  LNK-ORCSC-JYO2            PIC X(01).
      *
      *��ϥӥ�������������
           03  LNK-RIGAKU-NCNT-AREA          PIC X(1301).
      *
      *    ���������ΰ�
           03  LNK-ORCSC-TEIGEN-AREA.
               05  LNK-ORCSC-TEIGEN-OCC        OCCURS   20.
                   07  LNK-ORCSC-TEIGEN-SRYCD     PIC X(09).
                   07  LNK-ORCSC-TEIGEN-HOU       PIC X(02).
                   07  LNK-ORCSC-TEIGEN-CNT       PIC 9(03).
                   07  LNK-ORCSC-TEIGEN-SANTEI    PIC 9(03).
      *
      *����ޤȤỻ�������ΰ�
           03  LNK-ORCSC-KT01-AREA.
               05  LNK-ORCSC-KT01FLG         PIC X(01).
               05  LNK-ORCSC-KT01-OCC        OCCURS   100.
                   07  LNK-ORCSC-KT01-SRYCD  PIC X(09).
                   07  LNK-ORCSC-KT01-DAY-G.
                       09  LNK-ORCSC-KT01-DAY     PIC 9(03)
                                                  OCCURS   31.
      *