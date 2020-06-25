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
      *****************************************************************
      *    �����԰٣ӣУ����ǡ��������� ���� COPY (I4SCR-NYU.INC)
      *             ���̤Υ����ΰ��ǡ����ǻ��Ѥ���
      *                                  �ʳƹ԰٥����å��˻��ѡ�
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *
      ************************************************************
      *
           03  SPA-GMN-MAX             PIC 9(04).
      *
           03  SPA-GMN-REC.
               05  SPA-GMN-TBL             OCCURS   200.
                   07  SPA-GMN-TBLREC.
                       09  SPA-GMN-SRYKBN        PIC X(02).
                       09  SPA-GMN-INPUTCD       PIC X(22).
                       09  SPA-GMN-MEISYO-G.
                           11  SPA-GMN-MEIH      PIC X(02).
                           11  SPA-GMN-MEISYO    PIC X(60).
                       09  SPA-GMN-SURYO         PIC X(09).
                       09  SPA-GMN-TANINAME      PIC X(12).
                       09  SPA-GMN-TENSU         PIC X(08).
                       09  SPA-GMN-KAISU         PIC X(04).
                       09  SPA-GMN-KEI           PIC 9(08).
      *                ���ϥ����ɤη��
                       09  SPA-GMN-INCNT         PIC 9(02).
      *
                   07  SPA-COM-TBLREC.
                       09  SPA-COM-SET           PIC X(01).
                       09  SPA-COM-PAGE          PIC 9(04).
                       09  SPA-COM-GYOU          PIC 9(04).
                       09  SPA-COM-CUR           PIC X(01).
      *                �������ϰ���
                       09  SPA-COM-IN            PIC X(01).
      *                ��������ͭ��
                       09  SPA-COM-IN2           PIC X(01).
      *                ��������ͭ��
                       09  SPA-COM-IN3           PIC X(01).
                       09  SPA-MAE-INPUTCD       PIC X(22).
      *
                       09  SPA-COM-SRYSYUKBN     PIC X(03).
                       09  SPA-COM-SRYKBN        PIC X(02).
                       09  SPA-COM-INPUTCD       PIC X(09).
                       09  SPA-COM-SURYO         PIC 9(05)V9(03).
      *                �޹������
                       09  SPA-COM-TENSU         PIC 9(08).
      *                ����
                       09  SPA-COM-TEN           PIC 9(07)V99.
      *                �����ʤ������ͷ׻����
                       09  SPA-COM-KISOTEN       PIC 9(07)V99.
                       09  SPA-COM-KAISU         PIC 9(08).
                       09  SPA-COM-KEI           PIC 9(08).
      *                ��������������
                       09  SPA-COM-SURYO2        PIC 9(05)V9(03).
                       09  SPA-COM-KAISU2        PIC 9(08).
      *
                       09  SPA-COM-BUNGSU        PIC 9(08).
                       09  SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI1         PIC X(08).
                           11  SPA-COM-ATAI2         PIC X(08).
                           11  SPA-COM-ATAI3         PIC X(08).
                           11  SPA-COM-ATAI4         PIC X(08).
                       09  SPA-COM-INPUTCHI-R    REDEFINES
                                                 SPA-COM-INPUTCHI-G.
                           11  SPA-COM-ATAI          PIC X(08)
                                                     OCCURS  4.
      *                �굻���ף�
                       09  SPA-COM-SYUTEN1       PIC 9(08).
      *                �굻���ף�
                       09  SPA-COM-SYUTEN2       PIC 9(08).
      *                ��������
                       09  SPA-COM-YKZTEN        PIC 9(08).
      *                ��������
                       09  SPA-COM-KIZAITEN      PIC 9(08).
      *
                       09  SPA-COM-ZAINUM        PIC 9(08).
      *                �޽�λ
                       09  SPA-COM-ZAIEND        PIC X(01).
      *                ��ư�����ʬ�ʣ�������ɽ�����������Ͼ��ˤ���ѹ���
                       09  SPA-COM-AUTOKBN       PIC X(01).
      *                ��������
                       09  SPA-COM-SURFLG        PIC X(01).
      *                �������
                       09  SPA-COM-KAIFLG        PIC X(01).
      *                ̾������
                       09  SPA-COM-MEIFLG        PIC X(01).
      *                ������
                       09  SPA-COM-KEIFLG        PIC X(01).
      *                ��������
                       09  SPA-COM-JIHIFLG       PIC X(01).
      *                �������ѿ��Ź԰٥�����
                       09  SPA-COM-CHK-SRYCD     PIC X(09).
      *                ����ɽ�������ϥ�����
                       09  SPA-COM-ICD-INPUTCD   PIC X(22).
      *                ���ϥ�����ɽ���Ѥ�
                       09  SPA-COM-GMNFLG        PIC X(01).
      *
                       09  SPA-COM-FLGAREA.
      *                    �ش����޷ٹ�ѥե饰
                           11  SPA-COM-KNKFLG        PIC X(01).
                           11  SPA-COM-KNKUMU        PIC X(01).
      *                    ϫ���ɤ��ؤ��û�ʬ
                           11  SPA-COM-RSIFLG2       PIC X(01).
      *                    �ٹ�ѥե饰
                           11  SPA-COM-KZMFLG        PIC X(01).
      *                    ʻ�ѻ���ѥե饰
                           11  SPA-COM-SDOFLG        PIC X(01).
      *                    ���ܥ����å��ѥե饰
                           11  SPA-COM-CHKFLG        PIC X(01).
      *                    ���ֲû��ե饰
                           11  SPA-COM-TIMEFLG       PIC 9(01).
      *                �׻������å��ѥե饰����󥯥ꥢ��
                       09  SPA-COM-SANFLG        PIC 9(01).
      *                �����о�̵ͭ�ʣ�������ͭ���
                       09  SPA-COM-TEIGEN-FLG    PIC  9(01).
      *                �����оݥ����å���
                       09  SPA-COM-TEIGEN-CHK    PIC  9(01).
      *                ϫ�Ҷ�ʬ��ϫ���ѤΥ����ɡ�
                       09  SPA-COM-RSIKBN        PIC  9(01).
      *                ϫ�� �ʳ�������û����ɤ��ؤ��ե饰��
                       09  SPA-COM-RSIGIRI       PIC  9(01).
      *                �������������
                       09  SPA-COM-NYUIN-AREA.
                           11  SPA-COM-NYU-KAI        PIC 9(03).
                           11  SPA-COM-NYUINDAY       PIC 9(01)
                                                      OCCURS  31.
      *        �����ޥ�������
      *                �����ޥ����ο��Ŷ�ʬ
                       09  SPA-COM-TNSSRYKBN     PIC X(02).
      *                �����ޥ����ο��ż���
                       09  SPA-COM-TNSSRYSYUKBN  PIC X(03).
      *                �ǡ�����ʬ
                       09  SPA-COM-DATAKBN       PIC X(01).
      *                �����ޥ�������������
                       09  SPA-COM-MADOKUKBN     PIC 9(01).
                       09  SPA-COM-SINKEIKBN     PIC 9(01).
                       09  SPA-COM-SEIBUTUKBN    PIC 9(01).
                       09  SPA-COM-ZOEIZAIKBN    PIC 9(01).
                       09  SPA-COM-CSYYOURYO     PIC 9(05).
      *                ñ�̥�����
                       09  SPA-COM-TANICD        PIC X(03).
      *                ���޶�ʬ
                       09  SPA-COM-YKZKBN        PIC 9(01).
                       09  SPA-COM-ZAIGATAKBN    PIC 9(01).
      *                �����ޥ��������̶�ʬ
                       09  SPA-COM-BUIKBN        PIC X(01).
      *                �������»�Ƚ�Ƕ�ʬ
                       09  SPA-COM-KNSJISKBN     PIC  9(01).
      *                �������»�Ƚ�ǥ��롼��
                       09  SPA-COM-KNSJISGRPKBN  PIC  9(02).
      *                ���û�������
                       09  SPA-COM-CHUKSNCD      PIC  X(04).
      *                ���û�����
                       09  SPA-COM-CHUKSNTUBAN   PIC  X(01).
      *                ��§�û��оݳ���ʬ
                       09  SPA-COM-TUSOKUGAIKBN   PIC  X(01).
      *                ��§ǯ���ʬ
                       09  SPA-COM-TSUSOKUAGE    PIC  X(01).
      *                ���ֲû���ʬ
                       09  SPA-COM-TIMEKSNKBN    PIC  9(01).
      *                �����ѡ�����оݸ���
                       09  SPA-COM-HOUKNSKBN     PIC  9(02).
      *                �����ѡ���ʬ�ֹ�
                       09  SPA-COM-CDKBN.
                           11  SPA-COM-CDKBN-KBN   PIC  X(01).
                           11  SPA-COM-CDKBN-SYO   PIC  X(01).
                           11  SPA-COM-CDKBN-BU    PIC  X(02).
      *                    ϫ���ѡ���ʬ�ֹ�
                           11  SPA-COM-CDKBN-KBNNUM     PIC  X(03).
      *                ���������ʬ
                       09  SPA-COM-SANTEIRRKKBN    PIC  9(01).
      *                ��������û���ʬ
                       09  SPA-COM-GAIKANRIKBN     PIC  9(01).
      *                ��Ƴ������
                       09  SPA-COM-SDOKNRYO        PIC  9(02).
      *                ��������
                       09  SPA-COM-TENSIKIBETU     PIC  9(01).
      *                ������ǯ��û���ʬ
                       09  SPA-COM-TOKUKIZAIAGEKSNKBN   PIC  9(01).
      *                ��������ʬ
                       09  SPA-COM-SANSOKBN        PIC  9(01).
      *                ��������̣�
                       09  SPA-COM-TOKUKIZAISBT1   PIC  9(01).
      *                ��������ʴ��ξ�¡�
                       09  SPA-COM-JGNTEN          PIC  9(07).
      *                �ݸ�Ŭ�Ѷ�ʬ
                       09  SPA-COM-HKNTEKKBN       PIC  9(01).
      *                �����󽸷��輱��
                       09  SPA-COM-GAITENTTLKBN    PIC  9(03).
      *                �����󽸷��輱��
                       09  SPA-COM-NYUTENTTLKBN    PIC  9(03).
      *                ���Ƕ�ʬ
                       09  SPA-COM-OSINKBN         PIC  9(01).
      *    ��������
      *                �������ͷ׻�����
                       09  SPA-COM-KZMCOMPSIKIBETU     PIC 9(01).
      *                �������ͷ׻����
                       09  SPA-COM-KZMJGN              PIC  9(08).
      *                ���������ʬ
                       09  SPA-COM-HOUKATUTEIGENKBN    PIC  9(03).
      *                ǯ��û�
                       09  SPA-COM-AGEKSNTBL           OCCURS 4.
      *                    ǯ��û�����
                           11  SPA-COM-AGEKSNKGN       PIC  X(02).
      *                    ǯ��û����
                           11  SPA-COM-AGEKSNJGN       PIC  X(02).
      *                    ǯ��û����ť�����
                           11  SPA-COM-AGEKSNSRYCD     PIC  X(09).
      *                ���������
                       09  SPA-COM-DAYCNT              PIC  9(01).
      *                �����оݶ�ʬ
                       09  SPA-COM-TEIGENKBN           PIC  9(01).
      *                ��ȯ�����ʶ�ʬ
                       09  SPA-COM-KOUHATUKBN          PIC  9(01).
      *                �����Ŭ��������ʬ
                       09  SPA-COM-KIJUNTEIGENKBN      PIC  9(01).
      *                �����Ŭ�������оݻ��ߴ��
                       09  SPA-COM-KIJUNTEIGENCD       PIC  9(03).
      *    �����ޥ����ղþ�����
      *                �η��ʬ
                       09  SPA-COM-SAIKETUKBN          PIC 9(01).
      *                ���ϥ����å���ʬ
                       09  SPA-COM-INPCHKKBN           PIC 9(01).
      * 