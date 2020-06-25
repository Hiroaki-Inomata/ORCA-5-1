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
      *    ���Ʒ׻������ԥ����å��ѥѥ�᥿   COPY (CPORCSSYUNOUCHK.INC)
      *                    ���ѥץ������            (ORCSSYUNOUCHK)
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
       01  LNK-ORCSSYUNOUCHK-REC.
      *
      *���ŵ��أɣ�
           03  LNK-ORCSSYUNOUCHK-HOSPNUM        PIC 9(2).
      *������ʬ
           03  LNK-ORCSSYUNOUCHK-NYUGAIKBN      PIC X(01).
      *����ǯ��
           03  LNK-ORCSSYUNOUCHK-SRYYM          PIC X(06).
      *��������ۤ��ѹ����ʤ����ΰ��Ʒ׻�
      *�������������ԥ����å����˼�ư�ǰ��Ʒ׻���Ԥ�ʤ��ʥǥե���ȡ�
      *�������������ԥ����å����˼�ư�ǰ��Ʒ׻���Ԥ�
           03  LNK-ORCSSYUNOUCHK-OPTION1        PIC X(01).
      *
      *�ֵ���
      *
           03  LNK-ORCSSYUNOUCHK-OUT-AREA.
             05  LNK-ORCSSYUNOUCHK-OUT-OCC      OCCURS 10000.
      *���ԣɣ�
               07  LNK-ORCSSYUNOUCHK-PTID       PIC 9(10).
      *���֥ե饰
      *���������ڴ�������ۤ��ѹ�����������
      *���������ڴ�������ۤ��ѹ����ʤ�����
      *���������ڴ�������ۤ��ѹ����ʤ����ۡ��������ԥ����å����˼�ư�ǰ��Ʒ׻���
      *���������ڳ����ԥ����å��ǥ��顼�Ȥʤä�����
               07  LNK-ORCSSYUNOUCHK-JTIFLG     PIC 9(01).
      *������ʬ
      *���������軰�Թ԰ٸ�ʪ���ա��̾����ʬ��ͥ��׻�������
      *��������������̤����ʪ���աʸ���ʻ��ʬ��ͥ��׻�������
      *������������Ը�ʪ���աʸ���ʻ��ʬ��ͥ��׻�������
      *����������Ǽ����̤�����ʲ�׾Ȳ�ڼ�Ǽ������̤�����μ�Ǽ�����
      *�������������ԥ����å����ʬ�ʥǡ��������ͤ��ѹ��Ȥʤ��Ǽ�����
      *�����������Ź԰٤�������Ͽ��ɬ�פʼ�Ǽ���ꡣ������Ͽ��˺ƥ����å���
      *���������軰�Թ԰ٸ�ʪ���ա��軰�Թ԰�ʬ��ͥ��׻�������
      *���������ޥ�Ĺ�׻�
               07  LNK-ORCSSYUNOUCHK-GAITOKBN   PIC 9(02).
      *
      *����������
      *
           03  LNK-ORCSSYUNOUCHK-RC             PIC 9(02).
      *