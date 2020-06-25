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
      ************************************************************
      *    ������������              COPY (CPPTNYUINRRK.INC)     *
      ************************************************************
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      ************************************************************
      *01  PTNYUINRRK-REC.
      *����
           03  PTNYUINRRK-KEY.
      *���ŵ��أɣ�
               05  PTNYUINRRK-HOSPNUM              PIC 9(2).
      *���ԣɣ�
               05  PTNYUINRRK-PTID                PIC  9(10).
      *���������ֹ����������ǡܣ����ĺ��֡�
               05  PTNYUINRRK-RRKNUM              PIC  9(03).
      *����������ֹ��ž�ʡ�ž����ž����˺��֡�
               05  PTNYUINRRK-RRKEDANUM           PIC  9(03).
      *�����ֹ�
           03  PTNYUINRRK-BTUNUM                  PIC  X(02).
      *����̾��
           03  PTNYUINRRK-BTUNAME                 PIC  X(20).
      *�¼��ֹ�
           03  PTNYUINRRK-BRMNUM                  PIC  X(08).
      *������
           03  PTNYUINRRK-NYUINKA                 PIC  X(02).
      *�ݸ��ȹ礻�ֹ�
           03  PTNYUINRRK-HKNCOMBINUM             PIC  X(04).
      *������
           03  PTNYUINRRK-NYUINYMD                PIC  X(08).
      *�ౡ��
           03  PTNYUINRRK-TAIINYMD                PIC  X(08).
      *�ౡ��ͳ
           03  PTNYUINRRK-TAIINCD                 PIC  X(02).
      *������������ʬ�ʣ������ŵ��ء�������������¼���
           03  PTNYUINRRK-TOKUTEI-KBN             PIC  X(01).
      *����������
           03  PTNYUINRRK-TOKUTEI-NYUIN           PIC  X(02).
      *���������������׷��ʬ�ʶ��򡧳������롿�����������ʤ���
           03  PTNYUINRRK-TOKU-SANTEIYOUKENKBN    PIC  X(01).
      *�����û�������
           03  PTNYUINRRK-KSNSRYCD-G.
               05  PTNYUINRRK-KSNSRYCD-OCC        OCCURS   3.
                   07  PTNYUINRRK-KSNSRYCD        PIC  X(09).
      *��������
           03  PTNYUINRRK-RM-SAGAKU               PIC  X(02).
      *��������ʬ
           03  PTNYUINRRK-TEIKI-SEIKYUKBN         PIC  X(01).
      *����ɽ����ʬ
           03  PTNYUINRRK-KENSAKU-DISPKBN         PIC  X(01).
      *����³��ʬ
           03  PTNYUINRRK-SHOKAIKBN               PIC  X(01).
      *����ֹ�
           03  PTNYUINRRK-SHONUM                  PIC  9(03).
      *ô���壱�ʼ缣���
           03  PTNYUINRRK-DRCD1                   PIC  X(05).
      *ô���売
           03  PTNYUINRRK-DRCD2                   PIC  X(05).
      *ô���壳
           03  PTNYUINRRK-DRCD3                   PIC  X(05).
      *���ֶ�ʬ�ʣ�:����/��:ž��/��:ž��/
      *          ��:ž��/��:¾����������:������/��:�������ѹ���
           03  PTNYUINRRK-JTIKBN                  PIC  X(01).
      *��������
           03  PTNYUINRRK-SANTEI-BTUNUM           PIC  X(02).
      *�����¼�
           03  PTNYUINRRK-SANTEI-BRMNUM           PIC  X(08).
      *ž����
           03  PTNYUINRRK-TENNYUYMD               PIC  X(08).
      *ž����
           03  PTNYUINRRK-TENSTUYMD               PIC  X(08).
      *�̻��о�������¾����ǹ�θ����������
           03  PTNYUINRRK-TAISYONISSU             PIC  9(05).
      *�̻��������ʣ������ۤ������ѡ�
           03  PTNYUINRRK-TUSANNISSU1             PIC  9(05).
      *�̻��������ʣ��������ۤ������ѡ�
           03  PTNYUINRRK-TUSANNISSU2             PIC  9(05).
      *�����������̻��о�����
           03  PTNYUINRRK-TOKU-TAISYONISSU        PIC  9(05).
      *��������������ʬ
      *  ������Ǻ����������
      *  ������Ͽ�Ǻ����������쥻�ץȾ壱�Ĥ��������֤Ȥ�����
      *  ��硢������Υǡ������ͤ����ꤹ�롣
      *  �ʣ����ౡ�ѡ������������
           03  PTNYUINRRK-NYUINCHUKBN             PIC  X(01).
      *Ʊ����������ʬ�ʣ��������ౡ����κ�����������¾���򤫤�κ�������
           03  PTNYUINRRK-DOUJITSUKBN             PIC  X(01).
      *�������������ֹ�
           03  PTNYUINRRK-MAXEDANUM               PIC  9(03).
      *����������ʬ�ʣ������ʤ������������꡿�����ÿ�������ήư����
      *�������������ʣ����ѹ��ʤ����á����̻ؼ����¡����̻ؼ��ѹ��ʤ���
           03  PTNYUINRRK-SKJKBN                  PIC  X(01).
           03  PTNYUINRRK-SKJKBN-KOBETSU-G.
      *����������ʬ��ī�ʣ������ʤ������������꡿�����ÿ�������ήư����
               05  PTNYUINRRK-SKJKBN-ASA              PIC  X(01).
      *����������ʬ����ʣ������ʤ������������꡿�����ÿ�������ήư����
               05  PTNYUINRRK-SKJKBN-HIRU             PIC  X(01).
      *����������ʬ��ͼ�ʣ������ʤ������������꡿�����ÿ�������ήư����
               05  PTNYUINRRK-SKJKBN-YU               PIC  X(01).
           03  PTNYUINRRK-SKJKBN-KOBETSU-R
                           REDEFINES PTNYUINRRK-SKJKBN-KOBETSU-G.
               05  PTNYUINRRK-SKJKBN-KOBETSU      PIC  X(01)
                                                  OCCURS 3.
      *�������������ϻ��ֶ�ʬ
      *�ʣ������ʤ�������ī���顿�����뤫�顿����ͼ�����
           03  PTNYUINRRK-TIMEKBN-IN              PIC  X(01).
      *����������ʬ�ʣ��������оݡ����������оݳ���
           03  PTNYUINRRK-SENTEIKBN               PIC  X(01).
      *¾���ط���ʬ�ʣ������̤ʴط��ˤ�����ŵ��ء�
           03  PTNYUINRRK-TAINRELKBN              PIC  X(01).
      *����ư��ʬ�ʣ��������ߤ���ε��������ˤ���ư��
           03  PTNYUINRRK-KAIGOKBN                PIC  X(01).
      *�����������������ʬ�ʣ������������ꤷ�ʤ����������������ꤹ���
           03  PTNYUINRRK-NYUCALKBN               PIC  X(01).
      *ü���ɣ�
           03  PTNYUINRRK-TERMID                  PIC  X(16).
      *���ڡ��졼���ɣ�
           03  PTNYUINRRK-OPID                    PIC  X(16).
      *����ǯ����
           03  PTNYUINRRK-CREYMD                  PIC  X(08).
      *����ǯ����
           03  PTNYUINRRK-UPYMD                   PIC  X(08).
      *��������
           03  PTNYUINRRK-UPHMS                   PIC  X(06).