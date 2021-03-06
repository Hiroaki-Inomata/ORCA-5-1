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
      ************************************************************
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2008/04/26   ��������(���������б�)
      *  01.00.02    ����      2008/05/03   �ݸ����ֹ��ɲ�
      *  01.00.03    ���      2019/01/10   �������򥳥��Ⱥ��
      ************************************************************
        01  SKT3415.
      *    ����ǯ��
            03  SKT3415-SRYYM          PIC X(16).
            03  SKT3415-HKNJANUM       PIC X(16).
      *    ����Ϣ��̾
            03  SKT3415-HKNJANAME      PIC X(10).
      *    ��
            03  SKT3415-ZAI1           PIC X(2).
      *    �ߡ�
            03  SKT3415-ZAI2           PIC X(2).
      *    ����ǯ����
            03  SKT3415-SEIYMD         PIC X(22).
      *    ���ŵ��إ�����
            03  SKT3415-HOSPCD         PIC X(40).
      *    �ݸ����ŵ��ؤν����
            03  SKT3415-ADRS           PIC X(100).
      *    �ݸ����ŵ��ؤ�̾��
            03  SKT3415-HOSPNAME       PIC X(100).
      *    ���߼Ի�̾
            03  SKT3415-KAISETUNAME    PIC X(40).
      *    NO
            03  SKT3415-NO             PIC X(4).
      *    ����������
            03  SKT3415-TOKUBETU        PIC  X(10).
      *    ����ơ��֥�
            03  SKT3415-SEIKYU-TBL     OCCURS  2 TIMES.
      *    �����ơ��֥�
              05  SKT3415-NYUGAI-TBL   OCCURS  2 TIMES.
      *    ���
                07  SKT3415-KENSU      PIC X(5).
      *    ���ż�����
                07  SKT3415-NISSU      PIC X(5).
      *    ����
                07  SKT3415-TENSU      PIC X(9).
      *    ������ô��
                07  SKT3415-ITBFTN     PIC X(9).
      *    �������ܡ����
              05  SKT3415-S-KENSU      PIC X(5).
      *    �������ܡ�����
              05  SKT3415-S-NISSU      PIC X(5).
      *    �������ܡ����
              05  SKT3415-S-KINGK      PIC X(9).
      *    �������ܡ�ɸ����ô��
              05  SKT3415-S-FTNGK      PIC X(9).
      *
      *    �����ơ��֥�
            03  SKT3415-GSEIKYU-TBL.
      *    �����ơ��֥�
              05  SKT3415-GNYUGAI-TBL  OCCURS  2 TIMES.
      *    ���
                07  SKT3415-GKENSU     PIC X(5).
      *    ���ż�����
                07  SKT3415-GNISSU     PIC X(5).
      *    ����
                07  SKT3415-GTENSU     PIC X(9).
      *    ������ô��
                07  SKT3415-GITBFTN    PIC X(9).
      *    �������ܡ����
              05  SKT3415-S-GKENSU     PIC X(5).
      *    �������ܡ�����
              05  SKT3415-S-GNISSU     PIC X(5).
      *    �������ܡ����
              05  SKT3415-S-GKINGK     PIC X(9).
      *    �������ܡ�ɸ����ô��
              05  SKT3415-S-GFTNGK     PIC X(9).
      *
      *    ʡ����ťơ��֥�
            03  SKT3415-FSEIKYU-TBL    OCCURS  3 TIMES.
      *    �����ơ��֥�
              05  SKT3415-FNYUGAI-TBL  OCCURS  2 TIMES.
      *    ���
                07  SKT3415-FKENSU     PIC X(5).
      *    ���ż�����
                07  SKT3415-FNISSU     PIC X(5).
      *    ����
                07  SKT3415-FTENSU     PIC X(9).
      *    ������ô��
                07  SKT3415-FITBFTN    PIC X(9).
      *
      *    ��������ơ��֥�
            03  SKT3415-KSEIKYU-TBL    OCCURS  3 TIMES.
      *    �����ơ��֥�
              05  SKT3415-KNYUGAI-TBL  OCCURS  2 TIMES.
      *    ���
                07  SKT3415-KKENSU     PIC X(5).
      *    ���ż�����
                07  SKT3415-KNISSU     PIC X(5).
      *    ����
                07  SKT3415-KTENSU     PIC X(9).
      *    ������ô��
                07  SKT3415-KITBFTN    PIC X(9).
      *    ����ˡ���ֹ�
              05  SKT3415-HBTNUM       PIC X(2).
      *    ����̾��
              05  SKT3415-KOHNAME      PIC X(10).
      *    �������ܡ����
              05  SKT3415-S-KKENSU     PIC X(5).
      *    �������ܡ�����
              05  SKT3415-S-KNISSU     PIC X(5).
      *    �������ܡ�ɸ����ô��
              05  SKT3415-S-KKINGK     PIC X(9).
      *    �������ܡ�ɸ����ô��
              05  SKT3415-S-KFTNGK     PIC X(9).
      *
      *    Ĺ���������������ơ��֥�
            03  SKT3415-TYKSEIKYU-TBL.
      *    �ɾ���(���̡��࿦)�ơ��֥�
              05  SKT3415-KANSYO-TBL   OCCURS  2 TIMES.
      *    ���
                07  SKT3415-TKENSU     PIC X(5).
      *    ���
                07  SKT3415-S-TKINGK   PIC X(9).
