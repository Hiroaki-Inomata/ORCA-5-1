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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������  ����        ����
      *  01.00.01    ����    2011/01/12  ��������������б�
      *  01.00.02    ����    2017/04/14  �����η������
      *  01.00.03    ����    2019/02/07  �������򥳥��Ⱥ��
      ********************************************************************
        01  SKT4140.
      *     ���ŵ��إ�����
            03  SKT4140-HOSPCD                       PIC X(07).
      *     �ڡ����ֹ�
            03  SKT4140-NO                           PIC X(04).
      *     ˡ���ֹ�
            03  SKT4140-HBTNUM                       PIC X(02).
      *     ����̾
            03  SKT4140-KOHNAME                      PIC X(10).
      *     ��������ơ��֥�
            03  SKT4140-KOUHI-TBL                  OCCURS  8 TIMES.
      *     �����ơ��֥�
                05  SKT4140-KNYUGAI-TBL            OCCURS  2 TIMES.
      *     ���
                    07  SKT4140-KKENSU                 PIC X(05).
      *     ������
                    07  SKT4140-KNISSU                 PIC X(05).
      *     ����
                    07  SKT4140-KTENSU                 PIC X(10).
      *     ���ް�����ô��
                    07  SKT4140-KYKZFTN                PIC X(07).
      *     ������ô��
                    07  SKT4140-KITBFTN                PIC X(09).
      *     ������
                    07  SKT4140-KKOUJO                 PIC X(09).
      *     �������ܡ����
                05  SKT4140-S-KKENSU                   PIC X(05).
      *     �������ܡ�����
                05  SKT4140-S-KNISSU                   PIC X(05).
      *     �������ܡ����
                05  SKT4140-S-KKINGK                   PIC X(09).
      *     �������ܡ�ɸ����ô��
                05  SKT4140-S-KFTNGK                   PIC X(09).
      *     ����������
            03  SKT4140-TOKUBETU                     PIC  X(10).
