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
      *  �ץ�����ཤ������
      * Maj/Min/Rev     ������   ����         ����
      *  01.00.01       ����   07/01/23    Ģɼ�ͼ����ѹ���ȼ������
      *  01.00.02       Ĺ��   08/04/26    Ģɼ�ͼ���ȼ������
      *  01.00.03       ����   11/01/11    ��������������б�
      *  01.00.04       ���   18/10/09    ���ץ����Ȥ���
      *****************************************************************
        01  SKT0820.
      *���ŵ��إ�����
            03  SKT0820-HOSPCDN                      PIC X(20).
      *�ݸ����ֹ�
            03  SKT0820-HKNJANUM                     PIC X(12).
      *�������߰���
            03  SKT0820-ZAI1                         PIC X(02).
            03  SKT0820-ZAI2                         PIC X(02).
      *�ڡ����ֹ�
            03  SKT0820-NO                           PIC X(04).
      *    ����������
           03  SKT0820-TOKUBETU                      PIC  X(10).
      *�������
            03  SKT0820-SEIKYU                 OCCURS  2 TIMES.
      *����������ơ��֥�
              05  SKT0820-NYUGAIKBN            OCCURS  2 TIMES.
      *���ܤε��ա����
                07  SKT0820-KENSU                    PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-NISSU                    PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-TENSU                    PIC X(09).
      *���ܤε��ա�������ô��
                07  SKT0820-ITBFTN                   PIC X(09).
      *�������ܡ����
              05  SKT0820-S-KENSU                    PIC X(05).
      *�������ܡ�����
              05  SKT0820-S-NISSU                    PIC X(05).
      *�������ܡ����
              05  SKT0820-S-KINGK                    PIC X(09).
      *�������ܡ�ɸ����ô��
              05  SKT0820-S-FTNGK                    PIC X(09).
      *�����������
            03  SKT0820-KSEIKYU                OCCURS  5 TIMES.
      *����Ƚ���ֹ�
              05  SKT0820-HBTNUM                     PIC X(02).
      *����̾
              05  SKT0820-KOHNAME                    PIC X(10).
      *����������ơ��֥�
              05  SKT0820-KNYUGAIKBN           OCCURS  2 TIMES.
      *���ܤε��ա����
                07  SKT0820-KKENSU                   PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-KNISSU                   PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-KTENSU                   PIC X(09).
      *���ܤε��ա�������ô��
                07  SKT0820-KITBFTN                  PIC X(09).
      *�������ܡ����
              05  SKT0820-S-KKENSU                   PIC X(05).
      *�������ܡ�����
              05  SKT0820-S-KNISSU                   PIC X(05).
      *�������ܡ����
              05  SKT0820-S-KKINGK                   PIC X(09).
      *�������ܡ�ɸ����ô��
              05  SKT0820-S-KFTNGK                   PIC X(09).
      *�����󥨥ꥢ
            03  SKT0820-BIKO-AREA              OCCURS  4 TIMES.
      *����������ơ��֥�
              05  SKT0820-BNYUGAIKBN           OCCURS  2 TIMES.
      *���ܤε��ա����
                07  SKT0820-BKENSU                   PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-BNISSU                   PIC X(05).
      *���ܤε��ա�����
                07  SKT0820-BTENSU                   PIC X(09).
      *���ܤε��ա�������ô��
                07  SKT0820-BITBFTN                  PIC X(09).
      *������ʬ
                07  SKT0820-BKBN                     PIC X(02).
      *�ϥ��ե�
                07  SKT0820-HAIFUN1                  PIC X(01).
                07  SKT0820-HAIFUN2                  PIC X(01).
      *������ôʸ��
                07  SKT0820-FTNMOJI                  PIC X(02).
                07  SKT0820-FTNMARU                  PIC X(02).
      *����ˡ���ֹ�
              05  SKT0820-B-MARU1                    PIC X(02).
              05  SKT0820-B-HBTNUM                   PIC X(02).
      *�������ܡ����
              05  SKT0820-S-BKENSU                   PIC X(05).
      *�������ܡ�����
              05  SKT0820-S-BNISSU                   PIC X(05).
      *�������ܡ����
              05  SKT0820-S-BKINGK                   PIC X(09).
      *�������ܡ�ɸ����ô��
              05  SKT0820-S-BFTNGK                   PIC X(09).
      *����ʸ��
              05  SKT0820-S-SMOJI                    PIC X(02).
              05  SKT0820-S-SMARU                    PIC X(02).
      *�����ѥϥ��ե�
              05  SKT0820-S-HAIFUN1                  PIC X(01).
              05  SKT0820-S-HAIFUN2                  PIC X(01).
              05  SKT0820-S-HAIFUN3                  PIC X(01).
