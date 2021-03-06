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
      *  ���ԡ��罤������
      * Maj/Min/Rev  ������       ����      ����
      *  01.00.01    ����      2007/10/03   �����ƥ�����ޥ�����1901�פ�
      *                                     ���ŵ��ؾ���򻲾�
      *  01.00.02    ����      2018/09/10   �������򥳥��Ⱥ��
      ************************************************************
       01  SC03.
      *��Į¼̾
              03  SC03-CITYNAME        PIC  X(22).
      *������ô���ֹ�
              03  SC03-FTNJANUM        PIC  X(16).
      *��������
              03  SC03-PRTYM           PIC  X(16).
      *��������
              03  SC03-SEIYMD          PIC  X(22).
      *�ڡ���
              03  SC03-PAGE            PIC  X(4).
      *���ŵ��إ�����
              03  SC03-HOSPCD          PIC  X(14).
      *����
              03  SC03-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC03-HOSPNAME        PIC  X(100).
      *������̾
              03  SC03-KAISETUNAME     PIC  X(40).
      *������ʬ
              03  SC03-NYUGAIKBN-TBL OCCURS  2.
                  05  SC03-NYUGAIKBN   PIC  X(2).
      *������ֹ�
              03  SC03-JKYSNUM-TBL   OCCURS  10.
                  05  SC03-JKYSNUM     PIC  X(20).
      *����Ի�̾
              03  SC03-NAME-TBL      OCCURS  10.
                  05  SC03-NAME        PIC  X(22).
      *��ǯ����
              03  SC03-BIRTHDAY-TBL  OCCURS  10.
                  05  SC03-BIRTHDAY    PIC  X(12).
      *������ô���
              03  SC03-FTNRATE-TBL   OCCURS  10.
                  05  SC03-FTNRATE     PIC  X(1).
      *��������
              03  SC03-NISSU-TBL     OCCURS  10.
                  05  SC03-NISSU       PIC  X(2).
      *�ݸ�������
              03  SC03-TENSU-TBL     OCCURS  10.
                  05  SC03-TENSU       PIC  X(7).
      *���ħ����
              03  SC03-ITBFTN-TBL    OCCURS  10.
                  05  SC03-ITBFTN      PIC  X(7).
      *��������������ݴ���
              03  SC03-SKINGK-TBL    OCCURS  10.
                  05  SC03-SKINGK      PIC  X(7).
      *���������������ɸ����ô��
              03  SC03-SFTNGK-TBL OCCURS  10.
                  05  SC03-SFTNGK      PIC  X(7).
      *����
              03  SC03-BIKO-TBL      OCCURS  10.
                  05  SC03-BIKO        PIC  X(20).
      *����
              03  SC03-SYOKEI-TBL.
      *���סݷ��
                  05  SC03-S-KENSU      PIC  X(2).
      *���סݿ�������
                  05  SC03-S-NISSU      PIC  X(3).
      *���ס��ݸ�������
                  05  SC03-S-TENSU      PIC  X(9).
      *���ס����ħ����
                  05  SC03-S-ITBFTN     PIC  X(9).
      *���ס�����������������ݴ���
                  05  SC03-S-SKINGK     PIC  X(9).
      *���ס������������������ɸ����ô��
                  05  SC03-S-SFTNGK     PIC  X(9).
      *���
              03  SC03-GOKEI-TBL.
      *��סݷ��
                  05  SC03-G-KENSU      PIC  X(5).
      *��סݿ�������
                  05  SC03-G-NISSU      PIC  X(5).
      *��ס��ݸ�������
                  05  SC03-G-TENSU      PIC  X(9).
      *��ס����ħ����
                  05  SC03-G-ITBFTN    PIC  X(9).
      *��ס�����������������ݴ���
                  05  SC03-G-SKINGK    PIC  X(9).
      *��ס������������������ɸ����ô��
                  05  SC03-G-SFTNGK    PIC  X(9).
