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
      *  01.00.01    ����      2018/09/10   �������򥳥��Ⱥ��
      ************************************************************
       01  SC05.
      *������ô���ֹ�
              03  SC05-FTNJANUM        PIC  X(16).
      *�ڡ���
              03  SC05-PAGE            PIC  X(04).
      *��������
              03  SC05-PRTYM           PIC  X(16).
      *��������
              03  SC05-SEIYMD          PIC  X(22).
      *��Į¼̾
              03  SC05-CITYNAME        PIC  X(22).
      *���ŵ��إ�����
              03  SC05-HOSPCD          PIC  X(14).
      *����
              03  SC05-ADRS            PIC  X(100).
      *���ŵ���̾
              03  SC05-HOSPNAME        PIC  X(100).
      *���߼�̾
              03  SC05-KAISETUNAME     PIC  X(40).
      *����̾��
              03  SC05-KOHNAME         PIC  X(10).
      *������ʬ
              03  SC05-NYUGAIKBN-TBL OCCURS  2.
                  05  SC05-NYUGAIKBN   PIC  X(02).
      *������ֹ�
              03  SC05-JKYSNUM-TBL   OCCURS  10.
                  05  SC05-JKYSNUM     PIC  X(20).
      *����Ի�̾
              03  SC05-NAME-TBL      OCCURS  10.
                  05  SC05-NAME        PIC  X(22).
      *��ǯ����
              03  SC05-TANJOY-TBL  OCCURS  10.
                  05  SC05-TANJOY      PIC  X(04).
              03  SC05-TANJOM-TBL  OCCURS  10.
                  05  SC05-TANJOM      PIC  X(04).
              03  SC05-TANJOD-TBL  OCCURS  10.
                  05  SC05-TANJOD      PIC  X(04).
      *������ô���
              03  SC05-FTNRATE-TBL   OCCURS  10.
                  05  SC05-FTNRATE     PIC  X(01).
      *��������
              03  SC05-NISSU-TBL     OCCURS  10.
                  05  SC05-NISSU       PIC  X(02).
      *�ݸ�������
              03  SC05-TENSU-TBL     OCCURS  10.
                  05  SC05-TENSU       PIC  X(07).
      *���ħ����
              03  SC05-ITBFTN-TBL    OCCURS  10.
                  05  SC05-ITBFTN      PIC  X(07).
      *��������������ݴ���
              03  SC05-SKINGK-TBL OCCURS  10.
                  05  SC05-SKINGK      PIC  X(07).
      *���������������ɸ����ô��
              03  SC05-SFTNGK-TBL OCCURS  10.
                  05  SC05-SFTNGK      PIC  X(07).
      *����(������)
              03  SC05-BIKO-TBL OCCURS  10.
                  05  SC05-BIKO        PIC  X(20).
      *����
              03  SC05-SYOKEI-TBL.
      *���סݷ��
                  05  SC05-S-KENSU     PIC  X(02).
      *���סݿ�������
                  05  SC05-S-NISSU     PIC  X(03).
      *���ס��ݸ�������
                  05  SC05-S-TENSU     PIC  X(09).
      *���ס����ħ����
                  05  SC05-S-ITBFTN    PIC  X(09).
      *���ס�����������������ݴ���
                  05  SC05-S-SKINGK    PIC  X(09).
      *���ס������������������ɸ����ô��
                  05  SC05-S-SFTNGK    PIC  X(09).
      *���
              03  SC05-GOKEI-TBL.
      *��סݷ��
                  05  SC05-G-KENSU     PIC  X(05).
      *��סݿ�������
                  05  SC05-G-NISSU     PIC  X(05).
      *��ס��ݸ�������
                  05  SC05-G-TENSU     PIC  X(09).
      *��ס����ħ����
                  05  SC05-G-ITBFTN    PIC  X(09).
      *��ס�����������������ݴ���
                  05  SC05-G-SKINGK    PIC  X(09).
      *��ס������������������ɸ����ô��
                  05  SC05-G-SFTNGK    PIC  X(09).
