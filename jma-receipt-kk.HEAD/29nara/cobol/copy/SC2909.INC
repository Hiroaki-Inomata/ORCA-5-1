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
      *  01.00.01    ����         19/04/25  �������򥳥��Ⱥ��
      *                                     ����ǯ��˸�����ɲ�
      ************************************************************
        01  SC09.
      *�о�ǯ��
            03  SC09-PRTYM                           PIC X(16).
      *����ǯ����
            03  SC09-SEIYMD                          PIC X(22).
      *���ŵ��إ�����
            03  SC09-HOSPCD                          PIC X(07).
      *���ŵ��ؽ���
            03  SC09-ADRS                            PIC X(100).
      *���ŵ���̾
            03  SC09-HOSPNAME                        PIC X(100).
      *���߼�̾
            03  SC09-KAISETUNAME                     PIC X(40).
      *�����ֹ�
            03  SC09-TEL                             PIC X(15).
      *�ݸ���ʬ
            03  SC09-HKNKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *����ɽ��ʬ
            03  SC09-TENSUHYO
                                        PIC X(02)    OCCURS  5 TIMES.
      *���٥ơ��֥�
            03  SC09-MEISAI-TBL                      OCCURS  12 TIMES.
      *��ô���ֹ�
                05  SC09-FTNJANUM1                   PIC X(04).
                05  SC09-FTNJANUM2                   PIC X(08).
      *������ֹ�
                05  SC09-JKYSNUM                     PIC X(14).
      *�ݸ����ֹ�
                05  SC09-HKNJANUM                    PIC X(16).
      *��̾
                05  SC09-NAME                        PIC X(24).
      *��ǯ����
                05  SC09-TANJOYMD.
                    07  SC09-TANJOGG                 PIC X(02).
                    07  SC09-TANJOYY                 PIC X(04).
                    07  SC09-TANJOMM                 PIC X(04).
                    07  SC09-TANJODD                 PIC X(04).
      *������ʬ
                05  SC09-NYUGAIKBN
                                        PIC X(02)    OCCURS  2 TIMES.
      *���ճ��
                05  SC09-KYURATE                     PIC X(02).
      *����
                05  SC09-NISSU                       PIC X(02).
      *����
                05  SC09-TENSU                       PIC X(07).
      *������ô��
                05  SC09-ITBFTN                      PIC X(07).
      *Ĺ��
                05  SC09-TYOKI                       PIC X(02).
      *����ǯ��
                05  SC09-SRYYM.
                    07  SC09-SRYG                    PIC X(02).
                    07  SC09-SRYY                    PIC X(04).
                    07  SC09-SRYM                    PIC X(04).
      *����
                05  SC09-BIKO                        PIC X(10).
      *��ץơ��֥�
            03  SC09-GOKEI-AREA.
      *��ס�����
                05  SC09-GTENSU                      PIC X(09).
      *��ס�������ô��
                05  SC09-GITBFTN                     PIC X(09).
      *�ڡ�����
            03  SC09-PAGE                            PIC X(03).
      *���ڡ�����
            03  SC09-GPAGE                           PIC X(03).